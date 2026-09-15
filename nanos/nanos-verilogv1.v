`timescale 1ns / 1ps

// ------------------------------------------------------------------
// Clock Gate (Translated from BlackBox)
// ------------------------------------------------------------------
module ClockGate(
  input  clk_in,
  input  clk_en,
  output clk_out
);
  reg latch_q;
  initial latch_q = 1'b0;
  always @(*) begin
    if (!clk_in) latch_q = clk_en;
  end
  assign clk_out = clk_in & latch_q;
endmodule

// ------------------------------------------------------------------
// Core (64-bit CPU with power management)
// ------------------------------------------------------------------
module Core (
  input wire clock,
  input wire reset,
  output wire [63:0] io_iAddr,
  input  wire [63:0] io_iData,
  output wire        io_dREn,
  output wire [63:0] io_dRAddr,
  input  wire [63:0] io_dRData,
  input  wire        io_dWReady,
  output wire        io_dWEn,
  output wire [63:0] io_dWAddr,
  output wire [63:0] io_dWData,
  output wire [7:0]  io_dWMask,
  output wire        io_syscall,
  output wire [63:0] io_syscallCode,
  output wire [63:0] io_pc,
  input  wire        io_wake,
  output wire        io_idle
);

  reg [63:0] pc;
  reg [63:0] regs [0:15];
  reg cf, zf, sf, of_reg;
  reg idleReg;
  integer i;

  initial begin
    pc = 64'h0;
    for (i=0; i<16; i=i+1) regs[i] = 64'h0;
    regs[4] = 64'hfff8; // startRsp
    cf = 1'b0; zf = 1'b0; sf = 1'b0; of_reg = 1'b0;
    idleReg = 1'b0;
  end

  wire [63:0] instr = io_iData;
  wire [7:0] op = instr[63:56];
  wire [3:0] cond = instr[55:52];
  wire [1:0] dstMode = instr[51:50];
  wire [1:0] srcMode = instr[49:48];
  wire byteOp = instr[47];
  wire [1:0] memMode = instr[46:45];
  wire [3:0] dstReg = instr[44:41];
  wire [3:0] srcReg = instr[40:37];
  wire [31:0] imm32 = instr[36:5];
  wire [63:0] imm64 = {{32{imm32[31]}}, imm32};

  wire aluOp = (op == 8'h02) || (op == 8'h03) || (op == 8'h04) || (op == 8'h05) || 
               (op == 8'h06) || (op == 8'h08) || (op == 8'h09) || (op == 8'h0a) || (op == 8'h10);
  wire memWriteOp = (op == 8'h01) || aluOp;

  function [63:0] effAddr;
    input [63:0] base;
    begin
      if (memMode == 2'b01) effAddr = pc + imm64;
      else if (memMode == 2'b10) effAddr = imm64;
      else effAddr = base + imm64;
    end
  endfunction

  wire [63:0] srcAddr = effAddr(regs[srcReg]);
  wire [63:0] dstAddr = effAddr(regs[dstReg]);

  reg [63:0] memReadAddr;
  reg memReadEn;

  always @(*) begin
    memReadAddr = 64'h0;
    memReadEn = 1'b0;
    if (op == 8'h0e) begin // Op.RET
      memReadEn = 1'b1;
      memReadAddr = regs[4];
    end else if (srcMode == 2'b11) begin // Mode.SRC_MEM
      memReadEn = 1'b1;
      memReadAddr = srcAddr;
    end else if (dstMode == 2'b10 && (aluOp || op == 8'h07)) begin // Mode.DST_MEM && (aluOp || Op.CMP)
      memReadEn = 1'b1;
      memReadAddr = dstAddr;
    end
  end

  assign io_dREn = memReadEn && !idleReg;
  assign io_dRAddr = memReadAddr;

  wire [2:0] rLane = io_dRAddr[2:0];
  wire [7:0] rByte = (io_dRData >> {rLane, 3'b000})[7:0];
  wire [63:0] memRVal = byteOp ? {56'h0, rByte} : io_dRData;

  wire [63:0] regRight = regs[srcReg];
  wire [63:0] right = (srcMode == 2'b01) ? (byteOp ? {56'h0, regRight[7:0]} : regRight) :
                      (srcMode == 2'b10) ? imm64 :
                      (srcMode == 2'b11) ? memRVal : 64'h0;

  wire [63:0] left = (dstMode == 2'b10) ? memRVal : regs[dstReg];

  reg [63:0] alu;
  wire [63:0] sum = left + right;
  wire [63:0] diff = left - right;
  wire [5:0] amt = right[5:0];
  wire [63:0] shl = (left << amt);
  wire [63:0] shr = left >> amt;
  wire signed [63:0] left_signed = left;
  wire signed [63:0] sar_signed = left_signed >>> amt;
  wire [63:0] sar = sar_signed;
  wire signed [63:0] right_signed = right;
  wire signed [127:0] prod = left_signed * right_signed;

  always @(*) begin
    alu = 64'h0;
    if (op == 8'h01) alu = right;
    else if (op == 8'h02) alu = sum;
    else if (op == 8'h03) alu = left | right;
    else if (op == 8'h04) alu = left & right;
    else if (op == 8'h05) alu = diff;
    else if (op == 8'h06) alu = left ^ right;
    else if (op == 8'h07) alu = diff;
    else if (op == 8'h08) alu = shl;
    else if (op == 8'h09) alu = shr;
    else if (op == 8'h0a) alu = sar;
    else if (op == 8'h10) alu = prod[63:0];
  end

  wire [64:0] addFull = {1'b0, left} + {1'b0, right};
  wire addCarry = addFull[64];
  wire subBorrow = left < right;
  wire ofAdd = (left[63] == right[63]) && (sum[63] != left[63]);
  wire ofSub = (left[63] != right[63]) && (diff[63] != left[63]);

  reg flagsWen, ncf, nzf, nsf, nof;

  always @(*) begin
    flagsWen = 1'b0;
    ncf = 1'b0;
    nzf = 1'b0;
    nsf = 1'b0;
    nof = 1'b0;

    if (op == 8'h02) begin
      flagsWen = 1'b1;
      ncf = addCarry;
      nzf = (sum == 64'h0);
      nsf = sum[63];
      nof = ofAdd;
    end else if (op == 8'h05 || op == 8'h07) begin
      flagsWen = 1'b1;
      ncf = subBorrow;
      nzf = (diff == 64'h0);
      nsf = diff[63];
      nof = ofSub;
    end else if (op == 8'h04 || op == 8'h03 || op == 8'h06) begin
      flagsWen = 1'b1;
      ncf = 1'b0;
      nzf = (alu == 64'h0);
      nsf = alu[63];
      nof = 1'b0;
    end else if (op == 8'h08 || op == 8'h09 || op == 8'h0a) begin
      flagsWen = 1'b1;
      ncf = 1'b0;
      nzf = (alu == 64'h0);
      nsf = alu[63];
      nof = 1'b0;
    end
  end

  reg condTaken;
  always @(*) begin
    condTaken = 1'b0;
    if (cond == 4'd0) condTaken = zf;
    else if (cond == 4'd1) condTaken = !zf;
    else if (cond == 4'd2) condTaken = (sf != of_reg);
    else if (cond == 4'd3) condTaken = zf || (sf != of_reg);
    else if (cond == 4'd4) condTaken = !zf && (sf == of_reg);
    else if (cond == 4'd5) condTaken = (sf == of_reg);
    else if (cond == 4'd6) condTaken = cf;
    else if (cond == 4'd7) condTaken = cf || zf;
    else if (cond == 4'd8) condTaken = !cf && !zf;
    else if (cond == 4'd9) condTaken = !cf;
    else if (cond == 4'd10) condTaken = sf;
    else if (cond == 4'd11) condTaken = !sf;
  end

  reg [63:0] nextPc;
  wire [63:0] target = pc + imm64;
  wire branchTaken = (op == 8'h0b) || ((op == 8'h0c) && condTaken);

  always @(*) begin
    nextPc = pc + 64'h8;
    if (branchTaken) nextPc = target;
    if (op == 8'h0d) nextPc = target;
    if (op == 8'h0e) nextPc = io_dRData;
  end

  reg regWen;
  reg [3:0] regWaddr;
  reg [63:0] regWdata;

  wire canWriteReg = (dstMode == 2'b01) && 
                     (op != 8'h07) && (op != 8'h0b) && (op != 8'h0c) && 
                     (op != 8'h0d) && (op != 8'h0e) && (op != 8'h0f) && 
                     (op != 8'h11) && (op != 8'h12) && (op != 8'h20);

  wire [63:0] dstRegVal = regs[dstReg];

  always @(*) begin
    regWen = 1'b0;
    regWaddr = 4'h0;
    regWdata = 64'h0;

    if (canWriteReg) begin
      regWen = 1'b1;
      regWaddr = dstReg;
      regWdata = byteOp ? {dstRegVal[63:8], alu[7:0]} : alu;
    end

    if (op == 8'h0d) begin
      regWen = 1'b1;
      regWaddr = 4'h4;
      regWdata = regs[4] - 64'h8;
    end

    if (op == 8'h0e) begin
      regWen = 1'b1;
      regWaddr = 4'h4;
      regWdata = regs[4] + 64'h8;
    end
  end

  reg memWEn;
  reg [63:0] memWAddr;
  reg [63:0] memWData;
  reg [7:0] memWMask;

  wire [63:0] pushAddr = regs[4] - 64'h8;

  always @(*) begin
    memWEn = 1'b0;
    memWAddr = 64'h0;
    memWData = 64'h0;
    memWMask = 8'h00;

    if (dstMode == 2'b10 && memWriteOp) begin
      wire [63:0] storeData = (op == 8'h01) ? right : alu;
      memWEn = 1'b1;
      memWAddr = dstAddr & ~64'h7;
      if (byteOp) begin
        wire [2:0] lane = dstAddr[2:0];
        memWData = ({56'h0, storeData[7:0]} << {lane, 3'b000});
        memWMask = (8'h01 << lane);
      end else begin
        memWData = storeData;
        memWMask = 8'hff;
      end
    end

    if (op == 8'h0d) begin
      memWEn = 1'b1;
      memWAddr = pushAddr & ~64'h7;
      memWData = pc + 64'h8;
      memWMask = 8'hff;
    end
  end

  wire memStall = memWEn && !io_dWReady && !idleReg;
  wire wakeNow = idleReg && io_wake;

  assign io_iAddr = pc;
  assign io_pc = pc;
  assign io_dWEn = memWEn && !idleReg;
  assign io_dWAddr = memWAddr;
  assign io_dWData = memWData;
  assign io_dWMask = memWMask;
  assign io_syscall = (op == 8'h0f) && !memStall && !idleReg;
  assign io_syscallCode = regs[0];
  assign io_idle = idleReg;

  always @(posedge clock) begin
    if (reset) begin
      pc <= 64'h0;
      for (i=0; i<16; i=i+1) regs[i] <= 64'h0;
      regs[4] <= 64'hfff8;
      cf <= 1'b0; zf <= 1'b0; sf <= 1'b0; of_reg <= 1'b0;
      idleReg <= 1'b0;
    end else begin
      if (wakeNow) begin
        idleReg <= 1'b0;
        pc <= pc + 64'h8;
      end else if (!idleReg) begin
        if (!memStall) begin
          pc <= nextPc;
          if (regWen) begin
            regs[regWaddr] <= regWdata;
          end
          if (flagsWen) begin
            cf <= ncf;
            zf <= nzf;
            sf <= nsf;
            of_reg <= nof;
          end
          if ((op == 8'h12) && !io_wake) begin
            idleReg <= 1'b1;
            pc <= pc;
          end
        end
      end
    end
  end
endmodule

// ------------------------------------------------------------------
// Power Management Unit (Parameterized)
// ------------------------------------------------------------------
module PowerManagementUnit #(
  parameter NUM_CORES = 1
) (
  input wire clock,
  input wire reset,
  input wire [NUM_CORES-1:0] io_coreIdle,
  input wire io_wakeReq,
  input wire io_allowSleep,
  output reg [NUM_CORES-1:0] io_coreClockEn,
  output wire [NUM_CORES-1:0] io_coreWake,
  output wire io_clusterClockEn,
  output wire io_powerGate,
  output wire [1:0] io_acpiState
);

  reg [1:0] state;
  reg wakePending;
  reg [6:0] idleCount;

  wire allIdle = &io_coreIdle; // Reduction AND: true if all cores are idle
  wire wakeActive = io_wakeReq || wakePending;
  integer j;

  always @(posedge clock) begin
    if (reset) begin
      state <= 2'h0;
      wakePending <= 1'b0;
      idleCount <= 7'h0;
    end else begin
      if (io_wakeReq) wakePending <= 1'b1;
      else if (!allIdle) wakePending <= 1'b0;

      if (allIdle && !wakeActive) begin
        if (idleCount < 7'd64) idleCount <= idleCount + 7'd1;
        if (idleCount >= 7'd16) state <= 2'h1;
        if (io_allowSleep && idleCount >= 7'd64) state <= 2'h2;
      end else begin
        state <= 2'h0;
        idleCount <= 7'h0;
      end
    end
  end

  always @(*) begin
    for (j=0; j<NUM_CORES; j=j+1) begin
      io_coreClockEn[j] = (state == 2'h0) || wakeActive || !io_coreIdle[j];
    end
  end

  assign io_coreWake = {NUM_CORES{wakeActive}};
  assign io_clusterClockEn = (state == 2'h0) || wakeActive;
  assign io_powerGate = (state == 2'h2) && io_allowSleep && !wakeActive;
  assign io_acpiState = state;

endmodule

// ------------------------------------------------------------------
// Main Top (Fully Parameterized)
// ------------------------------------------------------------------
module MainTop #(
  parameter NUM_CORES = 2,
  parameter IMEM_WORDS = 4096,
  parameter DMEM_WORDS = 8192
) (
  input wire clock,
  input wire reset,
  input wire io_wake,
  input wire io_allowSleep,
  output wire [1:0] io_acpiState,
  output wire io_clusterClockEnable,
  output wire io_clusterPowerGate,
  output wire [NUM_CORES-1:0] io_coreClockEnable,
  output wire [NUM_CORES-1:0] io_corePowerGate,
  output wire [NUM_CORES*64-1:0] io_pc,
  output wire [NUM_CORES-1:0] io_syscall,
  output wire [NUM_CORES*64-1:0] io_syscallCode,
  input wire io_imemInitEn,
  input wire [63:0] io_imemInitAddr,
  input wire [63:0] io_imemInitData,
  input wire io_dmemInitEn,
  input wire [63:0] io_dmemInitAddr,
  input wire [63:0] io_dmemInitData,
  input wire [7:0] io_dmemInitMask
);

  localparam IMEM_ADDR_WIDTH = (IMEM_WORDS > 1) ? $clog2(IMEM_WORDS) : 1;
  localparam DMEM_ADDR_WIDTH = (DMEM_WORDS > 1) ? $clog2(DMEM_WORDS) : 1;
  localparam ARB_WIDTH = (NUM_CORES > 1) ? $clog2(NUM_CORES) : 1;

  reg [63:0] imem [0:IMEM_WORDS-1];
  reg [63:0] dmem [0:DMEM_WORDS-1];

  wire [NUM_CORES-1:0] coreIdle;
  wire [NUM_CORES-1:0] coreClockEn;
  wire [NUM_CORES-1:0] coreWake;
  wire clusterClockEn;
  wire powerGate;
  wire [1:0] acpiState;

  PowerManagementUnit #(
    .NUM_CORES(NUM_CORES)
  ) pmu (
    .clock(clock),
    .reset(reset),
    .io_coreIdle(coreIdle),
    .io_wakeReq(io_wake),
    .io_allowSleep(io_allowSleep),
    .io_coreClockEn(coreClockEn),
    .io_coreWake(coreWake),
    .io_clusterClockEn(clusterClockEn),
    .io_powerGate(powerGate),
    .io_acpiState(acpiState)
  );

  assign io_acpiState = acpiState;
  assign io_clusterClockEnable = clusterClockEn;
  assign io_clusterPowerGate = powerGate;

  wire [NUM_CORES-1:0] dWEn;
  wire [NUM_CORES*64-1:0] dWAddr;
  wire [NUM_CORES*64-1:0] dWData;
  wire [NUM_CORES*8-1:0] dWMask;
  wire [NUM_CORES-1:0] dWReady;

  wire [NUM_CORES*64-1:0] core_iAddr;
  wire [NUM_CORES*64-1:0] core_iData;
  wire [NUM_CORES-1:0] core_dREn;
  wire [NUM_CORES*64-1:0] core_dRAddr;
  wire [NUM_CORES*64-1:0] core_dRData;
  wire [NUM_CORES-1:0] core_dWEn;
  wire [NUM_CORES*64-1:0] core_dWAddr;
  wire [NUM_CORES*64-1:0] core_dWData;
  wire [NUM_CORES*8-1:0] core_dWMask;
  wire [NUM_CORES-1:0] core_syscall;
  wire [NUM_CORES*64-1:0] core_syscallCode;
  wire [NUM_CORES-1:0] core_idle;

  genvar k;
  generate
    for (k=0; k<NUM_CORES; k=k+1) begin : gen_core
      wire coreClock;
      wire [63:0] core_iAddr_w;
      wire [63:0] core_iData_w;
      wire [63:0] core_dRAddr_w;
      wire [63:0] core_dRData_w;
      wire [63:0] core_dWAddr_w;
      wire [63:0] core_dWData_w;
      wire [7:0]  core_dWMask_w;
      wire [63:0] core_syscallCode_w;
      wire [63:0] core_pc_w;
      wire core_syscall_w;
      wire core_idle_w;

      // Map wide vectors to local per-core wires
      assign core_iAddr_w = core_iAddr[k*64 +: 64];
      assign core_iData[k*64 +: 64] = core_iData_w;
      assign core_dRAddr_w = core_dRAddr[k*64 +: 64];
      assign core_dRData[k*64 +: 64] = core_dRData_w;
      assign core_dWAddr[k*64 +: 64] = core_dWAddr_w;
      assign core_dWData[k*64 +: 64] = core_dWData_w;
      assign core_dWMask[k*8 +: 8] = core_dWMask_w;
      assign core_syscallCode[k*64 +: 64] = core_syscallCode_w;
      
      // Map outputs to top-level ports
      assign io_pc[k*64 +: 64] = core_pc_w;
      assign io_syscall[k] = core_syscall_w;
      assign io_syscallCode[k*64 +: 64] = core_syscallCode_w;
      assign coreIdle[k] = core_idle_w;
      assign io_coreClockEnable[k] = coreClockEn[k] || reset;
      assign io_corePowerGate[k] = powerGate && core_idle_w;

      ClockGate cg (
        .clk_in(clock),
        .clk_en(coreClockEn[k] || reset),
        .clk_out(coreClock)
      );

      Core core_inst (
        .clock(coreClock),
        .reset(reset),
        .io_iAddr(core_iAddr_w),
        .io_iData(core_iData_w),
        .io_dREn(core_dREn[k]),
        .io_dRAddr(core_dRAddr_w),
        .io_dRData(core_dRData_w),
        .io_dWReady(dWReady[k]),
        .io_dWEn(core_dWEn[k]),
        .io_dWAddr(core_dWAddr_w),
        .io_dWData(core_dWData_w),
        .io_dWMask(core_dWMask_w),
        .io_syscall(core_syscall_w),
        .io_syscallCode(core_syscallCode_w),
        .io_pc(core_pc_w),
        .io_wake(coreWake[k]),
        .io_idle(core_idle_w)
      );

      // Instruction memory read
      wire [IMEM_ADDR_WIDTH-1:0] imem_idx = (core_iAddr_w >> 3) & (IMEM_WORDS - 1);
      assign core_iData_w = imem[imem_idx];

      // Data memory read
      wire [DMEM_ADDR_WIDTH-1:0] dmem_r_idx = (core_dRAddr_w >> 3) & (DMEM_WORDS - 1);
      assign core_dRData_w = dmem[dmem_r_idx];
    end
  endgenerate

  // ------------------------------------------------------------------
  // Shared data write arbitration (Generic Round Robin)
  // ------------------------------------------------------------------
  reg [ARB_WIDTH-1:0] rr_priority;
  reg [ARB_WIDTH-1:0] arb_winner;
  reg found;
  integer n;

  always @(*) begin
    found = 1'b0;
    arb_winner = {ARB_WIDTH{1'b0}};
    
    // Search from current priority to end
    for (n = rr_priority; n < NUM_CORES; n = n + 1) begin
      if (!found && dWEn[n]) begin
        found = 1'b1;
        arb_winner = n[ARB_WIDTH-1:0];
      end
    end
    
    // Wrap around: search from start to current priority - 1
    for (n = 0; n < rr_priority; n = n + 1) begin
      if (!found && dWEn[n]) begin
        found = 1'b1;
        arb_winner = n[ARB_WIDTH-1:0];
      end
    end
  end

  integer p;
  always @(*) begin
    for (p = 0; p < NUM_CORES; p = p + 1) begin
      dWReady[p] = found && (arb_winner == p[ARB_WIDTH-1:0]) && !io_dmemInitEn && !powerGate;
    end
  end

  wire arb_fire = found && !io_dmemInitEn && !powerGate;

  always @(posedge clock) begin
    if (reset) rr_priority <= {ARB_WIDTH{1'b0}};
    else begin
      if (arb_fire) begin
        if (arb_winner == NUM_CORES - 1) rr_priority <= {ARB_WIDTH{1'b0}};
        else rr_priority <= arb_winner + 1'b1;
      end
    end
  end

  wire [63:0] outReq_addr = dWAddr[arb_winner*64 +: 64];
  wire [63:0] outReq_data = dWData[arb_winner*64 +: 64];
  wire [7:0]  outReq_mask = dWMask[arb_winner*8 +: 8];

  wire [DMEM_ADDR_WIDTH-1:0] wIdx = (outReq_addr >> 3) & (DMEM_WORDS - 1);
  wire [DMEM_ADDR_WIDTH-1:0] initIdx = (io_dmemInitAddr >> 3) & (DMEM_WORDS - 1);
  wire [IMEM_ADDR_WIDTH-1:0] imemInitIdx = (io_imemInitAddr >> 3) & (IMEM_WORDS - 1);

  integer b;
  always @(posedge clock) begin
    if (io_imemInitEn) begin
      imem[imemInitIdx] <= io_imemInitData;
    end
    
    if (io_dmemInitEn) begin
      for (b=0; b<8; b=b+1) begin
        if (io_dmemInitMask[b]) begin
          dmem[initIdx][b*8 +: 8] <= io_dmemInitData[b*8 +: 8];
        end
      end
    end else if (arb_fire) begin
      for (b=0; b<8; b=b+1) begin
        if (outReq_mask[b]) begin
          dmem[wIdx][b*8 +: 8] <= outReq_data[b*8 +: 8];
        end
      end
    end
  end

endmodule