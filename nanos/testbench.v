`timescale 1ns / 1ps
// iverilog -g2012 -o sim MainTop_Param.v testbench.v
// vvp sim
module testbench;
  reg clock;
  reg reset;
  reg io_wake;
  reg io_allowSleep;
  reg io_imemInitEn;
  reg [63:0] io_imemInitAddr;
  reg [63:0] io_imemInitData;
  reg io_dmemInitEn;
  reg [63:0] io_dmemInitAddr;
  reg [63:0] io_dmemInitData;
  reg [7:0] io_dmemInitMask;

  wire [1:0] io_acpiState;
  wire io_clusterClockEnable;
  wire io_clusterPowerGate;
  wire [1:0] io_coreClockEnable;
  wire [1:0] io_corePowerGate;
  wire [127:0] io_pc;
  wire [1:0] io_syscall;
  wire [127:0] io_syscallCode;

  // Instantiate the Unit Under Test (UUT)
  MainTop #(
    .NUM_CORES(2),
    .IMEM_WORDS(4096),
    .DMEM_WORDS(8192)
  ) uut (
    .clock(clock),
    .reset(reset),
    .io_wake(io_wake),
    .io_allowSleep(io_allowSleep),
    .io_acpiState(io_acpiState),
    .io_clusterClockEnable(io_clusterClockEnable),
    .io_clusterPowerGate(io_clusterPowerGate),
    .io_coreClockEnable(io_coreClockEnable),
    .io_corePowerGate(io_corePowerGate),
    .io_pc(io_pc),
    .io_syscall(io_syscall),
    .io_syscallCode(io_syscallCode),
    .io_imemInitEn(io_imemInitEn),
    .io_imemInitAddr(io_imemInitAddr),
    .io_imemInitData(io_imemInitData),
    .io_dmemInitEn(io_dmemInitEn),
    .io_dmemInitAddr(io_dmemInitAddr),
    .io_dmemInitData(io_dmemInitData),
    .io_dmemInitMask(io_dmemInitMask)
  );

  // Clock generation (100 MHz)
  initial begin
    clock = 0;
    forever #5 clock = ~clock;
  end

  // Stimulus
  initial begin
    // Initialize inputs
    reset = 1;
    io_wake = 0;
    io_allowSleep = 0;
    io_imemInitEn = 0;
    io_imemInitAddr = 0;
    io_imemInitData = 0;
    io_dmemInitEn = 0;
    io_dmemInitAddr = 0;
    io_dmemInitData = 0;
    io_dmemInitMask = 0;

    // Wait a bit
    #10;

    // Load a MOV instruction into instruction memory while reset is high
    // Instruction: MOV r1, 0x12345678
    // Encoding: op=0x01, dstMode=1(REG), srcMode=2(IMM), dstReg=1, imm32=0x12345678
    @(posedge clock);
    io_imemInitEn = 1;
    io_imemInitAddr = 64'h0;
    io_imemInitData = 64'h01060202468ACF00;
    @(posedge clock);
    io_imemInitEn = 0;

    // Wait a bit more, then release reset
    #10;
    reset = 0;

    // Run for a while to see PC increment and register update
    #200;

    // Finish simulation
    $finish;
  end

  // Waveform dump for GTKWave
  initial begin
    $dumpfile("MainTop_Param.vcd");
    $dumpvars(0, testbench);
  end

endmodule
