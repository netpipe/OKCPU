// main.scala
//
// Multicore 64-bit CPU with ACPI-like power management.
//
// Features:
//   - configurable core count
//   - hlt instruction
//   - idle detection
//   - clock gating
//   - sleep / power-gating control outputs
//   - external wake input
//
// ACPI-like states used here:
//   0 = Running
//   1 = Idle / clock-gated
//   2 = Sleep / power-gating requested
//   3 = Reserved / Off
//
// Note:
// True power gating requires physical library cells, isolation, retention,
// and power-switch control. This design provides the control signals.

import chisel3._
import chisel3.util._
import circt.stage.ChiselStage

// ------------------------------------------------------------------
// Opcodes
// ------------------------------------------------------------------

object Op {
  val MOV     = 0x01.U(8.W)
  val ADD     = 0x02.U(8.W)
  val OR      = 0x03.U(8.W)
  val AND     = 0x04.U(8.W)
  val SUB     = 0x05.U(8.W)
  val XOR     = 0x06.U(8.W)
  val CMP     = 0x07.U(8.W)
  val SHL     = 0x08.U(8.W)
  val SHR     = 0x09.U(8.W)
  val SAR     = 0x0a.U(8.W)
  val JMP     = 0x0b.U(8.W)
  val JCC     = 0x0c.U(8.W)
  val CALL    = 0x0d.U(8.W)
  val RET     = 0x0e.U(8.W)
  val SYSCALL = 0x0f.U(8.W)
  val IMUL    = 0x10.U(8.W)
  val IDIV    = 0x11.U(8.W)

  // New power-management instruction.
  // hlt puts the core into idle until wake is asserted.
  val HLT     = 0x12.U(8.W)

  // db is normally an assembler directive. If executed, treat as nop.
  val DB      = 0x20.U(8.W)
}

object Mode {
  // dstMode
  val DST_NONE = 0.U(2.W)
  val DST_REG  = 1.U(2.W)
  val DST_MEM  = 2.U(2.W)

  // srcMode
  val SRC_NONE = 0.U(2.W)
  val SRC_REG  = 1.U(2.W)
  val SRC_IMM  = 2.U(2.W)
  val SRC_MEM  = 3.U(2.W)

  // memMode
  val MEM_BASE = 0.U(2.W) // [base + disp]
  val MEM_RIP  = 1.U(2.W) // [rip + disp]
  val MEM_ABS  = 2.U(2.W) // [disp]
}

// ------------------------------------------------------------------
// Core
// ------------------------------------------------------------------

class Core(
  startPc: BigInt = 0,
  startRsp: BigInt = 0xfff8,
  enableDiv: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val iAddr = Output(UInt(64.W))
    val iData = Input(UInt(64.W))

    val dREn   = Output(Bool())
    val dRAddr = Output(UInt(64.W))
    val dRData = Input(UInt(64.W))

    val dWReady = Input(Bool())
    val dWEn    = Output(Bool())
    val dWAddr  = Output(UInt(64.W))
    val dWData  = Output(UInt(64.W))
    val dWMask  = Output(UInt(8.W))

    val syscall     = Output(Bool())
    val syscallCode = Output(UInt(64.W))
    val pc          = Output(UInt(64.W))

    // Power management
    val wake = Input(Bool())
    val idle = Output(Bool())
  })

  // ------------------------------------------------------------------
  // State
  // ------------------------------------------------------------------

  val pc = RegInit(startPc.U(64.W))

  val initRegs = VecInit(Seq.tabulate(16)(i =>
    if (i == 4) startRsp.U(64.W) else 0.U(64.W)
  ))
  val regs = RegInit(initRegs)

  val cf = RegInit(false.B)
  val zf = RegInit(false.B)
  val sf = RegInit(false.B)
  val of = RegInit(false.B)

  val idleReg = RegInit(false.B)
  io.idle := idleReg

  // ------------------------------------------------------------------
  // Decode
  // ------------------------------------------------------------------

  val instr   = io.iData
  val op      = instr(63, 56)
  val cond    = instr(55, 52)
  val dstMode = instr(51, 50)
  val srcMode = instr(49, 48)
  val byteOp  = instr(47)
  val memMode = instr(46, 45)
  val dstReg  = instr(44, 41)
  val srcReg  = instr(40, 37)
  val imm32   = instr(36, 5)

  val imm64 = Cat(Fill(32, imm32(31)), imm32)

  val aluOp =
    op === Op.ADD ||
    op === Op.OR  ||
    op === Op.AND ||
    op === Op.SUB ||
    op === Op.XOR ||
    op === Op.SHL ||
    op === Op.SHR ||
    op === Op.SAR ||
    op === Op.IMUL

  val memWriteOp = op === Op.MOV || aluOp

  // ------------------------------------------------------------------
  // Effective address
  // ------------------------------------------------------------------

  def effAddr(base: UInt): UInt = {
    Mux(
      memMode === Mode.MEM_RIP,
      pc + imm64,
      Mux(
        memMode === Mode.MEM_ABS,
        imm64,
        base + imm64
      )
    )
  }

  val srcAddr = effAddr(regs(srcReg))
  val dstAddr = effAddr(regs(dstReg))

  // ------------------------------------------------------------------
  // Data memory read
  // ------------------------------------------------------------------

  val memReadAddr = Wire(UInt(64.W))
  val memReadEn   = Wire(Bool())

  memReadAddr := 0.U
  memReadEn   := false.B

  when(op === Op.RET) {
    memReadEn   := true.B
    memReadAddr := regs(4)
  }.elsewhen(srcMode === Mode.SRC_MEM) {
    memReadEn   := true.B
    memReadAddr := srcAddr
  }.elsewhen(dstMode === Mode.DST_MEM && (aluOp || op === Op.CMP)) {
    memReadEn   := true.B
    memReadAddr := dstAddr
  }

  io.dREn   := memReadEn && !idleReg
  io.dRAddr := memReadAddr

  val rLane = io.dRAddr(2, 0)
  val rByte = (io.dRData >> (rLane << 3))(7, 0)
  val memRVal = Mux(byteOp, Cat(0.U(56.W), rByte), io.dRData)

  // ------------------------------------------------------------------
  // Operands
  // ------------------------------------------------------------------

  val regRight = regs(srcReg)

  val right =
    Mux(
      srcMode === Mode.SRC_REG,
      Mux(byteOp, Cat(0.U(56.W), regRight(7, 0)), regRight),
      Mux(
        srcMode === Mode.SRC_IMM,
        imm64,
        Mux(
          srcMode === Mode.SRC_MEM,
          memRVal,
          0.U
        )
      )
    )

  val left =
    Mux(
      dstMode === Mode.DST_MEM,
      memRVal,
      regs(dstReg)
    )

  // ------------------------------------------------------------------
  // ALU
  // ------------------------------------------------------------------

  val alu  = Wire(UInt(64.W))
  val sum  = left + right
  val diff = left - right

  val amt = right(5, 0)

  val shl = (left << amt)(63, 0)
  val shr = left >> amt
  val sar = (left.asSInt >> amt).asUInt

  val prod = (left.asSInt * right.asSInt).asUInt

  alu := 0.U

  when(op === Op.MOV) {
    alu := right
  }.elsewhen(op === Op.ADD) {
    alu := sum
  }.elsewhen(op === Op.OR) {
    alu := left | right
  }.elsewhen(op === Op.AND) {
    alu := left & right
  }.elsewhen(op === Op.SUB) {
    alu := diff
  }.elsewhen(op === Op.XOR) {
    alu := left ^ right
  }.elsewhen(op === Op.CMP) {
    alu := diff
  }.elsewhen(op === Op.SHL) {
    alu := shl
  }.elsewhen(op === Op.SHR) {
    alu := shr
  }.elsewhen(op === Op.SAR) {
    alu := sar
  }.elsewhen(op === Op.IMUL) {
    alu := prod(63, 0)
  }

  // ------------------------------------------------------------------
  // Optional IDIV
  // ------------------------------------------------------------------

  val divQ = Wire(UInt(64.W))
  val divR = Wire(UInt(64.W))

  if (enableDiv) {
    val dividend = regs(0)
    val divisor  = right

    val dZero = divisor === 0.U
    val dNeg1 = divisor === Fill(64, 1.U)
    val nMin  = dividend === Cat(1.U(1.W), 0.U(63.W))

    val special = dZero || (nMin && dNeg1)

    val safeDivisor = Mux(special, 1.U(64.W), divisor)

    val qS = dividend.asSInt / safeDivisor.asSInt
    val rS = dividend.asSInt % safeDivisor.asSInt

    divQ := Mux(special, 0.U, qS.asUInt)
    divR := Mux(dZero, dividend, Mux(nMin && dNeg1, 0.U, rS.asUInt))
  } else {
    divQ := 0.U
    divR := 0.U
  }

  // ------------------------------------------------------------------
  // Flags
  // ------------------------------------------------------------------

  val addFull   = Cat(0.U(1.W), left) + Cat(0.U(1.W), right)
  val addCarry  = addFull(64)
  val subBorrow = left < right
  val ofAdd     = (left(63) === right(63)) && (sum(63) =/= left(63))
  val ofSub     = (left(63) =/= right(63)) && (diff(63) =/= left(63))

  val flagsWen = Wire(Bool())
  val ncf      = Wire(Bool())
  val nzf      = Wire(Bool())
  val nsf      = Wire(Bool())
  val nof      = Wire(Bool())

  flagsWen := false.B
  ncf      := false.B
  nzf      := false.B
  nsf      := false.B
  nof      := false.B

  when(op === Op.ADD) {
    flagsWen := true.B
    ncf := addCarry
    nzf := sum === 0.U
    nsf := sum(63)
    nof := ofAdd
  }.elsewhen(op === Op.SUB || op === Op.CMP) {
    flagsWen := true.B
    ncf := subBorrow
    nzf := diff === 0.U
    nsf := diff(63)
    nof := ofSub
  }.elsewhen(op === Op.AND || op === Op.OR || op === Op.XOR) {
    flagsWen := true.B
    ncf := false.B
    nzf := alu === 0.U
    nsf := alu(63)
    nof := false.B
  }.elsewhen(op === Op.SHL || op === Op.SHR || op === Op.SAR) {
    flagsWen := true.B
    ncf := false.B
    nzf := alu === 0.U
    nsf := alu(63)
    nof := false.B
  }

  // ------------------------------------------------------------------
  // Condition codes
  // ------------------------------------------------------------------

  val condTaken = Wire(Bool())
  condTaken := false.B

  when(cond === 0.U) {
    condTaken := zf
  }.elsewhen(cond === 1.U) {
    condTaken := !zf
  }.elsewhen(cond === 2.U) {
    condTaken := sf =/= of
  }.elsewhen(cond === 3.U) {
    condTaken := zf || (sf =/= of)
  }.elsewhen(cond === 4.U) {
    condTaken := !zf && (sf === of)
  }.elsewhen(cond === 5.U) {
    condTaken := sf === of
  }.elsewhen(cond === 6.U) {
    condTaken := cf
  }.elsewhen(cond === 7.U) {
    condTaken := cf || zf
  }.elsewhen(cond === 8.U) {
    condTaken := !cf && !zf
  }.elsewhen(cond === 9.U) {
    condTaken := !cf
  }.elsewhen(cond === 10.U) {
    condTaken := sf
  }.elsewhen(cond === 11.U) {
    condTaken := !sf
  }

  // ------------------------------------------------------------------
  // PC next
  // ------------------------------------------------------------------

  val nextPc = Wire(UInt(64.W))
  val target = pc + imm64

  nextPc := pc + 8.U

  val branchTaken =
    op === Op.JMP ||
    (op === Op.JCC && condTaken)

  when(branchTaken) {
    nextPc := target
  }

  when(op === Op.CALL) {
    nextPc := target
  }

  when(op === Op.RET) {
    nextPc := io.dRData
  }

  // ------------------------------------------------------------------
  // Register write control
  // ------------------------------------------------------------------

  val regWen   = Wire(Bool())
  val regWaddr = Wire(UInt(4.W))
  val regWdata = Wire(UInt(64.W))

  regWen   := false.B
  regWaddr := 0.U
  regWdata := 0.U

  val canWriteReg =
    (dstMode === Mode.DST_REG) &&
    op =/= Op.CMP &&
    op =/= Op.JMP &&
    op =/= Op.JCC &&
    op =/= Op.CALL &&
    op =/= Op.RET &&
    op =/= Op.SYSCALL &&
    op =/= Op.IDIV &&
    op =/= Op.HLT &&
    op =/= Op.DB

  when(canWriteReg) {
    regWen   := true.B
    regWaddr := dstReg

    regWdata := Mux(
      byteOp,
      Cat(regs(dstReg)(63, 8), alu(7, 0)),
      alu
    )
  }

  val pushAddr = regs(4) - 8.U

  when(op === Op.CALL) {
    regWen   := true.B
    regWaddr := 4.U
    regWdata := pushAddr
  }

  when(op === Op.RET) {
    regWen   := true.B
    regWaddr := 4.U
    regWdata := regs(4) + 8.U
  }

  // ------------------------------------------------------------------
  // Data memory write control
  // ------------------------------------------------------------------

  val memWEn   = Wire(Bool())
  val memWAddr = Wire(UInt(64.W))
  val memWData = Wire(UInt(64.W))
  val memWMask = Wire(UInt(8.W))

  memWEn   := false.B
  memWAddr := 0.U
  memWData := 0.U
  memWMask := 0.U

  when(dstMode === Mode.DST_MEM && memWriteOp) {
    val storeData = Mux(op === Op.MOV, right, alu)

    memWEn   := true.B
    memWAddr := dstAddr & ~7.U(64.W)

    when(byteOp) {
      val lane = dstAddr(2, 0)

      memWData := (Cat(0.U(56.W), storeData(7, 0)) << (lane << 3))(63, 0)
      memWMask := (1.U(8.W) << lane)(7, 0)
    }.otherwise {
      memWData := storeData
      memWMask := 0xff.U(8.W)
    }
  }

  when(op === Op.CALL) {
    memWEn   := true.B
    memWAddr := pushAddr & ~7.U(64.W)
    memWData := pc + 8.U
    memWMask := 0xff.U(8.W)
  }

  // ------------------------------------------------------------------
  // Stall / idle control
  // ------------------------------------------------------------------

  val memStall = memWEn && !io.dWReady && !idleReg
  val wakeNow  = idleReg && io.wake

  io.iAddr := pc
  io.pc    := pc

  io.dWEn   := memWEn && !idleReg
  io.dWAddr := memWAddr
  io.dWData := memWData
  io.dWMask := memWMask

  io.syscall     := op === Op.SYSCALL && !memStall && !idleReg
  io.syscallCode := regs(0)

  // ------------------------------------------------------------------
  // Sequential update
  // ------------------------------------------------------------------

  when(wakeNow) {
    idleReg := false.B
    pc := pc + 8.U
  }.elsewhen(!idleReg) {
    when(!memStall) {
      pc := nextPc

      when(op === Op.IDIV) {
        regs(0) := divQ
        regs(2) := divR
      }.elsewhen(regWen) {
        regs(regWaddr) := regWdata
      }

      when(flagsWen) {
        cf := ncf
        zf := nzf
        sf := nsf
        of := nof
      }

      when(op === Op.HLT && !io.wake) {
        idleReg := true.B
        pc := pc
      }
    }
  }
}

// ------------------------------------------------------------------
// Configuration
// ------------------------------------------------------------------

case class CpuConfig(
  numCores: Int = 1,
  imemWords: Int = 4096,
  dmemWords: Int = 8192,
  startPc: BigInt = 0,
  startRsp: BigInt = 0xfff8,
  enableDiv: Boolean = false,
  idleDebounce: Int = 16,
  enableClockGating: Boolean = true
)

class WriteReq extends Bundle {
  val addr = UInt(64.W)
  val data = UInt(64.W)
  val mask = UInt(8.W)
}

// ------------------------------------------------------------------
// Clock gate
//
// This is a latch-based clock gate. For real silicon, replace this
// with your foundry's integrated clock gate cell if needed.
// ------------------------------------------------------------------

class ClockGate extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk_in  = Input(Clock())
    val clk_en  = Input(Bool())
    val clk_out = Output(Clock())
  })

  setInline(
    "ClockGate.v",
    """module ClockGate(
      |  input  clk_in,
      |  input  clk_en,
      |  output clk_out
      |);
      |  reg latch_q;
      |
      |  initial latch_q = 1'b0;
      |
      |  always @(*) begin
      |    if (!clk_in) latch_q = clk_en;
      |  end
      |
      |  assign clk_out = clk_in & latch_q;
      |
      |endmodule
      |""".stripMargin
  )
}

// ------------------------------------------------------------------
// Power Management Unit
//
// ACPI-like behavior:
//   - if any core is active, state is Running
//   - if all cores execute hlt, state becomes Idle
//   - if idle persists and allowSleep is high, state becomes Sleep
//   - wake input returns the system to Running
// ------------------------------------------------------------------

class PowerManagementUnit(numCores: Int, debounce: Int = 16) extends Module {
  require(numCores >= 1)
  require(debounce >= 1)

  val io = IO(new Bundle {
    val coreIdle   = Input(Vec(numCores, Bool()))
    val wakeReq    = Input(Bool())
    val allowSleep = Input(Bool())

    val coreClockEn   = Output(Vec(numCores, Bool()))
    val coreWake      = Output(Vec(numCores, Bool()))
    val clusterClockEn = Output(Bool())
    val powerGate     = Output(Bool())
    val acpiState     = Output(UInt(2.W))
  })

  val RUNNING = 0.U(2.W)
  val IDLE    = 1.U(2.W)
  val SLEEP   = 2.U(2.W)

  val state = RegInit(RUNNING)

  val wakePending = RegInit(false.B)

  val sleepThreshold = (debounce * 4).max(1)
  val counterBits = log2Ceil(sleepThreshold + 1).max(1)
  val idleCount = RegInit(0.U(counterBits.W))

  val allIdle = io.coreIdle.asUInt.andR
  val wakeActive = io.wakeReq || wakePending

  when(io.wakeReq) {
    wakePending := true.B
  }.elsewhen(!allIdle) {
    wakePending := false.B
  }

  when(allIdle && !wakeActive) {
    when(idleCount < sleepThreshold.U) {
      idleCount := idleCount + 1.U
    }

    when(idleCount >= debounce.U) {
      state := IDLE
    }

    when(io.allowSleep && idleCount >= sleepThreshold.U) {
      state := SLEEP
    }
  }.otherwise {
    state := RUNNING
    idleCount := 0.U
  }

  for (i <- 0 until numCores) {
    io.coreClockEn(i) := (state === RUNNING) || wakeActive || !io.coreIdle(i)
    io.coreWake(i) := wakeActive
  }

  io.clusterClockEn := (state === RUNNING) || wakeActive
  io.powerGate := (state === SLEEP) && io.allowSleep && !wakeActive
  io.acpiState := state
}

// ------------------------------------------------------------------
// Main top
// ------------------------------------------------------------------

class MainTop(cfg: CpuConfig) extends Module {
  require(cfg.numCores >= 1)

  val io = IO(new Bundle {
    // Power management control
    val wake       = Input(Bool())
    val allowSleep = Input(Bool())

    // Power management observation
    val acpiState       = Output(UInt(2.W))
    val clusterClockEnable = Output(Bool())
    val clusterPowerGate   = Output(Bool())
    val coreClockEnable    = Output(Vec(cfg.numCores, Bool()))
    val corePowerGate      = Output(Vec(cfg.numCores, Bool()))

    // Per-core observation
    val pc          = Output(Vec(cfg.numCores, UInt(64.W)))
    val syscall     = Output(Vec(cfg.numCores, Bool()))
    val syscallCode = Output(Vec(cfg.numCores, UInt(64.W)))

    // Simple memory initialization ports
    val imemInitEn   = Input(Bool())
    val imemInitAddr = Input(UInt(64.W))
    val imemInitData = Input(UInt(64.W))

    val dmemInitEn   = Input(Bool())
    val dmemInitAddr = Input(UInt(64.W))
    val dmemInitData = Input(UInt(64.W))
    val dmemInitMask = Input(UInt(8.W))
  })

  def wordIndex(addr: UInt, words: Int): UInt = {
    if (words <= 1) 0.U
    else (addr >> 3)(log2Ceil(words) - 1, 0)
  }

  // ------------------------------------------------------------------
  // Memories
  // ------------------------------------------------------------------

  val imem = Mem(cfg.imemWords, UInt(64.W))
  val dmem = Seq.fill(8)(Mem(cfg.dmemWords, UInt(8.W)))

  when(io.imemInitEn) {
    imem.write(wordIndex(io.imemInitAddr, cfg.imemWords), io.imemInitData)
  }

  // ------------------------------------------------------------------
  // Power management unit
  // ------------------------------------------------------------------

  val pmu = Module(new PowerManagementUnit(cfg.numCores, cfg.idleDebounce))

  pmu.io.wakeReq := io.wake
  pmu.io.allowSleep := io.allowSleep

  io.acpiState := pmu.io.acpiState
  io.clusterClockEnable := pmu.io.clusterClockEn
  io.clusterPowerGate := pmu.io.powerGate

  val coreIdle = Wire(Vec(cfg.numCores, Bool()))
  pmu.io.coreIdle := coreIdle

  // ------------------------------------------------------------------
  // Core write signals
  // ------------------------------------------------------------------

  val dWEn   = Wire(Vec(cfg.numCores, Bool()))
  val dWAddr = Wire(Vec(cfg.numCores, UInt(64.W)))
  val dWData = Wire(Vec(cfg.numCores, UInt(64.W)))
  val dWMask = Wire(Vec(cfg.numCores, UInt(8.W)))
  val dWReady = Wire(Vec(cfg.numCores, Bool()))

  // ------------------------------------------------------------------
  // Instantiate cores with gated clocks
  // ------------------------------------------------------------------

  for (i <- 0 until cfg.numCores) {
    val cg = Module(new ClockGate)

    val gateEnable =
      if (cfg.enableClockGating) pmu.io.coreClockEn(i)
      else true.B

    cg.io.clk_in := clock
    cg.io.clk_en := gateEnable || reset.asBool

    io.coreClockEnable(i) := cg.io.clk_en
    io.corePowerGate(i) := pmu.io.powerGate && coreIdle(i)

    val coreClock = cg.io.clk_out

    val core = withClockAndReset(coreClock, reset) {
      Module(new Core(cfg.startPc, cfg.startRsp, cfg.enableDiv))
    }

    core.io.wake := pmu.io.coreWake(i)
    coreIdle(i) := core.io.idle

    // Instruction memory
    core.io.iData := imem.read(wordIndex(core.io.iAddr, cfg.imemWords))

    // Data memory read
    val rIdx = wordIndex(core.io.dRAddr, cfg.dmemWords)
    core.io.dRData := Cat(dmem.reverse.map(_.read(rIdx)))

    // Observation
    io.pc(i)          := core.io.pc
    io.syscall(i)     := core.io.syscall
    io.syscallCode(i) := core.io.syscallCode

    // Write port collection
    dWEn(i)   := core.io.dWEn
    dWAddr(i) := core.io.dWAddr
    dWData(i) := core.io.dWData
    dWMask(i) := core.io.dWMask
    core.io.dWReady := dWReady(i)
  }

  // ------------------------------------------------------------------
  // Shared data write arbitration
  // ------------------------------------------------------------------

  val arb = Module(new RRArbiter(new WriteReq, cfg.numCores))

  for (i <- 0 until cfg.numCores) {
    dWReady(i) := arb.io.in(i).ready

    arb.io.in(i).valid     := dWEn(i)
    arb.io.in(i).bits.addr := dWAddr(i)
    arb.io.in(i).bits.data := dWData(i)
    arb.io.in(i).bits.mask := dWMask(i)
  }

  arb.io.out.ready := !io.dmemInitEn && !pmu.io.powerGate

  val outReq = arb.io.out.bits
  val wIdx   = wordIndex(outReq.addr, cfg.dmemWords)
  val initIdx = wordIndex(io.dmemInitAddr, cfg.dmemWords)

  when(io.dmemInitEn) {
    for (b <- 0 until 8) {
      when(io.dmemInitMask(b)) {
        dmem(b).write(initIdx, io.dmemInitData(8 * b + 7, 8 * b))
      }
    }
  }.elsewhen(arb.io.out.fire) {
    for (b <- 0 until 8) {
      when(outReq.mask(b)) {
        dmem(b).write(wIdx, outReq.data(8 * b + 7, 8 * b))
      }
    }
  }
}

// ------------------------------------------------------------------
// Main generator
// ------------------------------------------------------------------

object Main extends App {
  // Change this to however many cores you want.
  val numCores = 2

  val cfg = CpuConfig(
    numCores = numCores,
    imemWords = 4096,
    dmemWords = 8192,
    startPc = 0,
    startRsp = 0xfff8,
    enableDiv = false,
    idleDebounce = 16,
    enableClockGating = true
  )

  // If your Chisel version uses a different emitter API, adjust this line.
  ChiselStage.emitVerilogFile("MainTop.v", new MainTop(cfg))
}