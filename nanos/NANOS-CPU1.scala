// MultiCoreX86Like.scala
//
// Compact multicore 64-bit Chisel CPU with x86-like mnemonics.
//
// Supported:
//   mov, add, or, and, sub, xor, cmp, shl, shr, sar,
//   jmp, jcc, call, ret, syscall,
//   optional imul/idiv,
//   byte-precise mov al/cl/dl/bl <-> [mem]
//
// This is a fixed 64-bit instruction-word design, not full x86.

import chisel3._
import chisel3.util._

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

class Core(
  startPc: BigInt = 0,
  startRsp: BigInt = 0xfff8,
  enableDiv: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    // Instruction port: single-cycle read-only
    val iAddr = Output(UInt(64.W))
    val iData = Input(UInt(64.W))

    // Data read port: single-cycle read
    val dREn   = Output(Bool())
    val dRAddr = Output(UInt(64.W))
    val dRData = Input(UInt(64.W))

    // Data write port: may stall if not ready
    val dWReady = Input(Bool())
    val dWEn    = Output(Bool())
    val dWAddr  = Output(UInt(64.W))
    val dWData  = Output(UInt(64.W))
    val dWMask  = Output(UInt(8.W))

    // Diagnostics / environment
    val syscall     = Output(Bool())
    val syscallCode = Output(UInt(64.W))
    val pc          = Output(UInt(64.W))
  })

  // ------------------------------------------------------------------
  // State
  // ------------------------------------------------------------------

  val pc = RegInit(startPc.U(64.W))

  // Register numbering:
  // 0 rax, 1 rcx, 2 rdx, 3 rbx, 4 rsp, 5 rbp, 6 rsi, 7 rdi,
  // 8..15 r8..r15
  val initRegs = VecInit(Seq.tabulate(16)(i =>
    if (i == 4) startRsp.U(64.W) else 0.U(64.W)
  ))
  val regs = RegInit(initRegs)

  // Flags
  val cf = RegInit(false.B)
  val zf = RegInit(false.B)
  val sf = RegInit(false.B)
  val of = RegInit(false.B)

  // ------------------------------------------------------------------
  // Instruction decode
  // ------------------------------------------------------------------
  //
  // 64-bit instruction layout:
  //
  // 63:56  opcode
  // 55:52  cond / aux
  // 51:50  dstMode
  // 49:48  srcMode
  // 47     byteOp
  // 46:45  memMode
  // 44:41  dstReg / dst base
  // 40:37  srcReg / src base
  // 36:5   sign-extended imm32 / disp32
  // 4:0    reserved
  //
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

  // Sign-extend 32-bit immediate/displacement to 64 bits.
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
    // ret reads [rsp]
    memReadEn   := true.B
    memReadAddr := regs(4)
  }.elsewhen(srcMode === Mode.SRC_MEM) {
    memReadEn   := true.B
    memReadAddr := srcAddr
  }.elsewhen(dstMode === Mode.DST_MEM && (aluOp || op === Op.CMP)) {
    // read-modify-write or cmp [mem], ...
    memReadEn   := true.B
    memReadAddr := dstAddr
  }

  io.dREn   := memReadEn
  io.dRAddr := memReadAddr

  // Byte extraction for byte loads.
  val rLane = io.dRAddr(2, 0)
  val rByte = (io.dRData >> (rLane << 3))(7, 0)

  val memRVal = Mux(byteOp, Cat(0.U(56.W), rByte), io.dRData)

  // ------------------------------------------------------------------
  // Operand selection
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

  // For register destination, left is dst register.
  // For memory destination, left is old memory value when needed.
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
  // Optional hardware IDIV
  //
  // Simplified semantics:
  //   dividend = RAX
  //   divisor  = source operand
  //   RAX <= quotient
  //   RDX <= remainder
  //
  // A full 64-bit divider is large. It is optional.
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

    // Avoid actual divide-by-zero and min/-1 overflow in simulation.
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

  val addFull    = Cat(0.U(1.W), left) + Cat(0.U(1.W), right)
  val addCarry   = addFull(64)
  val subBorrow  = left < right
  val ofAdd      = (left(63) === right(63)) && (sum(63) =/= left(63))
  val ofSub      = (left(63) =/= right(63)) && (diff(63) =/= left(63))

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
    // je / jz
    condTaken := zf
  }.elsewhen(cond === 1.U) {
    // jne / jnz
    condTaken := !zf
  }.elsewhen(cond === 2.U) {
    // jl
    condTaken := sf =/= of
  }.elsewhen(cond === 3.U) {
    // jle
    condTaken := zf || (sf =/= of)
  }.elsewhen(cond === 4.U) {
    // jg
    condTaken := !zf && (sf === of)
  }.elsewhen(cond === 5.U) {
    // jge
    condTaken := sf === of
  }.elsewhen(cond === 6.U) {
    // jb / jc
    condTaken := cf
  }.elsewhen(cond === 7.U) {
    // jbe
    condTaken := cf || zf
  }.elsewhen(cond === 8.U) {
    // ja
    condTaken := !cf && !zf
  }.elsewhen(cond === 9.U) {
    // jae / jnc
    condTaken := !cf
  }.elsewhen(cond === 10.U) {
    // js
    condTaken := sf
  }.elsewhen(cond === 11.U) {
    // jns
    condTaken := !sf
  }

  // ------------------------------------------------------------------
  // PC next logic
  // ------------------------------------------------------------------

  val nextPc = Wire(UInt(64.W))
  val target = pc + imm64

  nextPc := pc + 8.U // fixed 64-bit instruction size

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
    op =/= Op.DB

  when(canWriteReg) {
    regWen   := true.B
    regWaddr := dstReg

    // Byte register writes merge into the low byte of the full register.
    regWdata := Mux(
      byteOp,
      Cat(regs(dstReg)(63, 8), alu(7, 0)),
      alu
    )
  }

  // CALL pushes return address and updates RSP.
  val pushAddr = regs(4) - 8.U

  when(op === Op.CALL) {
    regWen   := true.B
    regWaddr := 4.U
    regWdata := pushAddr
  }

  // RET pops return address and updates RSP.
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

      // Byte-precise store: only one byte lane is written.
      memWData := (Cat(0.U(56.W), storeData(7, 0)) << (lane << 3))(63, 0)
      memWMask := (1.U(8.W) << lane)(7, 0)
    }.otherwise {
      // Qword store. Assumes 8-byte alignment for qword accesses.
      memWData := storeData
      memWMask := 0xff.U(8.W)
    }
  }

  // CALL stack push has priority over normal encoded stores.
  when(op === Op.CALL) {
    memWEn   := true.B
    memWAddr := pushAddr & ~7.U(64.W)
    memWData := pc + 8.U
    memWMask := 0xff.U(8.W)
  }

  // ------------------------------------------------------------------
  // Stall handling
  //
  // Writes are stallable because multiple cores share one data write port.
  // Reads are not stalled in this design because the top level gives each
  // core its own combinational read port.
  // ------------------------------------------------------------------

  val stall = memWEn && !io.dWReady

  // ------------------------------------------------------------------
  // Outputs
  // ------------------------------------------------------------------

  io.iAddr := pc
  io.pc    := pc

  io.dWEn   := memWEn
  io.dWAddr := memWAddr
  io.dWData := memWData
  io.dWMask := memWMask

  io.syscall     := op === Op.SYSCALL && !stall
  io.syscallCode := regs(0) // rax

  // ------------------------------------------------------------------
  // Sequential update
  // ------------------------------------------------------------------

  when(!stall) {
    pc := nextPc

    // IDIV writes both RAX and RDX.
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
  }
}

class WriteReq extends Bundle {
  val addr = UInt(64.W)
  val data = UInt(64.W)
  val mask = UInt(8.W)
}

class MultiCoreCpu(
  cores: Int = 2,
  imemWords: Int = 4096,     // 64-bit instruction words
  dmemWords: Int = 8192,     // 64-bit data words = 64 KiB
  startPc: BigInt = 0,
  startRsp: BigInt = 0xfff8,
  enableDiv: Boolean = false
) extends Module {
  require(cores >= 1)

  val io = IO(new Bundle {
    // Simple initialization ports. Usually used under reset.
    val imemInitEn   = Input(Bool())
    val imemInitAddr = Input(UInt(64.W))
    val imemInitData = Input(UInt(64.W))

    val dmemInitEn   = Input(Bool())
    val dmemInitAddr = Input(UInt(64.W))
    val dmemInitData = Input(UInt(64.W))
    val dmemInitMask = Input(UInt(8.W))

    // Per-core observability / environment interface.
    val pc          = Output(Vec(cores, UInt(64.W)))
    val syscall     = Output(Vec(cores, Bool()))
    val syscallCode = Output(Vec(cores, UInt(64.W)))
  })

  def wordIndex(addr: UInt, words: Int): UInt = {
    if (words <= 1) 0.U
    else (addr >> 3)(log2Ceil(words) - 1, 0)
  }

  // ------------------------------------------------------------------
  // Memories
  //
  // Instruction memory is a shared read-only 64-bit memory.
  // Data memory is byte-banked so byte stores are truly byte-precise.
  // ------------------------------------------------------------------

  val imem = Mem(imemWords, UInt(64.W))
  val dmem = Seq.fill(8)(Mem(dmemWords, UInt(8.W)))

  val coreMods = Seq.fill(cores)(Module(new Core(startPc, startRsp, enableDiv)))

  // Instruction memory initialization / read.
  when(io.imemInitEn) {
    imem.write(wordIndex(io.imemInitAddr, imemWords), io.imemInitData)
  }

  for ((c, i) <- coreMods.zipWithIndex) {
    c.io.iData := imem.read(wordIndex(c.io.iAddr, imemWords))

    io.pc(i)          := c.io.pc
    io.syscall(i)     := c.io.syscall
    io.syscallCode(i) := c.io.syscallCode

    // Each core gets its own combinational data read port.
    val rIdx = wordIndex(c.io.dRAddr, dmemWords)
    c.io.dRData := Cat(dmem.reverse.map(_.read(rIdx)))
  }

  // ------------------------------------------------------------------
  // Shared data write port arbitration
  // ------------------------------------------------------------------

  val arb = Module(new RRArbiter(new WriteReq, cores))

  for ((c, i) <- coreMods.zipWithIndex) {
    c.io.dWReady := arb.io.in(i).ready

    arb.io.in(i).valid       := c.io.dWEn
    arb.io.in(i).bits.addr   := c.io.dWAddr
    arb.io.in(i).bits.data   := c.io.dWData
    arb.io.in(i).bits.mask   := c.io.dWMask
  }

  // External data-memory initialization has priority and stalls core writes.
  arb.io.out.ready := !io.dmemInitEn

  val outReq = arb.io.out.bits
  val wIdx   = wordIndex(outReq.addr, dmemWords)

  val initIdx = wordIndex(io.dmemInitAddr, dmemWords)

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