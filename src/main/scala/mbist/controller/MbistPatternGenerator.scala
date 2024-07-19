package xs.utils.mbist.controller

import chisel3._
import chisel3.util._
import xs.utils.mbist.algorithm.{AlgoInstr, AlgoLoopInfo, AlgoOpCode, PatternCode}

class MbistPatternGenerator(param: MbistControllerParam) extends Module {

  val io = IO(new Bundle {
    val sram = Input(Valid(new SramInfo(param)))
    val instr = Flipped(Decoupled(new AlgoInstr))
    val loop = Input(Valid(new AlgoLoopInfo(param)))
    val wreq = Output(Valid(new MbistReqBundle(param)))
    val rreq = Output(Valid(new MbistReqBundle(param)))
    val singlePort = Output(Bool())
    val lastLoop = Output(Bool())
  })

  private val sram = RegEnable(io.sram.bits, io.sram.valid)
  private val maskShifter = Reg(UInt(param.maxMaskWidth.W))
  private val colStart = if (param.maxCol == 1) 0.U(0.W) else Reg(UInt(param.colsBits.W))
  private val colEnd = if (param.maxCol == 1) 0.U(0.W) else Reg(UInt(param.colsBits.W))
  private val colCounter = if (param.maxCol == 1) 0.U(0.W) else Reg(UInt(param.colsBits.W))
  private val rowStart = Reg(UInt(param.rowsBits.W))
  private val rowEnd = Reg(UInt(param.rowsBits.W))
  private val rowCounter = Reg(UInt(param.rowsBits.W))
  private val loop = RegEnable(io.loop.bits, io.loop.valid)
  private val instr = RegEnable(io.instr.bits, io.instr.fire)
  private val maskInstr = loop.maskWalk || io.instr.bits.op === AlgoOpCode.opWriteMask0
  private val activeInstr = io.instr.fire && Mux(maskInstr, sram.maskBits > 1.U, true.B)
  private val active = RegNext(activeInstr, false.B)
  private val working = RegNext(io.instr.fire, false.B)
  private val mcpHold = RegInit(false.B)
  private val useMask = io.instr.bits.op === AlgoOpCode.opWriteSeg || io.instr.bits.op === AlgoOpCode.opReadSeg
  private val useMaskReg = RegEnable(useMask, io.instr.fire)
  private val colsNumReg = RegEnable(io.sram.bits.cols +& 1.U(1.W), io.loop.valid)
  mcpHold := Mux(mcpHold, !mcpHold, sram.mcp & io.instr.fire)
  io.instr.ready := !mcpHold

  when(io.loop.valid) {
    if (param.maxCol >1) {
      colStart := Mux(io.loop.bits.colAll, 0.U, io.loop.bits.colIdx)
      colEnd := Mux(io.loop.bits.colAll, sram.cols, io.loop.bits.colIdx)
    }
    rowStart := Mux(io.loop.bits.rowAll, 0.U, io.loop.bits.rowIdx)
    rowEnd := Mux(io.loop.bits.rowAll, sram.rows, io.loop.bits.rowIdx)
  }

  private val maskFinished = maskShifter((sram.maskBits - 1.U)(log2Ceil(param.maxMaskWidth) - 1, 0))
  private val colFinished = colCounter === Mux(loop.colForward, colEnd, colStart)
  private val rowFinished = rowCounter === Mux(loop.rowForward, rowEnd, rowStart)
  if (param.maxCol == 1) {
    io.lastLoop := maskFinished && rowFinished
  } else {
    io.lastLoop := maskFinished && colFinished && rowFinished
  }

  private val loopValMove = working & instr.last

  private val colInit = Mux(loop.colForward, colStart, colEnd)
  private val colMove = Mux(loop.rowFirst, rowFinished, true.B) & loopValMove

  private val rowInit = Mux(loop.rowForward, rowStart, rowEnd)
  private val rowMove = Mux(loop.rowFirst, true.B, colFinished) & loopValMove

  private val maskInit = Mux(loop.maskWalk, 1.U, Fill(param.maxMaskWidth, 1.U(1.W)))
  private val maskMove = loop.maskWalk & loopValMove & rowFinished & colFinished

  private val counterUpdate = RegNext(io.loop.valid, false.B)

  if(param.maxCol >1) {
    when(counterUpdate) {
      colCounter := colInit
    }.elsewhen(colMove) {
      colCounter := Mux(colFinished, colInit, Mux(loop.colForward, colCounter + 1.U, colCounter - 1.U))
    }
  }

  when(counterUpdate) {
    rowCounter := rowInit
  }.elsewhen(rowMove) {
    rowCounter := Mux(rowFinished, rowInit, Mux(loop.rowForward, rowCounter + 1.U, rowCounter - 1.U))
  }

  when(counterUpdate) {
    maskShifter := maskInit
  }.elsewhen(maskMove) {
    maskShifter := Cat(maskShifter(param.maxMaskWidth - 2, 0), 0.U(1.W))
  }
  private val applyMask = Wire(UInt(param.maxMaskWidth.W))
  when(instr.op === AlgoOpCode.opWriteMask0) {
    applyMask := 0.U
  }.elsewhen(useMaskReg) {
    applyMask := maskShifter
  }.otherwise {
    applyMask := Fill(param.maxMaskWidth, true.B)
  }

  private val evenGrid = !(colCounter(0) ^ rowCounter(0))
  private val pattern0 = Cat(Seq.fill((param.maxDataWidth + 1) / 2)(0.U(2.W)))(param.maxDataWidth - 1, 0)
  private val pattern5 = Cat(Seq.fill((param.maxDataWidth + 1) / 2)(1.U(2.W)))(param.maxDataWidth - 1, 0)
  private val patternA = Cat(Seq.fill((param.maxDataWidth + 1) / 2)(2.U(2.W)))(param.maxDataWidth - 1, 0)
  private val pattern1 = Cat(Seq.fill((param.maxDataWidth + 1) / 2)(3.U(2.W)))(param.maxDataWidth - 1, 0)
  private val patternC = Mux(evenGrid, pattern0, pattern1)
  private val patternIC = Mux(evenGrid, pattern1, pattern0)
  private val patCodeSeq =
    Seq(PatternCode.pat0, PatternCode.pat5, PatternCode.patA, PatternCode.pat1, PatternCode.patC, PatternCode.patIC)
  private val patHitVec = patCodeSeq.map(_ === instr.pat)
  private val patSeq = Seq(pattern0, pattern5, patternA, pattern1, patternC, patternIC)
  private val patSel = Mux1H(patHitVec, patSeq)

  private val widthMask = Cat(
    Seq
      .tabulate(param.maxDataWidth)({ idx =>
        val bit = Wire(Bool())
        bit := idx.U < io.sram.bits.dataBits
        bit
      })
      .reverse
  )
  private val widthMaskReg = RegEnable(widthMask, io.sram.valid)

  private val flippedMask = Cat(
    Seq
      .tabulate(param.maxDataWidth)({ idx =>
        val bit = Wire(Bool())
        val maskIdx = UIntToOH(idx.U / sram.seg, param.maxMaskWidth)
        bit := (maskShifter & maskIdx).orR
        bit
      })
      .reverse
  )

  private val dmask0 = flippedMask.asUInt
  private val dmask1 = (~flippedMask).asUInt
  private val p0 = (~patSel).asUInt
  private val p1 = patSel.asUInt
  private val readPat = dmask0 & p0 | dmask1 & p1
  private val finalReadPat = Mux(loop.maskWalk & useMaskReg, readPat, patSel) & widthMaskReg

  private val rowAdj = WireInit(rowCounter)
  when(instr.op === AlgoOpCode.opWriteAdj || instr.op === AlgoOpCode.opReadAdj) {
    rowAdj := Mux(rowCounter === 0.U, rowCounter + 1.U(1.W), rowCounter - 1.U(1.W))
  }
  private val rowIssue = rowAdj + sram.rowOff
  private val addr = rowIssue * colsNumReg + colCounter

  private val writeEn = AlgoOpCode.writeOps.map(_ === instr.op).reduce(_ || _) & active
  io.wreq.valid := RegNext(writeEn, false.B)
  io.wreq.bits.addr := RegEnable(addr, writeEn)
  io.wreq.bits.data := RegEnable(patSel, writeEn)
  io.wreq.bits.mask := RegEnable(applyMask, writeEn)
  io.wreq.bits.array := RegEnable(sram.array, writeEn)
  io.wreq.bits.depth := DontCare
  io.wreq.bits.noCompare := DontCare

  private val readEn = AlgoOpCode.readOps.map(_ === instr.op).reduce(_ || _) & active
  private val cReadEn = sram.dp && instr.cncrntRead && active
  private val isConcurrentRead = cReadEn && !readEn
  private val cReadRowWhenWen = Mux(rowIssue(0), rowIssue - 1.U, rowIssue + 1.U)
  private val cReadAddrWhenWen = cReadRowWhenWen * colsNumReg + colCounter
  private val cReadAddr = Mux(writeEn, cReadAddrWhenWen, addr)
  private val allRen = readEn | cReadEn

  io.rreq.valid := RegNext(allRen, false.B)
  io.rreq.bits.addr := RegEnable(Mux(isConcurrentRead, cReadAddr, addr), allRen)
  io.rreq.bits.data := RegEnable(finalReadPat, allRen)
  io.rreq.bits.mask := DontCare
  io.rreq.bits.array := RegEnable(sram.array, allRen)
  io.rreq.bits.depth := RegEnable(sram.latency + sram.mcp, allRen)
  io.rreq.bits.noCompare := RegEnable(instr.op === AlgoOpCode.opReadNoCompare || isConcurrentRead, allRen)

  io.singlePort := !sram.dp
}
