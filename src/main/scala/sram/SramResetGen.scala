package xs.utils.sram

import chisel3._
import chisel3.util._

class SramResetGen(
  set: Int,
  multicycle: Int,
  holdMcp: Boolean,
  resetDelay: Int = 4
) extends Module {
  private val setBits = log2Ceil(set)
  val io = IO(new Bundle {
    val resetState = Output(Bool())
    val waddr = Output(UInt(setBits.W))
    val earlyWen = if(holdMcp) Some(Output(Bool())) else None
    val wen = Output(Bool())
  })
  private val resetCounter = RegInit((set - 1).U(setBits.W))
  private val resetState = RegInit(true.B)
  private val resetHold = RegInit(Fill(resetDelay, true.B))
  resetHold := Cat(0.U(1.W), resetHold(resetDelay - 1, 1))
  private val step = Wire(Bool())
  if(multicycle > 1) {
    if(holdMcp) require(multicycle == 2)
    val wens = RegInit((1 << (multicycle - 1)).U(multicycle.W))
    val upds = RegInit(0.U((multicycle - 1).W))
    when(resetState) {
      wens := Cat(wens(0), wens)(multicycle, 1)
      upds := Cat(io.wen, upds)(multicycle - 1, 1)
    }
    io.earlyWen.foreach(_ := wens(multicycle - 1) && !resetHold(0))
    io.wen := wens(multicycle - 2) && !resetHold(0)
    step := upds(0)
  } else {
    io.wen := resetState && !resetHold(0)
    step := resetState
  }
  when(step) {
    resetCounter := Mux(resetCounter === 0.U, 0.U, resetCounter - 1.U)
    resetState := resetCounter === 0.U
  }
  io.resetState := resetState
  io.waddr := resetCounter
}
