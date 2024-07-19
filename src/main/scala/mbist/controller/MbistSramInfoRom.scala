package xs.utils.mbist.controller

import chisel3._
import chisel3.util._
import xs.utils.sram.SramMeta

class MbistSramInfoRom(meta: Seq[SramMeta], param: MbistControllerParam) extends Module {

  val io = IO(new Bundle {
    val info = Output(Valid(new SramInfo(param)))
    val testIdStart = Input(UInt(param.arrayBits.W))
    val testIdEnd = Input(UInt(param.arrayBits.W))
    val start = Input(Bool())
    val kill = Input(Bool())
    val next = Input(Bool())
    val finished = Output(Bool())
    val req = Output(Bool())
    val ack = Input(Bool())
  })

  private val entry = new SramInfo(param)
  private val contents = Seq.fill(meta.length)(Wire(UInt(entry.getWidth.W)))
  contents
    .zip(meta)
    .foreach({
      case (e, m) =>
        val c = Wire(entry)
        c.array := m.array.U
        c.cols := (m.colNum - 1).U
        c.rows := (m.rowNum - 1).U
        c.dataBits := m.dataBits.U
        c.mcp := m.mcp.B
        c.dp := m.dp.B
        c.seg := m.segLen.U
        c.maskBits := m.maskBits.U
        c.latency := m.latency.U
        c.rowOff := m.rowOff.U
        e := c.asTypeOf(UInt(entry.getWidth.W))
    })

  private val idxBits = log2Ceil(meta.length + 1)
  private val s_idle :: s_req :: s_send :: s_wait :: Nil = Enum(4)
  private val state = RegInit(s_idle)
  private val ptr = RegInit(0.U(idxBits.W))

  private val selVec = contents.indices.map(_.U === ptr)
  private val infoSel = Mux1H(selVec, contents).asTypeOf(entry)
  private val endMet = ptr === contents.length.U
  private val shouldTest = io.testIdStart <= infoSel.array && infoSel.array <= io.testIdEnd && !endMet
  private val ptrMove = Mux(shouldTest, io.next, true.B) && !endMet && state =/= s_idle
  private val infoValid = state === s_send && shouldTest && !endMet
  private val backToIdle = endMet || io.kill

  when(backToIdle) {
    ptr := 0.U
  }.elsewhen(ptrMove) {
    ptr := ptr + 1.U
  }

  switch(state) {
    is(s_idle) {
      state := Mux(io.start, s_req, state)
    }
    is(s_req) {
      state := Mux(io.ack, s_send, state)
    }
    is(s_send) {
      state := Mux(backToIdle, s_idle, Mux(infoValid, s_wait, state))
    }
    is(s_wait) {
      state := Mux(backToIdle, s_idle, Mux(io.next, s_send, state))
    }
  }
  io.req := state =/= s_idle
  io.finished := endMet
  io.info.valid := RegNext(infoValid, false.B)
  io.info.bits := RegEnable(infoSel, infoValid)
}
