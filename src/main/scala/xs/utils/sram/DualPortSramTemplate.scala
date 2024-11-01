package xs.utils.sram

import chisel3._
import chisel3.util._

class DpSramWrite[T <: Data](gen: T, set:Int, way:Int) extends Bundle {
  val addr = UInt(log2Ceil(set).W)
  val mask = if(way == 1) None else Some(UInt(way.W))
  val data = Vec(way, gen)
}

class DualPortSramTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  shouldReset: Boolean = false,
  holdRead: Boolean = false,
  setup:Int = 1, // ask your leader to changed this
  latency: Int = 1, // ask your leader to changed this
  extraHold: Boolean = false,  //ask your leader to changed this
  hasMbist: Boolean = false,
  suffix: String = "",
  powerCtl: Boolean = false,
) extends Module {
  private val hold = if(extraHold) setup + 1 else setup
  val io = IO(new Bundle{
    val wreq = Flipped(Decoupled(new DpSramWrite(gen, set, way)))
    val rreq = Flipped(Decoupled(UInt(log2Ceil(set).W)))
    val rresp = Valid(Vec(way, gen))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })
  private val ram = Module(new SRAMTemplate(
    gen = gen,
    set = set,
    way = way,
    singlePort = false,
    shouldReset = shouldReset,
    extraReset = false,
    holdRead = holdRead,
    bypassWrite = false,
    setup = setup,
    latency = latency,
    extraHold = extraHold,
    hasMbist = false,
    hasBroadCast = hasMbist,
    suffix = suffix,
    powerCtl = powerCtl
  ))
  ram.io.pwctl.foreach(_ := io.pwctl.get)
  io.rresp.valid := ram.io.r.resp.valid
  io.rresp.bits := ram.io.r.resp.data

  ram.io.r.req.valid := io.rreq.valid
  ram.io.w.req.valid := io.wreq.valid
  io.rreq.ready := ram.io.r.req.ready
  io.wreq.ready := ram.io.w.req.ready

  private val wreq = if(hold > 1) RegEnable(io.wreq.bits, io.wreq.fire) else io.wreq.bits
  private val rreq = if(hold > 1) RegEnable(io.rreq.bits, io.rreq.fire) else io.rreq.bits
  ram.io.r.req.bits.setIdx := rreq
  ram.io.w.req.bits.setIdx := wreq.addr
  ram.io.w.req.bits.data := wreq.data
  ram.io.w.req.bits.waymask.foreach(_ := wreq.mask.get)
}
