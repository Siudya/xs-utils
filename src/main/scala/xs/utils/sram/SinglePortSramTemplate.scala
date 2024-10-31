package xs.utils.sram

import chisel3._
import chisel3.util._

class SpSramReq[T <: Data](gen: T, set:Int, way:Int) extends Bundle {
  val addr = UInt(log2Ceil(set).W)
  val mask = if(way == 1) None else Some(UInt(way.W))
  val write = Bool()
  val data = Vec(way, gen)
}

class SpSramResp[T <: Data](gen: T, way:Int) extends Bundle {
  val data = Vec(way, gen)
}

class SinglePortSramTemplate[T <: Data](
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
  private val interval = if(extraHold) setup + 1 else setup
  require(latency <= interval)
  val io = IO(new Bundle{
    val req = Flipped(Decoupled(new SpSramReq(gen, set, way)))
    val resp = Valid(new SpSramResp(gen, way))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })
  private val ram = Module(new SRAMTemplate(
    gen = gen,
    set = set,
    way = way,
    singlePort = true,
    shouldReset = shouldReset,
    extraReset = false,
    holdRead = holdRead,
    bypassWrite = false,
    setup = setup,
    latency = latency,
    extraHold = extraHold,
    hasMbist = hasMbist,
    suffix = suffix,
    powerCtl = powerCtl
  ))
  ram.io.pwctl.foreach(_ := io.pwctl.get)
  io.resp.valid := ram.io.r.resp.valid
  io.resp.bits.data := ram.io.r.resp.data

  private val reqReg = if(interval > 1) RegEnable(io.req.bits, io.req.fire) else io.req.bits
  ram.io.r.req.valid := io.req.fire && !io.req.bits.write
  ram.io.w.req.valid := io.req.fire && io.req.bits.write
  io.req.ready := ram.io.w.req.ready

  ram.io.r.req.bits.setIdx := reqReg.addr
  ram.io.w.req.bits.data := reqReg.data
  ram.io.w.req.bits.setIdx := reqReg.addr
  ram.io.w.req.bits.waymask.foreach(_ := reqReg.mask.get)
}
