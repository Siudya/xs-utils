/** *************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

// See LICENSE.SiFive for license details.

package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.mbist.MbistClockGateCell

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt): SRAMBundleA = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(Vec(way, gen))
  val waymask = if(way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    super.apply(setIdx)
    this.data := data
    this.waymask.foreach(_ := waymask)
    this
  }

  // this could only be used when waymask is onehot or nway is 1
  def apply(data: T, setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
  val valid = Output(Bool())
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt): SRAMReadBus[T] = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

// WARNING: this SRAMTemplate assumes the SRAM lib itself supports holdRead.
class SRAMTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  singlePort: Boolean = false,
  shouldReset: Boolean = false,
  extraReset: Boolean = false,
  holdRead: Boolean = false,
  bypassWrite: Boolean = false,
  setup:Int = 1, // ask your leader to changed this
  latency: Int = 1, // ask your leader to changed this
  extraHold: Boolean = false,  //ask your leader to changed this
  hasMbist: Boolean = false,
  suffix: String = "",
  powerCtl: Boolean = false,
  val foundry: String = "Unknown",
  val sramInst: String = "STANDARD")
  extends Module {
  private val inputMcp = setup > 1 || extraHold
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })
  require(latency >= 1)
  require(setup >= 1)

  private val hold = if(extraHold) setup + 1 else setup
  private val cg = Module(new MbistClockGateCell(hold > 1))
  private val dataWidth = gen.getWidth * way
  private val (mbistBd, brcBd, array, nodeNum, realMaskBits, vname) = SramHelper.genRam(
    gen.getWidth,
    way,
    set,
    !singlePort,
    setup,
    hold,
    latency,
    hasMbist,
    io.pwctl,
    reset,
    cg.out_clock,
    None,
    suffix,
    foundry,
    sramInst,
    this
  )
  val sramName: String = vname
  if(extraReset) require(shouldReset)

  private val resetState = WireInit(false.B)
  private val resetAddr = WireInit(0.U(log2Ceil(set + 1).W))
  private val resetWen = WireInit(false.B)
  private val resetWbusy = Wire(Bool())

  val extra_reset = if(extraReset) Some(IO(Input(AsyncReset()))) else None
  if(shouldReset) {
    val resetGen = Module(new SramResetGen(set, hold))
    resetGen.clock := clock
    if(extraReset) {
      resetGen.reset := (extra_reset.get.asBool | reset.asBool).asAsyncReset
    } else {
      resetGen.reset := reset
    }
    resetState := resetGen.io.resetState
    resetAddr := resetGen.io.waddr
    resetWen := resetGen.io.wen
    resetGen.io.wbusy := resetWbusy
  }

  private val wen = Mux(resetState, resetWen, io.w.req.fire)
  private val wmask = Mux(resetState, Fill(way, true.B), io.w.req.bits.waymask.getOrElse("b1".U))
  private val wdata = Mux(resetState, 0.U, io.w.req.bits.data.asUInt)
  private val waddr = Mux(resetState, resetAddr, io.w.req.bits.setIdx)
  private val ren = io.r.req.fire
  private val raddr = io.r.req.bits.setIdx

  private val ramWen = if(hasMbist) Mux(mbistBd.ack, mbistBd.we, wen) else wen
  private val ramWaddr = if(hasMbist & singlePort) {
    Mux(mbistBd.ack, mbistBd.addr_rd, waddr)
  } else if(hasMbist & !singlePort) {
    Mux(mbistBd.ack, mbistBd.addr, waddr)
  } else {
    waddr
  }

  private val nto1 = realMaskBits > way
  private val fullMbistMask = if(way > 1) Fill(nodeNum, mbistBd.wmask) else Fill(realMaskBits, true.B)
  private val mbistWmask = if(nto1) {
    mbistBd.selectedOH & fullMbistMask
  } else {
    val n = realMaskBits / nodeNum
    val selMask = Cat(Seq.tabulate(realMaskBits)(i => mbistBd.selectedOH(i / n)).reverse)
    selMask & fullMbistMask
  }

  private val funcWmask = if(nto1) {
    val n = realMaskBits / way
    Cat(Seq.tabulate(realMaskBits)(i => wmask(i / n)).reverse)
  } else {
    wmask
  }

  private val mbistWdata = Fill(nodeNum, mbistBd.wdata)
  private val ramWmask = if(hasMbist) Mux(mbistBd.ack, mbistWmask, funcWmask) else funcWmask
  private val ramWdata = if(hasMbist) Mux(mbistBd.ack, mbistWdata, wdata) else wdata
  private val ramRen = if(hasMbist) Mux(mbistBd.ack, mbistBd.re, ren) else ren
  private val ramRaddr = if(hasMbist) Mux(mbistBd.ack, mbistBd.addr_rd, raddr) else raddr

  private val (ckRen, renStretched, rbusy) = if(inputMcp) {
    val rreqReg = RegInit(0.U(hold.W))
    rreqReg.suggestName("rreqReg")
    when(ramRen) {
      rreqReg := Fill(rreqReg.getWidth, true.B)
    }.otherwise {
      rreqReg := Cat(false.B, rreqReg) >> 1.U
    }
    val cgEn = if(extraHold) rreqReg(1) else rreqReg(0)
    val ramEn = rreqReg(0)
    val busy = rreqReg(hold - 1, 1).orR
    (cgEn, ramEn, busy)
  } else {
    (ramRen, ramRen, false.B)
  }

  private val (ckWen, wenStretched, wbusy) = if(inputMcp) {
    val wreqReg = RegInit(0.U(hold.W))
    wreqReg.suggestName("wreqReg")
    when(ramWen) {
      wreqReg := Fill(wreqReg.getWidth, true.B)
    }.otherwise {
      wreqReg := Cat(false.B, wreqReg) >> 1.U
    }
    val cgEn = if(extraHold) wreqReg(1) else wreqReg(0)
    val ramEn = wreqReg(0)
    val busy = wreqReg(hold - 1, 1).orR
    (cgEn, ramEn, busy)
  } else {
    (ramWen, ramWen, false.B)
  }
  resetWbusy := wbusy

  private val respReg = RegInit(0.U(latency.W))
  when(ckRen) {
    respReg := Fill(latency, true.B)
  }.otherwise {
    respReg := Cat(false.B, respReg) >> 1.U
  }

  private val ramRdata = SramProto.read(array, singlePort, ramRaddr, renStretched)
  when(wenStretched && !brcBd.ram_hold) {
    SramProto.write(array, singlePort, ramWaddr, ramWdata, ramWmask)
  }

  cg.dft.fromBroadcast(brcBd)
  cg.mbist.req := mbistBd.ack
  cg.mbist.readen := mbistBd.re
  cg.mbist.writeen := mbistBd.we
  cg.E := ckRen | ckWen

  private val concurrentRW = io.w.req.fire && io.r.req.fire && io.w.req.bits.setIdx === io.r.req.bits.setIdx
  private val doBypass = if(bypassWrite) concurrentRW else false.B
  private val doBypassReg = RegEnable(doBypass, false.B, io.r.req.fire)
  private val wmaskReg = RegEnable(wmask, 0.U, doBypass & io.r.req.fire)
  private val segment = dataWidth / wmask.getWidth
  private val bypassMask = Cat(Seq.tabulate(wmask.getWidth)(i => wmaskReg(i / segment).asBool).reverse)
  private val keepMask = Cat(Seq.tabulate(wmask.getWidth)(i => !wmaskReg(i / segment).asBool).reverse)
  private val rdataReg = Reg(UInt(dataWidth.W))
  private val bypassData = bypassMask & rdataReg | keepMask & ramRdata
  if(bypassWrite) {
    when(doBypass) {
      rdataReg := wdata.asUInt
    }.elsewhen(respReg(0)) {
      rdataReg := Mux(doBypassReg, bypassData, ramRdata)
    }
  } else {
    when(respReg(0)) {
      rdataReg := ramRdata
    }
  }

  if(!bypassWrite && !holdRead) {
    io.r.resp.data := ramRdata.asTypeOf(io.r.resp.data)
  } else if(!bypassWrite && holdRead) {
    io.r.resp.data := Mux(respReg(0), ramRdata, rdataReg).asTypeOf(io.r.resp.data)
  } else if(bypassWrite && !holdRead) {
    io.r.resp.data := Mux(doBypassReg, bypassData, ramRdata).asTypeOf(io.r.resp.data)
  } else {
    when(respReg(0)) {
      io.r.resp.data := Mux(doBypassReg, bypassData, ramRdata).asTypeOf(io.r.resp.data)
    }.otherwise {
      io.r.resp.data := rdataReg.asTypeOf(io.r.resp.data)
    }
  }
  io.r.resp.valid := respReg(0)
  private val selectOHReg = RegEnable(mbistBd.selectedOH, respReg(0))
  mbistBd.rdata := Mux1H(selectOHReg, rdataReg.asTypeOf(Vec(nodeNum, UInt((dataWidth / nodeNum).W))))

  private val singleHold = if(singlePort) io.w.req.valid else false.B
  private val mcpHold = if(inputMcp) rbusy | wbusy else false.B
  private val resetHold = if(shouldReset) resetState else false.B
  io.r.req.ready := !mcpHold && !resetHold && !singleHold
  io.w.req.ready := !mcpHold && !resetHold
}
