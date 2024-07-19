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
import xs.utils.HoldUnless

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt): SRAMBundleA = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(Vec(way, gen))
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

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
  gen:          T,
  set:          Int,
  way:          Int = 1,
  singlePort:   Boolean = false,
  shouldReset:  Boolean = false,
  extraReset:   Boolean = false,
  holdRead:     Boolean = false,
  bypassWrite:  Boolean = false,
  multicycle:   Int = 1,
  holdMcp:      Boolean = false,
  hasMbist:     Boolean = false,
  suffix:       String = "",
  val foundry:  String = "Unknown",
  val sramInst: String = "STANDARD")
    extends Module {

  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val earlyRen = if(holdMcp) Some(Input(Bool())) else None
    val w = Flipped(new SRAMWriteBus(gen, set, way))
    val earlyWen = if(holdMcp) Some(Input(Bool())) else None
  })
  require(multicycle >= 1)
  private val mcp = multicycle > 1
  private val cg = Module(new MbistClockGateCell(mcp))
  private val dataWidth = gen.getWidth * way
  private val (mbistBd, brcBd, array, nodeNum, nto1, vname) = SramHelper.genRam(
    gen.getWidth,
    way,
    set,
    !singlePort,
    mcp,
    hasMbist,
    cg.out_clock,
    None,
    suffix,
    foundry,
    sramInst,
    this
  )
  val sramName:String = vname

  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  require(multicycle == 1 & shouldReset || !shouldReset, "MCP rams do not support reset!")
  if (shouldReset) {
    withClockAndReset(cg.out_clock, reset) {
      val _resetState = RegInit(true.B)
      val (_resetSet, resetFinish) = Counter(_resetState, set)
      when(resetFinish) {
        _resetState := false.B
      }
      if (extra_reset.isDefined) {
        when(extra_reset.get) {
          _resetState := true.B
        }
      }
      resetState := _resetState
      resetSet := _resetSet
    }
  }
  private val resetReg = RegNext(false.B, true.B)
  private val wen = io.w.req.fire || resetState && !resetReg
  private val wmask = Mux(resetState, Fill(way, true.B), io.w.req.bits.waymask.getOrElse("b1".U))
  private val wdata = Mux(resetState, 0.U, io.w.req.bits.data.asUInt)
  private val waddr = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  private val ren = io.r.req.fire
  private val raddr = io.r.req.bits.setIdx

  private val ramWen = if (hasMbist) Mux(mbistBd.ack, mbistBd.we, wen) else wen
  private val ramWaddr = if (hasMbist & singlePort) {
    Mux(mbistBd.ack, mbistBd.addr_rd, waddr)
  } else if (hasMbist & !singlePort) {
    Mux(mbistBd.ack, mbistBd.addr, waddr)
  } else {
    waddr
  }
  private val mbistWmask = if(nto1) {
    val n = nodeNum / way
    val selMask = Cat(Seq.tabulate(way)(idx => mbistBd.selectedOH(n * idx + n - 1, n * idx).orR).reverse)
    val fullMask = Fill(way, mbistBd.wmask)
    selMask & fullMask
  } else {
    val n = way / nodeNum
    val selMask = Cat(Seq.tabulate(way)(idx => mbistBd.selectedOH(idx / n)).reverse)
    val fullMask = Fill(nodeNum, mbistBd.wmask)
    selMask & fullMask
  }
  private val mbistWdata = Fill(nodeNum, mbistBd.wdata)
  private val ramWmask = if (hasMbist) Mux(mbistBd.ack, mbistWmask, wmask) else wmask
  private val ramWdata = if (hasMbist) Mux(mbistBd.ack, mbistWdata, wdata) else wdata
  private val ramRen = if (hasMbist) Mux(mbistBd.ack, mbistBd.re, ren) else ren
  private val ramRaddr = if (hasMbist) Mux(mbistBd.ack, mbistBd.addr_rd, raddr) else raddr

  private val wenReg = RegInit(0.U(multicycle.W))
  private val renReg = RegInit(0.U(multicycle.W))
  wenReg := Cat(ramWen, wenReg) >> 1.U
  renReg := Cat(ramRen, renReg) >> 1.U

  private val wenStretched = if(holdMcp) {
    val ewe = if(hasMbist) mbistBd.ewe || io.earlyWen.get else io.earlyWen.get
    val wens = RegInit(0.U((multicycle - 1).W))
    when(ewe) {
      wens := Fill(multicycle - 1, true.B)
    }.otherwise {
      wens := Cat(ramWen, wens) >> 1.U
    }
    wens(0)
  } else {
    ramWen
  }

  private val renStretched = if(holdMcp) {
    val ere = if(hasMbist) mbistBd.ere || io.earlyRen.get else io.earlyRen.get
    val rens = RegInit(0.U((multicycle - 1).W))
    when(ere) {
      rens := Fill(multicycle - 1, true.B)
    }.otherwise {
      rens := Cat(ramRen, rens) >> 1.U
    }
    rens(0)
  } else {
    ramRen
  }

  private val ramRdata = SramProto.read(array, singlePort, ramRaddr, renStretched)
  when(wenStretched) {
    SramProto.write(array, singlePort, ramWaddr, ramWdata, ramWmask)
  }

  cg.dft.fromBroadcast(brcBd)
  cg.mbist.req := mbistBd.ack
  cg.mbist.readen := mbistBd.re
  cg.mbist.writeen := mbistBd.we
  cg.E := ren | wen

  private val concurrentRW = io.w.req.fire && io.r.req.fire && io.w.req.bits.setIdx === io.r.req.bits.setIdx
  private val doBypass = if (bypassWrite) concurrentRW else false.B
  private val doBypassReg = RegEnable(doBypass, false.B, io.r.req.fire)
  private val wmaskReg = RegEnable(wmask, 0.U, doBypass & io.r.req.fire)
  private val segment = dataWidth / wmask.getWidth
  private val bypassMask = Cat(Seq.tabulate(wmask.getWidth)(i => wmaskReg(i / segment).asBool).reverse)
  private val keepMask = Cat(Seq.tabulate(wmask.getWidth)(i => !wmaskReg(i / segment).asBool).reverse)
  private val rdataReg = Reg(UInt(dataWidth.W))
  private val bypassData = bypassMask & rdataReg | keepMask & ramRdata
  if (bypassWrite) {
    when(doBypass) {
      rdataReg := wdata.asUInt
    }.elsewhen(renReg(0)) {
      rdataReg := Mux(doBypassReg, bypassData, ramRdata)
    }
  } else {
    when(renReg(0)) {
      rdataReg := ramRdata
    }
  }

  if (!bypassWrite && !holdRead) {
    io.r.resp.data := ramRdata.asTypeOf(io.r.resp.data)
  } else if (!bypassWrite && holdRead) {
    io.r.resp.data := Mux(renReg(0), ramRdata, rdataReg).asTypeOf(io.r.resp.data)
  } else if (bypassWrite && !holdRead) {
    io.r.resp.data := Mux(doBypassReg, bypassData, ramRdata).asTypeOf(io.r.resp.data)
  } else {
    when(renReg(0)) {
      io.r.resp.data := Mux(doBypassReg, bypassData, ramRdata).asTypeOf(io.r.resp.data)
    }.otherwise {
      io.r.resp.data := rdataReg.asTypeOf(io.r.resp.data)
    }
  }
  private val selectOHReg = RegEnable(mbistBd.selectedOH, renReg(0))
  mbistBd.rdata := Mux1H(selectOHReg, rdataReg.asTypeOf(Vec(nodeNum, UInt((dataWidth / nodeNum).W))))

  private val reqVector = renReg | wenReg
  private val singleHold = if (singlePort) io.w.req.valid else false.B
  private val mcpHold = if (mcp) reqVector(multicycle - 1, 1).orR else false.B
  private val resetHold = if (shouldReset) resetState || resetReg else false.B
  io.r.req.ready := !mcpHold && !resetHold && !singleHold
  io.w.req.ready := !mcpHold && !resetHold
}

class FoldedSRAMTemplate[T <: Data](
  gen:         T,
  set:         Int,
  way:         Int = 1,
  width:       Int = 4,
  singlePort:  Boolean = false,
  shouldReset: Boolean = false,
  extraReset:  Boolean = false,
  holdRead:    Boolean = false,
  bypassWrite: Boolean = false,
  multicycle:  Int = 1,
  hasMbist:    Boolean = false,
  foundry:     String = "Unknown",
  sramInst:    String = "STANDARD")
    extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None
  //   |<----- setIdx ----->|
  //   | ridx | width | way |

  require(width > 0 && isPow2(width))
  require(way > 0 && isPow2(way))
  require(set % width == 0)

  private val nRows = set / width

  private val array = Module(
    new SRAMTemplate(
      gen,
      set = nRows,
      way = width * way,
      shouldReset = shouldReset,
      extraReset = extraReset,
      holdRead = holdRead,
      bypassWrite = bypassWrite,
      singlePort = singlePort,
      multicycle = multicycle,
      hasMbist = hasMbist,
      foundry = foundry,
      sramInst = sramInst
    )
  )
  if (array.extra_reset.isDefined) {
    array.extra_reset.get := extra_reset.get
  }

  io.r.req.ready := array.io.r.req.ready
  io.w.req.ready := array.io.w.req.ready

  private val raddr = io.r.req.bits.setIdx >> log2Ceil(width)
  private val ridx = RegNext(if (width != 1) io.r.req.bits.setIdx(log2Ceil(width) - 1, 0) else 0.U(1.W))
  private val ren = io.r.req.valid

  array.io.r.req.valid := ren
  array.io.r.req.bits.setIdx := raddr

  private val rdata = array.io.r.resp.data
  for (w <- 0 until way) {
    val wayData = VecInit(rdata.indices.filter(_ % way == w).map(rdata(_)))
    val holdRidx = HoldUnless(ridx, RegNext(io.r.req.valid))
    val realRidx = if (holdRead) holdRidx else ridx
    io.r.resp.data(w) := Mux1H(UIntToOH(realRidx, width), wayData)
  }

  private val wen = io.w.req.valid
  private val wdata = VecInit(Seq.fill(width)(io.w.req.bits.data).flatten)
  private val waddr = io.w.req.bits.setIdx >> log2Ceil(width)
  private val widthIdx = if (width != 1) io.w.req.bits.setIdx(log2Ceil(width) - 1, 0) else 0.U
  private val wmask = (width, way) match {
    case (1, 1) => 1.U(1.W)
    case (x, 1) => UIntToOH(widthIdx)
    case _ =>
      VecInit(Seq.tabulate(width * way)(n => (n / way).U === widthIdx && io.w.req.bits.waymask.get(n % way))).asUInt
  }
  require(wmask.getWidth == way * width)
  array.io.w.apply(wen, wdata, waddr, wmask)
}
