package xs.utils.mbist.algorithm

import chisel3._
import chisel3.util._
import xs.utils.mbist.algorithm.AlgoOpCode.opWait
import xs.utils.mbist.controller.MbistControllerParam

import scala.collection.mutable

object AlgoOpCode {
  private val width = 4

  def opWait: UInt = 0.U(width.W)

  def opWrite: UInt = 1.U(width.W)

  def opRead: UInt = 2.U(width.W)

  def opWriteAdj: UInt = 3.U(width.W)

  def opReadAdj: UInt = 4.U(width.W)

  def opWriteSeg: UInt = 5.U(width.W)

  def opReadSeg: UInt = 6.U(width.W)

  def opWriteMask0: UInt = 7.U(width.W)

  def opReadNoCompare: UInt = 8.U(width.W)

  def apply(): UInt = UInt(width.W)

  val readOps = Seq(opRead, opReadAdj, opReadSeg, opReadNoCompare)
  val writeOps = Seq(opWrite, opWriteAdj, opWriteSeg, opWriteMask0)
}

object PatternCode {
  def patA: UInt = 0.U(3.W)

  def pat5: UInt = 7.U(3.W)

  def patC: UInt = 1.U(3.W)

  def patIC: UInt = 6.U(3.W)

  def pat1: UInt = 2.U(3.W)

  def pat0: UInt = 5.U(3.W)

  def apply(): UInt = UInt(3.W)
}


class AlgoInstr extends Bundle {
  val op = AlgoOpCode()
  val pat = PatternCode()
  val last = Bool()
  val cncrntWrite = Bool()
  val cncrntRead = Bool()
  val valid = Bool()
}

class AlgoLoopInfo(param: MbistControllerParam) extends Bundle {
  val colAll = Bool()
  val colIdx = UInt(param.colsBits.W)
  val rowAll = Bool()
  val rowIdx = UInt(param.rowsBits.W)
  val rowFirst = Bool()
  val maskWalk = Bool()
  val colForward = Bool()
  val rowForward = Bool()
}

class AlgoBlock(instrNum: Int, param: MbistControllerParam) extends Bundle {
  val loop = new AlgoLoopInfo(param)
  val ops = Vec(instrNum, new AlgoInstr)
}

class AlgoServiceIntf(instrNum: Int, param: MbistControllerParam) extends Bundle {
  val req = Input(Bool())
  val ack = Output(Bool())
  val ackD = Output(new AlgoBlock(instrNum, param))
}

case class OpDesc(op: UInt, pat: UInt, cr: Boolean, cw: Boolean)

object LoopContext {
  var id = 0

  def getId(): Int = {
    id = id + 1
    id - 1
  }
}

class LoopContext {
  val id = LoopContext.getId()
  var ca = true
  var ci = 0
  var ra = true
  var ri = 0
  var ma = false
  var rof = false
  var cf = true
  var rf = true
  val ops = mutable.ListBuffer[OpDesc]()

  override def toString: String = {
    s"ID: $id ops: ${ops.length}"
  }
}

trait AlgoUtil {
  val ctxs = mutable.ListBuffer[LoopContext]()
  var psrt_s2p_start = 0
  var psrt_s2p_end = 0
  var psrt_p2p_start = 0
  var psrt_p2p_end = 0
  var psrt_p2e_start = 0
  var psrt_p2e_end = 0

  import PatternCode._

  def for_elm(rowFirst: Boolean = false, cForward: Boolean = true, rForward: Boolean = true)(block: => Any): Unit = {
    val res = new LoopContext
    res.rof = rowFirst
    res.cf = cForward
    res.rf = rForward
    ctxs += res
    block
  }

  def for_elm(block: => Any): Unit = {
    val res = new LoopContext
    ctxs += res
    block
  }

  def for_col(rowIdx: Int, cForward: Boolean = true)(block: => Any): Unit = {
    val res = new LoopContext
    res.ra = false
    res.ri = rowIdx
    res.cf = cForward
    ctxs += res
    block
  }

  def for_row(colIdx: Int, rForward: Boolean = true)(block: => Any): Unit = {
    val res = new LoopContext
    res.ca = false
    res.ci = colIdx
    res.cf = rForward
    ctxs += res
    block
  }

  def wr(pat: UInt, cw: Boolean = true, cr: Boolean = true): Unit = {
    val ctx = ctxs.last
    ctx.ops += OpDesc(AlgoOpCode.opWrite, pat, cw, cr)
    ctx.ops += OpDesc(AlgoOpCode.opRead, pat, cw, cr)
  }

  def wrs(pat: UInt, cw: Boolean = true, cr: Boolean = true): Unit = {
    val ctx = ctxs.last
    ctx.ops += OpDesc(AlgoOpCode.opWriteSeg, pat, cw, cr)
    ctx.ops += OpDesc(AlgoOpCode.opReadSeg, (~pat).asUInt, cw, cr)
  }

  def w(pat: UInt, cw: Boolean = true, cr: Boolean = true): Unit = {
    val ctx = ctxs.last
    ctx.ops += OpDesc(AlgoOpCode.opWrite, pat, cw, cr)
  }

  def wrm0(pat: UInt, previousPat: UInt, cw: Boolean = true, cr: Boolean = true): Unit = {
    val ctx = ctxs.last
    ctx.ops += OpDesc(AlgoOpCode.opWriteMask0, pat, cw, cr)
    ctx.ops += OpDesc(AlgoOpCode.opRead, previousPat, cw, cr)
  }

  def rc(pat: UInt, cw: Boolean = true, cr: Boolean = true): Unit = {
    val ctx = ctxs.last
    ctx.ops += OpDesc(AlgoOpCode.opRead, pat, cw, cr)
  }

  def rnc(cw: Boolean = true, cr: Boolean = true): Unit = {
    val ctx = ctxs.last
    ctx.ops += OpDesc(AlgoOpCode.opReadNoCompare, pat0, cw, cr)
  }

  def rc_adj(pat: UInt, cw: Boolean = true, cr: Boolean = true): Unit = {
    val ctx = ctxs.last
    ctx.ops += OpDesc(AlgoOpCode.opReadAdj, pat, cw, cr)
  }

  def w_adj(pat: UInt, cw: Boolean = true, cr: Boolean = true): Unit = {
    val ctx = ctxs.last
    ctx.ops += OpDesc(AlgoOpCode.opWriteAdj, pat, cw, cr)
  }

  def finish(): Unit = {
    val res = new LoopContext
    res.ops += OpDesc(AlgoOpCode.opWait, pat0, true, true)
    ctxs += res
  }

  def s2p_start(): Unit = {
    psrt_s2p_start = ctxs.length
  }

  def s2p_end(): Unit = {
    psrt_s2p_end = ctxs.length
  }

  def p2p_start(): Unit = {
    psrt_p2p_start = ctxs.length
  }

  def p2p_end(): Unit = {
    psrt_p2p_end = ctxs.length
  }

  def p2e_start(): Unit = {
    psrt_p2e_start = ctxs.length
  }

  def p2e_end(): Unit = {
    psrt_p2e_end = ctxs.length
  }
}


trait AlgoInstantiation {
  m: Module with AlgoUtil =>

  def param: MbistControllerParam

  finish()

  private val opBlockNum = ctxs.length
  val maxInstrNum: Int = ctxs.map(_.ops.length).max
  private val pointer = RegInit(1.U(opBlockNum.W))
  private val entry = new AlgoBlock(maxInstrNum, param)
  private val instrRom = Wire(Vec(opBlockNum, UInt(entry.getWidth.W)))
  dontTouch(instrRom)

  for ((elm, ctx) <- instrRom.zip(ctxs)) {
    val rom = Wire(entry)
    rom.loop.colAll := ctx.ca.B
    rom.loop.colIdx := ctx.ci.U
    rom.loop.rowAll := ctx.ra.B
    rom.loop.rowIdx := ctx.ri.U
    rom.loop.rowFirst := ctx.rof.B
    rom.loop.maskWalk := ctx.ma.B
    rom.loop.colForward := ctx.cf.B
    rom.loop.rowForward := ctx.rf.B
    for ((op, idx) <- ctx.ops.zipWithIndex) {
      val rop = rom.ops(idx)
      rop.op := op.op
      rop.pat := op.pat
      rop.last := (idx == ctx.ops.length - 1).B
      rop.cncrntWrite := op.cw.B
      rop.cncrntRead := op.cr.B
      rop.valid := true.B
    }
    for (rop <- rom.ops.drop(ctx.ops.length)) {
      rop := DontCare
      rop.last := false.B
      rop.valid := false.B
    }
    elm := rom.asTypeOf(UInt(entry.getWidth.W))
  }

  val io = IO(new Bundle {
    val intf = new AlgoServiceIntf(maxInstrNum, param)
    val algoDone = Output(Bool())
    val scoreBoardEmpty = Input(Bool())
    val retMode = Input(UInt(2.W))
    val start = Input(Bool())
    val kill = Input(Bool())
  })
  private val restart = io.start || io.kill
  private val entrySel = Mux1H(pointer, instrRom).asTypeOf(entry)
  private val algoDoneReg = RegInit(false.B)
  dontTouch(entrySel)
  require(psrt_s2p_start < psrt_s2p_end)
  require(psrt_p2p_start < psrt_p2p_end)
  require(psrt_p2e_start < psrt_p2e_end)

  private val start = MuxCase(1.U, Seq(
    (io.retMode === 1.U) -> (1 << psrt_s2p_start).U,
    (io.retMode === 2.U) -> (1 << psrt_p2p_start).U,
    (io.retMode === 3.U) -> (1 << psrt_p2e_start).U,
  ))

  private val endMet = MuxCase(pointer(ctxs.length - 1), Seq(
    (io.retMode === 1.U) -> pointer(psrt_s2p_end),
    (io.retMode === 2.U) -> pointer(psrt_p2p_end),
    (io.retMode === 3.U) -> pointer(psrt_p2e_end),
  ))

  when(algoDoneReg || restart) {
    pointer := start
  }.elsewhen(io.intf.req && io.intf.ack && !endMet) {
    pointer := Cat(pointer(opBlockNum - 2, 0), false.B)
  }

  private val ackValid = io.intf.req && !endMet && !algoDoneReg
  io.intf.ack := RegNext(ackValid, false.B)
  io.intf.ackD := RegEnable(entrySel, ackValid)
  when(algoDoneReg) {
    algoDoneReg := !io.scoreBoardEmpty
  }.otherwise {
    algoDoneReg := endMet && io.intf.req
  }
  io.algoDone := algoDoneReg & io.scoreBoardEmpty
}
