package xs.utils.mbist.controller

import chisel3._
import chisel3.util._

class ScoreBoardEntry(param: MbistControllerParam) extends Bundle {
  val data = UInt(param.maxDataWidth.W)
  val array = UInt(param.arrayBits.W)
  val addr = UInt(param.addrBits.W)
}

class MbistScoreBoard(param: MbistControllerParam) extends Module {
  val io = IO(new Bundle {
    val rreq = Input(Valid(new MbistReqBundle(param)))
    val rdata = Input(UInt(param.maxDataWidth.W))
    val failed = Output(Valid(new ScoreBoardEntry(param)))
    val empty = Output(Bool())
  })
  private val size = param.maxLatency + 1

  private val refDataVec = Reg(Vec(size, new ScoreBoardEntry(param)))
  private val refCntVec = Reg(Vec(size, UInt(param.latencyBits.W)))
  private val valids = RegInit(VecInit(Seq.fill(size)(false.B)))

  private val empties = valids.map(!_)
  private val allocatedIdxOH = PriorityEncoderOH(empties)

  private val rreq = io.rreq.bits
  private val rreqDepth = rreq.depth
  for (idx <- refDataVec.indices) {
    val storeValid = io.rreq.valid && allocatedIdxOH(idx) && !rreq.noCompare
    when(storeValid) {
      refDataVec(idx).data := rreq.data
      refDataVec(idx).addr := rreq.addr
      refDataVec(idx).array := rreq.array
    }
    when(valids(idx)) {
      valids(idx) := refCntVec(idx) =/= 0.U
    }.otherwise {
      valids(idx) := storeValid
    }
    when(storeValid) {
      refCntVec(idx) := rreqDepth
    }.elsewhen(refCntVec(idx).orR) {
      refCntVec(idx) := refCntVec(idx) - 1.U
    }

    when(storeValid) {
      assert(valids(idx) === false.B, s"entry $idx can not be allocated!")
    }
  }

  private val refSelVec = valids.zip(refCntVec).map({ case (v, c) => v && c === 0.U })
  private val refDataSel = Mux1H(refSelVec, refDataVec)
  private val checkRdata = refSelVec.reduce(_ || _)
  private val failed = refDataSel.data =/= io.rdata && checkRdata
  io.failed.valid := RegNext(failed, false.B)
  io.failed.bits := RegEnable(refDataSel, failed)
  io.empty := RegNext(!valids.reduce(_ || _), false.B)
}
