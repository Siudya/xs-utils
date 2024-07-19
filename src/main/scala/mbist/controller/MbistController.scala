package xs.utils.mbist.controller

import chisel3._
import chisel3.util._
import xs.utils.dft.BapBundle
import xs.utils.mbist.{InterfaceInfo, MbitsStandardInterface}
import xs.utils.mbist.Mbist.PipelineBaseNode
import xs.utils.mbist.algorithm.AlgoInstantiation
import xs.utils.sram.SramMeta

class MbistController(node: PipelineBaseNode, algorithm: String = "SMarchCHKBvcd", name: String = "MbistController")
    extends Module {
  override val desiredName: String = name + "_" + algorithm
  private val sramMetaSeq = SramMeta(node.ramParamsBelongToThis, node.array_id, node.array_depth)
  val param: MbistControllerParam = MbistControllerParam(sramMetaSeq)
  private val algoName = "xs.utils.mbist.algorithm." + algorithm

  val io = IO(new Bundle {
    val bap = Flipped(new BapBundle(param))
    val mbist = Flipped(new MbitsStandardInterface(node.bd.params))
  })
  private val sramInfoRom = Module(new MbistSramInfoRom(sramMetaSeq, param))
  private val algo = Module(
    Class
      .forName(algoName)
      .getConstructor(classOf[MbistControllerParam])
      .newInstance(param)
      .asInstanceOf[Module with AlgoInstantiation]
  )
  private val processor = Module(new MbistAlgorithmProcessor(algo.maxInstrNum, param))
  private val scoreBoard = Module(new MbistScoreBoard(param))

  private val startSyncDepth = 6
  private val startSampler = RegInit(0.U(startSyncDepth.W))
  startSampler := Cat(startSampler(startSyncDepth - 2, 0), io.bap.start)
  private val resetPulse = startSampler(startSyncDepth - 2, startSyncDepth - 3) === 1.U
  private val startPulse = startSampler(startSyncDepth - 1, startSyncDepth - 2) === 1.U
  private val testIdStartReg = RegEnable(io.bap.arrayStart, resetPulse)
  private val testIdEndReg = RegEnable(io.bap.arrayEnd, resetPulse)
  private val allReg = RegEnable(io.bap.all, resetPulse)
  private val retModeReg = RegEnable(io.bap.retMode, resetPulse)

  private val endReg = RegInit(false.B)
  private val failedReg = RegInit(false.B)
  when(endReg && resetPulse) {
    endReg := false.B
    failedReg := false.B
  }.elsewhen(!endReg) {
    endReg := scoreBoard.io.failed.valid || sramInfoRom.io.finished
    failedReg := scoreBoard.io.failed.valid
  }
  private val killPulse = endReg || resetPulse

  io.bap.end := endReg
  io.bap.failed := failedReg
  io.bap.failed_array := RegEnable(scoreBoard.io.failed.bits.array, scoreBoard.io.failed.valid)
  io.bap.failed_addr := RegEnable(scoreBoard.io.failed.bits.addr, scoreBoard.io.failed.valid)

  sramInfoRom.io.start := startPulse
  sramInfoRom.io.kill := killPulse
  sramInfoRom.io.testIdStart := testIdStartReg
  sramInfoRom.io.testIdEnd := testIdEndReg
  sramInfoRom.io.next := algo.io.algoDone

  algo.io.intf <> processor.io.instrReq
  algo.io.scoreBoardEmpty := scoreBoard.io.empty
  algo.io.start := startPulse
  algo.io.kill := killPulse
  algo.io.retMode := retModeReg

  scoreBoard.io.rreq := processor.io.rreq
  scoreBoard.io.rdata := io.mbist.outdata

  processor.io.sram := sramInfoRom.io.info
  processor.io.start := algo.io.algoDone || startPulse
  processor.io.kill := killPulse
  private val wen = processor.io.wreq.valid
  private val ren = processor.io.rreq.valid
  private val wreq = processor.io.wreq.bits
  private val rreq = processor.io.rreq.bits

  io.mbist.array := Mux(wen, wreq.array, rreq.array)
  io.mbist.all := allReg
  io.mbist.req := sramInfoRom.io.req
  sramInfoRom.io.ack := io.mbist.ack

  io.mbist.writeen := wen
  io.mbist.be := wreq.mask
  io.mbist.addr := wreq.addr
  io.mbist.indata := wreq.data

  io.mbist.readen := ren
  io.mbist.addr_rd := Mux(processor.io.singlePort & wen, wreq.addr, rreq.addr)
}
