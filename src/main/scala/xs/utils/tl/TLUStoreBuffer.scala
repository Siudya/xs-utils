package xs.utils.tl

import chisel3._
import chisel3.util._
import xs.utils.CircularQueuePtr
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleField

class TLUStoreBuffer(outstanding: Int = 16)(implicit p: Parameters) extends LazyModule {

  private def cfn(in: Seq[TLMasterPortParameters]): TLMasterPortParameters = {
    in.foreach(e => require(e.masters.count(s => s.supports.probe) == 0, "TLUStoreBuffer does not support TLC client!"))
    in.head.v1copy(
      echoFields = BundleField.union(in.flatMap(_.echoFields)),
      requestFields = BundleField.union(in.flatMap(_.requestFields)),
      responseKeys = in.flatMap(_.responseKeys).distinct,
      minLatency = in.map(_.minLatency).min,
      clients = Seq(TLMasterParameters.v1(
        name = "tlusb",
        sourceId = IdRange(0, outstanding)
      ))
    )
  }

  private def mfn(in: Seq[TLSlavePortParameters]): TLSlavePortParameters = {
    require(in.length == 1, "TLUStoreBuffer does not suppport multiple slaves!")
    in.head
  }

  val node = TLNexusNode(clientFn = cfn, managerFn = mfn)
  lazy val module = new Impl

  sealed class TluSbPtr extends CircularQueuePtr[TluSbPtr](outstanding)

  class Impl extends LazyModuleImp(this) {
    private val ins = node.in
    private val out = node.out.head
    private val maxInSourceBits = ins.map(_._1.params.sourceBits).max
    private val portIdBits = log2Ceil(ins.length)
    private val entryParams = out._1.params.copy(sourceBits = portIdBits + maxInSourceBits)
    private val payload = Mem(outstanding, new TLBundleA(entryParams))
    private val waits = Reg(Vec(outstanding, Bool()))
    private val writebackeds = Reg(Vec(outstanding, Bool()))
    private val enqPtr = RegInit(0.U.asTypeOf(new TluSbPtr))
    private val issPtr = RegInit(0.U.asTypeOf(new TluSbPtr))
    private val respPtr = RegInit(0.U.asTypeOf(new TluSbPtr))
    private val deqPtr = RegInit(0.U.asTypeOf(new TluSbPtr))

    assert(deqPtr <= respPtr)
    assert(respPtr <= issPtr)
    assert(issPtr <= enqPtr)

    //Enqueue
    private val inputArb = Module(new Arbiter(new TLBundleA(out._1.params.copy(sourceBits = portIdBits + maxInSourceBits)), ins.length))
    for (((arb, a), idx) <- inputArb.io.in.zip(ins.map(_._1.a)).zipWithIndex) {
      arb <> a
      arb.bits.source := Cat(idx.U(portIdBits.W), a.bits.source.asTypeOf(UInt(maxInSourceBits.W)))
    }
    private val full = enqPtr.value === deqPtr.value && enqPtr.flag =/= deqPtr.flag
    inputArb.io.out.ready := !full
    when(inputArb.io.out.fire) {
      payload.write(enqPtr.value, inputArb.io.out.bits)
      enqPtr := enqPtr + 1.U
    }

    //Issue
    private val issValidReg = RegInit(false.B)
    private val issBitsReg = Reg(new TLBundleA(out._1.params))
    private val pipeEn = !issValidReg || out._1.a.ready
    private val allowIss = Mux(waits(issPtr.value), deqPtr === issPtr, true.B)
    private val issValid = issPtr =/= enqPtr && allowIss
    when(pipeEn) {
      issValidReg := issValid
    }
    when(pipeEn && issValid) {
      issBitsReg := payload(issPtr.value)
      issBitsReg.source := issPtr.value
      issPtr := issPtr + 1.U
    }
    out._1.a.valid := issValidReg
    out._1.a.bits := issBitsReg

    //Response
    private val outputArbs = Seq.fill(ins.length)(Module(new Arbiter(new TLBundleD(out._1.params.copy(sourceBits = maxInSourceBits)), 2)))
    private val respv = respPtr.value
    private val respEntry = payload(respv)
    private val respValidReg = RegInit(false.B)
    private val respBitsReg = Reg(new TLBundleD(entryParams))
    private val fromOutFires = Wire(Vec(ins.length, Bool()))
    private val fromThisFires = Wire(Vec(ins.length, Bool()))
    for (((arb, d), idx) <- outputArbs.zip(ins.map(_._1.d)).zipWithIndex) {
      d <> arb.io.out
      val fromOut = arb.io.in.head
      val fromThis = arb.io.in.last
      fromOut.valid := respValidReg && respBitsReg.source(portIdBits + maxInSourceBits - 1, maxInSourceBits) === idx.U
      fromOut.bits := respBitsReg
      fromOut.bits.source := respBitsReg.source(maxInSourceBits - 1, 0)
      fromOutFires(idx) := fromOut.fire

      fromThis.valid := respPtr =/= issPtr && !waits(respv) &&
        respEntry.source(portIdBits + maxInSourceBits - 1, maxInSourceBits) === idx.U
      fromThis.bits.opcode := TLMessages.AccessAck
      fromThis.bits.param := 0.U
      fromThis.bits.size := respEntry.size
      fromThis.bits.source := respEntry.source(maxInSourceBits - 1, 0)
      fromThis.bits.data := 0.U
      fromThis.bits.sink := 0.U
      fromThis.bits.denied := false.B
      fromThis.bits.corrupt := false.B
      fromThisFires(idx) := fromThis.fire
    }
    private val dPipeEn = !respValidReg
    out._1.d.ready := dPipeEn || out._1.d.bits.opcode =/= TLMessages.AccessAckData
    private val readRespValid = out._1.d.valid && out._1.d.bits.opcode === TLMessages.AccessAckData
    when(dPipeEn) {
      respValidReg := readRespValid
    }.elsewhen(Cat(fromOutFires).orR) {
      respValidReg := false.B
    }
    when(dPipeEn && readRespValid) {
      respBitsReg := out._1.d.bits
      respBitsReg.source := payload(out._1.d.bits.source).source
    }
    private val respPtrMove = Cat(fromThisFires).orR || writebackeds(respv) && waits(respv) && respPtr =/= issPtr
    when(respPtrMove) {
      respPtr := respPtr + 1.U
    }

    //Dequeue
    when(deqPtr =/= respPtr && writebackeds(deqPtr.value)) {
      deqPtr := deqPtr + 1.U
    }

    //Flags update
    for (idx <- 0 until outstanding) {
      //waits
      when(inputArb.io.out.fire && enqPtr.value === idx.U) {
        waits(idx) := inputArb.io.out.bits.opcode === TLMessages.Get
      }
      //writebackeds
      when(inputArb.io.out.fire && enqPtr.value === idx.U) {
        writebackeds(idx) := false.B
      }.elsewhen(out._1.d.fire && out._1.d.bits.source === idx.U) {
        writebackeds(idx) := true.B
      }
    }
  }
}
