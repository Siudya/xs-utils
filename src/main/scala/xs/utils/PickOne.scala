package xs.utils
import chisel3._
import chisel3.util._
class IdxGenerator(entryNum:Int) extends Module {
  private val idxWidth = entryNum
  val io = IO(new Bundle {
    val entriesValidBitVec = Input(UInt(entryNum.W))
    val entryIndexOH = Valid(UInt(idxWidth.W))
  })
  private val idxOHList = Seq.tabulate(entryNum)(idx => (1 << idx).U(idxWidth.W))
  private val candidates = io.entriesValidBitVec.asBools zip idxOHList
  private def validMux(a:(Bool,UInt), b:(Bool,UInt)) : (Bool,UInt) = {
    val resData = Mux(a._1, a._2, b._2)
    val valid = a._1 | b._1
    (valid, resData)
  }
  private val res = ParallelOperation(candidates, validMux)
  io.entryIndexOH.valid := res._1
  io.entryIndexOH.bits := res._2

  assert(Mux(io.entryIndexOH.valid, (io.entryIndexOH.bits & io.entriesValidBitVec).orR, true.B))
}

object PickOneHigh{
  def apply(in:UInt):Valid[UInt] = {
    val entryIdxGenerator = Module(new IdxGenerator(in.getWidth))
    entryIdxGenerator.io.entriesValidBitVec := in
    entryIdxGenerator.io.entryIndexOH
  }
  def apply(in: Seq[Bool]): Valid[UInt] = {
    val entryIdxGenerator = Module(new IdxGenerator(in.length))
    entryIdxGenerator.io.entriesValidBitVec := Cat(in.reverse)
    entryIdxGenerator.io.entryIndexOH
  }
}
object PickOneLow{
  def apply(in: UInt): Valid[UInt] = {
    val entryIdxGenerator = Module(new IdxGenerator(in.getWidth))
    entryIdxGenerator.io.entriesValidBitVec := ~in
    entryIdxGenerator.io.entryIndexOH
  }

  def apply(in: Seq[Bool]): Valid[UInt] = {
    val entryIdxGenerator = Module(new IdxGenerator(in.length))
    entryIdxGenerator.io.entriesValidBitVec := ~Cat(in.reverse)
    entryIdxGenerator.io.entryIndexOH
  }
}