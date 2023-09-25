package xs.util.perf
import chisel3._
import chisel3.util.HasBlackBoxResource
class LogHelper extends BlackBox with HasBlackBoxResource{
  val io = IO(new Bundle{
    val clean = Output(Bool())
    val dump = Output(Bool())
    val logEnable = Output(Bool())
    val timer = Output(UInt(64.W))
  })
  addResource("/LogHelper.v")
}
