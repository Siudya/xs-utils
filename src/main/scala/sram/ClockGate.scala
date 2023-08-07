package xs.utils.sram
import chisel3._
import chisel3.util._

class ClockGate extends BlackBox with HasBlackBoxResource {
    val io = IO(new Bundle{
        val in = Input(Clock())
        val test_en = Input(Bool())
        val en = Input(Bool())
        val out = Output(Clock())
    })
    addResource("/ClockGate.v")
}