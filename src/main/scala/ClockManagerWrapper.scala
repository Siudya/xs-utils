package xs.utils

import chisel3._
import chisel3.util.HasBlackBoxInline

class ClockManagerWrapper extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val cfg = Input(Vec(8, UInt(64.W)))
    val in_clock = Input(Clock())
    val cpu_clock = Output(Clock())
    val timer_clock = Output(Clock())
  })
  private val modName = s"${GlobalData.prefix}ClockManagerWrapper"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""
       |module $modName (
       |  input  wire [63:0] cfg_0,
       |  input  wire [63:0] cfg_1,
       |  input  wire [63:0] cfg_2,
       |  input  wire [63:0] cfg_3,
       |  input  wire [63:0] cfg_4,
       |  input  wire [63:0] cfg_5,
       |  input  wire [63:0] cfg_6
       |  input  wire [63:0] cfg_7,
       |  input  wire in_clock,
       |  output wire cpu_clock,
       |  output wire timer_clock
       |);
       |  assign cpu_clock = in_clock,
       |  assign timer_clock = in_clock
       |endmodule
       |""".stripMargin)
}
