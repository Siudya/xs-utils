package xs.util.perf

import chisel3._
import chisel3.util.{HasBlackBoxInline, HasBlackBoxResource}

class LogHelper extends HasBlackBoxInline {
  val io = IO(new Bundle {
    val clean = Output(Bool())
    val dump = Output(Bool())
    val logEnable = Output(Bool())
    val timer = Output(UInt(64.W))
  })
  private val modName = xs.utils.dft.GlobalData.modulePrefix + "LogHelper"
  setInline(modName + ".sv",
    s"""|module $modName (
        |  output clean,
        |  output dump,
        |  output logEnable,
        |  output [63:0] timer
        |);
        |  `ifndef SIM_TOP_MODULE_NAME
        |    `define SIM_TOP_MODULE_NAME SimTop
        |  `endif
        |  assign timer         = `SIM_TOP_MODULE_NAME.timer;
        |  assign logEnable     = `SIM_TOP_MODULE_NAME.logEnable;
        |  assign clean         = `SIM_TOP_MODULE_NAME.clean;
        |  assign dump          = `SIM_TOP_MODULE_NAME.dump;
        |endmodule""".stripMargin)
}
