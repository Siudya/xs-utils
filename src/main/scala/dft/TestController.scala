package xs.utils.dft

import chisel3._
import xs.utils.DFTResetSignals
import xs.utils.sram.SramBroadcastBundle
import xs.utils.GlobalData

class TestController
    extends BaseInstrument(
      Seq(
        "dft_lgc_rst_n" -> (1, 1.U, true),
        "dft_mode" -> (1, 0.U, true),
        "scan_mode" -> (1, 0.U, true),
        "dft_ram_hold" -> (1, 0.U, true),
        "dft_ram_bypass" -> (1, 0.U, true),
        "dft_ram_bp_clken" -> (1, 0.U, true),
        "dft_ram_aux_ckbp" -> (1, 0.U, true),
        "dft_ram_mcp_hold" -> (1, 0.U, true),
        "dft_cgen" -> (1, 0.U, true),
        "dft_occ_shift_only" -> (1, 0.U, true)
      ),
      "TestController"
    ) {
  GlobalData.addCtrl(this)
  val io = IO(new Bundle {
    val ram_aux_clk = Input(Bool())
    val sram = Flipped(new SramBroadcastBundle)
    val rstCtrl = Output(new DFTResetSignals)
    val shiftOnlyMode = Output(Bool())
  })
  io.sram.ram_hold := tdrFields("dft_ram_hold")
  io.sram.ram_bypass := tdrFields("dft_ram_bypass")
  io.sram.ram_bp_clken := tdrFields("dft_ram_bp_clken")
  io.sram.ram_aux_clk := io.ram_aux_clk
  io.sram.ram_aux_ckbp := tdrFields("dft_ram_aux_ckbp")
  io.sram.ram_mcp_hold := tdrFields("dft_ram_mcp_hold")
  io.sram.cgen := tdrFields("dft_cgen")

  io.rstCtrl.lgc_rst_n := tdrFields("dft_lgc_rst_n").asBool.asAsyncReset
  io.rstCtrl.mode := tdrFields("dft_mode")
  io.rstCtrl.scan_mode := tdrFields("scan_mode")
  io.shiftOnlyMode := tdrFields("dft_occ_shift_only")
  FileManager.add("pdl", "TestController", TestController.getPdl(modName))
}

object TestController {
  private val pdl =
    """|
       |iProcsForModule __XSMODNAMESTRREPL__
       |iProc sysreset {} {
       |   iNote " "
       |   iNote "****************************************************************"
       |   iNote "  System Reset"
       |   iNote "****************************************************************"
       |   iNote " "
       |   iWrite dft_mode 0b1;
       |   iWrite dft_lgc_rst_n 0b0;
       |   iApply;
       |   iWrite dft_lgc_rst_n 0b1;
       |   iApply;
       |   iNote "End of the System Reset"
       |   iNote " "
       |}
       |""".stripMargin

  def getPdl(modName: String): String = {
    pdl.replace("__XSMODNAMESTRREPL__", modName)
  }
}
