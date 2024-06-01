package xs.utils.dft

import chisel3._
import xs.utils.DFTResetSignals
import xs.utils.sram.BroadCastBundle

class TestController extends BaseInstrument(Seq(
  "dft_lgc_rst_n" -> (1, 1.U, true),
  "dft_mode" -> (1, 0.U, true),
  "scan_mode" -> (1, 0.U, true),
  "dft_ram_hold" -> (1, 0.U, true),
  "dft_ram_bypass" -> (1, 0.U, true),
  "dft_ram_bp_clken" -> (1, 0.U, true),
  "dft_l3dataramclk_bypass" -> (1, 0.U, true),
  "dft_cgen" -> (1, 0.U, true)
), "TestController") {
  GlobalData.ctrlMods += this
  val io = IO(new Bundle {
    val rf2p_ctrl = Input(UInt(20.W))
    val rmsp_hd_ctrl = Input(UInt(13.W))
    val rmsp_hs_ctrl = Input(UInt(17.W))
    val l3dataram_clk = Input(Bool())

    val sram = Flipped(new BroadCastBundle)
    val rstCtrl = Output(new DFTResetSignals)
  })
  io.sram.ram_hold := tdrFields("dft_ram_hold")
  io.sram.ram_bypass := tdrFields("dft_ram_bypass")
  io.sram.ram_bp_clken := tdrFields("dft_ram_bp_clken")
  io.sram.l3dataram_clk := io.l3dataram_clk
  io.sram.l3dataramclk_bypass := tdrFields("dft_l3dataramclk_bypass")
  io.sram.cgen := tdrFields("dft_cgen")
  io.sram.rf2p_ctrl := io.rf2p_ctrl
  io.sram.rmsp_hd_ctrl := io.rmsp_hd_ctrl
  io.sram.rmsp_hs_ctrl := io.rmsp_hs_ctrl

  io.rstCtrl.lgc_rst_n := tdrFields("dft_lgc_rst_n").asBool.asAsyncReset
  io.rstCtrl.mode := tdrFields("dft_mode")
  io.rstCtrl.scan_mode := tdrFields("scan_mode")
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
       |   iWrite dft_mode 0b0;
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

