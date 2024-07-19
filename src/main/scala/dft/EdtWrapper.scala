package xs.utils.dft

import chisel3._
import chisel3.util.HasBlackBoxInline
import xs.utils.{FileRegisters, GlobalData}
class EdtFuncBundle(ci:Int, co:Int) extends Bundle {
  val update = Input(Bool())
  val in_channels = Input(UInt(ci.W))
  val out_channels = Output(UInt(co.W))
}

class EdtWrapper(val mName: String = "XsEdtWrapper", val instName: String = "edt", ciw: Int, cow: Int, sw: Int, chainRange: (Int, Int)) extends BlackBox with HasIjtagNode with HasBlackBoxInline {
  require(ciw > 0)
  require(cow > 0)
  require(sw > 0)
  GlobalData.addEdt(this)
  override val desiredName: String = mName
  ijtagNode.instName = instName
  val io = IO(new Bundle {
    val ijtag = new IjtagIntf(ijtagNode)
    val ci = Input(UInt(ciw.W))
    val co = Output(UInt(cow.W))
    val update = Input(Bool())
    val scan_en = Input(Bool())
    val atpg_clock = Input(Clock())
  })
  private val ciStr = if (ciw == 1) "input ci" else s"input [${ciw - 1}:0] ci"
  private val coStr = if (cow == 1) "output co" else s"output [${cow - 1}:0] co"
  setInline(s"$modName.sv",
    s"""|module $modName(
        |  input ijtag_ce,
        |  input ijtag_reset,
        |  input ijtag_se,
        |  input ijtag_sel,
        |  input ijtag_si,
        |  output ijtag_so,
        |  input ijtag_tck,
        |  input ijtag_ue,
        |  $ciStr,
        |  $coStr,
        |  input atpg_clock,
        |  input scan_en,
        |  input update
        |);
        |  assign co = 0;
        |  assign ijtag_so = 0;
        |endmodule
        |""".stripMargin
  )
  private var channelsDesc = ""
  for (idx <- 0 until ciw) {
    if (ciw == 1) {
      channelsDesc +=
        s"""|        EdtChannelsIn(${idx + 1}) {
            |          port_pin_name : ci;
            |        }
            |""".stripMargin
    } else {
      channelsDesc +=
        s"""|        EdtChannelsIn(${idx + 1}) {
            |          port_pin_name : ci[$idx];
            |        }
            |""".stripMargin
    }
  }
  for (idx <- 0 until cow) {
    if (cow == 1) {
      channelsDesc +=
        s"""|        EdtChannelsOut(${idx + 1}) {
            |          port_pin_name : co;
            |        }
            |""".stripMargin
    } else {
      channelsDesc +=
        s"""|        EdtChannelsOut(${idx + 1}) {
            |          port_pin_name : co[$idx];
            |        }
            |""".stripMargin
    }
  }
  private val scr =
    s"""|set rtl_dir ""
        |foreach item $$argv {
        |  set args [split $$item "="]
        |  set len [llength $$args]
        |  if {$$len == 2} {
        |    set key [lindex $$args 0]
        |    if {$$key eq "rtl_dir"} {
        |      set rtl_dir [lindex $$args 1]
        |    } else {
        |      display_message -error "Invalid parameter '$$key' ."
        |      exit
        |    }
        |  }
        |}
        |
        |if {$$rtl_dir eq ""} {
        |  display_message -error "rtl_dir must be set."
        |  exit
        |}
        |
        |puts "rtl_dir: $$rtl_dir"
        |
        |set_context dft -rtl
        |
        |read_verilog $$rtl_dir/$modName.sv -format sv2009
        |
        |set_current_design $modName
        |
        |set_design_level instrument_block
        |
        |set_tsdb_output_directory tsdb_outdir
        |
        |add_dft_signals scan_en edt_update test_clock -source_node { scan_en update atpg_clock }
        |
        |add_dft_signals edt_clock shift_capture_clock -create_from_other_signals
        |
        |set_system_mode analysis
        |
        |set spec [create_dft_specification -sri_sib_list edt]
        |
        |read_config_data -in $$spec -from_string {
        |  EDT {
        |    ijtag_host_interface : Sib(edt);
        |    Controller (int) {
        |      longest_chain_range : ${chainRange._1}, ${chainRange._2};
        |      scan_chain_count : $sw;
        |      input_channel_count : $ciw;
        |      output_channel_count : $cow;
        |      Connections {
        |$channelsDesc      }
        |    }
        |  }
        |}
        |
        |process_dft_specification
        |
        |extract_icl -create_ijtag_graybox on
        |
        |exit
        |""".stripMargin
  FileManager.add("tcl", modName, scr)
}
