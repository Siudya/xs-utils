package xs.utils.dft

import chisel3._
import chisel3.util._
import xs.utils.mbist.InterfaceInfo

class BapBundle(info: InterfaceInfo) extends Bundle {
  val start = Output(Bool())
  val arrayStart = Output(UInt(info.arrayWidth.W))
  val arrayEnd = Output(UInt(info.arrayWidth.W))
  val all = Output(Bool())
  val retMode = Output(UInt(2.W))
  val end = Input(Bool())
  val failed = Input(Bool())
  val failed_array = Input(UInt(info.arrayWidth.W))
  val failed_addr = Input(UInt(info.addrWidth.W))
}

class BAP(info: InterfaceInfo) extends BaseInstrument(Seq(
  "mbist_start" -> (1, 0.U, true),
  "mbist_all" -> (1, 0.U, true),
  "mbist_ret_mode" -> (2, 0.U, true),
  "mbist_array_start" -> (info.arrayWidth, 0.U(info.arrayWidth.W), true),
  "mbist_array_end" -> (info.arrayWidth, Fill(info.arrayWidth, true.B), true),
  "mbist_end" -> (1, 0.U, false),
  "mbist_failed" -> (1, 0.U, false),
  "mbist_failed_array" -> (info.arrayWidth, 0.U, false),
  "mbist_failed_addr" -> (info.addrWidth, 0.U, false)
), s"${info.name}_bap") {
  GlobalData.bapMods += this
  val mbist = IO(new BapBundle(info))
  xs.utils.mbist.MBIST.noDedup(this)
  override val desiredName = s"${info.name}_bap"
  mbist.start := tdrFields("mbist_start")
  mbist.all := tdrFields("mbist_all")
  mbist.retMode := tdrFields("mbist_ret_mode")
  mbist.arrayStart := tdrFields("mbist_array_start")
  mbist.arrayEnd := tdrFields("mbist_array_end")
  tdrShiftFields("mbist_end") := mbist.end
  tdrShiftFields("mbist_failed") := mbist.failed
  tdrShiftFields("mbist_failed_array") := mbist.failed_array
  tdrShiftFields("mbist_failed_addr") := mbist.failed_addr
  FileManager.add("pdl", s"${info.name}_bap", BAP.getPdl(modName))
}

object BAP {
  private val pdl =
    """|
       |iProcsForModule __XSMODNAMESTRREPL__
       |iProc start_mbist {args} {
       |   set ret_mode 0
       |   set all 0
       |   if {[expr [llength $args]%2 != 0]} {
       |      display_message -error "Odd number of arguments. Expecting parameter and value pairs."
       |      return -code error
       |   }
       |   foreach {param value} $args {
       |      set param [string tolower $param]
       |      if {$param eq "ret_mode"} {
       |         set ret_mode $value
       |      } elseif {$param eq "all"} {
       |         set all $value
       |      } else {
       |         display_message -error "Invalid parameter '$param'. Valid parameters are 'ret_mode', 'all' ."
       |         return -code error
       |      }
       |   }
       |
       |   iNote " "
       |   iNote "****************************************************************"
       |   iNote "  Mbist Start __XSMODNAMESTRREPL__"
       |   iWrite mbist_all $all;
       |   iWrite mbist_ret_mode $ret_mode;
       |   iWrite mbist_start 0b1;
       |   iApply;
       |   iWrite mbist_start 0b0;
       |   iApply;
       |   iRunLoop 3000 -tck;
       |   iNote "****************************************************************"
       |   iNote "End of the MBist Start __XSMODNAMESTRREPL__"
       |   iNote " "
       |}
       |
       |iProc check_mbist {} {
       |   iNote " "
       |   iNote "****************************************************************"
       |   iNote "  Mbist Check __XSMODNAMESTRREPL__"
       |   iRead mbist_end 0b1;
       |   iRead mbist_failed 0b0;
       |   iWrite mbist_end 0b0;
       |   iApply;
       |   iNote "****************************************************************"
       |   iNote "End of the MBist Check __XSMODNAMESTRREPL__"
       |   iNote " "
       |}
       |
       |""".stripMargin

  def getPdl(modName: String): String = {
    pdl.replace("__XSMODNAMESTRREPL__", modName)
  }
}
