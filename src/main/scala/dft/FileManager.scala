package xs.utils.dft
import xs.utils.FileRegisters
import scala.collection.mutable
import xs.utils.GlobalData


class FileManager(val merge: Boolean, ft: String) {
  private val fileStrs: mutable.ListBuffer[String] = mutable.ListBuffer[String]()
  private val filesKeys: mutable.ListBuffer[String] = mutable.ListBuffer[String]()
  private val suffix = ft

  def addContents(key: String, str: String): Unit = {
    if (!filesKeys.contains(key)) {
      filesKeys += key
      if (merge) {
        if (fileStrs.isEmpty) {
          fileStrs += str
        } else {
          fileStrs(0) = fileStrs.head + str
        }
      } else {
        fileStrs += str
      }
    }
  }

  def writeOut(): Unit = {
    for ((str, fn) <- fileStrs.zip(fileNames)) {
      FileRegisters.add("dft", fn, str)
    }
  }

  lazy val fileNames:Seq[String] = {
    fileStrs.zip(filesKeys).map({case(str, key) =>
      val fn = if (merge || key == "") s"$suffix" else s"$key.$suffix"
      fn.replace("..", ".")
    }).toSeq
  }
}

object FileManager {
  var rtlDir = "../rtl"
  var macroDir = "../macro"
  private val typeSeq = Seq(("icl", true), ("pdl", true), ("tcl", false), ("mk", true), ("tcd", false))
  private val fileMap = new mutable.HashMap[String, FileManager]
  for ((t, m) <- typeSeq) fileMap(t) = new FileManager(m, t)

  def add(ftype: String, key: String, str: String): Unit = {
    if (fileMap.contains(ftype)) fileMap(ftype).addContents(key, str)
  }

  def writeOut(name: String): Unit = {
    Ijtag.exportIcl()
    add("mk", name, MakefileGen(name, rtlDir, macroDir))
    add("tcl", "", TopTclGen())
    for ((_, fm) <- fileMap) fm.writeOut()
  }
}

object MakefileGen {
  def apply(prefix: String, rtlDir: String, macroDir:String): String = {
    var res = s"RTL_DIR ?= $rtlDir\n"
    res += s"MACRO_DIR ?= $macroDir\n"
    res += "ABS_RTL_DIR = $(abspath $(RTL_DIR))\n"
    res += "ABS_MACRO_DIR = $(abspath $(MACRO_DIR))\n"
    res += s"TOP_FILELIST = $prefix.f\n"
    res += s"TOP_CTL = $prefix.tcl\n\n"
    res += getEdtTarget(prefix)
    res += s"TOP_ICL = $prefix.tcl\n"
    res += getTopFileTarget
    res += getTopTarget
    res += cleanTarget
    res += getPhonyTarget
    res
  }

  def getEdtTarget(prefix: String): String = {
    var res = "edt: $(EDT_TCL_LIST)\n"
    for (edt <- GlobalData.edtMods.map(_.modName).toSet[String]) {
      val name = s"$prefix.$edt.tcl"
      res += s"\ttessent -shell -dofile $name -arg rtl_dir=$$(ABS_RTL_DIR)\n"
    }
    res + "\n"
  }

  def getTopFileTarget: String = {
    var res = "\n"
    res += "$(TOP_FILELIST):\n"
    res += "\t@find $(ABS_RTL_DIR) -regex \".*\\.sv\\|.*\\.v\" > $@\n"
    res += "\t@find $(ABS_MACRO_DIR) -regex \".*\\.sv\\|.*\\.v\" >> $@\n"
    for (edt <- GlobalData.edtMods.map(_.modName).toSet[String]) {
      res += s"\t@sed -i '/$edt/d' $$@\n"
    }
    res
  }

  def getTopTarget: String = {
    s"""
       |top: $$(TOP_ICL) $$(TOP_FILELIST) edt
       |\ttessent -shell -dofile $$(TOP_ICL)
       |
       |all: top
       |
       |default: all
       |""".stripMargin
  }

  def cleanTarget: String = {
    s"""
       |clean:
       |\trm -rf tsdb_outdir
       |\trm $$(TOP_FILELIST)
       |""".stripMargin
  }

  def getPhonyTarget: String = {
    s"""
       |.PHONY: clean $$(TOP_FILELIST) edt top
       |""".stripMargin
  }
}

object TopTclGen {
  private def getPath(pathName: String): String = {
    pathName.split("\\.").tail.reduce((a: String, b: String) => s"$a.$b")
  }

  def apply(): String = {
    val topName = GlobalData.topNode.modName
    val edtNames = GlobalData.edtMods.map(_.modName).toSet
    val edtDesigns = edtNames.map(n => s"read_design $n -design_id rtl -include_child_blocks_icl_and_pdl -view full -verbose\n")
    val edtIcls = edtNames.map(n => s"read_icl tsdb_outdir/dft_inserted_designs/${n}_rtl.dft_inserted_design/$n.icl_instrument_block\n")
    val bapNames = GlobalData.bapMods.map(m => getPath(m.pathName)).toSeq
    require(GlobalData.ctrlMods.length == 1)
    var res =
      s"""|set_context dft -rtl -design_id rtl2
          |
          |set_tsdb_output_directory tsdb_outdir
          |
          |read_verilog -f $topName.f -format sv2009
          |
          |read_icl $topName.icl
          |
          |""".stripMargin

    res += edtDesigns.reduce(_ + _)
    res += "\n"
    res += edtIcls.reduce(_ + _)
    res +=
      s"""
         |set_current_design $topName
         |
         |set_design_level sub_block
         |
         |set_system_mode analysis
         |
         |extract_icl
         |
         |set pspec [create_pattern_specification]
         |
         |source ${topName}.pdl
         |
         |read_config_data -in $${pspec} -from_string {
         |  IjtagRetargetingOptions {
         |    tck_period : 42ns;
         |    tck_ratio  : 1;
         |  }
         |}
         |""".stripMargin

    res += mbistPat(bapNames)
    res += mbistRetPat(bapNames)
    res +=
      s"""
         |report_config_data $$pspec
         |
         |process_pattern_specification
         |
         |run_testbench_simulations -simulator vcs -parallel_simulations 3 -simulator_options "+no_tchk_msg -sverilog" -store_simulation_waveforms on
         |
         |exit""".stripMargin

    res
  }

  def mbistPat(bapNames: Seq[String]): String = {
    var res = ""
    res +=
      s"""
         |read_config_data -in $${pspec} -from_string {
         |  Patterns(Mbist) {
         |    ClockPeriods {
         |      io_clock: 0.5ns;
         |    }\n""".stripMargin
    res += sysRst
    res += bapStart(0, bapNames, "StartMbist")
    res += bapCheck(bapNames, "CheckMbist")
    res += "  }\n"
    res + "}\n"
  }

  def mbistRetPat(bapNames: Seq[String]): String = {
    var res = ""
    res +=
      s"""
         |read_config_data -in $${pspec} -from_string {
         |  Patterns(MbistPrst) {
         |    ClockPeriods {
         |      io_clock: 0.5ns;
         |    }
         |""".stripMargin
    res += sysRst
    res += bapStart(1, bapNames, "StartMbistS2P")
    res += bapCheck(bapNames, "CheckMbistS2P")
    res += bapStart(2, bapNames, "StartMbistP2P")
    res += bapCheck(bapNames, "CheckMbistP2P")
    res += bapStart(3, bapNames, "StartMbistP2E")
    res += bapCheck(bapNames, "CheckMbistP2E")
    res += "  }\n"
    res + "}\n"
  }

  def sysRst: String = {
    val ctrlName = GlobalData.ctrlMods.map(m => getPath(m.pathName)).head
    s"""
       |    ProcedureStep(SystemReset) {
       |      iCall($ctrlName.sysreset) {
       |      }
       |    }
       |""".stripMargin
  }

  def bapStart(ret: Int, bapNames: Seq[String], procName: String): String = {
    var res = s"    ProcedureStep($procName) {\n"
    for (bap <- bapNames) {
      res +=
        s"""|      iCall($bap.start_mbist) {
            |        iProcArguments {
            |          args: "all 0 ret_mode $ret";
            |        }
            |      }
            |""".stripMargin
    }
    res + "\n    }\n"
  }

  def bapCheck(bapNames: Seq[String], procName: String): String = {
    var res = s"    ProcedureStep($procName) {\n"
    for (bap <- bapNames) {
      res +=
        s"""|      iCall($bap.check_mbist) {
            |      }
            |""".stripMargin
    }
    res + "\n    }\n"
  }
}