package xs.utils.dft

import chisel3._

class SIB extends RawModule with HasIjtag {
  val mName: String = "SIB"
  val host: IjtagIntf = IO(Flipped(new IjtagIntf(ijtagNode)))
  private val sel = Wire(Bool())
  private val shiftIn = Mux(sel, host.so, ijtag.si)
  private val rst = (!ijtag.reset.asBool).asAsyncReset
  private val shiftBitReg = PReg(shiftIn, false.B, ijtag.se & ijtag.sel, rst, ijtag.tck)
  private val updateBitReg = NReg(shiftBitReg, false.B, ijtag.ue & ijtag.sel, rst, ijtag.tck)
  private val selDelayBitReg = NReg(updateBitReg, false.B, rst, ijtag.tck)
  private val shiftDelayBitReg = NReg(shiftBitReg, ijtag.tck)
  sel := updateBitReg.io.Q

  host.si := ijtag.si
  ijtag.so := shiftDelayBitReg.io.Q
  host.ce := ijtag.ce
  host.se := ijtag.se
  host.ue := ijtag.ue
  host.sel := selDelayBitReg.io.Q & ijtag.sel
  host.reset := ijtag.reset
  host.tck := ijtag.tck

  icl +=
    s"""|  ScanOutPort ijtag_so {
        |    Source tdr;
        |  }
        |  ToSelectPort host_sel {
        |    Source tdr;
        |    Attribute connection_rule_option = \"allowed_no_destination\";
        |  }
        |  ScanOutPort host_si {
        |    Source ijtag_si;
        |    Attribute connection_rule_option = \"allowed_no_destination\";
        |  }
        |  ScanInPort host_so {
        |    Attribute connection_rule_option = \"allowed_no_source\";
        |  }
        |  ScanInterface host {
        |    Port host_si;
        |    Port host_so;
        |    Port host_sel;
        |  }
        |  Attribute keep_active_during_scan_test = \"true\";
        |  ScanMux scan_in_mux SelectedBy tdr {
        |      1'b0 : ijtag_si;
        |      1'b1 : host_so;
        |  }
        |  ScanRegister tdr {
        |    ScanInSource scan_in_mux;
        |    CaptureSource tdr;
        |    ResetValue 1'b0;
        |  }
        |}
        |""".stripMargin
  icl = icl.replace("__XSMODNAMESTRREPL__", modName)
}
