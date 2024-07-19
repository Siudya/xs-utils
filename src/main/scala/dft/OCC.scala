package xs.utils.dft

import chisel3._
import chisel3.util._
import xs.utils.ClockGate

class ScanBundle extends Bundle {
  val en = Input(Bool())
  val in = Input(Bool())
  val out = Output(Bool())
}

sealed class OccShifter extends RawModule {
  val io = IO(new Bundle {
    val scan = new ScanBundle
    val pulseWidth = Input(UInt(2.W))
    val clkEn = Output(Bool())
    val clock = Input(Clock())
  })
  private val sie = io.scan.en & io.scan.in
  withClock(io.clock) {
    val shifter = Reg(UInt(4.W))
    val v0 = Cat(0.U(3.W), sie)
    val v1 = Cat(0.U(2.W), sie, shifter(1, 1))
    val v2 = Cat(0.U(1.W), sie, shifter(2, 1))
    val v3 = Cat(sie, shifter(3, 1))
    shifter := MuxCase(
      v0,
      Seq(
        (io.pulseWidth === 0.U) -> v0,
        (io.pulseWidth === 1.U) -> v1,
        (io.pulseWidth === 2.U) -> v2,
        (io.pulseWidth === 3.U) -> v3
      )
    )
    io.scan.out := shifter(0)
    io.clkEn := shifter.orR
  }
}

sealed class OccCtrl extends RawModule {
  val io = IO(new Bundle {
    val scan = new ScanBundle
    val testMode = Input(Bool())
    val captureFast = Input(Bool())
    val fastClock = Input(Clock())
    val slowClock = Input(Clock())
    val shiftOnlyMode = Input(Bool())
    val parentActive = Input(Bool())
    val pulseWidth = Input(UInt(2.W))
    val injectTck = Input(Bool())
    val fclkEn = Output(Bool())
    val sclkEn = Output(Bool())
    val clkSel = Output(Bool())
  })
  private val shifter = Module(new OccShifter)
  private val shifterCg = Module(new ClockGate)

  private val syncReset0 = !io.testMode
  private val syncReset1 = io.scan.en || !io.testMode
  private val sync0 = NReg(!io.scan.en, 0.U, syncReset0, io.slowClock)
  private val sync1 = PReg(sync0, 0.U, syncReset1, io.fastClock)
  private val sync2 = PReg(sync1, 0.U, syncReset1, io.fastClock)

  private val sei = sync2.io.Q(0)
  private val tm = io.testMode
  private val sm = io.shiftOnlyMode
  private val so = shifter.io.scan.out
  private val se = io.scan.en
  private val it = io.injectTck
  private val pa = io.parentActive
  private val fm = io.captureFast

  private val shifterClockEn = shifter.io.clkEn && tm && (sei || pa)
  shifter.io.scan <> io.scan
  shifter.io.pulseWidth := io.pulseWidth
  shifterCg.io.CK := io.fastClock
  shifterCg.io.TE := shifterClockEn
  shifterCg.io.E := shifterClockEn

  private val bypassSclk = it || sm && se
  private val testSclk = se || !fm
  io.fclkEn := so && fm && (sei || pa) || !tm
  io.sclkEn := (so && !fm || se) && tm || bypassSclk
  io.clkSel := testSclk && tm || bypassSclk
  shifter.io.clock := Mux(testSclk, io.slowClock, shifterCg.io.Q)
}

class OCC extends BaseInstrument(Seq(
  "test_mode" -> (1, 0.U, true),
  "fast_capture_mode" -> (1, 0.U, true),
  "active_upstream_parent_occ" -> (1, 0.U, true),
  "capture_cycle_width" -> (2, 0.U, true),
  "inject_tck" -> (1, 0.U, true)
), "OCC") {
  val io = IO(new Bundle {
    val scan = new ScanBundle
    val shiftOnlyMode = Input(Bool())
    val fastClock = Input(Clock())
    val slowClock = Input(Clock())
    val outClock = Output(Clock())
  })
  dontTouch(io.scan)
  io.scan := DontCare
  private val ctrl = Module(new OccCtrl)
  private val fcg = Module(new ClockGate)
  private val scg = Module(new ClockGate)
  ctrl.io.scan.in := io.scan.in
  ctrl.io.scan.en := io.scan.en
  ctrl.io.testMode := tdrFields("test_mode")
  ctrl.io.captureFast := tdrFields("fast_capture_mode")
  ctrl.io.parentActive := tdrFields("active_upstream_parent_occ")
  ctrl.io.pulseWidth := tdrFields("capture_cycle_width")
  ctrl.io.injectTck := tdrFields("inject_tck")
  ctrl.io.shiftOnlyMode := io.shiftOnlyMode
  ctrl.io.fastClock := io.fastClock
  private val slowClockInjected = Mux(ctrl.io.injectTck, ijtag.tck, io.slowClock)
  ctrl.io.slowClock := slowClockInjected

  fcg.io.CK := io.fastClock
  fcg.io.TE := ctrl.io.fclkEn
  fcg.io.E := ctrl.io.fclkEn

  scg.io.CK := slowClockInjected
  scg.io.TE := ctrl.io.sclkEn
  scg.io.E := ctrl.io.sclkEn

  io.outClock := Mux(ctrl.io.clkSel, scg.io.Q, fcg.io.Q)

  private val bypassScanIn = io.scan.in && io.scan.en
  private val bypassShiftReg = PReg(bypassScanIn, io.outClock)
  private val scanOutSel = Mux(tdrFields("test_mode").asBool, ctrl.io.scan.out, bypassShiftReg.io.Q.asBool)
  private val scanOutReg = NReg(scanOutSel && io.scan.en, io.outClock)
  io.scan.out := scanOutReg.io.Q.asBool

  FileManager.add("tcd", "OCC", OCC.getTcd(modName))
  FileManager.add("pdl", "OCC", OCC.getPdl(modName))
}

object OCC {
  private val tcd =
    """|Core(__XSMODNAMESTRREPL__) {
       |   Occ {
       |      version: 4;
       |      ijtag_scan_interface: on;
       |      type: standard;
       |      Interface {
       |         FastClock(io_fastClock) {
       |         }
       |         SlowClock(io_slowClock) {
       |         }
       |         ScanEnable(io_scan_en) {
       |         }
       |         ScanInput(io_scan_in) {
       |         }
       |         ScanOutput(io_scan_out) {
       |         }
       |         ShiftRegisterClock(ctrl/shifterCg/Q) {
       |         }
       |         GatedFastClock(fcg/Q) {
       |         }
       |         GatedClock(io_outClock) {
       |            source: io_fastClock;
       |         }
       |      }
       |      Parameters {
       |         iproc: setup;
       |         FastCaptureMode {
       |            parameter: fast_capture_mode;
       |            type: boolean;
       |            default_value: off;
       |         }
       |         ActiveUpstreamParentOcc {
       |            parameter: active_upstream_parent_occ;
       |            type: boolean;
       |            default_value: off;
       |         }
       |         CaptureCycleWidth {
       |            parameter: capture_window_size;
       |            type: int;
       |            legal_int: 1..4;
       |            default_value: 4;
       |         }
       |      }
       |      ControlBits {
       |         ControlBit(ctrl/shifter/ShiftReg/shifter[0]) {
       |         }
       |         ControlBit(ctrl/shifter/ShiftReg/shifter[1]) {
       |         }
       |         ControlBit(ctrl/shifter/ShiftReg/shifter[2]) {
       |         }
       |         ControlBit(ctrl/shifter/ShiftReg/shifter[3]) {
       |         }
       |      }
       |      capture_trigger: shift_en;
       |      static_clock_control: off;
       |   }
       |}
       |""".stripMargin

  private val pdl =
    """|iProcsForModule __XSMODNAMESTRREPL__
       |iProc setup {args} {
       |   if {[expr [llength $args]%2 != 0]} {
       |      display_message -error "Odd number of arguments. Expecting parameter and value pairs."
       |      return -code error
       |   }
       |   set fast_capture_mode 0
       |   set parent_mode 0
       |   set capture_window_size 4
       |   set inject_tck 0
       |   set active_upstream_parent_occ 0
       |   foreach {param value} $args {
       |      set param [string tolower $param]
       |      if {$param eq "capture_window_size"} {
       |         set capture_window_size $value
       |      } elseif {$param eq "fast_capture_mode"} {
       |         set fast_capture_mode $value
       |      } elseif {$param eq "active_upstream_parent_occ"} {
       |         set active_upstream_parent_occ $value
       |      } elseif {$param eq "inject_tck"} {
       |         set inject_tck $value
       |      } else {
       |         display_message -error "Invalid parameter '$param'. Valid parameters are 'fast_capture_mode', 'capture_window_size' and 'inject_tck'."
       |         return -code error
       |      }
       |   }
       |
       |   if {![string is boolean -strict $fast_capture_mode]} {
       |      display_message -error "Invalid non-boolean value '$fast_capture_mode' for parameter 'fast_capture_mode'."
       |      return -code error
       |   }
       |   if {![string is boolean -strict $active_upstream_parent_occ]} {
       |      display_message -error "Invalid non-boolean value '$active_upstream_parent_occ' for parameter 'active_upstream_parent_occ'."
       |      return -code error
       |   }
       |   if {![string is boolean -strict $parent_mode]} {
       |      display_message -error "Invalid non-boolean value '$parent_mode' for parameter 'parent_mode'."
       |      return -code error
       |   } elseif {$parent_mode} {
       |      display_message -error "Parameter 'parent_mode' cannot be set to 1 as the controller type is not 'parent'."
       |      return -code error
       |   }
       |   if {![string is integer -strict $capture_window_size]} {
       |      display_message -error "Invalid non-integer value '$capture_window_size' for parameter 'capture_window_size'."
       |      return -code error
       |   }
       |   if {$capture_window_size < 1 || $capture_window_size > 4} {
       |      display_message -error "Invalid value '$capture_window_size' for parameter 'capture_window_size'. Must be 1 to 4."
       |      return -code error
       |   }
       |   if {![string is boolean -strict $inject_tck]} {
       |      display_message -error "Invalid non-boolean value '$inject_tck' for parameter 'inject_tck'."
       |      return -code error
       |   }
       |
       |   iWrite test_mode 1;
       |   iWrite fast_capture_mode [string is true $fast_capture_mode];
       |   iWrite capture_cycle_width [expr {$capture_window_size - 1}];
       |   iWrite active_upstream_parent_occ  [string is true $active_upstream_parent_occ]
       |   iWrite inject_tck [string is true $inject_tck];
       |
       |   iApply;
       |}
       |""".stripMargin

  def getPdl(modName: String): String = {
    pdl.replace("__XSMODNAMESTRREPL__", modName)
  }

  def getTcd(modName: String): String = {
    tcd.replace("__XSMODNAMESTRREPL__", modName)
  }
}
