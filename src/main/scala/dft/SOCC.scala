package xs.utils.dft
import chisel3._
import xs.utils.ClockGate

sealed class SoccCtrl extends RawModule {
  val io = IO(new Bundle {
    val scan = new ScanBundle
    val testMode = Input(Bool())
    val clock = Input(Clock())
    val shiftOnlyMode = Input(Bool())
    val pulseWidth = Input(UInt(2.W))
    val clkCtrl = Input(Bool())
    val clkEn = Output(Bool())
  })
  private val shifter = Module(new OccShifter)
  private val shifterCg = Module(new ClockGate)

  private val syncReset0 = !io.testMode
  private val syncReset1 = io.scan.en || !io.testMode
  private val sync0 = NReg(!io.scan.en, 0.U, syncReset0, io.clock)
  private val sync1 = PReg(sync0, 0.U, syncReset1, io.clock)
  private val sync2 = PReg(sync1, 0.U, syncReset1, io.clock)

  private val sei = sync2.io.Q(0)
  private val tm = io.testMode
  private val sm = io.shiftOnlyMode
  private val so = shifter.io.scan.out
  private val se = io.scan.en

  private val shifterClockEn = shifter.io.clkEn && tm && sei && io.clkCtrl
  shifter.io.scan <> io.scan
  shifter.io.pulseWidth := io.pulseWidth
  shifterCg.io.CK := io.clock
  shifterCg.io.TE := shifterClockEn
  shifterCg.io.E := shifterClockEn
  shifter.io.clock := shifterCg.io.Q

  io.clkEn := (se && tm || so && sei && tm || sm && se || !tm) && io.clkCtrl
}

class SOCC extends BaseInstrument(Seq(
  "test_mode" -> (1, 0.U, true),
  "capture_cycle_width" -> (2, 0.U, true)
), "SOCC") {
  val io = IO(new Bundle {
    val scan = new ScanBundle
    val shiftOnlyMode = Input(Bool())
    val clock = Input(Clock())
    val clkCtrl = Input(Bool())
    val outClock = Output(Clock())
  })
  dontTouch(io.scan)
  io.scan := DontCare
  private val ctrl = Module(new SoccCtrl)
  private val cg = Module(new ClockGate)
  ctrl.io.scan.in := io.scan.in
  ctrl.io.scan.en := io.scan.en
  ctrl.io.testMode := tdrFields("test_mode")
  ctrl.io.pulseWidth := tdrFields("capture_cycle_width")
  ctrl.io.shiftOnlyMode := io.shiftOnlyMode
  ctrl.io.clkCtrl := io.clkCtrl
  ctrl.io.clock := io.clock

  cg.io.CK := io.clock
  cg.io.TE := ctrl.io.clkEn
  cg.io.E := ctrl.io.clkEn

  io.outClock := cg.io.Q

  private val bypassScanIn = io.scan.in && io.scan.en
  private val bypassShiftReg = PReg(bypassScanIn, io.outClock)
  private val scanOutSel = Mux(ctrl.io.testMode, ctrl.io.scan.out, bypassShiftReg.io.Q.asBool)
  private val scanOutReg = NReg(scanOutSel && io.scan.en, io.outClock)
  io.scan.out := scanOutReg.io.Q.asBool
}
