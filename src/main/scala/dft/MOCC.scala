package xs.utils.dft
import chisel3._
import xs.utils.ClockGate

class MOCC extends BaseInstrument(Seq(
  "test_mode" -> (1, 0.U, true),
  "fast_capture_mode" -> (1, 0.U, true),
  "inject_tck" -> (1, 0.U, true)
), "MOCC") {
  val io = IO(new Bundle {
    val scanEn = Input(Bool())
    val shiftOnlyMode = Input(Bool())
    val fastClock = Input(Clock())
    val slowClock = Input(Clock())
    val outClock = Output(Clock())
  })
  dontTouch(io.scanEn)
  io.scanEn := DontCare
  private val tm = tdrFields("test_mode")(0)
  private val fm = tdrFields("fast_capture_mode")(0)
  private val it = tdrFields("inject_tck")(0)
  private val sm = io.shiftOnlyMode
  private val se = io.scanEn

  private val slowClockInjected = Mux(it, ijtag.tck, io.slowClock)
  private val syncReset0 = !tm
  private val syncReset1 = io.scanEn || !tm
  private val sync0 = NReg(!io.scanEn, 0.U, syncReset0, slowClockInjected)
  private val sync1 = PReg(sync0, 0.U, syncReset1, io.fastClock)
  private val sync2 = PReg(sync1, 0.U, syncReset1, io.fastClock)
  private val sei = sync2.io.Q(0)

  private val sce = (se || !fm) && tm || it || sm && se
  private val fce = fm && sei || !tm

  private val scg = Module(new ClockGate)
  private val fcg = Module(new ClockGate)

  scg.io.CK := slowClockInjected
  scg.io.TE := sce
  scg.io.E := false.B

  fcg.io.CK := io.fastClock
  fcg.io.TE := fce
  fcg.io.E := false.B

  io.outClock := Mux(sce, scg.io.Q, fcg.io.Q)
}
