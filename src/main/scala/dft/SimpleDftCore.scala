package xs.utils.dft

import chisel3._
import org.chipsalliance.cde.config._
import xs.utils.ResetGen
import xs.utils.mbist.controller.MbistController
import xs.utils.mbist.{MbistInterface, MbistPipeline}
import xs.utils.sram.SramHelper

case object SimpleDftParamsKey extends Field[SimpleDftParams]

case class SimpleDftParams(
  edtCi: Int = 1,
  edtCo: Int = 1,
  edtScan: Int = 10,
  edtMaxRange: (Int, Int) = (250, 350)
)

trait SimpleDftCore {

  m: RawModule with HasIjtag =>
  def dftParams: SimpleDftParams
  private val clock = IO(Input(Clock()))
  private val reset = IO(Input(AsyncReset()))
  val masterClock: Clock = WireInit(clock)
  val masterReset: AsyncReset = WireInit(reset)
  lazy private val atpg_clock = IO(Input(Clock()))
  lazy private val scan_en = IO(Input(Bool()))
  lazy private val edt_ci = IO(Input(UInt(dftParams.edtCi.W)))
  lazy private val edt_co = IO(Output(UInt(dftParams.edtCo.W)))
  lazy private val edt_update = IO(Input(Bool()))
  lazy private val dft_ram_aux_clk = IO(Input(Bool()))
  lazy private val resetGen = withClockAndReset(clock, reset) {
    Module(new ResetGen)
  }

  lazy private val sib = Module(new SIB)
  lazy private val occ = Module(new OCC)
  lazy private val testCtrl = Module(new TestController)
  lazy private val edt = Module(new EdtWrapper("TopEdtWrapper", "edt",
    dftParams.edtCi, dftParams.edtCo, dftParams.edtScan, dftParams.edtMaxRange))
  lazy private val sramBd = SramHelper.genBroadCastBundleTop()

  lazy private val bap = withClockAndReset(masterClock, masterReset) {
    val mbistPl = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "MbistPipeTop")
    val intf = Module(new MbistInterface(
      params = Seq(mbistPl.get.nodeParams),
      ids = Seq(mbistPl.get.childrenIds),
      name = s"MbistIntfTop",
      pipelineNum = 1
    ))
    val mbistCtrl = Module(new MbistController(mbistPl.get.myNode))
    val _bap = Module(new BAP(mbistCtrl.param, "BAP"))
    makeChain(Seq(sib.host, _bap.ijtag))
    intf.toPipeline.head <> mbistPl.get.mbist
    mbistPl.get.registerCSV(intf.info, "MbistTop")
    intf.mbist <> mbistCtrl.io.mbist
    mbistCtrl.io.bap <> _bap.mbist
    _bap
  }

  lazy val dftInit: Boolean = {
    occ.io.fastClock := clock
    occ.io.slowClock := atpg_clock
    occ.io.scan.en := scan_en
    occ.io.shiftOnlyMode := testCtrl.io.shiftOnlyMode
    occ.io.scan.in := DontCare
    dontTouch(occ.io.scan)
    edt.io.scan_en := scan_en
    edt.io.ci := edt_ci
    edt.io.update := edt_update
    edt.io.atpg_clock := atpg_clock
    edt_co := edt.io.co
    testCtrl.io.ram_aux_clk := dft_ram_aux_clk
    resetGen.dft := testCtrl.io.rstCtrl
    sramBd := testCtrl.io.sram

    masterClock := occ.io.outClock
    masterReset := resetGen.o_reset

    atpg_clock.suggestName("atpg_clock")
    scan_en.suggestName("scan_en")
    edt_ci.suggestName("edt_ci")
    edt_co.suggestName("edt_co")
    edt_update.suggestName("edt_update")
    dft_ram_aux_clk.suggestName("dft_ram_aux_clk")
    makeChain(Seq(ijtag, sib.ijtag))
    makeChain(Seq(sib.host, testCtrl.ijtag, occ.ijtag, bap.ijtag, edt.io.ijtag))
    true
  }
}
