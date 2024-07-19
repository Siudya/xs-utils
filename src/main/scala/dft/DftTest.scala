package xs.utils.dft

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.ClockGate
import xs.utils.ResetGen
import xs.utils.mbist.controller.MbistController
import xs.utils.mbist.{MbistInterface, MbistPipeline}
import xs.utils.sram.{SRAMTemplate, SramHelper}

class MbistSubMod extends Module {
  private val sram0 = Module(
    new SRAMTemplate(
      gen = UInt(24.W),
      set = 32,
      way = 1,
      singlePort = true,
      shouldReset = false,
      extraReset = false,
      holdRead = true,
      bypassWrite = false,
      multicycle = 2,
      hasMbist = true,
      foundry = "TSMC28",
      sramInst = "ts1n28hpcpuhdlvtb32x24m2sw_170a"
    )
  )
  private val sram1 = Module(
    new SRAMTemplate(
      gen = UInt(6.W),
      set = 64,
      way = 2,
      singlePort = false,
      shouldReset = false,
      extraReset = false,
      holdRead = true,
      bypassWrite = true,
      multicycle = 1,
      hasMbist = true,
      foundry = "TSMC28",
      sramInst = "ts6n28hpcplvta64x12m4sw_130a"
    )
  )
  val io = IO(new Bundle {
    val intf0 = sram0.io.cloneType
    val intf1 = sram1.io.cloneType
  })
  private val cg0 = ClockGate(false.B, io.intf0.r.req.valid || io.intf0.w.req.valid, clock)
  private val cg1 = ClockGate(false.B, io.intf1.r.req.valid || io.intf1.w.req.valid, clock)
  sram0.io <> io.intf0
  sram1.io <> io.intf1
  sram0.clock := cg0
  sram1.clock := cg1
}

class DftTest(ciw:Int, cow:Int) extends RawModule with HasIjtag {
  val mName = "DftTest"
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(AsyncReset())
    val ram_aux_clk = Input(Bool())
    val wen = Input(Bool())
    val ren = Input(Bool())
    val waddr = Input(UInt(6.W))
    val raddr = Input(UInt(6.W))
    val wdata = Input(UInt(24.W))
    val wmask = Input(UInt(2.W))
    val rdata = Output(UInt(24.W))
  })
  val dft = IO(new DftModBundle(ciw, cow))

  private val globalReset = Wire(AsyncReset())
  private val globalClock = Wire(Clock())
  withClockAndReset(globalClock, globalReset) {
    val occ = Module(new OCC)
    val occSib = Module(new SIB)
    val rstGen = Module(new ResetGen)
    val testCtrl = Module(new TestController)
    val ctrlSib = Module(new SIB)
    val sramMod = Module(new MbistSubMod())
    val pipeline = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "TopMbistPipeline").get
    val intf = Module(new MbistInterface(Seq(pipeline.nodeParams), Seq(pipeline.childrenIds), "TopMbistIntf", 1))
    val mbistCtrl = Module(new MbistController(pipeline.myNode))
    val bap = Module(new BAP(mbistCtrl.param, "BAP"))
    val bapSib = Module(new SIB)
    val edt_0 = Module(
      new EdtWrapper(
        "XsEdtWrapper_0",
        instName = "edt_0",
        ciw = 1,
        cow = 1,
        sw = 4,
        chainRange = (100, 200)
      )
    )
    val edt_1 = Module(
      new EdtWrapper(
        "XsEdtWrapper_1",
        instName = "edt_1",
        ciw = 1,
        cow = 2,
        sw = 3,
        chainRange = (50, 140)
      )
    )
    makeChain(Seq(ijtag, occSib.ijtag, ctrlSib.ijtag, bapSib.ijtag, edt_0.io.ijtag, edt_1.io.ijtag))
    makeChain(Seq(occSib.host, occ.ijtag))
    makeChain(Seq(ctrlSib.host, testCtrl.ijtag))
    makeChain(Seq(bapSib.host, bap.ijtag))

    occ.io.shiftOnlyMode := testCtrl.io.shiftOnlyMode
    occ.io.fastClock := io.clock
    occ.io.slowClock := dft.atpg_clock
    occ.io.scan := DontCare
    occ.io.scan.en := dft.scan_en
    globalClock := occ.io.outClock
    dontTouch(occ.io)

    rstGen.clock := globalClock
    rstGen.reset := io.reset
    rstGen.dft := testCtrl.io.rstCtrl
    globalReset := rstGen.o_reset

    val broadCasts = SramHelper.genBroadCastBundleTop()
    broadCasts := testCtrl.io.sram
    dontTouch(testCtrl.io)
    testCtrl.io.ram_aux_clk := io.ram_aux_clk

    pipeline.mbist <> intf.toPipeline.head
    intf.mbist <> mbistCtrl.io.mbist
    mbistCtrl.io.bap <> bap.mbist

    edt_0.io.ci := dft.edt.in_channels(0)
    edt_0.io.atpg_clock := dft.atpg_clock
    edt_0.io.update := dft.edt.update
    edt_0.io.scan_en := dft.scan_en
    dontTouch(edt_0.io)
    edt_1.io.ci := dft.edt.in_channels(1)
    edt_1.io.atpg_clock := dft.atpg_clock
    edt_1.io.update := dft.edt.update
    edt_1.io.scan_en := dft.scan_en
    dontTouch(edt_1.io)

    dft.edt.out_channels := Cat(edt_1.io.co, edt_0.io.co)

    sramMod.io.intf0.w(io.wen && io.waddr(5) === 0.U, io.wdata, io.waddr, io.wmask)
    sramMod.io.intf1.w(io.wen && io.waddr(5) === 1.U, io.wdata, io.waddr, io.wmask)
    val d0 = sramMod.io.intf0.r(io.ren && io.raddr(5) === 0.U, io.raddr).resp.data.asUInt
    val d1 = sramMod.io.intf1.r(io.ren && io.raddr(5) === 1.U, io.raddr).resp.data.asUInt
    val rdataSelReg = RegNext(io.ren && io.raddr(5) === 0.U, false.B)
    io.rdata := Mux(rdataSelReg, d0, d1)

    val cg = ClockGate.getTop
    cg.te := testCtrl.io.sram.cgen
  }
}

class DftOuter extends RawModule with HasIjtag {
  val mName = "DftOuter"
  private val ciw = 2
  private val cow = 3
  private val test = Module(new DftTest(ciw, cow))
  private val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(AsyncReset())
    val ram_aux_clk = Input(Bool())
    val wen = Input(Bool())
    val ren = Input(Bool())
    val waddr = Input(UInt(6.W))
    val raddr = Input(UInt(6.W))
    val wdata = Input(UInt(24.W))
    val wmask = Input(UInt(2.W))
    val rdata = Output(UInt(24.W))
  })
  private val dft = IO(new DftModBundle(ciw, cow))
  test.io <> io
  test.dft <> dft
  makeChain(Seq(ijtag, test.ijtag))
}
