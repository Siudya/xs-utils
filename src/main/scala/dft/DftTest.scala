package xs.utils.dft

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Config, Parameters}
import xs.utils.ResetGen
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import xs.utils.mbist.controller.MbistController
import xs.utils.mbist.{MBISTInterface, MBISTPipeline}
import xs.utils.sram.SRAMTemplate

class MyConfig extends Config((site, here, up) => {
  case DebugOptionsKey => DebugOptions(FPGAPlatform = true, EnablePerfDebug = false)
})

class MBISTSubMod(implicit p: Parameters) extends Module {
  private val sram0 = Module(new SRAMTemplate(
    gen = UInt(12.W), set = 32, way = 2, singlePort = true,
    shouldReset = false, holdRead = true, extraReset = false,
    bypassWrite = false, hasClkGate = true, clk_div_by_2 = true,
    hasMbist = true, hasShareBus = true, parentName = "sram0_",
    foundry = "TSMC28", sramInst = "ts1n28hpcpuhdlvtb32x24m2sw_170a"
  ))
  private val sram1 = Module(new SRAMTemplate(
    gen = UInt(6.W), set = 64, way = 2, singlePort = false,
    shouldReset = false, holdRead = true, extraReset = false,
    bypassWrite = true, hasClkGate = true, clk_div_by_2 = false,
    hasMbist = true, hasShareBus = true, parentName = "sram1_",
    foundry = "TSMC28", sramInst = "ts6n28hpcplvta64x12m4sw_130a"
  ))
  val io = IO(new Bundle {
    val intf0 = sram0.io.cloneType
    val intf1 = sram1.io.cloneType
  })
  sram0.io <> io.intf0
  sram1.io <> io.intf1
}

class DftTest extends RawModule with HasIjtag {
  val mName = "DftTest"
  private val ciw = 2
  private val cow = 3
  private val p = new MyConfig
  val io = IO(new Bundle {
    val atpgClock = Input(Clock())
    val clock = Input(Clock())
    val reset = Input(AsyncReset())
    val shiftOnlyMode = Input(Bool())
    val edt_update = Input(Bool())
    val edt_channelIn = Input(UInt(ciw.W))
    val edt_channelOut = Output(UInt(cow.W))
    val l3dataram_clk = Input(Bool())
    val wen = Input(Bool())
    val ren = Input(Bool())
    val waddr = Input(UInt(6.W))
    val raddr = Input(UInt(6.W))
    val wdata = Input(UInt(24.W))
    val wmask = Input(UInt(2.W))
    val rdata = Output(UInt(24.W))
  })

  private val globalReset = Wire(AsyncReset())
  private val globalClock = Wire(Clock())
  withClockAndReset(globalClock, globalReset) {
    val occ = Module(new OCC)
    val occSib = Module(new SIB)
    val rstGen = Module(new ResetGen)
    val testCtrl = Module(new TestController)
    val ctrlSib = Module(new SIB)
    val sramMod = Module(new MBISTSubMod()(p))
    val pipeline = MBISTPipeline.PlaceMbistPipeline(Int.MaxValue, "TopMBISTPipeline").get
    val intf = Module(new MBISTInterface(Seq(pipeline.nodeParams), Seq(pipeline.childrenIds), "TopMBISTIntf", 1))
    val mbistCtrl = Module(new MbistController(pipeline.myNode))
    val bap = Module(new BAP(intf.info))
    val bapSib = Module(new SIB)
    val edt_0 = Module(new EdtWrapper("XsEdtWrapper_0", instName = "edt_0", ciw = 1, cow = 1, sw = 4, chainRange = (100, 200)))
    val edt_1 = Module(new EdtWrapper("XsEdtWrapper_1", instName = "edt_1", ciw = 1, cow = 2, sw = 3, chainRange = (50, 140)))
    makeChain(Seq(ijtag, occSib.ijtag, ctrlSib.ijtag, bapSib.ijtag, edt_0.io.ijtag, edt_1.io.ijtag))
    makeChain(Seq(occSib.host, occ.ijtag))
    makeChain(Seq(ctrlSib.host, testCtrl.ijtag))
    makeChain(Seq(bapSib.host, bap.ijtag))

    occ.io.shiftOnlyMode := io.shiftOnlyMode
    occ.io.fastClock := io.clock
    occ.io.slowClock := io.atpgClock
    occ.io.scan := DontCare
    globalClock := occ.io.outClock
    dontTouch(occ.io)

    rstGen.clock := globalClock
    rstGen.reset := io.reset
    rstGen.dft := testCtrl.io.rstCtrl
    globalReset := rstGen.o_reset

    val broadCasts = SRAMTemplate.genBroadCastBundleTop()
    broadCasts := testCtrl.io.sram
    dontTouch(testCtrl.io)
    testCtrl.io.rf2p_ctrl := 0.U
    testCtrl.io.rmsp_hd_ctrl := 0.U
    testCtrl.io.rmsp_hs_ctrl := 0.U
    testCtrl.io.l3dataram_clk := io.l3dataram_clk

    pipeline.mbist <> intf.toPipeline.head
    intf.mbist <> mbistCtrl.io.mbist
    mbistCtrl.io.bap <> bap.mbist

    edt_0.io.ci := io.edt_channelIn(0)
    edt_0.io.atpgClock := io.atpgClock.asBool
    edt_0.io.update := io.edt_update
    edt_0.io.scanEn := false.B
    dontTouch(edt_0.io)
    edt_1.io.ci := io.edt_channelIn(1)
    edt_1.io.atpgClock := io.atpgClock.asBool
    edt_1.io.update := io.edt_update
    edt_1.io.scanEn := false.B
    dontTouch(edt_1.io)

    io.edt_channelOut := Cat(edt_1.io.co, edt_0.io.co)

    sramMod.io.intf0.w(io.wen && io.waddr(5) === 0.U, io.wdata, io.waddr, io.wmask)
    sramMod.io.intf1.w(io.wen && io.waddr(5) === 1.U, io.wdata, io.waddr, io.wmask)
    val d0 = sramMod.io.intf0.r(io.ren && io.raddr(5) === 0.U, io.raddr).resp.data.asUInt
    val d1 = sramMod.io.intf1.r(io.ren && io.raddr(5) === 1.U, io.raddr).resp.data.asUInt
    val rdataSelReg = RegNext(io.ren && io.raddr(5) === 0.U, false.B)
    io.rdata := Mux(rdataSelReg, d0, d1)
  }
}

class DftOuter extends RawModule with HasIjtag {
  val mName = "DftOuter"
  private val test = Module(new DftTest)
  private val io = IO(new Bundle {
    val atpgClock = Input(Clock())
    val clock = Input(Clock())
    val reset = Input(AsyncReset())
    val shiftOnlyMode = Input(Bool())
    val edt_update = Input(Bool())
    val edt_channelIn = Input(UInt(2.W))
    val edt_channelOut = Output(UInt(3.W))
    val l3dataram_clk = Input(Bool())
    val wen = Input(Bool())
    val ren = Input(Bool())
    val waddr = Input(UInt(6.W))
    val raddr = Input(UInt(6.W))
    val wdata = Input(UInt(24.W))
    val wmask = Input(UInt(2.W))
    val rdata = Output(UInt(24.W))
  })
  test.io <> io
  makeChain(Seq(ijtag, test.ijtag))
}