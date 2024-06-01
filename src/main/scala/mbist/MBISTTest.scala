package xs.utils.mbist

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import xs.utils.ResetGen
import xs.utils.dft.{HasIjtag, SIB, TestController}
import xs.utils.mbist.controller.MbistController
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import xs.utils.sram.SRAMTemplate

class MyConfig extends Config((site, here, up) => {
  case DebugOptionsKey => DebugOptions(FPGAPlatform = true, EnablePerfDebug = false)
})

class MBISTSubMod(implicit p: Parameters) extends Module {
  private val sram0 = Module(new SRAMTemplate(
    gen = UInt(24.W), set = 32, way = 1, singlePort = true,
    shouldReset = false, holdRead = true, extraReset = false,
    bypassWrite = false, hasClkGate = true, clk_div_by_2 = true,
    hasMbist = true, hasShareBus = true, parentName = "sram0_",
    foundry = "TSMC28", sramInst = "ts1n28hpcpuhdlvtb32x24m2sw_170a"
  ))
  private val sram1 = Module(new SRAMTemplate(
    gen = UInt(6.W), set = 32, way = 2, singlePort = false,
    shouldReset = false, holdRead = true, extraReset = false,
    bypassWrite = true, hasClkGate = true, clk_div_by_2 = false,
    hasMbist = true, hasShareBus = true, parentName = "sram1_",
    foundry = "TSMC28", sramInst = "ts6n28hpcplvta32x6m4sw_130a"
  ))
  val io = IO(new Bundle {
    val intf0 = sram0.io.cloneType
    val intf1 = sram1.io.cloneType
  })
  sram0.io <> io.intf0
  sram1.io <> io.intf1
}

class MbistTest extends Module {
  private val p = new MyConfig

  private val sramMod = Module(new MBISTSubMod()(p))
  private val pipeline = MBISTPipeline.PlaceMbistPipeline(Int.MaxValue, "TopMBISTPipeline").get
  private val intf = Module(new MBISTInterface(Seq(pipeline.nodeParams), Seq(pipeline.childrenIds), "TopMBISTIntf", 1))
  private val mbistCtrl = Module(new MbistController(pipeline.myNode))

  pipeline.mbist <> intf.toPipeline.head
  intf.mbist <> mbistCtrl.io.mbist

  private val io = IO(new Bundle {
    val start = Input(Bool())
    val retMode = Input(UInt(2.W))
    val end = Output(Bool())
    val kill = Output(Bool())
  })

  sramMod.io := DontCare
  dontTouch(sramMod.io)
  private val broadCasts = SRAMTemplate.genBroadCastBundleTop()
  broadCasts := DontCare
  dontTouch(broadCasts)
  mbistCtrl.io.bap.start := io.start
  mbistCtrl.io.bap.arrayStart := 0.U
  mbistCtrl.io.bap.arrayEnd := Fill(mbistCtrl.io.bap.arrayEnd.getWidth, true.B)
  mbistCtrl.io.bap.retMode := io.retMode
  mbistCtrl.io.bap.all := false.B
  dontTouch(mbistCtrl.io.bap)

  private val startSyncDepth = 6
  private val startSampler = RegInit(0.U(startSyncDepth.W))
  startSampler := Cat(startSampler(startSyncDepth - 2, 0), io.start)
  private val resetPulse = startSampler(startSyncDepth - 2, startSyncDepth - 3) === 1.U
  private val startPulse = startSampler(startSyncDepth - 1, startSyncDepth - 2) === 1.U

  private val working = RegInit(false.B)
  when(working) {
    working := Mux(resetPulse || mbistCtrl.io.bap.end, false.B, true.B)
  }.otherwise {
    working := startPulse
  }
  when(mbistCtrl.io.bap.end & working) {
    when(mbistCtrl.io.bap.failed) {
      printf(cf"Test failed in [SRAM ${mbistCtrl.io.bap.failed_array}] @ addr ${mbistCtrl.io.bap.failed_addr}\n")
    }.otherwise {
      printf("Test passed!\n")
    }
  }
  private val mbist = mbistCtrl.io.mbist
  when(mbist.writeen) {
    printf(cf"[SRAM ${mbist.array}] write addr 0x${Mux(mbist.array === 0.U, mbist.addr_rd, mbist.addr)}%x data 0x${mbist.indata}%x with mask 0x${mbist.be}%x\n")
  }
  when(mbist.readen) {
    printf(cf"[SRAM ${mbist.array}] read addr 0x${mbist.addr_rd}%x\n")
  }
  io.kill := mbistCtrl.io.bap.end & working & mbistCtrl.io.bap.failed
  io.end := mbistCtrl.io.bap.end & working
}
