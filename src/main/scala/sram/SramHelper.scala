package xs.utils.sram

import chisel3._
import chisel3.experimental.hierarchy.Instance
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.GlobalData
import xs.utils.mbist.Mbist._
import xs.utils.mbist.{Mbist, Ram2Mbist, Ram2MbistParams}

import scala.collection.{immutable, mutable}
import scala.math.sqrt

object SramHelper {
  private var nodeId = 0
  private var wrapperId = 0
  private var domainId = 0
  private val broadCastBdQueue = new mutable.Queue[SramBroadcastBundle]

  private def getWayNumForEachNodeAndNodeNum_1toN(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val dataNum1toNNode = mw / dw
    val numVec = (1 to dataNum1toNNode)
    val validVec = numVec.map(num => (way % num == 0) && (way >= num))
    val validNum = numVec.zip(validVec).filter(_._2)
    val res = if(validNum.isEmpty) (1, way) else validNum.last
    (res._1, way / res._1)
  }

  private def getDivisor(in: Int): Seq[Int] = {
    val end = sqrt(in).toInt
    val divisors =
      Seq.tabulate(end)(_ + 1).map(idx => (in % idx == 0, Seq(idx, in / idx))).filter(_._1).flatMap(_._2).sorted
    divisors
  }

  private def getNodeNumForEachWayAndNodeNum_Nto1(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val divisors = getDivisor(dw)
    val validDivisors = divisors.filter(_ <= mw)
    val goodNodeNumForEachWay = dw / validDivisors.max
    val defaultNodeNumForEachWay = ((dw + mw - 1) / mw)
    val finalNodeNumForEachWay =
      if(goodNodeNumForEachWay > 4 * defaultNodeNumForEachWay) defaultNodeNumForEachWay else goodNodeNumForEachWay
    (finalNodeNumForEachWay, way * finalNodeNumForEachWay)
  }

  def restartIndexing(): Unit = domainId = 0

  def getDomainID: Int = domainId

  def increaseDomainID(add: Int): Unit = domainId += add

  def genBroadCastBundleTop(): SramBroadcastBundle = {
    val res = Wire(new SramBroadcastBundle)
    broadCastBdQueue.toSeq.foreach(bd => {
      BoringUtils.bore(bd) := res
    })
    broadCastBdQueue.clear()
    res
  }

  private val foundry = "TSMC28"
  private val SramInstMap: immutable.HashMap[String, String] = immutable.HashMap[String, String](
    ("sram_array_1p128x100m25", "TS1N28HPCPUHDLVTB128X100M1SW"),
    ("sram_array_1p128x1024m256", "TS1N28HPCPUHDLVTB128X256M1S"),
    ("sram_array_1p512x236m59", "TS1N28HPCPUHDLVTB512X59M4S"),
    ("sram_array_1p256x16m1", "TS1N28HPCPUHDLVTB256X16M2SW"),
    ("sram_array_1p256x24m12", "TS1N28HPCPUHDLVTB256X24M2SW"),
    ("sram_array_2p2048x2m2", "TS6N28HPCPLVTA2048X2M8S"),
    ("sram_array_2p512x12m6", "TS6N28HPCPLVTA512X12M4SW"),
    ("sram_array_1p128x50m50", "TS1N28HPCPUHDLVTB128X50M1S"),
    ("sram_array_1p128x100m50", "TS1N28HPCPUHDLVTB128X100M1SW"),
    ("sram_array_2p64x227m227", "TS6N28HPCPLVTA64X227M2S"),
    ("sram_array_2p64x256m256", "TS6N28HPCPLVTA64X128M2S"),
    ("sram_array_1p32x532m133", "TS1N28HPCPUHDLVTB32X133M2S"),
    ("sram_array_1p128x1352m169", "TS1N28HPCPUHDLVTB128X169M1S"),
    ("sram_array_1p256x64m64", "TS1N28HPCPUHDLVTB256X64M2S"),
    ("sram_array_1p256x100m25", "TS1N28HPCPUHDLVTB256X100M2SW"),
    ("sram_array_1p32x148m74", "TS1N28HPCPUHDLVTB32X148M1SW"),

    ("sram_array_1p32x176m22", "TS1N28HPCPUHDLVTB32X176M1SW"),
    ("sram_array_1p256x64m8", "TS1N28HPCPUHDLVTB256X64M2SW"),
    ("sram_array_1p256x7m7", "TS1N28HPCPUHDLVTB256X7M2S"),
    ("sram_array_1p256x512m256", "TS1N28HPCPUHDLVTB256X128M2S"),
    ("sram_array_1p256x20m20", "TS1N28HPCPUHDLVTB256X20M2S"),
    ("sram_array_1p15x537m179", "TS6N28HPCPLVTA15X179M2F"),

    ("sram_array_1p16x256m256", "TS1N28HPCPUHDLVTB16X256M1S"),
    ("sram_array_1p2048x128m128", "TS1N28HPCPUHDLVTB2048X128M4S"),
    ("sram_array_1p2048x18m18", "TS1N28HPCPUHDLVTB2048X18M4S"),
    ("sram_array_1p1024x40m4", "TS1N28HPCPUHDLVTB1024X40M4SW"),
    ("sram_array_1p1024x190m19", "TS1N28HPCPUHDLVTB1024X95M4SW"),
    ("sram_array_1p1024x60m6", "TS1N28HPCPUHDLVTB1024X60M4SW"),
    ("sram_array_1p2048x56m7", "TS1N28HPCPUHDLVTB2048X56M4SW"),
    ("sram_array_1p2048x144m18", "TS1N28HPCPUHDLVTB2048X144M4SW"),
    ("sram_array_1p2048x48m6", "TS1N28HPCPUHDLVTB2048X48M4SW"),
    ("sram_array_1p2048x7m7", "TS1N28HPCPLVTB2048X7M8S"),

    ("sram_array_1p64x128m128", "TS1N28HPCPLVTB64X128M1S")
  )
  private val pat = s"sram_array_[12]p[0-9]+x[0-9]+m[0-9]+".r
  def getSramInst(vname:String):String = {
    val res = pat.findFirstIn(vname)
    require(res.isDefined, s"$vname is illegal")
    require(SramInstMap.contains(res.get), s"$vname is not found in map")
    SramInstMap(res.get)
  }

  def genRam(
    ew: Int,
    way: Int,
    set: Int,
    dp: Boolean,
    mcp: Boolean,
    bist: Boolean,
    pwctl: Option[SramPowerCtl],
    reset: Reset,
    rclk: Clock,
    wclk: Option[Clock],
    suffix: String,
    foundry: String,
    sramInst: String,
    template: RawModule
  ): (Ram2Mbist, SramBroadcastBundle, Instance[SramArray], Int, Int, String) = {
    val isNto1 = ew > maxMbistDataWidth
    //** ******implement mbist interface node(multiple nodes for one way)******
    val (mbistNodeNumForEachWay, mbistNodeNumNto1) = getNodeNumForEachWayAndNodeNum_Nto1(ew, way, maxMbistDataWidth)
    val maskWidthNto1 = 1
    val mbistDataWidthNto1 = (ew + mbistNodeNumForEachWay - 1) / mbistNodeNumForEachWay
    //** *******implement mbist interface node(one node for multiple ways)******
    val (wayNumForEachNode, mbistNodeNum1toN) = getWayNumForEachNodeAndNodeNum_1toN(ew, way, maxMbistDataWidth)
    val mbistDataWidth1toN = wayNumForEachNode * ew
    val maskWidth1toN = wayNumForEachNode

    val mbistNodeNum = if(isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
    val mbistDataWidth = if(isNto1) mbistDataWidthNto1 else mbistDataWidth1toN
    val mbistMaskWidth = if(isNto1) maskWidthNto1 else maskWidth1toN
    val mbistArrayIds = Seq.tabulate(mbistNodeNum)(idx => getDomainID + idx)
    val bitWrite = way != 1
    val sramMaskBits = if(isNto1) mbistNodeNum else way

    val (array, vname) = SramProto(rclk, !dp, set, ew * way, sramMaskBits, mcp, wclk, bist, suffix, pwctl.isDefined)
    val bdParam =
      Ram2MbistParams(
        set,
        mbistDataWidth,
        mbistMaskWidth,
        !dp,
        vname,
        "",
        mbistNodeNum,
        mbistArrayIds.max,
        bitWrite,
        SramHelper.foundry,
        SramHelper.getSramInst(vname),
        0,
        "None",
        template
      )
    val mbist = Wire(new Ram2Mbist(bdParam))
    mbist := DontCare
    mbist.selectedOH := Fill(mbist.selectedOH.getWidth, 1.U(1.W))
    mbist.ack := false.B
    mbist.we := false.B
    mbist.re := false.B
    mbist.wmask := Fill(mbistMaskWidth, true.B)
    val broadCastSignals = Wire(new SramBroadcastBundle)
    broadCastSignals := DontCare
    if(bist) {
      dontTouch(mbist)
      Mbist.addRamNode(mbist, mbistArrayIds)
      val addId = if(isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
      nodeId += addId
      increaseDomainID(addId)
      val broadcast = IO(new SramBroadcastBundle)
      broadcast := DontCare
      dontTouch(broadcast)
      broadcast.suggestName("broadcast")
      broadCastSignals := broadcast
      broadCastBdQueue.enqueue(broadcast)
      array.mbist.get.dft_ram_bp_clken := broadcast.ram_bp_clken
      array.mbist.get.dft_ram_bypass := broadcast.ram_bypass
    }
    if(pwctl.isDefined) {
      array.pwctl.get.ret := pwctl.get.ret
      array.pwctl.get.stop := pwctl.get.stop | reset.asBool
    }
    (mbist, broadCastSignals, array, mbistNodeNum, sramMaskBits, vname)
  }
}
