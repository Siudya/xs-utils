package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.mbist.Ram2MbistParams

case class SramMeta(
  dataBits: Int,
  maskBits: Int,
  set:      Int,
  colNum:   Int,
  array:    Int,
  latency:  Int,
  mcp:      Boolean,
  dp:       Boolean,
  idx:      Int) {
  require(maskBits >= 1)
  val addrBits: Int = log2Ceil(set * (idx + 1))
  val rowNum:   Int = set / colNum
  val segLen:   Int = dataBits / maskBits
  val rowOff:   Int = idx * rowNum

  override def toString: String = {
    s"""
       |array: $array
       |dataBits: $dataBits
       |maskBits: $maskBits
       |depth: $set
       |colNum: $colNum
       |latency: $latency
       |rowOffset: $rowOff
       |mcp: $mcp
       |dp: $dp""".stripMargin
  }
}

object SramMeta {
  def apply(meta: Seq[Ram2MbistParams], arrays: Seq[Int], depth: Seq[Int]): Seq[SramMeta] = {
    val foundryToMetaMap = meta.indices.map(idx => (meta(idx), arrays(idx), depth(idx))).groupBy(_._1.foundry)
    foundryToMetaMap
      .flatMap({
        case (k, s) =>
          k.toUpperCase match {
            case "UNKNOWN" => GENERIC(s)
            case "TSMC28"  => TSMC28(s)
            case _ =>
              require(false, s"Not support foundry $k")
              Seq()
          }
      })
      .toSeq
  }
}

object GENERIC {
  def apply(meta: Seq[(Ram2MbistParams, Int, Int)]): Seq[SramMeta] = {
    meta.map({
      case (param, array, depth) =>
        val db = param.dataWidth
        val mb = param.maskWidth
        val set = param.set
        val latency = 2 * depth + param.latency
        val mcp = param.vname.contains("_multicycle")
        val dp = !param.singlePort
        SramMeta(db, mb, set, 1, array, latency, mcp, dp, 0)
    })
  }
}

object TSMC28 {
  private val mc0 = "tem5n28hpcp"
  private val mc1 = "ts5n28hpcp"
  private val mc2 = "ts6n28hpcp"
  private val mc3 = "ts1n28hpcp"
  private val mc4 = "tsdn28hpcp"
  private val mc5 = "tsdn28hpcpuhd"
  private val allmc = Seq(mc0, mc1, mc2, mc3, mc4, mc5).reduce(_ + "|" + _)
  private val prefix = s"[$allmc]"
  private val vt = "[lvt|hvt|svt]?[a|b]"
  private val pat = s"$prefix$vt([0-9]+)x([0-9]+)m([0-9]+)[s|f]?w?b?a?s?o?d?[r|c]?y?(_[0-9a-z]+)?"
  private val patR = pat.r.unanchored

  def apply(meta: Seq[(Ram2MbistParams, Int, Int)]): Seq[SramMeta] = {
    meta.flatMap({
      case (param, array, depth) =>
        val pmo = patR.findFirstMatchIn(param.sramInst.toLowerCase)
        require(pmo.isDefined, s"${param.sramInst} cannot be parsed with TSMC28 library")
        val pm = pmo.get
        val W = pm.group(1).toInt
        val N = pm.group(2).toInt
        val CM = pm.group(3).toInt
        val db = param.dataWidth
        val mb = param.maskWidth
        val set = param.set
        val latency = 2 * depth + param.latency
        val mcp = param.vname.contains("_multicycle")
        val dp = !param.singlePort
        val cols = if (N < db) {
          require(db % N == 0)
          CM
        } else {
          require(N % db == 0)
          N / db * CM
        }
        require(set % W == 0)

        val psramNum = if (N > db) {
          set / W / (N / db)
        } else {
          set / W
        }

        Seq.tabulate(psramNum)(idx => SramMeta(db, mb, set / psramNum, cols, array, latency, mcp, dp, idx))
    })
  }
}
