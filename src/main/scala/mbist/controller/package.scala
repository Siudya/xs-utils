package xs.utils.mbist

import chisel3._
import chisel3.util._
import xs.utils.sram.SramMeta

package object controller {
  case class MbistControllerParam
  (
    maxCol: Int,
    maxRow: Int,
    maxSet: Int,
    maxMaskWidth: Int,
    maxDataWidth: Int,
    maxArray: Int,
    maxLatency: Int,
    maxSegLen: Int,
    maxRowOff: Int,
    setSum:Int
  ) {
    val arrayBits: Int = log2Ceil(maxArray + 1)
    val latencyBits: Int = log2Ceil(maxLatency + 1)
    val addrBits: Int = log2Ceil(maxSet)
    val colsBits: Int = log2Ceil(maxCol)
    val rowsBits: Int = log2Ceil(maxRow)
    val dataInfoBits: Int = log2Ceil(maxDataWidth + 1)
    val segInfoBits: Int = log2Ceil(maxSegLen + 1)
    val maskInfoBits: Int = log2Ceil(maxMaskWidth + 1)
    val rowOffInfoBits: Int = log2Ceil(maxRowOff + 1)
  }

  object MbistControllerParam {
    def apply(meta: Seq[SramMeta]): MbistControllerParam = {
      val c = meta.map(_.colNum).max
      val r = meta.map(_.rowNum).max
      val s = meta.map(m => m.set * (m.idx + 1)).max
      val m = meta.map(_.maskBits).max
      val d = meta.map(_.dataBits).max
      val a = meta.map(_.array).max
      val l = meta.map(_.latency).max
      val seg = meta.map(_.segLen).max
      val rof = meta.map(_.rowOff).max
      val ssum = meta.map(m => if(m.mcp) m.rowNum * m.colNum * 2 else m.rowNum * m.colNum).sum
      MbistControllerParam(c, r, s, m, d, a, l, seg, rof, ssum)
    }
  }

  class MbistReqBundle(param: MbistControllerParam) extends Bundle {
    val addr = UInt(param.addrBits.W)
    val data = UInt(param.maxDataWidth.W)
    val mask = UInt(param.maxMaskWidth.W)
    val depth = UInt(param.latencyBits.W)
    val array = UInt(param.arrayBits.W)
    val noCompare = Bool()
  }

  class SramInfo(param: MbistControllerParam) extends Bundle {
    val array = UInt(param.arrayBits.W)
    val cols = UInt(param.colsBits.W)
    val rows = UInt(param.rowsBits.W)
    val dataBits = UInt(param.dataInfoBits.W)
    val mcp = Bool()
    val dp = Bool()
    val seg = UInt(param.segInfoBits.W)
    val maskBits = UInt(param.maskInfoBits.W)
    val latency = UInt(param.latencyBits.W)
    val rowOff = UInt(param.rowOffInfoBits.W)
  }
}
