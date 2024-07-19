package xs.utils.dft

import chisel3._
import chisel3.util._

import scala.collection.mutable

object BaseInstrument {
  private val shiftOutInfos = mutable.ListBuffer[String]("ijtag_si")

  def genTdrStr(name: String, width: Int, rstV: UInt, out: Boolean): String = {
    val rangeStr = if (width == 1) "" else s"[${width - 1}:0]"
    val shiftIn = shiftOutInfos.last
    val shiftOut = if (width == 1) name else name + "[0]"
    val capStr = if (out) name else s"$width'b" + Seq.fill(width)("x").reduce(_ + _)
    val rstStr = s"$width'b${rstV.litValue.toString(2)}"
    val res =
      s"""
         |  ScanRegister $name$rangeStr {
         |    ScanInSource $shiftIn;
         |    CaptureSource $capStr;
         |    ResetValue $rstStr;
         |  }""".stripMargin
    shiftOutInfos += shiftOut
    res
  }

  def genScanRegister(descSeq: Seq[(String, (Int, UInt, Boolean))]): String = {
    var res = ""
    for (idx <- descSeq.indices) {
      val thisInfo = descSeq(idx)
      res += genTdrStr(thisInfo._1, thisInfo._2._1, thisInfo._2._2, thisInfo._2._3)
    }
    res +=
      s"""
         |  ScanOutPort ijtag_so {
         |    Source ${shiftOutInfos.last};
         |  }
         |}
         |""".stripMargin
    shiftOutInfos.clear()
    shiftOutInfos += "ijtag_si"
    res
  }
}

abstract class BaseInstrument(descSeq: Seq[(String, (Int, UInt, Boolean))], val mName: String) extends RawModule with HasIjtag {
  val tdrWidth: Int = descSeq.map(_._2._1).sum
  private val fieldWidths = descSeq.map(_._2._1)
  private val fieldShiftVals = fieldWidths.zipWithIndex.map { case (_, idx) => fieldWidths.take(idx).sum }
  private val fieldRstVals = descSeq.map(_._2._2.litValue)
  private val tdrInitVal = fieldShiftVals.zip(fieldRstVals).map({ case (s, v) => v << s }).reduce(_ | _).U(tdrWidth.W)

  private val shifter = Wire(UInt(tdrWidth.W))
  private val rst = (!ijtag.reset.asBool).asAsyncReset
  private val tdrUpdateReg = NReg(shifter, tdrInitVal, ijtag.ue & ijtag.sel, rst, ijtag.tck)
  private val shiftDelayReg = NReg(shifter(0), ijtag.sel, ijtag.tck)
  ijtag.so := shiftDelayReg.io.Q

  private val tdrUpdate = Wire(UInt(tdrWidth.W))
  tdrUpdate := tdrUpdateReg.io.Q
  private val tdrToShift = Wire(UInt(tdrWidth.W))

  private val fieldOffsetSeq = descSeq.indices.map(i => descSeq.take(i).map(_._2._1).sum)

  val tdrFields: Map[String, UInt] = descSeq.zip(fieldOffsetSeq).map({
    case ((descStr, (width, _, _)), offset) =>
      val data = Wire(UInt(width.W))
      data := tdrUpdateReg.io.Q(offset + width - 1, offset)
      data.suggestName(descStr)
      dontTouch(data)
      descStr -> data
  }).toMap

  private def getTdrShiftFields: Map[String, UInt] = {
    val res = descSeq.map({ case (descStr, (width, _, _)) =>
      val data = Wire(UInt(width.W))
      data := tdrFields(descStr)
      data.suggestName(descStr + "_cap")
      dontTouch(data)
      descStr -> data
    }).toMap
    tdrToShift := Cat(descSeq.map(d => res(d._1)).reverse)
    res
  }

  val tdrShiftFields: Map[String, UInt] = getTdrShiftFields

  withClockAndReset(ijtag.tck, rst) {
    val shiftReg = RegInit(tdrInitVal)
    shifter := shiftReg
    when(ijtag.ce && ijtag.sel) {
      shiftReg := tdrToShift
    }.elsewhen(ijtag.se && ijtag.sel) {
      shiftReg := Cat(ijtag.si, shiftReg(tdrWidth - 1, 1))
    }
  }
  icl += BaseInstrument.genScanRegister(descSeq.reverse)
  icl = icl.replace("__XSMODNAMESTRREPL__", modName)
}

