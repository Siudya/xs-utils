package xs.utils.dft
import chisel3._
import xs.utils.sram.SramBroadcastBundle

import scala.collection.mutable



class DftModBundle(ci:Int, co:Int) extends Bundle {
  val atpg_clock: Clock = Input(Clock())
  val scan_en: Bool = Input(Bool())
  val edt = new EdtFuncBundle(ci, co)
}
