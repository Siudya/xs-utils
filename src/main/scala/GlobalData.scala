package xs.utils

import xs.utils.dft.{BAP, EdtWrapper, IjtagNode, TestController}

import scala.collection.mutable

object GlobalData {
  var prefix = ""
  val edtMods: mutable.ListBuffer[EdtWrapper] = mutable.ListBuffer[EdtWrapper]()
  val bapMods: mutable.ListBuffer[BAP] = mutable.ListBuffer[BAP]()
  val ctrlMods: mutable.ListBuffer[TestController] = mutable.ListBuffer[TestController]()
  var topNode: IjtagNode = null

  def addEdt(edt:EdtWrapper):Unit = {
    edtMods += edt
  }
  def addBap(bap:BAP):Unit = {
    bapMods += bap
  }
  def addCtrl(ctrl:TestController):Unit = {
    require(ctrlMods.isEmpty)
    ctrlMods += ctrl
  }
}
