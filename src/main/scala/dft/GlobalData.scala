package xs.utils.dft

import scala.collection.mutable


object GlobalData {
  var modulePrefix: String = ""
  val edtMods: mutable.ListBuffer[EdtWrapper] = mutable.ListBuffer[EdtWrapper]()
  val bapMods: mutable.ListBuffer[BAP] = mutable.ListBuffer[BAP]()
  val ctrlMods: mutable.ListBuffer[TestController] = mutable.ListBuffer[TestController]()
  var topNode: IjtagNode = null
}
