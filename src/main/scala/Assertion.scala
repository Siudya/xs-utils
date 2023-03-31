package xs.utils
import chisel3._
object Assertion {
  private var enable = true
  def xs_assert(cond:Bool, msg: String):Unit = {
    if(enable) assert(cond, msg + "\n?baifenhao!m @ ?baifenhao!t")
  }
  def xs_assert(cond: Bool): Unit = {
    if(enable) assert(cond, "\n?baifenhao!m @ ?baifenhao!t")
  }

  def set_enable(newval:Boolean):Unit = enable = newval
}
