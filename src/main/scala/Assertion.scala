package xs.utils
import chisel3._
object Assertion {
  def xs_assert(cond:Bool, msg: String):Unit = {
    assert(cond, msg + "\n?baifenhao!m @ ?baifenhao!t")
  }
  def xs_assert(cond: Bool): Unit = {
    assert(cond, "\n?baifenhao!m @ ?baifenhao!t")
  }
}
