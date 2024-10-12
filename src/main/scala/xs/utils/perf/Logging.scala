package xs.utils.perf
import chisel3._
import xs.utils.perf.XSLogLevel.XSLogLevel
import org.chipsalliance.cde.config.Parameters
import xs.utils.GTimer
object XSLogLevel extends Enumeration {
  type XSLogLevel = Value
  val ALL   = Value(0, "ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val PERF  = Value("PERF ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

class XSLogger(logEnable:Bool, logTimestamp:UInt, val logLevel: XSLogLevel)(implicit p: Parameters){
  private val MagicStr = "_LOG_MODULE_PATH_"
  private val debugOpts = p(DebugOptionsKey)
  private val enableDebug = debugOpts.EnableDebug && logLevel != XSLogLevel.PERF
  private val enablePerf = debugOpts.EnablePerfDebug && logLevel == XSLogLevel.PERF
  def apply(prefix: Boolean, cond: Bool, pable: Printable): Any = {
    if (!debugOpts.FPGAPlatform && (enableDebug || enablePerf || logLevel == XSLogLevel.ERROR)) {
      val check_cond = (if (logLevel == XSLogLevel.ERROR) true.B else logEnable) && cond
      when(check_cond) {
        val commonInfo = p"[$logLevel][time=$logTimestamp] $MagicStr: "
        printf((if (prefix) commonInfo else p"") + pable)
        if (logLevel >= XSLogLevel.ERROR) {
          assert(false.B)
        }
      }
    }
  }
  def displayLog: Bool = {
    val debugOpts = p(DebugOptionsKey)
    val ret = WireInit(false.B)
    if (!debugOpts.FPGAPlatform && debugOpts.EnableDebug) {
      ret := logEnable
    }
    ret
  }
  def trigger: Bool = displayLog
  def printPrefix(): Unit = {
    val commonInfo = p"[$logLevel][time=${GTimer()}] ${MagicStr}: "
    when(trigger) {
      printf(commonInfo)
    }
  }
  def apply(cond: Bool, fmt: String, data: Bits*): Any = apply(cond, Printable.pack(fmt, data: _*))
  def apply(cond: Bool, pable: Printable): Any = apply(true, cond, pable)
  def apply(fmt: String, data: Bits*): Any = apply(Printable.pack(fmt, data: _*))
  def apply(pable: Printable): Any = apply(true.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*): Any = apply(prefix, cond, Printable.pack(fmt, data: _*))
}
