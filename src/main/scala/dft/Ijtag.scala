package xs.utils.dft

import chisel3._
import chisel3.experimental.BaseModule

import scala.collection.mutable
import Ijtag._
import xs.utils.GlobalData

object Ijtag {
  protected[dft] val allNodes = mutable.ArrayBuffer[IjtagNode]()

  def exportIcl(): Unit = {
    allNodes.foreach { n =>
      val mod = n.holder.isInstanceOf[RawModule]
      if (mod) n.instName = n.holder.asInstanceOf[RawModule].instanceName
    }
    val wrapperNodes = allNodes.filter(n => n.subNext != null && !n.holder.isInstanceOf[SIB] && !n.holder.isInstanceOf[BlackBox])
    wrapperNodes.foreach(_.genIcl())
    val allOtherNodes = allNodes.filterNot(n => n.subNext != null && !n.holder.isInstanceOf[SIB] && !n.holder.isInstanceOf[BlackBox])
    allOtherNodes.foreach(n => FileManager.add("icl", n.modName, n.holder.icl))
    val topNode = wrapperNodes.filter(n => n.prev == null && n.next == null)
    require(topNode.length == 1)
    GlobalData.topNode = topNode.head
  }

  protected[dft] def iclFn(self: IjtagNode, master: IjtagNode, top: IjtagNode): Unit = {
    val prev = self.prev
    require(prev != null, s"ijtag of ${top.modName}.${self.instName} has no driver")
    val siSrc = if (prev == top) {
      "ijtag_si"
    } else if (prev == master) {
      s"${prev.instName}.host_si"
    } else {
      s"${prev.instName}.ijtag_so"
    }
    val selSrc = if (master != top) {
      s"${master.instName}.host_sel"
    } else {
      "ijtag_sel"
    }
    val isSib = self.holder.isInstanceOf[SIB]
    val sibHostSoStr = if (isSib) s"\n    InputPort host_so = ${self.subPrev.instName}.ijtag_so;\n" else "\n"
    top.holder.icl +=
      s"""|  Instance ${self.instName} Of ${self.modName} {
          |    InputPort ijtag_ce = ijtag_ce;
          |    InputPort ijtag_se = ijtag_se;
          |    InputPort ijtag_ue = ijtag_ue;
          |    InputPort ijtag_tck = ijtag_tck;
          |    InputPort ijtag_reset = ijtag_reset;
          |    InputPort ijtag_si = ${siSrc};
          |    InputPort ijtag_sel = ${selSrc};""".stripMargin
    top.holder.icl += sibHostSoStr
    top.holder.icl += "  }\n"
  }
}

class IjtagNode(val holder: HasIjtagNode) {
  var instName = ""
  var prev: IjtagNode = _
  var next: IjtagNode = _
  var subPrev: IjtagNode = _
  var subNext: IjtagNode = _
  lazy val modName: String = holder.modName

  protected[dft] def genIcl(): Unit = {
    if (subNext != null) {
      val ctxStack = mutable.Stack[IjtagNode]()
      ctxStack.push(this)
      var now = subNext
      while (now != this) {
        require(now.next != null)
        iclFn(now, ctxStack.top, this)
        if (now.holder.isInstanceOf[SIB]) {
          ctxStack.push(now)
          now = now.subNext
        } else if (now.next == ctxStack.top && ctxStack.top != this) {
          require(now.next.next != null)
          ctxStack.pop()
          now = now.next.next
        } else {
          now = now.next
        }
      }
      holder.icl +=
        s"""|  ScanOutPort ijtag_so {
            |    Source ${subPrev.instName}.ijtag_so;
            |  }
            |}
            |""".stripMargin
      holder.icl = holder.icl.replace("__XSMODNAMESTRREPL__", holder.modName)
    } else {
      holder.icl +=
        s"""|  ScanOutPort ijtag_so {
            |    Source ijtag_si;
            |  }
            |}
            |""".stripMargin
    }
    FileManager.add("icl", holder.modName, holder.icl)
  }
}

class IjtagIntf(val node: IjtagNode) extends Bundle {
  val si = Input(Bool())
  val so = Output(Bool())
  val ce = Input(Bool())
  val se = Input(Bool())
  val ue = Input(Bool())
  val sel = Input(Bool())
  val reset = Input(AsyncReset())
  val tck = Input(Clock())
}

trait HasIjtagNode {
  m: BaseModule =>
  def mName: String

  lazy val modName: String = GlobalData.prefix + mName
  var icl: String = ""
  val ijtagNode: IjtagNode = new IjtagNode(this)
}

trait HasIjtag extends HasIjtagNode {
  m: RawModule =>
  val ijtag: IjtagIntf = IO(new IjtagIntf(ijtagNode))
  ijtag.so := ijtag.si
  icl +=
    s"""|
        |Module __XSMODNAMESTRREPL__ {
        |  ResetPort ijtag_reset { ActivePolarity 0; }
        |  SelectPort ijtag_sel;
        |  ScanInPort ijtag_si;
        |  CaptureEnPort ijtag_ce;
        |  ShiftEnPort ijtag_se;
        |  UpdateEnPort ijtag_ue;
        |  TCKPort ijtag_tck;
        |  ScanInterface ijtag {
        |    Port ijtag_ce;
        |    Port ijtag_reset;
        |    Port ijtag_se;
        |    Port ijtag_sel;
        |    Port ijtag_si;
        |    Port ijtag_so;
        |    Port ijtag_tck;
        |    Port ijtag_ue;
        |  }
        |""".stripMargin
  allNodes.append(ijtagNode)

  protected[HasIjtag] def makeChain(ports: Seq[IjtagIntf]): Unit = {
    val slaves = ports.tail
    val master = ports.head
    for (s <- slaves) {
      s.ce := master.ce
      s.se := master.se
      s.ue := master.ue
      s.sel := master.sel
      s.reset := master.reset
      s.tck := master.tck
    }
    slaves.reduceLeft((a: IjtagIntf, b: IjtagIntf) => {
      b.si := a.so
      a.node.next = b.node
      b.node.prev = a.node
      b
    })
    slaves.head.si := master.si
    master.node.subNext = slaves.head.node
    slaves.head.node.prev = master.node

    master.so := slaves.last.so
    slaves.last.node.next = master.node
    master.node.subPrev = slaves.last.node
  }
}

