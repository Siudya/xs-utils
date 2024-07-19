package xs.utils.dft

import chisel3._
import chisel3.util._
import xs.utils.GlobalData

trait RegTraits {
  def width: Int

  val io = IO(new Bundle {
    val D = Input(UInt(width.W))
    val RSTV = Input(UInt(width.W))
    val RST = Input(Bool())
    val CP = Input(Clock())
    val CE = Input(Bool())
    val Q = Output(UInt(width.W))
  })
}

class NegedgeReg(val width: Int) extends BlackBox(Map("WIDTH" -> width)) with HasBlackBoxInline with RegTraits {
  private val modName = GlobalData.prefix + "NegedgeReg"
  setInline(modName + ".sv",
    s"""module $modName #(parameter WIDTH = 1) (
       |  input	[WIDTH - 1 : 0] D,
       |  input	[WIDTH - 1 : 0] RSTV,
       |  input  CP,
       |  input  RST,
       |  input  CE,
       |  output reg [WIDTH - 1 : 0] Q
       |);
       |  always @(negedge CP or posedge RST) begin
       |    if(RST) begin
       |      Q <= RSTV;
       |    end else if(CE) begin
       |      Q <= D;
       |    end
       |  end
       |endmodule""".stripMargin)
}

class PosedgeReg(val width: Int) extends BlackBox(Map("WIDTH" -> width)) with HasBlackBoxInline with RegTraits {
  private val modName = GlobalData.prefix + "PosedgeReg"
  setInline(modName + ".sv",
    s"""module $modName #(parameter WIDTH = 1) (
       |  input	[WIDTH - 1 : 0] D,
       |  input	[WIDTH - 1 : 0] RSTV,
       |  input  CP,
       |  input  RST,
       |  input  CE,
       |  output reg [WIDTH - 1 : 0] Q
       |);
       |  always @(posedge CP or posedge RST) begin
       |    if(RST) begin
       |      Q <= RSTV;
       |    end else if(CE) begin
       |      Q <= D;
       |    end
       |  end
       |endmodule""".stripMargin)
}

object RegPrimitive {
  def apply(data: Data, init: Data, en: Bool, reset: Reset, clock: Clock, pos: Boolean): RegTraits = {
    val reg = if (pos) Module(new PosedgeReg(data.getWidth)) else Module(new NegedgeReg(data.getWidth))
    reg.io.RSTV := init.asUInt
    reg.io.RST := reset.asBool
    reg.io.CP := clock
    reg.io.CE := en
    reg.io.D := data
    reg
  }

  def apply(data: Data, en: Bool, clock: Clock, pos: Boolean): RegTraits = {
    val reg = if (pos) Module(new PosedgeReg(data.getWidth)) else Module(new NegedgeReg(data.getWidth))
    reg.io.RSTV := 0.U(data.getWidth.W)
    reg.io.RST := false.B
    reg.io.CP := clock
    reg.io.CE := en
    reg.io.D := data
    reg
  }

  def apply(data: Data, init: Data, reset: Reset, clock: Clock, pos: Boolean): RegTraits = apply(data, init, true.B, reset, clock, pos)

  def apply(data: Data, clock: Clock, pos: Boolean): RegTraits = apply(data, true.B, clock, pos)

  def apply(data: RegTraits, init: Data, en: Bool, reset: Reset, clock: Clock, pos: Boolean): RegTraits = apply(data.io.Q, init, en, reset, clock, pos)

  def apply(data: RegTraits, init: Data, reset: Reset, clock: Clock, pos: Boolean): RegTraits = apply(data.io.Q, init, true.B, reset, clock, pos)

  def apply(data: RegTraits, clock: Clock, pos: Boolean): RegTraits = apply(data.io.Q, clock, pos)

  def apply(data: RegTraits, en: Bool, clock: Clock, pos: Boolean): RegTraits = apply(data.io.Q, en, clock, pos)
}

object NReg {
  def apply(data: Data, init: Data, en: Bool, reset: Reset, clock: Clock): NegedgeReg =
    RegPrimitive(data, init, en, reset, clock, false).asInstanceOf[NegedgeReg]

  def apply(data: Data, en: Bool, clock: Clock): NegedgeReg =
    RegPrimitive(data, en, clock, false).asInstanceOf[NegedgeReg]

  def apply(data: Data, init: Data, reset: Reset, clock: Clock): NegedgeReg =
    RegPrimitive(data, init, reset, clock, false).asInstanceOf[NegedgeReg]

  def apply(data: Data, clock: Clock): NegedgeReg =
    RegPrimitive(data, clock, false).asInstanceOf[NegedgeReg]

  def apply(data: RegTraits, init: Data, en: Bool, reset: Reset, clock: Clock): NegedgeReg =
    RegPrimitive(data, init, en, reset, clock, false).asInstanceOf[NegedgeReg]

  def apply(data: RegTraits, init: Data, reset: Reset, clock: Clock): NegedgeReg =
    RegPrimitive(data, init, reset, clock, false).asInstanceOf[NegedgeReg]

  def apply(data: RegTraits, clock: Clock): NegedgeReg =
    RegPrimitive(data, clock, false).asInstanceOf[NegedgeReg]

  def apply(data: RegTraits, en: Bool, clock: Clock): NegedgeReg =
    RegPrimitive(data, en, clock, false).asInstanceOf[NegedgeReg]
}

object PReg {
  def apply(data: Data, init: Data, en: Bool, reset: Reset, clock: Clock): PosedgeReg =
    RegPrimitive(data, init, en, reset, clock, true).asInstanceOf[PosedgeReg]

  def apply(data: Data, en: Bool, clock: Clock): PosedgeReg =
    RegPrimitive(data, en, clock, true).asInstanceOf[PosedgeReg]

  def apply(data: Data, init: Data, reset: Reset, clock: Clock): PosedgeReg =
    RegPrimitive(data, init, reset, clock, true).asInstanceOf[PosedgeReg]

  def apply(data: Data, clock: Clock): PosedgeReg =
    RegPrimitive(data, clock, true).asInstanceOf[PosedgeReg]

  def apply(data: RegTraits, init: Data, en: Bool, reset: Reset, clock: Clock): PosedgeReg =
    RegPrimitive(data, init, en, reset, clock, true).asInstanceOf[PosedgeReg]

  def apply(data: RegTraits, init: Data, reset: Reset, clock: Clock): PosedgeReg =
    RegPrimitive(data, init, reset, clock, true).asInstanceOf[PosedgeReg]

  def apply(data: RegTraits, clock: Clock): PosedgeReg =
    RegPrimitive(data, clock, true).asInstanceOf[PosedgeReg]

  def apply(data: RegTraits, en: Bool, clock: Clock): PosedgeReg =
    RegPrimitive(data, en, clock, true).asInstanceOf[PosedgeReg]
}