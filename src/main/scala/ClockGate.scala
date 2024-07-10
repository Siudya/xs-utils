package xs.utils

import chisel3._
import chisel3.util._

class ClockGate extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E = Input(Bool())
    val CK = Input(Clock())
    val Q = Output(Clock())
  })
  setInline("ClockGate.sv",
    """
      |module ClockGate (
      |  input  wire TE,
      |  input  wire E,
      |  input  wire CK,
      |  output wire Q
      |);
      |  reg EN;
      |  always_latch begin
      |    if(!CK) EN = TE | E;
      |  end
      |  assign Q = CK & EN;
      |endmodule
      |
      |""".stripMargin)
}