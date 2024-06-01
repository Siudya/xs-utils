package xs.utils.mbist

import chisel3._
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import firrtl.annotations.Annotation
import firrtl.transforms.NoDedupAnnotation

class STD_CLKGT_func extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E = Input(Bool())
    val CK = Input(Clock())
    val Q = Output(Clock())
    val dft_l3dataram_clk = Input(Bool())
    val dft_l3dataramclk_bypass = Input(Bool())
  })
  private val modName = xs.utils.dft.GlobalData.modulePrefix + "STD_CLKGT_func"
  setInline(modName + ".sv",
    s"""module $modName (
       |  input  wire TE,
       |  input  wire E,
       |  input  wire CK,
       |  output wire Q,
       |  input  wire dft_l3dataram_clk,
       |  input  wire dft_l3dataramclk_bypass
       |);
       |  reg EN;
       |  always_latch begin
       |    if(!CK) EN = TE | E;
       |  end
       |  assign Q = CK & EN;
       |endmodule""".stripMargin)
}

class CGBroadcastSignals extends Bundle {
  val cgen = Input(Bool())
  val l3dataram_clk = Input(Bool())
  val l3dataramclk_bypass = Input(Bool())
}

class MBISTClockGateCell extends Module {
  val mbist = IO(new Bundle {
    val writeen = Input(Bool())
    val readen = Input(Bool())
    val req = Input(Bool())
  })
  val E = IO(Input(Bool()))
  val dft = IO(new CGBroadcastSignals)
  val out_clock = IO(Output(Clock()))

  private val _E = Mux(mbist.req, mbist.readen | mbist.writeen, E)
  private val _TE = dft.cgen

  val CG = Module(new STD_CLKGT_func)
  CG.io.E := _E
  CG.io.TE := _TE
  CG.io.CK := clock
  out_clock := CG.io.Q
  CG.io.dft_l3dataram_clk := dft.l3dataram_clk
  CG.io.dft_l3dataramclk_bypass := dft.l3dataramclk_bypass
}

