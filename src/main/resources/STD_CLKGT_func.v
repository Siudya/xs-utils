module STD_CLKGT_func (
  input  wire TE,
  input  wire E,
  input  wire CK,
  output wire Q,
  input  wire dft_l3dataram_clk,
  input  wire dft_l3dataramclk_bypass
);
  reg EN;
  always_latch begin
    if(!CK) EN = TE | E;
  end
  assign Q = CK & EN;
endmodule