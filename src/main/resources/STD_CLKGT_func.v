module STD_CLKGT_func (
  input  wire TE,
  input  wire E,
  input  wire CK,
  output wire Q,
  input  wire dft_l3dataram_clk,
  input  wire dft_l3dataramclk_bypass
);

  wire clk_en;
  reg  clk_en_reg;

  assign clk_en = E | TE;
  assign Q = CK & clk_en_reg;

`ifdef VCS
  always @(CK or clk_en)
  begin
    if(CK == 1'b0) clk_en_reg <= clk_en;
  end
`elsif VERILATOR_5016
  always @(CK or clk_en)
  begin
    if(CK == 1'b0) clk_en_reg <= clk_en;
  end
`else
  always @(posedge CK) 
  begin
    clk_en_reg = clk_en;
  end
`endif

endmodule // Copy from Xihu
