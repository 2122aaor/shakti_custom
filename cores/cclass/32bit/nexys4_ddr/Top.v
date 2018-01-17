`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    11:22:20 02/17/2016 
// Design Name: 
// Module Name:    Top 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////
module Top(
    input CLK,
    input RST_N,
    input sin_in,
    output sout
    );
mkproc pc(.CLK(clock),
		       .RST_N(RST_N),
				 .EN_sin(1'b1),
		       .sin_in(sin_in),
				 .RDY_sout(),
				 .RDY_sin(),
		       .sout(sout));
				 
artix_clockdivider clk_div
 ( .clk_in1(CLK),
  .clk_out1(clock),
  .reset(1'b0),
  .locked());
endmodule
