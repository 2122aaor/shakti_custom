

module Tb(
//Signals Mem related
	 output reg [31:0] data_proc_o,
   output reg [31:0] addr_proc,
	 output reg cmd_proc,
	 output reg cmd_vld,
	 output reg [7:0] blen_proc,
   input rdata_sig_vld,
	 input [31:0] data_proc_i,
	 input cmd_cmptd,
	 input [7:0] tg_state,
	 input clk,
   input resetn,	
//Proc related Signals
   output reg rdy,
//Command   
   input wire [7:0] blen_i,
   input wire [31:0] addr_i,
   input  cmd_i,
   input wire [31:0] wdata_i,
//Command Strobe
   input cmd_vld_i,
//Read Output
   output wire [31:0] rdata_o,   
   output wire rdata_vld
   );

reg [31:0] memory [2047:0] ;
initial $readmemh("/home/vishvesh/ddr3_ex2/mig_7series_0_example/mig_7series_0_example.srcs/sources_1/ip/mig_7series_0/traffic_gen1/code.hex", memory) ;
reg [6:0] ctr;
wire [31:0] data_i;
reg [2:0] state,next_state;

assign rdata_o=data_proc_i;
assign rdata_vld=rdata_sig_vld;

initial $dumpvars();



always @(posedge clk)

begin
if(!resetn) begin
ctr=0;
rdy=0;
end else begin

if(state==0) begin

if(cmd_cmptd )
begin
addr_proc=ctr << 2;
data_proc_o=memory[ctr];
cmd_proc=1;
blen_proc=0;
ctr=ctr+1;
end
if(tg_state==2)
cmd_vld=1;
if((tg_state==4) || (tg_state==32))
cmd_vld=0;
end else 


if(state==1) begin

if(cmd_cmptd)
rdy=1;
if(cmd_vld_i) begin
addr_proc=addr_i;
cmd_proc=cmd_i;
blen_proc=blen_i-1;
data_proc_o=wdata_i;
cmd_vld=1;
rdy=0;
end
if((tg_state==4) || (tg_state==32))
cmd_vld=0;


end

end
end

always @(posedge clk)
begin
if(!resetn)
state=0;
else
state=next_state;
end








always @(*)
begin
case(state)

0: begin
if(ctr==30)
next_state=1; 
else
next_state=0;
end



endcase
end

endmodule

