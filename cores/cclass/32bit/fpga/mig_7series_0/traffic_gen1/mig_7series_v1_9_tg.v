//*****************************************************************************
// (c) Copyright 2009 - 2010 Xilinx, Inc. All rights reserved.
//
// This file contains confidential and proprietary information
// of Xilinx, Inc. and is protected under U.S. and
// international copyright and other intellectual property
// laws.
//
// DISCLAIMER
// This disclaimer is not a license and does not grant any
// rights to the materials distributed herewith. Except as
// otherwise provided in a valid license issued to you by
// Xilinx, and to the maximum extent permitted by applicable
// law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
// WITH ALL FAULTS, AND XILINX HEREBY DISCLAIMS ALL WARRANTIES
// AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
// BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
// INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
// (2) Xilinx shall not be liable (whether in contract or tort,
// including negligence, or under any other theory of
// liability) for any loss or damage of any kind or nature
// related to, arising under or in connection with these
// materials, including for any direct, or any indirect,
// special, incidental, or consequential loss or damage
// (including loss of data, profits, goodwill, or any type of
// loss or damage suffered as a result of any action brought
// by a third party) even if such damage or loss was
// reasonably foreseeable or Xilinx had been advised of the
// possibility of the same.
//
// CRITICAL APPLICATIONS
// Xilinx products are not designed or intended to be fail-
// safe, or for use in any application requiring fail-safe
// performance, such as life-support or safety devices or
// systems, Class III medical devices, nuclear facilities,
// applications related to the deployment of airbags, or any
// other applications that could lead to death, personal
// injury, or severe property or environmental damage
// (individually and collectively, "Critical
// Applications"). Customer assumes the sole risk and
// liability of any use of Xilinx products in Critical
// Applications, subject only to applicable laws and
// regulations governing limitations on product liability.
//
// THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
// PART OF THIS FILE AT ALL TIMES.
//
//*****************************************************************************
//   ____  ____
//  /   /\/   /
// /___/  \  /    Vendor: Xilinx
// \   \   \/     Version: 3.6
//  \   \         Application: MIG
//  /   /         Filename: tg.v
// /___/   /\     Date Last Modified: $Date: 2011/06/02 08:37:24 $
// \   \  /  \    Date Created: Sept 16, 2009
//  \___\/\___\
//
//Device: Virtex-6, Spartan-6 and 7series
//Design Name: DDR3 SDRAM
//Purpose:
//   This module generates and checks the AXI traffic 
//
//Reference:
//Revision History:
//*****************************************************************************

module mig_7series_v1_9_tg #(
  
   parameter C_AXI_ADDR_WIDTH     = 32, // This is AXI address width for all 
                                        // SI and MI slots
   parameter C_AXI_DATA_WIDTH     = 32, // Width of the AXI write and read data

   parameter C_AXI_NBURST_SUPPORT = 0, // Support for narrow burst transfers
                                       // 1-supported, 0-not supported 
   parameter C_BEGIN_ADDRESS      = 32'h0, // Start address of the address map

   parameter C_END_ADDRESS        = 32'h0000_00FF, // End address of the address map
 
   parameter C_EN_WRAP_TRANS      = 0, // Should be set to 1 for wrap transactions

   parameter CTL_SIG_WIDTH        = 3,  // Control signal width

   parameter WR_STS_WIDTH         = 16, // Write port status signal width

   parameter RD_STS_WIDTH         = 16, // Read port status signal width
 
   parameter DBG_WR_STS_WIDTH     = 32,

   parameter DBG_RD_STS_WIDTH     = 32,

   parameter ENFORCE_RD_WR        = 0,

   parameter ENFORCE_RD_WR_CMD    = 8'h11,

   parameter PRBS_EADDR_MASK_POS  = 32'hFFFFD000,

   parameter PRBS_SADDR_MASK_POS  = 32'h00002000,

   parameter ENFORCE_RD_WR_PATTERN = 3'b000

)
(
// 
   input                      clk,          // input clock
   input                      resetn,       // Active low reset signal

// Input start signals
   input                      init_cmptd,   // Initialization completed
   input                      init_test,    // Initialize the test
   input                      wrap_en,      // Enable wrap transactions

// Control ports
    input                      cmd_ack,      // Command has been accepted
   output reg                 cmd_en,       // Command enable
    output [2:0]               cmd,          // Command
    output reg [7:0]           blen,         // Length of the burst
    output reg [31:0]          addr,         // output address
   output [CTL_SIG_WIDTH-1:0] ctl,          // Control signal

// Write port
   input                      wdata_rdy,    // Write data ready to be accepted
   output                     wdata_vld,    // Write data valid
   output reg                 wdata_cmptd,  // Write data completed
    output reg [C_AXI_DATA_WIDTH-1:0] wdata,     // Write data 
   output reg [C_AXI_DATA_WIDTH/8-1:0] wdata_bvld, // Byte valids
   input                      wdata_sts_vld, // Status valid
   input [WR_STS_WIDTH-1:0]   wdata_sts,     // Write status 

// Read Port
   input                      rdata_vld,    // Read data valid 
    input [C_AXI_DATA_WIDTH-1:0] rdata,      // Write data 
   input [C_AXI_DATA_WIDTH/8-1:0] rdata_bvld, // Byte valids
   input                      rdata_cmptd,  // Read data completed
   input [RD_STS_WIDTH-1:0]   rdata_sts,    // Read status
   output                     rdata_rdy,    // Read data ready


// Error status signals
   output reg                 cmd_err,      // Error during command phase
   output reg                 data_msmatch_err, // Data mismatch
   output reg                 write_err,    // Write error occured
   output reg                 read_err,     // Read error occured
   output                     test_cmptd,   // Completed testing with all patterns
   output                     write_cmptd,  // Completed write operation
   output                     read_cmptd,   // Completed write operation

// Debug status signals
   output                     cmp_data_en,
   output [C_AXI_DATA_WIDTH-1:0] rdata_cmp,   // read data 
   output reg                 dbg_wr_sts_vld, // Write debug status valid,
   output [DBG_WR_STS_WIDTH-1:0] dbg_wr_sts,  // Write status
   output reg                 dbg_rd_sts_vld, // Read debug status valid
   output [DBG_RD_STS_WIDTH-1:0] dbg_rd_sts,   // Read status
   //Proc related Signals
         output  rdy,
      //Command   
         input wire [7:0] blen_i,
         input wire [31:0] addr_i,
         input  cmd_i,
         input wire [31:0] wdata_i,
         input wire [3:0] bvld_proc,
      //Command Strobe
         input cmd_vld_i,
      //Read Output
         output wire [31:0] rdata_op,   
         output wire rdata_vld1
      
   
);   
 wire init_cmptd;


//*****************************************************************************
// Proc related signals
//*****************************************************************************


//*****************************************************************************
// Internal parameter declarations
//*****************************************************************************

  parameter [8:0]             TG_IDLE       = 8'd0,
                              TG_GEN_PRBS   = 8'd1,
                              TG_WR_CMD     = 8'd2,
                              TG_WR_DATA    = 8'd3,
                              TG_WR_DONE    = 8'd4,
			      TG_RD_CMD     = 8'd5,
			      TG_RD_DATA    = 8'd6;


//*****************************************************************************
// Internal wire and reg declarations
//*****************************************************************************

  wire                             dgen_en;
  wire                             dgen_init;
  wire                             msmatch_err;
  wire                             rdata_sig_vld;
  reg                             cmd_gen_csr_sig;

  wire  [31:0]                       addr1;
  wire  [31:0]                       rdata_o;
  reg [31:0]                         addr_proc_r;
  wire  [31:0]                       wdata_proc;
  wire  [31:0]                       addr_proc;
  reg  [7:0]                       tg_state;
  reg  [7:0]                       next_tg_state;
  reg  [7:0]                       blen_cntr;
  wire  [7:0]                       blen_proc;
  reg  [7:0]                       wrd_cntr;  
  reg                              wr_proc;
  reg                              cmd_wr_en;
  reg                              cmd_wr_en_r;
  reg                              cmd_rd_en;
  reg                              cmd_err_dbg;
  reg                              data_msmatch_err_dbg;
  reg                              write_err_dbg;
  reg                              read_err_dbg;
  reg  [WR_STS_WIDTH-1:0]          wdata_sts_r; // Write status registered 
  reg  [RD_STS_WIDTH-1:0]          rdata_sts_r; // Read status registered
  



//*****************************************************************************
// Processor Interface
//*****************************************************************************
Tb u0 (
// Proc signals

	 wdata_proc,
   addr_proc,
	 cmd_proc,
	 cmd_vld,
	 blen_proc,
   rdata_sig_vld,
	 rdata_o,
	 cmd_cmptd,
	 tg_state,
	 clk,
   resetn,
//Proc related Signals
      rdy,
   //Command   
      blen_i,
      addr_i,
      cmd_i,
      wdata_i,
   //Command Strobe
      cmd_vld_i,
   //Read Output
      rdata_op,   
      rdata_vld1
   	);
   
//   assign rdata_o = (addr_proc[3:2] == 1) ? (rdata >> 32) : 
//                  (addr_proc[3:2] == 2) ? (rdata >> 64) : 
//                  (addr_proc[3:2] == 3) ? (rdata >> 96) : 
//                  rdata;

   assign rdata_o = rdata;



//*****************************************************************************
// FSM Control Block
//*****************************************************************************

  always @(posedge clk) begin
    if (!resetn | init_test)
      tg_state <= 8'h1;
    else 
      tg_state <= next_tg_state;
  end

  always @(*) begin
    next_tg_state = 8'h0;
    case (1'b1)
      tg_state[TG_IDLE]: begin // 8'h01
	if (init_cmptd) 
	  next_tg_state[TG_GEN_PRBS] = 1'b1;
        else
	  next_tg_state[TG_IDLE] = 1'b1;
      end

      tg_state[TG_GEN_PRBS]: begin // 8'h02
				if (cmd_vld) begin
					cmd_gen_csr_sig=cmd_proc;
					addr_proc_r=addr_proc;
					addr=addr1;
					blen=blen_proc;
					wdata=wdata_proc;
					wdata_bvld=bvld_proc;
//					case (addr_proc[3:2])
//					2'b00: begin
//					wdata={96'b0,wdata_proc};
//					wdata_bvld={12'b0,bvld_proc};
//					end
//					2'b01 : begin
//					wdata={64'b0,wdata_proc,32'b0};
//                    wdata_bvld={8'b0,bvld_proc,4'b0};
//                    end
//                    2'b10 : begin
//                    wdata={32'b0,wdata_proc,64'b0};
//                    wdata_bvld={4'b0,bvld_proc,8'b0};
//                    end
//                    2'b11: begin
//                    wdata={wdata_proc,96'b0};
//                    wdata_bvld={bvld_proc,12'b0};
//                    end
//                    endcase
					
          if (cmd_proc)
	    next_tg_state[TG_WR_CMD] = 1'b1;
          else
	    next_tg_state[TG_RD_CMD] = 1'b1;
        end else
	  next_tg_state[TG_GEN_PRBS] = 1'b1;
      end

      tg_state[TG_WR_CMD]: begin // 8'h04
	if (wdata_sts_vld) 
	    next_tg_state[TG_GEN_PRBS] = 1'b1;
	else if (wdata_rdy)
	  next_tg_state[TG_WR_DATA] = 1'b1;
	else
	  next_tg_state[TG_WR_CMD] = 1'b1;
      end

      tg_state[TG_WR_DATA]: begin // 8'h08
	if (wdata_sts_vld) 
	  next_tg_state[TG_GEN_PRBS] = 1'b1;
	else if (blen_cntr == 8'h0 & wdata_rdy)
	  next_tg_state[TG_WR_DONE] = 1'b1;
	else
	  next_tg_state[TG_WR_DATA] = 1'b1;
      end

      tg_state[TG_WR_DONE]: begin // 8'h10
        if (wdata_sts_vld) 
	    next_tg_state[TG_GEN_PRBS] = 1'b1;
        else
	  next_tg_state[TG_WR_DONE] = 1'b1;
      end
      tg_state[TG_RD_CMD]: begin // 8'h20
        if (rdata_cmptd) 
	    next_tg_state[TG_GEN_PRBS] = 1'b1;
        else if (cmd_ack)
	  next_tg_state[TG_RD_DATA] = 1'b1;
        else
	  next_tg_state[TG_RD_CMD] = 1'b1;
      end
      tg_state[TG_RD_DATA]: begin // 8'h040
        if (rdata_cmptd & rdata_vld & rdata_rdy) 
        next_tg_state[TG_GEN_PRBS] = 1'b1;
        else
	  next_tg_state[TG_RD_DATA] = 1'b1;
      end
    endcase
  end

//*****************************************************************************
// Control Signals
//*****************************************************************************

  always @(posedge clk) begin
    if (!resetn)
      cmd_wr_en <= 1'b0;
    else if (next_tg_state[TG_WR_CMD] & tg_state[TG_GEN_PRBS])
      cmd_wr_en <= 1'b1;
    else 
      cmd_wr_en <= 1'b0;
  end

  always @(posedge clk) begin
    if (!resetn)
      cmd_rd_en <= 1'b0;
    else if (next_tg_state[TG_RD_CMD] & tg_state[TG_GEN_PRBS])
      cmd_rd_en <= 1'b1;
    else 
      cmd_rd_en <= 1'b0;
  end



  always @(posedge clk) begin
    if (tg_state[TG_IDLE])
      wr_proc <= 1'b0;
    else if (cmd_wr_en)
      wr_proc <= 1'b1;
    else if (cmd_rd_en)
      wr_proc <= 1'b0;
  end     


  always @(posedge clk)
    cmd_wr_en_r <= cmd_wr_en;

  assign cmd_cmptd = next_tg_state[TG_GEN_PRBS] & !tg_state[TG_GEN_PRBS];
  assign dgen_en   = wr_proc ? (tg_state[TG_WR_DATA] & wdata_rdy) :
                               (tg_state[TG_RD_DATA] & rdata_vld) ;


  assign rdata_rdy = tg_state[TG_RD_DATA];

  assign rdata_sig_vld = wr_proc ? 1'b0 : (rdata_vld & tg_state[TG_RD_DATA]);

//*****************************************************************************
// Command generation
//*****************************************************************************



  always @(posedge clk) begin
    if (tg_state[TG_IDLE] | (next_tg_state[TG_GEN_PRBS] & !tg_state[TG_GEN_PRBS])) 
      blen_cntr <= 8'h00;
    else if (tg_state[TG_GEN_PRBS] & next_tg_state[TG_GEN_PRBS])
      blen_cntr <= blen_proc;
    else if (tg_state[TG_WR_DATA] & wdata_rdy & (blen_cntr != 8'h00))
      blen_cntr <= blen_cntr - 8'h01;
  end
 
  generate 
    begin: addr_axi_wr
        assign addr1 = addr_proc & 32'hffff_ffff;
    end
  endgenerate
 

//*****************************************************************************
// Control Output Signals
//*****************************************************************************

  always @(posedge clk) begin
    if (!resetn)
      cmd_en <= 1'b0;
    else if (tg_state[TG_WR_CMD] | tg_state[TG_RD_CMD])
      cmd_en <= 1'b1;
    else if (tg_state[TG_WR_DATA] | tg_state[TG_RD_DATA])
      cmd_en <= 1'b0;
  end

  assign cmd = {cmd_gen_csr_sig, 1'b0, wrap_en};


  generate 
    begin: cntrl_sig
      if (C_AXI_NBURST_SUPPORT == 1) begin
      end
      else begin
        if (C_AXI_DATA_WIDTH == 1024) 
          assign ctl[2:0] = 3'b111;
        else if (C_AXI_DATA_WIDTH == 512) 
          assign ctl[2:0] = 3'b110;
        else if (C_AXI_DATA_WIDTH == 256) 
          assign ctl[2:0] = 3'b101;
        else if (C_AXI_DATA_WIDTH == 128) 
          assign ctl[2:0] = 3'b100;
        else if (C_AXI_DATA_WIDTH == 64) 
          assign ctl[2:0] = 3'b011;
        else
          assign ctl[2:0] = 3'b010;
      end
    end
  endgenerate

//*****************************************************************************
// Write Output Signals
//*****************************************************************************

  always @(posedge clk) begin
    if (!resetn)
      wdata_cmptd <= 1'b0;
    else if (tg_state[TG_WR_DONE])
      wdata_cmptd <= 1'b0;
    else if ((tg_state[TG_WR_DATA] & wdata_rdy & blen_cntr == 8'h01) |
             (next_tg_state[TG_WR_DATA] & tg_state[TG_WR_CMD] & blen_cntr == 8'h00))
      wdata_cmptd <= 1'b1;
  end

  assign wdata_vld = tg_state[TG_WR_DATA];


assign msmatch_err=0;

//*****************************************************************************
// Status and Debug Signals
//*****************************************************************************

  always @(posedge clk) begin
    if (!resetn) begin
      cmd_err_dbg <= 1'b0;
      data_msmatch_err_dbg <= 1'b0;
      write_err_dbg <= 1'b0;
      read_err_dbg <= 1'b0;
    end
    else if (tg_state[TG_IDLE] & next_tg_state[TG_GEN_PRBS]) begin
      cmd_err_dbg <= 1'b0;
      data_msmatch_err_dbg <= 1'b0;
      write_err_dbg <= 1'b0;
      read_err_dbg <= 1'b0;
    end
    else begin
      if ((next_tg_state[TG_GEN_PRBS] ) &
          (tg_state[TG_RD_CMD] | tg_state[TG_WR_CMD])) 
        cmd_err_dbg <= 1'b0;
      if (msmatch_err & tg_state[TG_RD_DATA])
        data_msmatch_err_dbg <= 1'b1;
      if ((next_tg_state[TG_GEN_PRBS]) &
          tg_state[TG_WR_DATA]) 
        write_err_dbg <= 1'b1;
      if (rdata_cmptd & rdata_vld & rdata_rdy)    
        read_err_dbg <= (rdata_sts[3:2] == 2'b01);
    end
  end

  always @(posedge clk) begin
    if (!resetn) begin
      cmd_err <= 1'b0;
      data_msmatch_err <= 1'b0;
      write_err <= 1'b0;
      read_err <= 1'b0;
    end
    else begin
      if (cmd_err_dbg)
        cmd_err <= 1'b1;
      if (data_msmatch_err_dbg)  
        data_msmatch_err <= 1'b1;
      if (write_err_dbg) 
        write_err <= 1'b1;
      if (read_err_dbg)
        read_err <= 1'b1;
    end
  end

  always @(posedge clk) begin
    if (!resetn) begin
      dbg_wr_sts_vld <= 1'b0;
      dbg_rd_sts_vld <= 1'b0;
    end
    else if (tg_state[TG_GEN_PRBS]) begin
      dbg_wr_sts_vld <= 1'b0;
      dbg_rd_sts_vld <= 1'b0;
    end
    else begin
      if (wdata_sts_vld)
        dbg_wr_sts_vld <= 1'b1;
      if (rdata_cmptd & rdata_vld & rdata_rdy)
        dbg_rd_sts_vld <= 1'b1;
    end
  end 
  
  always @(posedge clk) begin
    if (tg_state[TG_GEN_PRBS] | tg_state[TG_IDLE]) begin
      wdata_sts_r <= {WR_STS_WIDTH{1'b0}};
      rdata_sts_r <= {RD_STS_WIDTH{1'b0}};
    end
    else begin
      if (wdata_sts_vld)
        wdata_sts_r <= wdata_sts;
      if (rdata_cmptd & rdata_vld & rdata_rdy)
        rdata_sts_r <= rdata_sts;
    end
  end      
  

  always @(posedge clk)
  begin
  if(tg_state[TG_GEN_PRBS] | tg_state[TG_IDLE] | !resetn)
  wrd_cntr=0;
  else if(rdata_sig_vld)
  wrd_cntr=wrd_cntr+1;
  end

  assign dbg_wr_sts = {11'h0, write_err_dbg, cmd_err_dbg, 3'b000, wdata_sts_r};
  assign dbg_rd_sts = {2'b00, data_msmatch_err_dbg, read_err_dbg, cmd_err_dbg, 3'b000, wrd_cntr, rdata_sts_r}; 
  assign test_cmptd = 1'b0;
  assign write_cmptd = (tg_state[TG_WR_DATA] | tg_state[TG_WR_DONE]) & 
                       (next_tg_state[TG_GEN_PRBS]);
  assign read_cmptd = tg_state[TG_RD_DATA] & (next_tg_state[TG_GEN_PRBS] );
  assign cmp_data_en = dgen_en & tg_state[TG_RD_DATA];
  assign rdata_cmp  = rdata;

// synthesis translate_off
//*****************************************************************************
// Simulation debug signals and messages
//*****************************************************************************
 
  
  always @(*) begin
    if (test_cmptd) begin
      $display ("[INFO] : All tests have been completed");
      if (cmd_err)
        $display ("[ERROR] Command error has occured");
      if (data_msmatch_err)
        $display ("[ERROR] Data mismatch error occured");
      if (write_err)
        $display ("[ERROR] Timeout occured during write transaction");
      if (read_err)
        $display ("[ERROR] Timeout occured during read transaction");
      if (!cmd_err & !data_msmatch_err & !write_err & !read_err)
        $display ("[INFO] : Tests PASSED");
      $finish;
    end 
  end  

// synthesis translate_on

endmodule
