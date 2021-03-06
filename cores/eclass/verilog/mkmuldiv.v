//
// Generated by Bluespec Compiler, version 2015.09.beta2 (build 34689, 2015-09-07)
//
// On Fri Dec 23 14:57:23 IST 2016
//
//
// Ports:
// Name                         I/O  size props
// RDY__start                     O     1
// result_                        O    33 reg
// RDY_result_                    O     1 const
// RDY__reset                     O     1 const
// CLK                            I     1 clock
// RST_N                          I     1 reset
// _start_inp1                    I    32
// _start_inp2                    I    32
// _start__div_type               I     1
// _start__div_or_rem             I     1
// _start_mul_or_div              I     1
// EN__start                      I     1
// EN__reset                      I     1
//
// No combinational paths from inputs to outputs
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkmuldiv(CLK,
		RST_N,

		_start_inp1,
		_start_inp2,
		_start__div_type,
		_start__div_or_rem,
		_start_mul_or_div,
		EN__start,
		RDY__start,

		result_,
		RDY_result_,

		EN__reset,
		RDY__reset);
  input  CLK;
  input  RST_N;

  // action method _start
  input  [31 : 0] _start_inp1;
  input  [31 : 0] _start_inp2;
  input  _start__div_type;
  input  _start__div_or_rem;
  input  _start_mul_or_div;
  input  EN__start;
  output RDY__start;

  // value method result_
  output [32 : 0] result_;
  output RDY_result_;

  // action method _reset
  input  EN__reset;
  output RDY__reset;

  // signals for module outputs
  wire [32 : 0] result_;
  wire RDY__reset, RDY__start, RDY_result_;

  // inlined wires
  reg [31 : 0] rg_state_counter_1$wget;
  wire [32 : 0] wr_final_result_1$wget;
  wire rg_state_counter_1$whas, wr_final_result_1$whas;

  // register div_or_rem
  reg div_or_rem;
  wire div_or_rem$D_IN, div_or_rem$EN;

  // register div_type
  reg div_type;
  wire div_type$D_IN, div_type$EN;

  // register partial
  reg [34 : 0] partial;
  wire [34 : 0] partial$D_IN;
  wire partial$EN;

  // register partial_prod
  reg [64 : 0] partial_prod;
  wire [64 : 0] partial_prod$D_IN;
  wire partial_prod$EN;

  // register rg_inp1
  reg [33 : 0] rg_inp1;
  wire [33 : 0] rg_inp1$D_IN;
  wire rg_inp1$EN;

  // register rg_inp1_sign
  reg rg_inp1_sign;
  wire rg_inp1_sign$D_IN, rg_inp1_sign$EN;

  // register rg_inp2
  reg [33 : 0] rg_inp2;
  wire [33 : 0] rg_inp2$D_IN;
  wire rg_inp2$EN;

  // register rg_inp2_sign
  reg rg_inp2_sign;
  wire rg_inp2_sign$D_IN, rg_inp2_sign$EN;

  // register rg_mul_or_div
  reg rg_mul_or_div;
  wire rg_mul_or_div$D_IN, rg_mul_or_div$EN;

  // register rg_state_counter
  reg [31 : 0] rg_state_counter;
  wire [31 : 0] rg_state_counter$D_IN;
  wire rg_state_counter$EN;

  // register rg_take_complement
  reg rg_take_complement;
  wire rg_take_complement$D_IN, rg_take_complement$EN;

  // register wr_final_result
  reg [32 : 0] wr_final_result;
  wire [32 : 0] wr_final_result$D_IN;
  wire wr_final_result$EN;

  // rule scheduling signals
  wire CAN_FIRE_RL_division,
       CAN_FIRE_RL_multiply,
       CAN_FIRE_RL_rg_state_counter__dreg_update,
       CAN_FIRE_RL_wr_final_result__dreg_update,
       CAN_FIRE__reset,
       CAN_FIRE__start,
       WILL_FIRE_RL_division,
       WILL_FIRE_RL_multiply,
       WILL_FIRE_RL_rg_state_counter__dreg_update,
       WILL_FIRE_RL_wr_final_result__dreg_update,
       WILL_FIRE__reset,
       WILL_FIRE__start;

  // inputs to muxes for submodule ports
  wire [64 : 0] MUX_partial_prod$write_1__VAL_1;
  wire [34 : 0] MUX_partial$write_1__VAL_2;
  wire [33 : 0] MUX_rg_inp1$write_1__VAL_2;
  wire [32 : 0] MUX_wr_final_result_1$wset_1__VAL_1,
		MUX_wr_final_result_1$wset_1__VAL_2;
  wire [31 : 0] MUX_rg_state_counter_1$wset_1__VAL_1,
		MUX_rg_state_counter_1$wset_1__VAL_2;
  wire MUX_partial$write_1__SEL_1,
       MUX_partial_prod$write_1__SEL_1,
       MUX_rg_inp1$write_1__SEL_1,
       MUX_wr_final_result_1$wset_1__SEL_1;

  // remaining internal signals
  reg [64 : 0] v__h3429;
  reg [63 : 0] v__h3205, v__h3322, v__h6519;
  wire [64 : 0] temp___1__h3470,
		temp___1__h3615,
		temp___1__h3762,
		temp__h3953,
		x__h4172;
  wire [63 : 0] IF_IF_div_type_9_AND_NOT_div_or_rem_8_21_71_TH_ETC__q1;
  wire [34 : 0] IF_IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2__ETC___d253,
		IF_IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2__ETC___d68,
		IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CON_ETC___d246,
		IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CON_ETC___d62,
		IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_ETC___d238,
		IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_ETC___d56,
		IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0__ETC___d231,
		IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_ETC___d50,
		IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98__ETC___d223,
		IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_in_ETC___d44,
		IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98_PLU_ETC___d216,
		IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_inp1__ETC___d38,
		IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98_PLUS_S_ETC___d208,
		IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_inp1_3_B_ETC___d32,
		IF_y003_BIT_34_THEN_IF_IF_IF_IF_partial_1_BITS_ETC__q6,
		IF_y056_BIT_34_THEN_IF_IF_IF_partial_1_BITS_33_ETC__q5,
		IF_y109_BIT_34_THEN_IF_IF_partial_1_BITS_33_TO_ETC__q4,
		IF_y162_BIT_34_THEN_IF_partial_1_BITS_33_TO_0__ETC__q3,
		IF_y215_BIT_34_THEN_partial_BITS_33_TO_0_CONCA_ETC__q2,
		IF_y897_BIT_34_THEN_IF_IF_IF_IF_IF_IF_partial__ETC__q8,
		IF_y950_BIT_34_THEN_IF_IF_IF_IF_IF_partial_1_B_ETC__q7,
		SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29,
		SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202,
		_theResult___fst__h1034,
		x__h4725,
		y__h1117,
		y__h1170,
		y__h1223,
		y__h1276,
		y__h1329,
		y__h1382,
		y__h1435,
		y__h1488,
		y__h4844,
		y__h4897,
		y__h4950,
		y__h5003,
		y__h5056,
		y__h5109,
		y__h5162,
		y__h5215;
  wire [33 : 0] _theResult___snd__h1035, x__h1589, x__h5318, x__h6471;
  wire [32 : 0] spliced_bits__h3635, spliced_bits__h3782, spliced_bits__h3809;
  wire [31 : 0] IF_NOT_div_or_rem_8_21_AND_NOT_div_type_9_0_69_ETC___d183,
		IF_NOT_div_type_9_0_AND_NOT_IF_IF_IF_IF_IF_IF__ETC___d89,
		IF_div_or_rem_8_THEN_IF_NOT_div_type_9_0_AND_N_ETC___d119,
		IF_start__div_type_AND_NOT_start__div_or_rem_5_ETC___d263,
		IF_start__div_type_AND_NOT_start__div_or_rem_5_ETC___d276,
		IF_start_inp1_BIT_31_85_THEN_INV_start_inp1_93_ETC___d195,
		inp1___1__h4622,
		inp2___1__h4623,
		rg_state_counter_4_PLUS_1___d76,
		temp__h6727,
		x__h2835,
		x__h3093,
		x__h6861,
		x__h6862,
		y__h2734;
  wire IF_div_type_9_AND_NOT_div_or_rem_8_21_71_THEN__ETC___d176,
       lv_take_complement___1__h4134;

  // action method _start
  assign RDY__start = rg_state_counter == 32'd0 ;
  assign CAN_FIRE__start = rg_state_counter == 32'd0 ;
  assign WILL_FIRE__start = EN__start ;

  // value method result_
  assign result_ = wr_final_result ;
  assign RDY_result_ = 1'd1 ;

  // action method _reset
  assign RDY__reset = 1'd1 ;
  assign CAN_FIRE__reset = 1'd1 ;
  assign WILL_FIRE__reset = EN__reset ;

  // rule RL_division
  assign CAN_FIRE_RL_division =
	     rg_mul_or_div &&
	     (rg_state_counter ^ 32'h80000000) <= 32'h80000003 &&
	     rg_state_counter != 32'd0 ;
  assign WILL_FIRE_RL_division = CAN_FIRE_RL_division ;

  // rule RL_multiply
  assign CAN_FIRE_RL_multiply =
	     !rg_mul_or_div &&
	     (rg_state_counter ^ 32'h80000000) <= 32'h80000020 &&
	     rg_state_counter != 32'd0 ;
  assign WILL_FIRE_RL_multiply = CAN_FIRE_RL_multiply ;

  // rule RL_wr_final_result__dreg_update
  assign CAN_FIRE_RL_wr_final_result__dreg_update = 1'd1 ;
  assign WILL_FIRE_RL_wr_final_result__dreg_update = 1'd1 ;

  // rule RL_rg_state_counter__dreg_update
  assign CAN_FIRE_RL_rg_state_counter__dreg_update = 1'd1 ;
  assign WILL_FIRE_RL_rg_state_counter__dreg_update = 1'd1 ;

  // inputs to muxes for submodule ports
  assign MUX_partial$write_1__SEL_1 = EN__start && _start_mul_or_div ;
  assign MUX_partial_prod$write_1__SEL_1 = EN__start && !_start_mul_or_div ;
  assign MUX_rg_inp1$write_1__SEL_1 =
	     WILL_FIRE_RL_division && rg_state_counter != 32'd3 ;
  assign MUX_wr_final_result_1$wset_1__SEL_1 =
	     WILL_FIRE_RL_division && rg_state_counter == 32'd3 ;
  assign MUX_partial$write_1__VAL_2 =
	     (rg_state_counter == 32'd3) ? 35'd0 : _theResult___fst__h1034 ;
  assign MUX_partial_prod$write_1__VAL_1 =
	     { 33'd0,
	       IF_start__div_type_AND_NOT_start__div_or_rem_5_ETC___d276 } ;
  assign MUX_rg_inp1$write_1__VAL_2 =
	     _start_mul_or_div ?
	       x__h6471 :
	       { 2'd0,
		 IF_start__div_type_AND_NOT_start__div_or_rem_5_ETC___d263 } ;
  assign MUX_rg_state_counter_1$wset_1__VAL_1 =
	     (rg_state_counter == 32'd16) ?
	       32'd0 :
	       rg_state_counter_4_PLUS_1___d76 ;
  assign MUX_rg_state_counter_1$wset_1__VAL_2 =
	     (rg_state_counter == 32'd3) ?
	       32'd0 :
	       rg_state_counter_4_PLUS_1___d76 ;
  assign MUX_wr_final_result_1$wset_1__VAL_1 =
	     { 1'd1,
	       IF_div_or_rem_8_THEN_IF_NOT_div_type_9_0_AND_N_ETC___d119 } ;
  assign MUX_wr_final_result_1$wset_1__VAL_2 =
	     { 1'd1,
	       IF_NOT_div_or_rem_8_21_AND_NOT_div_type_9_0_69_ETC___d183 } ;

  // inlined wires
  assign wr_final_result_1$wget =
	     MUX_wr_final_result_1$wset_1__SEL_1 ?
	       MUX_wr_final_result_1$wset_1__VAL_1 :
	       MUX_wr_final_result_1$wset_1__VAL_2 ;
  assign wr_final_result_1$whas =
	     WILL_FIRE_RL_division && rg_state_counter == 32'd3 ||
	     WILL_FIRE_RL_multiply && rg_state_counter == 32'd16 ;
  always@(WILL_FIRE_RL_multiply or
	  MUX_rg_state_counter_1$wset_1__VAL_1 or
	  WILL_FIRE_RL_division or
	  MUX_rg_state_counter_1$wset_1__VAL_2 or
	  EN__reset or EN__start or rg_state_counter_4_PLUS_1___d76)
  case (1'b1)
    WILL_FIRE_RL_multiply:
	rg_state_counter_1$wget = MUX_rg_state_counter_1$wset_1__VAL_1;
    WILL_FIRE_RL_division:
	rg_state_counter_1$wget = MUX_rg_state_counter_1$wset_1__VAL_2;
    EN__reset: rg_state_counter_1$wget = 32'd0;
    EN__start: rg_state_counter_1$wget = rg_state_counter_4_PLUS_1___d76;
    default: rg_state_counter_1$wget = 32'hAAAAAAAA /* unspecified value */ ;
  endcase
  assign rg_state_counter_1$whas =
	     EN__start || WILL_FIRE_RL_division || WILL_FIRE_RL_multiply ||
	     EN__reset ;

  // register div_or_rem
  assign div_or_rem$D_IN = _start__div_or_rem ;
  assign div_or_rem$EN = EN__start ;

  // register div_type
  assign div_type$D_IN = _start__div_type ;
  assign div_type$EN = EN__start ;

  // register partial
  assign partial$D_IN =
	     MUX_partial$write_1__SEL_1 ?
	       x__h4725 :
	       MUX_partial$write_1__VAL_2 ;
  assign partial$EN =
	     EN__start && _start_mul_or_div || WILL_FIRE_RL_division ;

  // register partial_prod
  assign partial_prod$D_IN =
	     MUX_partial_prod$write_1__SEL_1 ?
	       MUX_partial_prod$write_1__VAL_1 :
	       v__h3429 ;
  assign partial_prod$EN =
	     EN__start && !_start_mul_or_div || WILL_FIRE_RL_multiply ;

  // register rg_inp1
  assign rg_inp1$D_IN =
	     MUX_rg_inp1$write_1__SEL_1 ?
	       _theResult___snd__h1035 :
	       MUX_rg_inp1$write_1__VAL_2 ;
  assign rg_inp1$EN =
	     WILL_FIRE_RL_division && rg_state_counter != 32'd3 || EN__start ;

  // register rg_inp1_sign
  assign rg_inp1_sign$D_IN = _start_inp1[31] ;
  assign rg_inp1_sign$EN = EN__start ;

  // register rg_inp2
  assign rg_inp2$D_IN =
	     { 2'd0, _start_mul_or_div ? inp2___1__h4623 : _start_inp2 } ;
  assign rg_inp2$EN = EN__start ;

  // register rg_inp2_sign
  assign rg_inp2_sign$D_IN = _start_inp2[31] ;
  assign rg_inp2_sign$EN = EN__start ;

  // register rg_mul_or_div
  assign rg_mul_or_div$D_IN = _start_mul_or_div ;
  assign rg_mul_or_div$EN = EN__start ;

  // register rg_state_counter
  assign rg_state_counter$D_IN =
	     rg_state_counter_1$whas ? rg_state_counter_1$wget : 32'd0 ;
  assign rg_state_counter$EN = 1'd1 ;

  // register rg_take_complement
  assign rg_take_complement$D_IN =
	     !_start__div_type && !_start__div_or_rem &&
	     _start_inp1[31] ^ _start_inp2[31] &&
	     _start_inp2 != 32'd0 ;
  assign rg_take_complement$EN = MUX_partial$write_1__SEL_1 ;

  // register wr_final_result
  assign wr_final_result$D_IN =
	     { wr_final_result_1$whas && wr_final_result_1$wget[32],
	       wr_final_result_1$wget[31:0] } ;
  assign wr_final_result$EN = 1'd1 ;

  // remaining internal signals
  assign IF_IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2__ETC___d253 =
	     { IF_y897_BIT_34_THEN_IF_IF_IF_IF_IF_IF_partial__ETC__q8[33:0],
	       inp1___1__h4622[26] } ;
  assign IF_IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2__ETC___d68 =
	     y__h1170[34] ?
	       { IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CON_ETC___d62[33:0],
		 rg_inp1[27] } :
	       y__h1170 ;
  assign IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CON_ETC___d246 =
	     { IF_y950_BIT_34_THEN_IF_IF_IF_IF_IF_partial_1_B_ETC__q7[33:0],
	       inp1___1__h4622[27] } ;
  assign IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CON_ETC___d62 =
	     y__h1223[34] ?
	       { IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_ETC___d56[33:0],
		 rg_inp1[28] } :
	       y__h1223 ;
  assign IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_ETC___d238 =
	     { IF_y003_BIT_34_THEN_IF_IF_IF_IF_partial_1_BITS_ETC__q6[33:0],
	       inp1___1__h4622[28] } ;
  assign IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_ETC___d56 =
	     y__h1276[34] ?
	       { IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_ETC___d50[33:0],
		 rg_inp1[29] } :
	       y__h1276 ;
  assign IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0__ETC___d231 =
	     { IF_y056_BIT_34_THEN_IF_IF_IF_partial_1_BITS_33_ETC__q5[33:0],
	       inp1___1__h4622[29] } ;
  assign IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_ETC___d50 =
	     y__h1329[34] ?
	       { IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_in_ETC___d44[33:0],
		 rg_inp1[30] } :
	       y__h1329 ;
  assign IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98__ETC___d223 =
	     { IF_y109_BIT_34_THEN_IF_IF_partial_1_BITS_33_TO_ETC__q4[33:0],
	       inp1___1__h4622[30] } ;
  assign IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_in_ETC___d44 =
	     y__h1382[34] ?
	       { IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_inp1__ETC___d38[33:0],
		 rg_inp1[31] } :
	       y__h1382 ;
  assign IF_IF_div_type_9_AND_NOT_div_or_rem_8_21_71_TH_ETC__q1 =
	     IF_div_type_9_AND_NOT_div_or_rem_8_21_71_THEN__ETC___d176 ?
	       x__h4172[63:0] :
	       v__h3429[63:0] ;
  assign IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98_PLU_ETC___d216 =
	     { IF_y162_BIT_34_THEN_IF_partial_1_BITS_33_TO_0__ETC__q3[33:0],
	       inp1___1__h4622[31] } ;
  assign IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_inp1__ETC___d38 =
	     y__h1435[34] ?
	       { IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_inp1_3_B_ETC___d32[33:0],
		 rg_inp1[32] } :
	       y__h1435 ;
  assign IF_NOT_div_or_rem_8_21_AND_NOT_div_type_9_0_69_ETC___d183 =
	     (!div_or_rem && !div_type) ?
	       v__h3429[31:0] :
	       IF_IF_div_type_9_AND_NOT_div_or_rem_8_21_71_TH_ETC__q1[63:32] ;
  assign IF_NOT_div_type_9_0_AND_NOT_IF_IF_IF_IF_IF_IF__ETC___d89 =
	     (!div_type && _theResult___fst__h1034[31] != rg_inp1_sign) ?
	       x__h3093 + 32'd1 :
	       _theResult___fst__h1034[31:0] ;
  assign IF_div_or_rem_8_THEN_IF_NOT_div_type_9_0_AND_N_ETC___d119 =
	     div_or_rem ?
	       IF_NOT_div_type_9_0_AND_NOT_IF_IF_IF_IF_IF_IF__ETC___d89 :
	       (rg_take_complement ? x__h2835 + 32'd1 : y__h2734) ;
  assign IF_div_type_9_AND_NOT_div_or_rem_8_21_71_THEN__ETC___d176 =
	     (div_type && !div_or_rem) ?
	       lv_take_complement___1__h4134 :
	       !div_type && div_or_rem && rg_inp1_sign ;
  assign IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98_PLUS_S_ETC___d208 =
	     { IF_y215_BIT_34_THEN_partial_BITS_33_TO_0_CONCA_ETC__q2[33:0],
	       1'd0 } ;
  assign IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_inp1_3_B_ETC___d32 =
	     y__h1488[34] ? { partial[33:0], rg_inp1[33] } : y__h1488 ;
  assign IF_start__div_type_AND_NOT_start__div_or_rem_5_ETC___d263 =
	     (_start__div_type && !_start__div_or_rem ||
	      !_start__div_type && _start__div_or_rem) ?
	       IF_start_inp1_BIT_31_85_THEN_INV_start_inp1_93_ETC___d195 :
	       _start_inp1 ;
  assign IF_start__div_type_AND_NOT_start__div_or_rem_5_ETC___d276 =
	     (_start__div_type && !_start__div_or_rem) ?
	       temp__h6727 :
	       _start_inp2 ;
  assign IF_start_inp1_BIT_31_85_THEN_INV_start_inp1_93_ETC___d195 =
	     _start_inp1[31] ? x__h6861 : _start_inp1 ;
  assign IF_y003_BIT_34_THEN_IF_IF_IF_IF_partial_1_BITS_ETC__q6 =
	     y__h5003[34] ?
	       IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0__ETC___d231 :
	       y__h5003 ;
  assign IF_y056_BIT_34_THEN_IF_IF_IF_partial_1_BITS_33_ETC__q5 =
	     y__h5056[34] ?
	       IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98__ETC___d223 :
	       y__h5056 ;
  assign IF_y109_BIT_34_THEN_IF_IF_partial_1_BITS_33_TO_ETC__q4 =
	     y__h5109[34] ?
	       IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98_PLU_ETC___d216 :
	       y__h5109 ;
  assign IF_y162_BIT_34_THEN_IF_partial_1_BITS_33_TO_0__ETC__q3 =
	     y__h5162[34] ?
	       IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98_PLUS_S_ETC___d208 :
	       y__h5162 ;
  assign IF_y215_BIT_34_THEN_partial_BITS_33_TO_0_CONCA_ETC__q2 =
	     y__h5215[34] ? { partial[33:0], 1'd0 } : y__h5215 ;
  assign IF_y897_BIT_34_THEN_IF_IF_IF_IF_IF_IF_partial__ETC__q8 =
	     y__h4897[34] ?
	       IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CON_ETC___d246 :
	       y__h4897 ;
  assign IF_y950_BIT_34_THEN_IF_IF_IF_IF_IF_partial_1_B_ETC__q7 =
	     y__h4950[34] ?
	       IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_ETC___d238 :
	       y__h4950 ;
  assign SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 = { x__h1589[33], x__h1589 } ;
  assign SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 =
	     { x__h5318[33], x__h5318 } ;
  assign _theResult___fst__h1034 =
	     y__h1117[34] ?
	       { IF_IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2__ETC___d68[33:0],
		 rg_inp1[26] } :
	       y__h1117 ;
  assign _theResult___snd__h1035 =
	     { rg_inp1[25:0],
	       !y__h1488[34],
	       !y__h1435[34],
	       !y__h1382[34],
	       !y__h1329[34],
	       !y__h1276[34],
	       !y__h1223[34],
	       !y__h1170[34],
	       !y__h1117[34] } ;
  assign inp1___1__h4622 =
	     _start__div_type ?
	       _start_inp1 :
	       IF_start_inp1_BIT_31_85_THEN_INV_start_inp1_93_ETC___d195 ;
  assign inp2___1__h4623 = _start__div_type ? _start_inp2 : temp__h6727 ;
  assign lv_take_complement___1__h4134 = rg_inp1_sign ^ rg_inp2_sign ;
  assign rg_state_counter_4_PLUS_1___d76 = rg_state_counter + 32'd1 ;
  assign spliced_bits__h3635 = { 1'd0, partial_prod[64:33] } + rg_inp1[32:0] ;
  assign spliced_bits__h3782 =
	     { 1'd0, spliced_bits__h3809[32:1] } + rg_inp1[32:0] ;
  assign spliced_bits__h3809 = { 1'd0, partial_prod[63:32] } + rg_inp1[32:0] ;
  assign temp___1__h3470 = { 2'd0, spliced_bits__h3809, partial_prod[31:2] } ;
  assign temp___1__h3615 = { 1'd0, spliced_bits__h3635, partial_prod[32:2] } ;
  assign temp___1__h3762 =
	     { 1'd0,
	       spliced_bits__h3782,
	       spliced_bits__h3809[0],
	       partial_prod[31:2] } ;
  assign temp__h3953 = { 2'd0, partial_prod[64:2] } ;
  assign temp__h6727 = _start_inp2[31] ? ~_start_inp2 + 32'd1 : _start_inp2 ;
  assign x__h1589 = ~rg_inp2 + 34'd1 ;
  assign x__h2835 =
	     { ~rg_inp1[23:0],
	       ~(!y__h1488[34]),
	       ~(!y__h1435[34]),
	       ~(!y__h1382[34]),
	       ~(!y__h1329[34]),
	       ~(!y__h1276[34]),
	       ~(!y__h1223[34]),
	       ~(!y__h1170[34]),
	       ~(!y__h1117[34]) } ;
  assign x__h3093 = ~_theResult___fst__h1034[31:0] ;
  assign x__h4172 = ~v__h3429 + 65'd1 ;
  assign x__h4725 =
	     y__h4844[34] ?
	       IF_IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2__ETC___d253 :
	       y__h4844 ;
  assign x__h5318 = { 2'd3, ~inp2___1__h4623 } + 34'd1 ;
  assign x__h6471 =
	     { inp1___1__h4622[25:0],
	       !y__h5215[34],
	       !y__h5162[34],
	       !y__h5109[34],
	       !y__h5056[34],
	       !y__h5003[34],
	       !y__h4950[34],
	       !y__h4897[34],
	       !y__h4844[34] } ;
  assign x__h6861 = x__h6862 + 32'd1 ;
  assign x__h6862 = ~_start_inp1 ;
  assign y__h1117 =
	     { IF_IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2__ETC___d68[33:0],
	       rg_inp1[26] } +
	     SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 ;
  assign y__h1170 =
	     { IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CON_ETC___d62[33:0],
	       rg_inp1[27] } +
	     SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 ;
  assign y__h1223 =
	     { IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_ETC___d56[33:0],
	       rg_inp1[28] } +
	     SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 ;
  assign y__h1276 =
	     { IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_ETC___d50[33:0],
	       rg_inp1[29] } +
	     SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 ;
  assign y__h1329 =
	     { IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_in_ETC___d44[33:0],
	       rg_inp1[30] } +
	     SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 ;
  assign y__h1382 =
	     { IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_inp1__ETC___d38[33:0],
	       rg_inp1[31] } +
	     SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 ;
  assign y__h1435 =
	     { IF_partial_1_BITS_33_TO_0_2_CONCAT_rg_inp1_3_B_ETC___d32[33:0],
	       rg_inp1[32] } +
	     SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 ;
  assign y__h1488 =
	     { partial[33:0], rg_inp1[33] } +
	     SEXT_INV_rg_inp2_6_7_PLUS_1_8___d29 ;
  assign y__h2734 =
	     { rg_inp1[23:0],
	       !y__h1488[34],
	       !y__h1435[34],
	       !y__h1382[34],
	       !y__h1329[34],
	       !y__h1276[34],
	       !y__h1223[34],
	       !y__h1170[34],
	       !y__h1117[34] } ;
  assign y__h4844 =
	     IF_IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2__ETC___d253 +
	     SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 ;
  assign y__h4897 =
	     IF_IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CON_ETC___d246 +
	     SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 ;
  assign y__h4950 =
	     IF_IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_ETC___d238 +
	     SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 ;
  assign y__h5003 =
	     IF_IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0__ETC___d231 +
	     SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 ;
  assign y__h5056 =
	     IF_IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98__ETC___d223 +
	     SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 ;
  assign y__h5109 =
	     IF_IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98_PLU_ETC___d216 +
	     SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 ;
  assign y__h5162 =
	     IF_partial_1_BITS_33_TO_0_2_CONCAT_0_98_PLUS_S_ETC___d208 +
	     SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 ;
  assign y__h5215 =
	     { partial[33:0], 1'd0 } +
	     SEXT__3_CONCAT_INV_IF_start__div_type_THEN_sta_ETC___d202 ;
  always@(partial_prod or
	  temp__h3953 or
	  temp___1__h3470 or temp___1__h3615 or temp___1__h3762)
  begin
    case (partial_prod[1:0])
      2'd0: v__h3429 = temp__h3953;
      2'b01: v__h3429 = temp___1__h3470;
      2'b10: v__h3429 = temp___1__h3615;
      2'b11: v__h3429 = temp___1__h3762;
    endcase
  end

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        div_or_rem <= `BSV_ASSIGNMENT_DELAY 1'd0;
	div_type <= `BSV_ASSIGNMENT_DELAY 1'd0;
	partial <= `BSV_ASSIGNMENT_DELAY 35'd0;
	partial_prod <= `BSV_ASSIGNMENT_DELAY 65'd0;
	rg_inp1 <= `BSV_ASSIGNMENT_DELAY 34'd0;
	rg_inp1_sign <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_inp2 <= `BSV_ASSIGNMENT_DELAY 34'd0;
	rg_inp2_sign <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_mul_or_div <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_state_counter <= `BSV_ASSIGNMENT_DELAY 32'd0;
	rg_take_complement <= `BSV_ASSIGNMENT_DELAY 1'd0;
	wr_final_result <= `BSV_ASSIGNMENT_DELAY 33'h0AAAAAAAA;
      end
    else
      begin
        if (div_or_rem$EN)
	  div_or_rem <= `BSV_ASSIGNMENT_DELAY div_or_rem$D_IN;
	if (div_type$EN) div_type <= `BSV_ASSIGNMENT_DELAY div_type$D_IN;
	if (partial$EN) partial <= `BSV_ASSIGNMENT_DELAY partial$D_IN;
	if (partial_prod$EN)
	  partial_prod <= `BSV_ASSIGNMENT_DELAY partial_prod$D_IN;
	if (rg_inp1$EN) rg_inp1 <= `BSV_ASSIGNMENT_DELAY rg_inp1$D_IN;
	if (rg_inp1_sign$EN)
	  rg_inp1_sign <= `BSV_ASSIGNMENT_DELAY rg_inp1_sign$D_IN;
	if (rg_inp2$EN) rg_inp2 <= `BSV_ASSIGNMENT_DELAY rg_inp2$D_IN;
	if (rg_inp2_sign$EN)
	  rg_inp2_sign <= `BSV_ASSIGNMENT_DELAY rg_inp2_sign$D_IN;
	if (rg_mul_or_div$EN)
	  rg_mul_or_div <= `BSV_ASSIGNMENT_DELAY rg_mul_or_div$D_IN;
	if (rg_state_counter$EN)
	  rg_state_counter <= `BSV_ASSIGNMENT_DELAY rg_state_counter$D_IN;
	if (rg_take_complement$EN)
	  rg_take_complement <= `BSV_ASSIGNMENT_DELAY rg_take_complement$D_IN;
	if (wr_final_result$EN)
	  wr_final_result <= `BSV_ASSIGNMENT_DELAY wr_final_result$D_IN;
      end
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    div_or_rem = 1'h0;
    div_type = 1'h0;
    partial = 35'h2AAAAAAAA;
    partial_prod = 65'h0AAAAAAAAAAAAAAAA;
    rg_inp1 = 34'h2AAAAAAAA;
    rg_inp1_sign = 1'h0;
    rg_inp2 = 34'h2AAAAAAAA;
    rg_inp2_sign = 1'h0;
    rg_mul_or_div = 1'h0;
    rg_state_counter = 32'hAAAAAAAA;
    rg_take_complement = 1'h0;
    wr_final_result = 33'h0AAAAAAAA;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on

  // handling of system tasks

  // synopsys translate_off
  always@(negedge CLK)
  begin
    #0;
    if (RST_N != `BSV_RESET_VALUE)
      if (EN__start && _start_mul_or_div)
	$display("Division selected: Op1: %h, Op2: %h",
		 inp1___1__h4622,
		 inp2___1__h4623);
    if (RST_N != `BSV_RESET_VALUE)
      if (EN__start && _start_mul_or_div)
	begin
	  v__h6519 = $time;
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (EN__start && _start_mul_or_div)
	$display(v__h6519,
		 "\tRemainder:%b Quotient: %b",
		 x__h4725[33:0],
		 x__h6471);
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_division && rg_state_counter == 32'd3 && !div_or_rem &&
	  rg_take_complement)
	$display("Taking Complement");
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_division && rg_state_counter == 32'd3 && !div_or_rem &&
	  !rg_take_complement)
	$display("NOT Taking Complement");
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_division && rg_state_counter == 32'd3)
	begin
	  v__h3205 = $time;
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_division && rg_state_counter == 32'd3)
	$display(v__h3205,
		 "\tRemainder:%b Quotient: %b",
		 _theResult___fst__h1034[33:0],
		 _theResult___snd__h1035);
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_division && rg_state_counter != 32'd3)
	begin
	  v__h3322 = $time;
	  #0;
	end
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_division && rg_state_counter != 32'd3)
	$display(v__h3322,
		 "\tRemainder:%b Quotient: %b",
		 _theResult___fst__h1034[33:0],
		 _theResult___snd__h1035);
  end
  // synopsys translate_on
endmodule  // mkmuldiv

