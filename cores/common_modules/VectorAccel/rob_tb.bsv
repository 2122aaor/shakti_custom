import Vector::*;

import ROB::*;
import VectorAccelDefs::*;

`include "defined_parameters.bsv"

module tb (Empty);
	
	// ROBIfc#(8, 5, 32) rob <- mkROB;
	ROBIfc#(`ROB_BUFFER_SIZE, `VS_AddrWidth, 32) rob <- mkROB;

	`ifdef Scalar_WriteBack_Priority
		Vector#(16, Reg#(Bit#(TLog#(`ROB_BUFFER_SIZE)))) addr <- replicateM(mkReg(0));
	`else
		Vector#(16, Reg#(Bit#(TAdd#(TLog#(`ROB_BUFFER_SIZE),1)))) addr <- replicateM(mkReg(0));
	`endif

	Bit#(10) s_start	= 'd0;
	Bit#(10) s_end		= 'd15;
	Reg#(Bit#(10)) s	<- mkReg(s_start);
	
	rule disp ;
		$display("\n\n\n***********************************  s = %d  ***********************************\n",s);
	endrule: disp
	
	rule count_s ;
		s <= s+1;
	endrule: count_s
	
	rule test0 (s==0);
		$display("test0: commit");
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test0

	rule test1 (s==1);
		$display("test1: reserve");
		let yy <- rob.reserve(12);
		RdToken y = unpack(truncate(pack(yy)));
		addr[1] <= pack(y);
		$display("reserve: rob%d for r12",y);
	endrule: test1

	rule test2 (s==2);
		$display("test2: reserve store_data");
		let yy <- rob.reserve(15);
		RdToken y = truncate(unpack(pack(yy)));
		addr[2] <= pack(y);
		$display("reserve: rob%d for r15",y);
	endrule: test2

	rule test2_2 (s==2);
		rob.store_data(truncate(addr[1]), 'hdead0001);
	endrule: test2_2

	rule test3 (s==3);
		$display("test3: store_data");
		rob.store_data(truncate(addr[2])+1, 'hdeaddead);
	endrule: test3

	rule test4 (s==4);
		$display("test4: commit");
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test4

	rule test5 (s==5);
		$display("test5: reserve");
		let yy <- rob.reserve(1);
		RdToken y = truncate(unpack(pack(yy)));
		addr[3] <= pack(y);
		$display("reserve: rob%d for r1",y);
	endrule: test5

	rule test6 (s==6);
		$display("test6: reserve");
		let yy <- rob.reserve(2);
		RdToken y = truncate(unpack(pack(yy)));
		addr[4] <= pack(y);
		$display("reserve: rob%d for r2",y);
	endrule: test6

	(*conflict_free="test7,test7_2"*)
	rule test7 (s==7);
		$display("test7: store_data commit");
		rob.store_data(truncate(addr[4]), 'hdead0004);
	endrule: test7
	
	rule test7_2 (s==7);
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test7_2

	(*conflict_free="test8,test8_2"*)
	rule test8 (s==8);
		$display("test8: store_data commit");
		rob.store_data(truncate(addr[3]), 'hdead0003);
	endrule: test8
	
	rule test8_2 (s==8);
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test8_2

	(*conflict_free="test9,test9_2"*)
	rule test9 (s==9);
		$display("test9: store_data commit");
		rob.store_data(truncate(addr[2]), 'hdead0002);
	endrule: test9
	
	rule test9_2 (s==9);
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test9_2

	rule test10 (s==10);
		$display("test10: commit");
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test10

	rule test11 (s==11);
		$display("test11: commit");
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test11

	rule test12 (s==12);
		$display("test12: commit");
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test12

	rule test13 (s==13);
		$display("test13: commit");
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test13

	rule test14 (s==14);
		$display("test14: commit");
		let x <- rob.commit;
		$display(fshow(x));
	endrule: test14
	
	rule done (s==s_end);
		$display("\n\n\n");
		$finish;
	endrule: done

endmodule: tb

module tb2 (Empty);

	ROBIfc#(`ROB_BUFFER_SIZE, `VS_AddrWidth, 64) rob <- mkROB;

	Vector#(16, Reg#(Bit#(TAdd#(TLog#(`ROB_BUFFER_SIZE),1)))) addr <- replicateM(mkReg(0));

	Bit#(10) s_start	= 'd0;
	Bit#(10) s_end		= 'd20;
	Reg#(Bit#(10)) s	<- mkReg(s_start);
	Reg#(Bit#(5)) x	<- mkReg(0);
	
	rule disp ;
		$display("\n\n\n***********************************  s = %d  ***********************************\n",s);
	endrule: disp
	
	rule count_s ;
		s <= s+1;
	endrule: count_s
	
	(*conflict_free="reserve, commit, write"*)
	rule reserve (s>=s_start && s<s_end);
		let y	<- rob.reserve(truncate(s));
		addr[s] <= pack(y);
		$display("reserve entry");
	endrule: reserve

	rule commit ;
		let x <- rob.commit;
		$display("COMMIT VAL: ",fshow(x));
	endrule: commit

	rule write (s==10 || s==12 || s==13 || s==15);
		$display("store_data at rob buf no. %d",x);
		rob.store_data(truncate(addr[x]), {'hdead,s});
		x <= x+1;
	endrule: write

	rule fwd_data ;
		let x	= rob.forward_data(truncate(addr[2]));
		$display("forward path: ",fshow(x));
	endrule: fwd_data

	/*
	rule flush (s==11 || s==5 );
		$display("flush rob...");
		rob.flushROB();
	endrule: flush
	*/
	
	rule done (s==s_end);
		$display("\n\n\n");
		$finish;
	endrule: done


endmodule: tb2
