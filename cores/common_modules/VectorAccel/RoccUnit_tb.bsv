/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Testbench for Re-order Buffer
Author Name     : Sumanth Sridhar, Vinod G
e-mail Id       : sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th June 2016
*/



import VectorAccelDefs::*;
import RoccUnit::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;


interface RoccRxrIfc;
    interface Put#(CMDQReq) put_deqVCMDQ;
    interface Put#(CMDQReq) put_deqVRCMDQ;
    interface Put#(Maybe#(Vcfg)) rocc_writeVCFG;
    interface Put#(Maybe#(Vlen)) rocc_writeVLEN;
    interface Get#(Vcfg) rocc_readVCFG;
    interface Get#(Vlen) rocc_readVLEN;
endinterface: RoccRxrIfc

module mkRoccRxr (RoccRxrIfc);

	Wire#(CMDQReq) 	deqVCMDQ	<- mkWire;
	Wire#(CMDQReq) 	deqVRCMDQ	<- mkWire;
	RWire#(Vlen)	w_vlen		<- mkRWire;
	RWire#(Vcfg)	w_vcfg		<- mkRWire;
	Reg#(Vcfg)		vcfg		<- mkReg('habcdef);
	Reg#(Vlen)		vlen		<- mkReg('habcde);

	rule deq1 ;
		$display("deq VCMDQ : ", fshow(deqVCMDQ));
	endrule: deq1

	rule deq2 ;
		$display("deq VRCMDQ: ", fshow(deqVRCMDQ));
	endrule: deq2

	rule update_VLEN_VCFG ;
		if (isValid(w_vcfg.wget))
			vcfg <= fromMaybe(?,w_vcfg.wget);
		if (isValid(w_vlen.wget))
			vlen <= fromMaybe(?,w_vlen.wget);
	endrule: update_VLEN_VCFG

	interface Put put_deqVCMDQ;
	    method Action put(CMDQReq vcmdqReq);
	    	deqVCMDQ <= vcmdqReq;
	    endmethod    
	endinterface: put_deqVCMDQ

	interface Put put_deqVRCMDQ;
	    method Action put(CMDQReq vrcmdqReq);
	    	deqVRCMDQ <= vrcmdqReq;
	    endmethod    
	endinterface: put_deqVRCMDQ

	interface Get rocc_readVCFG;
	    method ActionValue#(Vcfg) get();
	    	return (isValid(w_vcfg.wget)? validValue(w_vcfg.wget) : vcfg);
	    endmethod
	endinterface: rocc_readVCFG

	interface Put rocc_writeVCFG;
	    method Action put(Maybe#(Vcfg) vcfg_in);
	    	if (isValid(vcfg_in)) begin
		    	w_vcfg.wset(fromMaybe('0,vcfg_in));
		    	$display("update vcfg = %h", fromMaybe('0,vcfg_in));
		    end
	    endmethod
	endinterface: rocc_writeVCFG

	interface Get rocc_readVLEN;
	    method ActionValue#(Vlen) get();
	    	return (isValid(w_vlen.wget)? validValue(w_vlen.wget) : vlen);
	    endmethod
	endinterface: rocc_readVLEN

	interface Put rocc_writeVLEN;
	    method Action put(Maybe#(Vlen) vlen_in);
	    	if (isValid(vlen_in)) begin
		    	w_vlen.wset(fromMaybe('0,vlen_in));
		    	$display("update vlen = %h", fromMaybe('0,vlen_in));
		    end
	    endmethod
	endinterface: rocc_writeVLEN

endmodule: mkRoccRxr


// (* descending_urgency = "disp_state_number" *)
module tb (Empty);
	RoccIfc dut		<- mkRoccUnit;
	RoccRxrIfc rxr	<- mkRoccRxr;
	Reg#(Bit#(8)) s	<- mkReg(0);

	`include "RoccUnit_test_vectors.bsv"

	mkConnection(dut.deqVCMDQ, rxr.put_deqVCMDQ);
	mkConnection(dut.deqVRCMDQ, rxr.put_deqVRCMDQ);
	mkConnection(dut.readVCFG, rxr.rocc_readVCFG);
	mkConnection(dut.writeVCFG, rxr.rocc_writeVCFG);
	mkConnection(dut.readVLEN, rxr.rocc_readVLEN);
	mkConnection(dut.writeVLEN, rxr.rocc_writeVLEN);

	rule disp_state_number;
		$display("--------- s = %d ---------",s);
	endrule: disp_state_number
	
	rule count_s ;
		s <= s+1;
	endrule: count_s

	// rule get_vcmdq;
	// 	let x <- dut.deqVCMDQ.get();
	// 	$display("deq VCMDQ : ",fshow(x));
	// endrule: get_vcmdq

	// rule get_vrcmdq;
	// 	let x <- dut.deqVRCMDQ.get();
	// 	$display("deq VRCMDQ: ",fshow(x));
	// endrule: get_vrcmdq

	rule get_hwacharesponse;
		let x <- dut.roccServer.response.get;
		$display("hwacha Response: ", fshow(x));
	endrule: get_hwacharesponse

	rule send_request (s<14);
		dut.roccServer.request.put(t[s]);
	endrule: send_request
	
	Reg#(Bit#(8)) n	<-	mkReg(16);

	rule finish (s==n);
		$display("---------  end  ---------\n\n");
		$finish;
	endrule: finish

endmodule
