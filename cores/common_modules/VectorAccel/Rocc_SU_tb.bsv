/*
Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : Testbench for ROCC Unit 
Author Name     : Sumanth Sridhar, Vinod G
e-mail Id       : sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th June 2016
*/


import VectorAccelDefs::*;
import RoccUnit::*;
import ScalarUnit::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;


module tb (Empty);
	RoccIfc			rocc	<- mkRoccUnit;
	ScalarUnitIfc 	su		<- mkScalarUnit;
	Reg#(Bit#(8)) 	s		<- mkReg(0);

	`include "RoccUnit_test_vectors.bsv"

	mkConnection(rocc.deqVCMDQ, su.putRoccInstr);
	mkConnection(rocc.readVCFG, su.rocc_readVCFG);
	mkConnection(rocc.resetVCFG, su.rocc_resetVCFG);
	mkConnection(rocc.readVLEN, su.rocc_readVLEN);

	rule disp_state_number;
		$display("--------- s = %d ---------",s);
	endrule: disp_state_number
	
	rule count_s ;
		s <= s+1;
	endrule: count_s

	// rule get_vrcmdq;
	// 	let x <- rocc.deqVRCMDQ.get();
	// 	$display("deq VRCMDQ: ",fshow(x));
	// endrule: get_vrcmdq

	rule get_roccResponse;
		let x <- rocc.roccServer.response.get;
		$display("rocc Response: ", fshow(x));
	endrule: get_roccResponse

	rule send_request (s<14);
		rocc.roccServer.request.put(t[s]);
	endrule: send_request
	
	Reg#(Bit#(8)) n	<-	mkReg(16);

	rule finish (s==n);
		$display("---------  end  ---------\n\n");
		$finish;
	endrule: finish

endmodule
