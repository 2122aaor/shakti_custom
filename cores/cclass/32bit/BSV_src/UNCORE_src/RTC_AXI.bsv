
// Copyright (c) 2017 Massachusetts Institute of Technology

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

package RTC_AXI;
import ClientServer::*;
import GetPut::*;
import Vector::*;
import BuildVector::*;
import TLM2::*;
`include "TLM.defines"
import TLMReqRsp::*;

interface RTC#(numeric type cores);
    // memory-mapped interface
    // 0x0000 = timer
    // 0x0008 = timecmp0
    // 0x0010 = timecmp1
    //  ...
		interface TLMRecvIFC #(Req_Mem, Rsp_Mem) bus_rd_ifc;
		interface TLMRecvIFC #(Req_Mem, Rsp_Mem) bus_wr_ifc;
    method Vector#(cores, Bool) timerInterrupt;
endinterface

// This is only supported on RV32 systems
// Only supports full-word memory accesses
module mkRTC_RV32(RTC#(1))
        provisos (NumAlias#(internalAddrSize, 4));
    // Address space:
    Bit#(internalAddrSize) timerLoAddr   = 'h0;
    Bit#(internalAddrSize) timerHiAddr   = 'h4;
    Bit#(internalAddrSize) timeCmpLoAddr = 'h8;
    Bit#(internalAddrSize) timeCmpHiAddr = 'hC;

    Reg#(Bit#(32)) timeRegLo <- mkReg(0);
    Reg#(Bit#(32)) timeRegHi <- mkReg(0);
    Reg#(Bit#(32)) timeCmpLo <- mkReg(0);
    Reg#(Bit#(32)) timeCmpHi <- mkReg(0);
    Reg#(Maybe#(Tuple2#(Bit#(internalAddrSize),Req_Desc_Mem))) pendingReq_rd[2] <- mkCReg(2, tagged Invalid);
    Reg#(Maybe#(Tuple2#(Bit#(internalAddrSize),Req_Desc_Mem))) pendingReq_wr[2] <- mkCReg(2, tagged Invalid);
    Bool timerInterruptEn = {timeRegHi, timeRegLo} >= {timeCmpHi, timeCmpLo};
    Reg #(Bit#(4)) counter_cycle <- mkReg(0);
	 
	 	rule rl_count_cycle;
	  	counter_cycle <= counter_cycle + 1;
	 	endrule

    rule incrementTimer(counter_cycle == 0);
        Bit#(64) timeValue = {timeRegHi, timeRegLo};
        Bit#(64) newTimeValue = timeValue + 1;
        timeRegLo <= newTimeValue[31:0];
        timeRegHi <= newTimeValue[63:32];
    endrule

    interface TLMRecvIFC bus_rd_ifc;
        interface Put rx;
            method Action put(Req_Mem reqs) if (!isValid(pendingReq_rd[1]));
							if (reqs matches tagged Descriptor .req)
                    pendingReq_rd[1] <= tagged Valid tuple2(truncate(req.addr),req);
            endmethod
        endinterface
        interface Get tx;
            method ActionValue#(Rsp_Mem) get if (pendingReq_rd[0] matches tagged Valid .reqTuple);
                Bit#(internalAddrSize) addr = tpl_1(reqTuple);
								Req_Desc_Mem req = tpl_2(reqTuple);
                Bit#(32) retVal = 0;
                case (truncate(addr))
                    timerLoAddr:    retVal = timeRegLo;
                    timerHiAddr:    retVal = timeRegHi;
                    timeCmpLoAddr:  retVal = timeCmpLo;
                    timeCmpHiAddr:  retVal = timeCmpHi;
                    default:        retVal = 0;
                endcase
                pendingReq_rd[0] <= tagged Invalid;
								return Rsp_Mem{command: READ, data:retVal, transaction_id:req.transaction_id, status: SUCCESS, thread_id:req.thread_id, export_id:req.export_id, custom:?};
            endmethod
        endinterface
    endinterface
    interface TLMRecvIFC bus_wr_ifc;
        interface Put rx;
            method Action put(Req_Mem reqs) if (!isValid(pendingReq_wr[1]));
							if (reqs matches tagged Descriptor .req)begin
                case (truncate(req.addr))
                    timerLoAddr:    timeRegLo <= req.data;
                    timerHiAddr:    timeRegHi <= req.data;
                    timeCmpLoAddr:  timeCmpLo <= req.data;
                    timeCmpHiAddr:  timeCmpHi <= req.data;
                    default:        noAction;
                endcase
                pendingReq_wr[1] <= tagged Valid tuple2(truncate(req.addr),req);
              end 
            endmethod
        endinterface
        interface Get tx;
            method ActionValue#(Rsp_Mem) get if (pendingReq_wr[0] matches tagged Valid .reqTuple);
								Req_Desc_Mem req = tpl_2(reqTuple);
                pendingReq_wr[0] <= tagged Invalid;
								return Rsp_Mem{command: WRITE, data:0, transaction_id:req.transaction_id, status: SUCCESS, thread_id:req.thread_id, export_id:req.export_id, custom:?};
            endmethod
        endinterface
    endinterface
    method Vector#(1, Bool) timerInterrupt = vec(timerInterruptEn);
endmodule
endpackage
