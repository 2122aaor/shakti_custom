
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

import ClientServer::*;
import GetPut::*;
import Vector::*;
import BuildVector::*;
import TLM2::*;
`include "TLM.defines"
`include "RVC.defines"
//import Req_Rsp::*;

	typedef TLMResponse#(`TLM_PRM_AHB_RSP) Rsp_tlm;
	typedef TLMRequest#(`TLM_PRM_AHB_REQ) Req_tlm;
	typedef RequestDescriptor#(`TLM_PRM_AHB_REQ) Req_Desc;
	typedef RequestData#(`TLM_PRM_AHB_REQ) Req_Data;
interface RTC#(numeric type cores);
    // memory-mapped interface
    // 0x0000 = timer
    // 0x0008 = timecmp0
    // 0x0010 = timecmp1
    //  ...
    interface TLMRecvIFC#(Req_tlm, Rsp_tlm) bus_ifc;
    method Bit#(64) timerValue;
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

    Reg#(Maybe#(Tuple2#(Bool, Bit#(internalAddrSize)))) pendingReq[2] <- mkCReg(2, tagged Invalid);

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

    interface TLMRecvIFC bus_ifc;
        interface Put rx;
            method Action put(Req_tlm reqs) if (!isValid(pendingReq[1]));
					if (reqs matches tagged Descriptor .req)
                if (req.command == WRITE) begin
                    case (truncate(req.addr))
                        timerLoAddr:    timeRegLo <= req.data;
                        timerHiAddr:    timeRegHi <= req.data;
                        timeCmpLoAddr:  timeCmpLo <= req.data;
                        timeCmpHiAddr:  timeCmpHi <= req.data;
                        default:        noAction;
                    endcase
                    pendingReq[1] <= tagged Valid tuple2(True, truncate(req.addr));
						  //$display($time, " RTC: WRITE Request recieved at Addr: %h, Value: %h", req.addr, req.data);
                end else begin
                    pendingReq[1] <= tagged Valid tuple2(False, truncate(req.addr));
						  //$display($time, " RTC: READ Request recieved at Addr: %h", req.addr);
                end
            endmethod
        endinterface
        interface Get tx;
            method ActionValue#(Rsp_tlm) get if (pendingReq[0] matches tagged Valid .reqTuple);
                Bool write = tpl_1(reqTuple);
                Bit#(internalAddrSize) addr = tpl_2(reqTuple);
                Bit#(32) retVal = 0;
                case (truncate(addr))
                    timerLoAddr:    retVal = timeRegLo;
                    timerHiAddr:    retVal = timeRegHi;
                    timeCmpLoAddr:  retVal = timeCmpLo;
                    timeCmpHiAddr:  retVal = timeCmpHi;
                    default:        retVal = 0;
                endcase
                pendingReq[0] <= tagged Invalid;
                return Rsp_tlm{ command: (write) ? WRITE : READ, data: write ? 0 : retVal };
            endmethod
        endinterface
    endinterface
    method Bit#(64) timerValue = {timeRegHi, timeRegLo};
    method Vector#(1, Bool) timerInterrupt = vec(timerInterruptEn);
endmodule
