/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 


Author Names : Rahul Bodduna
Email ID : rahul.bodduna@gmail.com
*/

package MMCsrs;

`include "ProcConfig.bsv"

import ClientServer::*;
import GetPut::*;
import Vector::*;

import Ehr::*;

import RVTypes::*;

interface IFC_MemoryMappedCSRs;
    // memory-mapped interface
    // 0x0000 = timer
    // 0x0008 = timecmp0
    // 0x0010 = timecmp1
    //  ...
    // 0x1000 = ipi0
    // 0x2000 = ipi1
    //  ...
    interface UncachedMemServer memifc;
    method Bit#(64) timerValue;
    method Bool timerInterrupt;
endinterface

typedef Server#(UncachedMemReq, UncachedMemResp) UncachedMemServer;    

module mkMemoryMappedCSRs#(PAddr baseaddr)(IFC_MemoryMappedCSRs);
    // this doesn't work for 16 or more cores since it assumes a 16 bit address space
    Reg#(Maybe#(UncachedMemResp)) resp <- mkReg(tagged Invalid);

    Reg#(Bit#(10)) subTimer <- mkReg(0);
    Reg#(Bit#(64)) timer <- mkReg(0);
    Ehr#(2, Maybe#(Bit#(64))) newTimeEhr <- mkEhr(tagged Invalid);
    Reg#(Bit#(64)) timeCmp <- mkReg('1);
    Reg#(Bool) ipiReg <- mkReg(False);

    rule incrementTimer;
        if (newTimeEhr[1] matches tagged Valid .validNewTime) begin
            subTimer <= 0;
            timer <= validNewTime;
        end else begin
            Bool overflow = subTimer == 999;
            subTimer <= overflow ? 0 : subTimer + 1;
            timer <= overflow ? timer + 1 : timer;
        end
        newTimeEhr[1] <= tagged Invalid;
    endrule

    interface UncachedMemServer memifc;
        interface Put request;
            method Action put(UncachedMemReq req) if (!isValid(resp));
                UncachedMemResp newResp = UncachedMemResp{write: req.write, data: 0};
                Bit#(16) addr = truncate(req.addr - baseaddr);
                if (addr < 16'h1000) begin
                    // RTC registers
                    if (((addr & 16'h0007) == 0) && (req.size == D)) begin
                        let index = addr >> 3;
                            if (index == 0) begin
                                if (req.write) begin
                                    newTimeEhr[0] <= tagged Valid req.data;
                                end else begin
                                    newResp.data = timer;
                                end
                            end else begin
                                if (req.write) begin
                                    timeCmp <= req.data;
                                end else begin
                                    newResp.data = timeCmp;
                                end
                            end
                    end else begin
                        $fdisplay(stderr, "[ERROR] MemoryMappedCSRs: unexpected mem request: ", fshow(req));
                    end
								end
                resp <= tagged Valid newResp;
            endmethod
        endinterface
        interface Get response;
            method ActionValue#(UncachedMemResp) get if (resp matches tagged Valid .validResp);
                resp <= tagged Invalid;
                return validResp;
            endmethod
        endinterface
    endinterface
    method Bit#(64) timerValue = timer;
    method Bool timerInterrupt = timer >= timeCmp;
endmodule
endpackage
