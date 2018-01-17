/*
Copyright (c) 2013-2015, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Name : Abhinaya Agrawal
Email ID : agrawal.abhinaya@gmail.com
*/

package Testbench;

// ================================================================
// Project imports

import TLM2             :: *; // Using local copy.
import Utils            :: *;
import Req_Rsp          :: *;
import Sys_Configs      :: *;
import TypeDefs         :: *;
import Interfaces       :: *;
import myRS232          :: *;
import GetPut           :: *;
import SoC              :: *;

`include "TLM.defines"
`include "RVC.defines"
`include "macro.defines"
`define BAUD_RATE 130

(*synthesize*)
module mkTestbench(Empty);
  SoC_IFC pc <- mkSoC;
  UART#(16) uart <-mkUART(8,NONE,STOP_1,`BAUD_RATE); // charasize,Parity,Stop Bits,BaudRate
  Reg#(Bit#(1)) rg_cnt <-mkReg(0);
  let reg_dump <- mkReg(InvalidFile);
  let freg_dump <- mkReg(InvalidFile);

  rule open_file(rg_cnt==0);
      String reg_dumpFile = "app_log";
      String freg_dumpFile = "rtl_fregister_dump.txt";
      File lfh <- $fopen(reg_dumpFile, "w");
      if (lfh == InvalidFile) begin
          $display("cannot open %s", reg_dumpFile);
          $finish(0);
      end
      reg_dump <= lfh;
      rg_cnt <= 1;
  endrule

  rule connect_sin;
    pc.uart_ifc.sin(uart.rs232.sout);
  endrule

  rule connect_sout;
    uart.rs232.sin(pc.uart_ifc.sout);
  endrule

  rule write_recieved_character_in_file(rg_cnt!=0);
    let data<-uart.tx.get;
    $fwrite(reg_dump,"%c",data);
  endrule
endmodule

endpackage: Testbench
