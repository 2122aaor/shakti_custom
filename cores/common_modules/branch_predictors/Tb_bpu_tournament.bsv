/* Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*/
package Tb_bpu_tournament;
  import bpu_tournament ::*;
  import defined_types ::*; 
  import FShow ::*;
  module mkTb_bpu_tournament (Empty);
    Ifc_bpu_tournament bimodal <- mkbpu_tournament;
    Reg #(int) rg_count <- mkReg(0);    

    rule rl_count;
        rg_count <= rg_count + 1;
    endrule
 
    rule rl_deq;
        bimodal._deq_FIFO();
    endrule

    rule rl_exec1 (rg_count == 1);    
      bimodal._training (16, 420, Taken);
    endrule : rl_exec1

       
    rule rl_exec2 (rg_count == 2);
      bimodal._training (16, 420, Taken);        
    endrule : rl_exec2

    rule rl_exec3 (rg_count == 3);
      bimodal._training (420, 768, Taken);
    endrule : rl_exec3


      rule rl_exec4 (rg_count == 4);
      bimodal._training (420, 768, Taken);
    endrule : rl_exec4

/*
    rule rl_exec5 (rg_count == 5);
      bimodal._training (786, 420, Notaken);
    endrule : rl_exec5

    rule rl_exec6 (rg_count == 6);
      bimodal._training (786, 420, Notaken);
    endrule : rl_exec6

    rule rl_exec7 (rg_count == 7);
      bimodal._training (786, 420, Taken);
    endrule : rl_exec7

    rule rl_exec8 (rg_count == 8);
      bimodal._training (786, 420, Notaken);
    endrule : rl_exec8
*/
    rule rl_display(rg_count >= 1);
      $display("count = %d. PC = %d ", rg_count, bimodal.send_output_().prog_counter_, fshow(bimodal.send_output_.prediction_));
      if(rg_count == 9)
          $finish (0);
    endrule : rl_display
  endmodule
  
endpackage
