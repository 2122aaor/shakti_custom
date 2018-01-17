/*

Copyright (c) 2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name     : ROCC Unit 
Author Name     : Sumanth Sridhar, Vinod.G
e-mail Id       : sumanthsridhar.009@gmail.com, g.vinod1993@gmail.com
Last updated on : 30th June 2016

This Module handles control thread instructions and queues them into their respective FIFOs. This acts as an interface between the I/C-Class Processor and the Vector Processor Core.

    RoCC Unit Actions:
                      VCMDQ?      VRCMDQ?    Respond?
        VSETCFG         Y           Y           N
        VSETVL          Y           Y           Y
        VGETCFG         N           N           Y       custom-0 instructions
        VGETVL          N           N           Y
        VUNCFG          N           N           N       
        ---------------------------------------------------------------------
        VMCS            Y           N           N       
        VMCA            Y           Y           N       custom-1 instructions
        VF              Y           Y           N

*/

package RoccUnit;
import GetPut::*;
import ClientServer::*;
import FIFO::*;
import VectorAccelDefs::*;

interface RoccIfc;
    interface Server#( VectorReq, Maybe#(VectorResp) ) roccServer;
    interface Get#(CMDQReq) deqVCMDQ;
    interface Get#(CMDQReq) deqVRCMDQ;
    interface Get#(Bool)        resetVCFG;  // only occurs when VUNCFG is executed
    interface Put#(Vcfg)        readVCFG;
    interface Put#(VlenStruct)  readVLEN;
    // interface Get#(Maybe#(Vlen)) writeVLEN;
endinterface: RoccIfc

(*synthesize*)
module mkRoccUnit (RoccIfc);
    
    // vector command & run-ahead Qs
    FIFO#(CMDQReq) vcmdq    <- mkFIFO;          // todo using mkSizedFIFO for improved performance
    FIFO#(CMDQReq) vrcmdq   <- mkFIFO;

    Reg#(Bool)          isAccelDisabled <- mkReg(True);     // NOTE init val shd be "True"
    Wire#(Bit#(64))     src1            <- mkWire;
    Wire#(Instr32)      instr32         <- mkWire;
    Wire#(DInstr32)     decodedInstr32  <- mkWire;
    Wire#(Vcfg)         vcfg_fromSU     <- mkWire;
    Wire#(Vlen)         vlen_fromSU     <- mkWire;
    Wire#(Bool)         send_setVL_resp <- mkWire;
    RWire#(Bit#(0))     resetVcfg       <- mkRWire;     // only used with VUNCFG; to reset vcfg-reg
    // Wire#(Maybe#(Vlen)) vlen_toSU       <- mkWire;

    Reg#(Maybe#(VectorResp)) vectorResp <- mkReg(Invalid);

    //-------------------------------------- DECODING LOGIC --------------------------------------//

    /* NOTE For decoding, minimal logic is used
            For eg, if any instr can be completely decoded using {xd, xs1}, then only these flags are used
            If reqd, additional checks can also be done to verify opcode val, funct7 val, etc.
    */

    let op          = i32_opcode (instr32);
    let x_flags     = i32_xd_xs1 (instr32);
    let f7_field    = i32_funct7 (instr32);
    let rs2_msb     = instr32[24];

    Bool isVSETCFG  = (x_flags == 2'b01) && (op == custom0);
    Bool isVSETVL   = (x_flags == 2'b11);
    Bool isVGETCFG  = (x_flags == 2'b10) && (f7_field == f7_VGETCFG);
    Bool isVGETVL   = (x_flags == 2'b10) && (f7_field == f7_VGETVL);
    Bool isVUNCFG   = (x_flags == 2'b00);

    Bool isVF       = (rs2_msb==1'b1) && (op == custom1);
    Bool isVMCS     = (rs2_msb==1'b0) && (f7_field == f7_VMCS) && (op == custom1);
    Bool isVMCA     = (rs2_msb==1'b0) && (f7_field == f7_VMCA) && (op == custom1);

    //--------------------------------------------------------------------------------------------//

    function Bool isInstrValid (Instr32 x_instr32);
        Bool cond0 = (i32_opcode(x_instr32) == custom0);
        Bool cond1 = (i32_opcode(x_instr32) == custom1);
        return (cond0 || cond1);
    endfunction: isInstrValid


    rule exception_IllegalInstruction (!isInstrValid(instr32));
        // todo Exception: illegal instruction
        $display("illegal instruction: opcode = %b", i32_opcode(instr32));
    endrule: exception_IllegalInstruction

    
    /*  When an VUNCFG operation is done, the vector accel is disabled
        After this, a VSETCFG must be executed before accel can be used again
        Else an 'accelerator disabled' exception is raised
    */
    rule exception_AccelDisabled (isInstrValid(instr32) && isAccelDisabled && !isVSETCFG);
        // todo Exception: accel disabled
        $display("AccelDisabled");
    endrule: exception_AccelDisabled

    rule enableAccel(isInstrValid(instr32) && isAccelDisabled && isVSETCFG);
        isAccelDisabled <= False;
        $display("isVSETCFG => Accel enabled");
    endrule: enableAccel


    rule decodeInstr32 (isInstrValid(instr32));
        
        if (isVSETCFG)      decodedInstr32  <= Vsetcfg;
        else if (isVSETVL)  decodedInstr32  <= Vsetvl;
        else if (isVGETCFG) decodedInstr32  <= Vgetcfg;
        else if (isVGETVL)  decodedInstr32  <= Vgetvl;
        else if (isVUNCFG)  decodedInstr32  <= Vuncfg;
        else if (isVMCS)    decodedInstr32  <= Vmcs;
        else if (isVMCA)    decodedInstr32  <= Vmca;
        else if (isVF)      decodedInstr32  <= Vf;
        else                decodedInstr32  <= ERROR;

    endrule: decodeInstr32

    rule display_instr ;
        $display("decodedInstr32: ", fshow(decodedInstr32));
    endrule: display_instr

    rule enq_VCMDQ ( !isAccelDisabled && isInstrValid(instr32));
        if (isVSETCFG || isVSETVL || isVMCS || isVMCA || isVF) begin
            vcmdq.enq(CMDQReq{
                instrName       : decodedInstr32,
                ctrlInstruction : instr32,
                value64         : src1
            });
            $display("enq_VCMDQ: ",fshow(decodedInstr32));
        end
    endrule: enq_VCMDQ

    rule enq_VRCMDQ ( !isAccelDisabled && isInstrValid(instr32));
        if (isVSETCFG || isVSETVL || isVMCA || isVF) begin
            vrcmdq.enq(CMDQReq{
                instrName       : decodedInstr32,
                ctrlInstruction : instr32,
                value64         : src1
            });
            $display("enq_VRCMDQ: ",fshow(decodedInstr32));
        end
    endrule: enq_VRCMDQ

    /*  explicitly adding "... && !isVUNCFG" as predicate is used to explicitly tell the compiler that
        rule_respond and rule_do_uncfg wont occur in the same cycle; else a cycle is formed b/w wires
        used in respond, uncfg, and the getput interfaces of vcfg & vlen */
    rule respond ( !isAccelDisabled && isInstrValid(instr32) && !isVUNCFG);
        if (send_setVL_resp) begin
            // todo respond to VSETVL in this unit or in Scalar & RunAhead pipes ...?
            vectorResp <= Valid(VectorResp {
                destReg : i32_rd(instr32),
                value64 : vlen_fromSU
            });
            $display("Send response for: Vsetvl");
        end else if (isVGETCFG) begin
            vectorResp <= Valid(VectorResp {
                destReg : i32_rd(instr32),
                value64 : vcfg_fromSU
            });
            $display("Sent response for: Vgetcfg");
        end else if (isVGETVL) begin
            vectorResp <= Valid(VectorResp {
                destReg : i32_rd(instr32),
                value64 : vlen_fromSU
            });
            $display("Sent response for: Vgetvl");
        end else begin
            vectorResp <= Invalid;
        end
    endrule: respond

    rule do_Uncfg (isInstrValid(instr32) && isVUNCFG);
        // indicate that the VCFG reg is to be reset to 0
        resetVcfg.wset('1);
        isAccelDisabled <= True;
    endrule: do_Uncfg
    
    interface Server roccServer;
        
        interface Put request;
            method Action put(VectorReq vectorReq);
                instr32     <= vectorReq.ctrlInstruction;
                src1        <= vectorReq.value64;
            endmethod
        endinterface: request
    
        interface Get response;
            method ActionValue#(Maybe#(VectorResp)) get();
                return vectorResp;
            endmethod
        endinterface: response

    endinterface: roccServer
    
    interface Get deqVCMDQ;
        method ActionValue#(CMDQReq) get();
            vcmdq.deq();
            return vcmdq.first();
        endmethod    
    endinterface: deqVCMDQ
    
    interface Get deqVRCMDQ;
        method ActionValue#(CMDQReq) get();
            vrcmdq.deq();
            return vrcmdq.first();
        endmethod    
    endinterface: deqVRCMDQ

    // basically a wire connected to VCFG so that this module can access the value
    interface Put readVCFG;
        method Action put(Vcfg vcfg_in);
            vcfg_fromSU <= vcfg_in;
            $display("w_vcfg = %d", vcfg_in);
        endmethod
    endinterface: readVCFG

    // basically a wire connected to VLEN so that this module can access the value
    interface Put readVLEN;
        method Action put(VlenStruct vlen_in);
            vlen_fromSU     <= vlen_in.vlen;
            send_setVL_resp <= vlen_in.setVL_response;
        endmethod
    endinterface: readVLEN

    interface Get resetVCFG;
        method ActionValue#(Bool) get();
            return isValid(resetVcfg.wget);
        endmethod
    endinterface: resetVCFG
    
    /*
    // use a maybe type so that the SU will know if a write has occured on VLEN
    interface Get writeVLEN;
        method ActionValue#(Maybe#(Vlen)) get();
            return vlen_toSU;
        endmethod
    endinterface: writeVLEN
    */
endmodule

endpackage
