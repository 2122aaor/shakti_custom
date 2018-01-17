package host;
  import defined_types::*;
  `include "defined_parameters.bsv"
	`ifdef AXI
	  import axi::*;
	`else
		import ahb::*;
	`endif
  import myRS232::*;
  import GetPut            ::*;

  (*synthesize*)
  module mkhost(Empty);
		Ifc_proc pc <-mkproc();
    UART#(16) uart <-mkUART(8,NONE,STOP_1,`BAUD_RATE); // charasize,Parity,Stop Bits,BaudRate
    Reg#(Bit#(1)) rg_cnt <-mkReg(0);
    let reg_dump <- mkReg(InvalidFile) ;
    let freg_dump <- mkReg(InvalidFile) ;
    rule open_file(rg_cnt==0);
        String reg_dumpFile = "app_log" ;
        String freg_dumpFile = "rtl_fregister_dump.txt" ;
        File lfh <- $fopen( reg_dumpFile, "w" ) ;
        if ( lfh == InvalidFile )begin
            $display("cannot open %s", reg_dumpFile);
            $finish(0);
        end
        reg_dump <= lfh ;
        rg_cnt <= 1 ;
    endrule

    rule connect_sin;
      pc.sin(uart.rs232.sout);
    endrule

    rule connect_sout;
      uart.rs232.sin(pc.sout);
    endrule

    rule write_recieved_character_in_file(rg_cnt!=0);
      let data<-uart.tx.get;
      $fwrite(reg_dump,"%c",data);
    endrule
  endmodule

endpackage
