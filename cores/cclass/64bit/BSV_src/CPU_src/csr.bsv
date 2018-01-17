package csr;

  import privilege_mapping::*;
  import defined_types::*;
	`include "defined_parameters.bsv"
  import ConfigReg::*;
  import myRS232::*;
  import GetPut            ::*;
  interface Ifc_csr;
    method ActionValue#(Bit#(`Reg_width)) inputs(Bit#(3) funct, Bit#(12) address, Bit#(`Reg_width) rs1data); 
    method    Action      sin(Bit#(1) in);
    method    Bit#(1)     sout();
  endinterface

  (*synthesize*)
  module mkcsr(Ifc_csr);
    ConfigReg#(Bit#(`Reg_width)) rg_mcpuid <-mkConfigReg('h1129);//RV32IMAFD
    ConfigReg#(Bit#(`Reg_width)) rg_mimpid <-mkConfigReg('h38000);// C class and anonymous source
    ConfigReg#(Bit#(8)) rg_tx <-mkConfigReg(0);
    ConfigReg#(Bit#(9)) rg_rx <-mkConfigReg(0);
    ConfigReg#(Bit#(`Reg_width)) rg_mhartid <-mkConfigReg(0); // for now no thread support
		ConfigReg#(Bit#(`Reg_width)) machine_rf[32]; // delcaring the machine register file.
		for (Integer j = 0; j < 32; j = j + 1)begin
      if(j==0)
        machine_rf[0]<-mkConfigReg('d4095);
      else
  			machine_rf [j] <- mkConfigReg(0);
		end

    /////////////////////////////////////////////////////// UART related wires //////////////////////////////////////////////////
    Wire#(Bit#(8)) wr_send_to_uart <-mkWire();
    Wire#(Bit#(1)) wr_transmission_notfull <-mkDWire(0);
    Wire#(Bit#(1)) wr_reciever_ready <-mkDWire(0);
    Wire#(Bool) wr_rx_reading_done <-mkDWire(False);
    Wire#(Bit#(8)) wr_data_from_uart <-mkDWire('h41);
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    //////////////////////////////// UART Debugger /////////////////////////////////
      UART#(3) uart <-mkUART(8); // charasize,Parity,Stop Bits,BaudRate
      rule connect_uart_and_core;
        uart.parity_sel(NONE);
        uart.stopbits_sel(STOP_1);
        uart.divider(`BAUD_RATE);
      endrule
      rule send_transimission_data0;
        $display($time,"\t Putting Data :%s in FIFO for transmision",wr_send_to_uart);
        uart.rx.put(wr_send_to_uart);
      endrule
      rule uart_tx_connection;
        wr_transmission_notfull<=pack(uart.transmission_fifo_notfull);
      endrule
      rule uart_rx_receiver_ready_signal;
        wr_reciever_ready<=pack(uart.recieve_fifo_notempty);
      endrule
      rule uart_rx_connection;
        let x<-uart.tx.get;
        wr_data_from_uart<=x;
        $display($time,"\tReceived Character: %s",x);
      endrule
      rule assign_to_register;
        rg_rx[8:0]<={wr_reciever_ready,wr_data_from_uart};
      endrule
      rule remove_recieved_fifo_data(wr_rx_reading_done);
        $display($time," Dequeing the RX FIFO since it read");
        uart.dequeue_rx_fifo();
      endrule
    ////////////////////////////////////////////////////////////////////////////////
    method ActionValue#(Bit#(`Reg_width)) inputs(Bit#(3) funct, Bit#(12) address, Bit#(`Reg_width) rs1data); 
      $display($time,"\t CSR: PRIVILEGE INSTRUCTION: funct:%d addr :%h rs1: %h",funct,address,rs1data);
      Bit#(`Reg_width) csr_read_data=0;
      if(address==`MCPUID)
        csr_read_data=rg_mcpuid;
      else if(address==`MIMPID)
        csr_read_data=rg_mimpid;
      else if(address==`MHARTID)
        csr_read_data=rg_mhartid;
      else if(address==`UARTTX)
        csr_read_data=zeroExtend({wr_transmission_notfull,rg_tx});
      else if(address==`UARTRX)begin
        wr_rx_reading_done<=True;
        csr_read_data=zeroExtend(rg_rx);
      end
      else 
        csr_read_data=machine_rf[priv_map(address)];

      $display("csr_read_data:%h",csr_read_data);
      Bit#(`Reg_width) csr_write_data=0;
      if(funct[1:0]==1) // CSRW
        csr_write_data=zeroExtend(rs1data);
      else if(funct[1:0]==2) // CSRS
        csr_write_data=zeroExtend(rs1data)|csr_read_data;
      else if(funct[1:0]==3) // CSRC
        csr_write_data=~zeroExtend(rs1data)&csr_read_data;              

      if(address==`UARTTX && funct==1)begin // only if CSRW is used to write into UART register.
        $display($time,"\tWriting into TX REGISTER :%d",csr_write_data);
        rg_tx<=truncate(csr_write_data);
        wr_send_to_uart<=truncate(csr_write_data);
      end
      else
        machine_rf[priv_map(address)]<=csr_write_data;

      return csr_read_data;
    endmethod
    method    Action      sin(Bit#(1) in);
      uart.rs232.sin(in);
    endmethod
    method    Bit#(1)     sout();
      return uart.rs232.sout;
    endmethod

  endmodule
endpackage
