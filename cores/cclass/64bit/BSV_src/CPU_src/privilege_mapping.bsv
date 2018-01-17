package privilege_mapping;
  function Bit#(5) priv_map(Bit#(12) cssr_addr);
    case (cssr_addr) matches
      'h300: return 0;
      'h301: return 1;
      'h302: return 2;
      'h304: return 3;
      'h321: return 4;
      'h701: return 5;
      'h741: return 6;
      'h340: return 7;
      'h341: return 8;
      'h342: return 9;
      'h343: return 10;
      'h344: return 11;
      'h380: return 12;
      'h381: return 13;
      'h382: return 14;
      'h383: return 15;
      'h384: return 16;
      'h385: return 17;
      'hC00: return 18;
      default: return 31;
    endcase
  endfunction
endpackage
