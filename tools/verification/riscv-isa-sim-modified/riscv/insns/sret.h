require_privilege(PRV_S);
set_pc_and_serialize(p->get_state()->sepc);
reg_t s = STATE.mstatus;
reg_t prev_prv = get_field(s, MSTATUS_SPP);
s = set_field(s, MSTATUS_UIE << prev_prv, get_field(s, MSTATUS_SPIE));
s = set_field(s, MSTATUS_SPIE, 0);
s = set_field(s, MSTATUS_SPP, PRV_U);
p->set_privilege(prev_prv);
p->set_csr(CSR_MSTATUS, s);
