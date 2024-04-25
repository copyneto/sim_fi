*&---------------------------------------------------------------------*
*& Report zfir_exec_comp_ov_adiantamento
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_exec_comp_ov_adiantamento.

TABLES: vbap, vbak.

SELECTION-SCREEN BEGIN OF BLOCK b01.
  SELECT-OPTIONS:
    s_vbeln FOR vbap-vbeln,
    s_bukrs FOR vbak-bukrs_vf.
SELECTION-SCREEN END OF BLOCK b01.

DATA(go_proc_pgto_antecipado) = NEW zclfi_proc_pgto_antecipado( ).

START-OF-SELECTION.
  CALL METHOD go_proc_pgto_antecipado->process_so_adv_clearing
    EXPORTING
      ir_salesorder = CORRESPONDING #( s_vbeln[] )
      ir_comp_code  = CORRESPONDING #( s_bukrs[] ).

  MESSAGE ID zclfi_proc_pgto_antecipado=>gc_msg_id TYPE 'I' NUMBER 003 INTO DATA(gv_dummy).
  WRITE / gv_dummy.
