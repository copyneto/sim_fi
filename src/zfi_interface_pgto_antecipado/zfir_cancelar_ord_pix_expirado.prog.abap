*&---------------------------------------------------------------------*
*& Report zfir_cancelar_ord_pix_expirado
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_cancelar_ord_pix_expirado.

TABLES: vbap, vbak.

SELECTION-SCREEN BEGIN OF BLOCK b01.
  SELECT-OPTIONS:
    s_vbeln FOR vbap-vbeln.
SELECTION-SCREEN END OF BLOCK b01.

DATA(go_proc_pgto_antecipado) = NEW zclfi_proc_pgto_antecipado( ).

START-OF-SELECTION.
  CALL METHOD go_proc_pgto_antecipado->process_estorno_solicitacao
    EXPORTING
      ir_salesorder = CORRESPONDING #( s_vbeln[] ).

  MESSAGE ID zclfi_proc_pgto_antecipado=>gc_msg_id TYPE 'I' NUMBER 003 INTO DATA(gv_dummy).
  WRITE / gv_dummy.
