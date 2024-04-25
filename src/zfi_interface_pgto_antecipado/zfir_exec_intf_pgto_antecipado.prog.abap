*&---------------------------------------------------------------------*
*& Report zfir_exec_intf_pgto_antecipado
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_exec_intf_pgto_antecipado.

TABLES: ztfi_adto_ov.

SELECTION-SCREEN BEGIN OF BLOCK b01.
  SELECT-OPTIONS:
    s_vbeln FOR ztfi_adto_ov-vbeln.
SELECTION-SCREEN END OF BLOCK b01.

DATA(go_proc_pgto_antecipado) = NEW zclfi_proc_pgto_antecipado( ).

START-OF-SELECTION.

  CALL FUNCTION 'ENQUEUE_E_TRDIR'
    EXPORTING
      name           = sy-repid
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    MESSAGE ID zclfi_proc_pgto_antecipado=>gc_msg_id TYPE 'I' NUMBER 008 INTO DATA(gv_dummy).
    WRITE / gv_dummy.
    RETURN.
  ENDIF.

  CALL METHOD go_proc_pgto_antecipado->process
    EXPORTING
      ir_salesorder = CORRESPONDING #( s_vbeln[] ).

  MESSAGE ID zclfi_proc_pgto_antecipado=>gc_msg_id TYPE 'I' NUMBER 003 INTO gv_dummy.
  WRITE / gv_dummy.

  CALL FUNCTION 'DEQUEUE_E_TRDIR'
    EXPORTING
      name = sy-repid.
