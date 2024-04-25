*&---------------------------------------------------------------------*
*& Report ZFIR_RESET_STATUS_PGTO_ANTEC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_reset_status_pgto_antec.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_ov TYPE vbak-vbeln.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_stat TYPE ztfi_adto_ov-status.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.

  SELECT SINGLE vbeln, status
    FROM ztfi_adto_ov
    INTO @DATA(ls_dados_itf)
   WHERE vbeln = @p_ov.

  IF sy-subrc <> 0.
    MESSAGE ID zclfi_proc_pgto_antecipado=>gc_msg_id TYPE zclfi_proc_pgto_antecipado=>if_xo_const_message~success
    NUMBER 007 DISPLAY LIKE zclfi_proc_pgto_antecipado=>if_xo_const_message~error.
    RETURN.
  ENDIF.

  UPDATE ztfi_adto_ov SET status = p_stat WHERE vbeln = p_ov.
  COMMIT WORK.

  IF sy-subrc IS INITIAL.
    DATA(lt_return) = VALUE bapiret2_t( (
      id         = zclfi_proc_pgto_antecipado=>gc_msg_id
      number     = 009
      type       = zclfi_proc_pgto_antecipado=>if_xo_const_message~info
      message_v1 = p_stat
      message_v2 = ls_dados_itf-status
    ) ).

    DATA(lv_external_id) = CONV balnrext( p_ov ).

    CALL FUNCTION 'ZFMCA_ADD_LOG'
      EXPORTING
        iv_ext_number = lv_external_id
        iv_object     = zclfi_proc_pgto_antecipado=>gc_log_object
        iv_subobject  = zclfi_proc_pgto_antecipado=>gc_log_subobj
        it_return     = lt_return.

    MESSAGE ID zclfi_proc_pgto_antecipado=>gc_msg_id TYPE zclfi_proc_pgto_antecipado=>if_xo_const_message~success
    NUMBER 004 WITH '1'.
  ENDIF.
