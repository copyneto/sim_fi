*----------------------------------------------------------------------*
***INCLUDE LZFGFI_FIELD_DOCDMSO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module M_BUSCA_DADOS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_busca_dados OUTPUT.
  IF o_badi_fdcb_subbas04 IS INITIAL.
    CALL METHOD cl_exithandler=>get_instance_for_subscreens
      CHANGING
        instance                      = o_badi_fdcb_subbas04
      EXCEPTIONS
        no_reference                  = 1
        no_interface_reference        = 2
        no_exit_interface             = 3
        data_incons_in_exit_managem   = 4
        class_not_implement_interface = 5
        OTHERS                        = 6.
    IF sy-subrc = 0.
      CHECK NOT o_badi_fdcb_subbas04 IS INITIAL.
    ENDIF.
  ENDIF.

  CHECK NOT o_badi_fdcb_subbas04 IS INITIAL.

* get data from main screen
  CALL METHOD o_badi_fdcb_subbas04->get_data_from_screen_object
    IMPORTING
      ex_invfo = invfo.

  IF invfo-zz1_docdms_mih IS NOT INITIAL.
    invfo-zz1_docdms_mih = |{ invfo-zz1_docdms_mih ALPHA = OUT }|.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module M_PROCESSA_CAMPO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_processa_campo OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'DM'.
      CASE sy-tcode.
        WHEN 'MIR4'.
          screen-input = 0.
          screen-active = 1.
        WHEN 'MIRO'.
          screen-input = 1.
          screen-active = 1.
        WHEN OTHERS.
          screen-active = 0.
      ENDCASE.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
