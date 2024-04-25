*----------------------------------------------------------------------*
***INCLUDE LZFGFI_FIELD_DOCDMSI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  M_PROCESSA_DADOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_processa_dados INPUT.

* object created ?
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
*&      Module  M_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0100 INPUT.

  CHECK NOT o_badi_fdcb_subbas04 IS INITIAL.

  IF invfo-zz1_docdms_mih IS NOT INITIAL.
    invfo-zz1_docdms_mih = |{ invfo-zz1_docdms_mih WIDTH = 25 ALIGN = RIGHT PAD = '0' }|.
  ENDIF.

* put data to main screen
  CALL METHOD o_badi_fdcb_subbas04->put_data_to_screen_object
    EXPORTING
      im_invfo = invfo.

ENDMODULE.
