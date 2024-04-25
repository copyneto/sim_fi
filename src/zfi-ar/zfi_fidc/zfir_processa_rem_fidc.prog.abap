*&---------------------------------------------------------------------*
*& Report zfir_processa_rem_fidc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_processa_rem_fidc.

TABLES bseg.

*text-p03 = Opções de Processamento
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-p03.

  SELECT-OPTIONS: s_bukrs FOR bseg-bukrs,
                  s_belnr FOR bseg-belnr,
                  s_gjahr FOR bseg-gjahr.

SELECTION-SCREEN END OF BLOCK b1.

DATA: go_process TYPE REF TO zclfi_processa_fidc.

INITIALIZATION.

  go_process = NEW zclfi_processa_fidc( ).

  GET REFERENCE OF s_bukrs[] INTO go_process->gs_refdata-bukrs.
  GET REFERENCE OF s_belnr[] INTO go_process->gs_refdata-belnr.
  GET REFERENCE OF s_gjahr[] INTO go_process->gs_refdata-gjahr.

START-OF-SELECTION.

  go_process->check_error(
    EXCEPTIONS
      not_parameter = 1
      OTHERS        = 2
  ).
  IF sy-subrc <> 0.
    MESSAGE TEXT-e01 TYPE 'E'.
  ENDIF.

  go_process->processa_remessa( ).
