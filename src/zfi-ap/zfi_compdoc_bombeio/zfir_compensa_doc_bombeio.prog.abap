***********************************************************************
***                      © REDE SIM                                 ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: JOB - Compensar Documentos de Fornecedor               *
*** AUTOR    : Alysson Anjos – META                                   *
*** FUNCIONAL: Lia Rocha – META                                       *
*** DATA     : 21/02/2024                                             *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES                                        *
***-------------------------------------------------------------------*
*** DATA      | AUTOR        | DESCRIÇÃO                              *
***-------------------------------------------------------------------*
***           |              |                                        *
***********************************************************************
*&---------------------------------------------------------------------*
*& Report ZFIR_COMPENSA_DOC_BOMBEIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_compensa_doc_bombeio.

DATA: gs_bsik  TYPE bsik_view.
DATA: gv_werks TYPE werks_d.

DATA: go_process TYPE REF TO zclfi_comp_doc_bombeio.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-p01.

  SELECT-OPTIONS: s_bukrs FOR gs_bsik-bukrs OBLIGATORY,
                  s_werks FOR gv_werks,
                  s_belnr FOR gs_bsik-belnr,
                  s_gjahr FOR gs_bsik-gjahr,
                  s_buzei FOR gs_bsik-buzei,
                  s_lifnr FOR gs_bsik-lifnr.

SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS: p_bomb  TYPE char1 RADIOBUTTON GROUP g1,
            p_todos TYPE char1 RADIOBUTTON GROUP g1 MODIF ID pt.




INITIALIZATION.

  LOOP AT SCREEN.
    IF screen-name = 'P_TODOS'.
      screen-invisible = 1.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


  go_process = NEW zclfi_comp_doc_bombeio( ).

  GET REFERENCE OF s_bukrs[] INTO go_process->gs_refdata-bukrs.
  GET REFERENCE OF s_werks[] INTO go_process->gs_refdata-werks.
  GET REFERENCE OF s_belnr[] INTO go_process->gs_refdata-belnr.
  GET REFERENCE OF s_gjahr[] INTO go_process->gs_refdata-gjahr.
  GET REFERENCE OF s_buzei[] INTO go_process->gs_refdata-buzei.
  GET REFERENCE OF s_lifnr[] INTO go_process->gs_refdata-lifnr.
  GET REFERENCE OF p_bomb    INTO go_process->gs_param-bomb.
  GET REFERENCE OF p_todos   INTO go_process->gs_param-todos.

START-OF-SELECTION.
  go_process->main( ).

  IF sy-batch IS INITIAL.
    MESSAGE TEXT-001 TYPE 'S'.
  ENDIF.
