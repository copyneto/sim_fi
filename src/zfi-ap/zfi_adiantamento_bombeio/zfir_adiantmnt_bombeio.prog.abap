***********************************************************************
***                      © REDE SIM                                 ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: JOB - Sugestão Sol. Adiantamento Bombeio               *
*** AUTOR    : Alysson Anjos – META                                   *
*** FUNCIONAL: Lia Rocha – META                                       *
*** DATA     : 19/02/2024                                             *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES                                        *
***-------------------------------------------------------------------*
*** DATA      | AUTOR        | DESCRIÇÃO                              *
***-------------------------------------------------------------------*
***           |              |                                        *
***********************************************************************
*&---------------------------------------------------------------------*
*& Report ZFIR_ADIANTMNT_BOMBEIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_adiantmnt_bombeio.

DATA: gs_ekko TYPE ekko,
      gs_ekpo TYPE ekpo.

DATA: gv_eindt TYPE eket-eindt.

DATA: go_process TYPE REF TO zclfi_sugst_adiat_bombeio.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-p01.

  SELECT-OPTIONS: s_bukrs FOR gs_ekpo-bukrs OBLIGATORY,
                  s_werks FOR gs_ekpo-werks,
                  s_aedat FOR gs_ekko-aedat,
                  s_ebeln FOR gs_ekpo-ebeln,
                  s_ebelp FOR gs_ekpo-ebelp.

SELECTION-SCREEN END OF BLOCK b1.

SELECT-OPTIONS: s_eindt FOR gv_eindt.

INITIALIZATION.
  go_process = NEW zclfi_sugst_adiat_bombeio( ).

  GET REFERENCE OF s_bukrs[] INTO go_process->gs_refdata-bukrs.
  GET REFERENCE OF s_werks[] INTO go_process->gs_refdata-werks.
  GET REFERENCE OF s_aedat[] INTO go_process->gs_refdata-aedat.
  GET REFERENCE OF s_ebeln[] INTO go_process->gs_refdata-ebeln.
  GET REFERENCE OF s_ebelp[] INTO go_process->gs_refdata-ebelp.
  GET REFERENCE OF s_eindt[] INTO go_process->gs_refdata-eindt.

START-OF-SELECTION.
  go_process->main( ).

  IF sy-batch IS INITIAL.
    MESSAGE TEXT-001 TYPE 'S'.
  ENDIF.
