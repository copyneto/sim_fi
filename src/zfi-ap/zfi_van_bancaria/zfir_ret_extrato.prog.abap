*&---------------------------------------------------------------------*
*& Report zfir_ret_extrato
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_ret_extrato LINE-SIZE 1023.

SELECTION-SCREEN  BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_var TYPE variant OBLIGATORY.
SELECTION-SCREEN  END OF BLOCK b1.

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
*&---------------------------------------------------------------------*
  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report      = 'RFEBKA00'
      pop_up      = abap_true
    IMPORTING
      sel_variant = p_var
    EXCEPTIONS
      OTHERS      = 0.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  DATA(lt_return) = NEW zclfi_van_bancaria( )->processa_ret_extrato( p_var ).

  WRITE: sy-datum, sy-uzeit.
  LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
    WRITE: / <fs_return>-files, 050 <fs_return>-message.
  ENDLOOP.
