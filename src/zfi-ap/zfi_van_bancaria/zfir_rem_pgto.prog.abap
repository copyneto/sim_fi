*&---------------------------------------------------------------------*
*& Report zfir_rem_pgto
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_rem_pgto LINE-SIZE 1023.

TABLES: reguh.

SELECTION-SCREEN  BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_laufd  FOR reguh-laufd NO-EXTENSION.
SELECTION-SCREEN  END OF BLOCK b1.

START-OF-SELECTION.
  DATA(lt_return) = NEW zclfi_van_bancaria( )->processa_remessa_pgto( s_laufd[] ).

  WRITE: sy-datum, sy-uzeit.
  LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
    WRITE: / <fs_return>-message.
  ENDLOOP.
