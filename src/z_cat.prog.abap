*&---------------------------------------------------------------------*
*& Report Z_CAT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_CAT.

DATA: itab(300) TYPE c OCCURS 0 WITH HEADER LINE,
      str(80).

PARAMETERS pname LIKE trdir-name OBLIGATORY.

START-OF-SELECTION.

*  check sy-uname = 'ZZ9CNV'.

  CHECK NOT pname IS INITIAL.

  READ REPORT pname INTO itab.
  CHECK sy-subrc = 0.

  EDITOR-CALL FOR itab BACKUP INTO itab.

  IF sy-subrc = 0.

    IF NOT itab[] IS INITIAL.
      INSERT REPORT pname FROM itab.
      IF sy-subrc = 0.
        CONCATENATE 'PROGRAMA' pname 'GRAVADO.'
        INTO str SEPARATED BY space.
        WRITE: / str.
      ENDIF.
    ENDIF.

  ENDIF.
