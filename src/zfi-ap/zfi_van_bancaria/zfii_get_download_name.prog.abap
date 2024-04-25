*&---------------------------------------------------------------------*
*& Include zfii_get_download_name
*&---------------------------------------------------------------------*
DATA: lv_van_bancaria  TYPE xfeld.

"Export executado no programa ZFIR_REM_PGTO
IMPORT lv_van_bancaria FROM MEMORY ID 'VAN_BANCARIA'.
IF  lv_van_bancaria = abap_true
AND sy-binpt        = abap_true.
  ok_code  = 'DOWN'.
  label    = regud-label.
  filename = rlgrap-filename.
  "Retorna sem mostrar tela para download,
  "pois o arquivo já foi selecionado
  EXIT.
ENDIF.
