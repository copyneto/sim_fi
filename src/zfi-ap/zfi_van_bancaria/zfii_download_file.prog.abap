*&---------------------------------------------------------------------*
*& Include zfii_download_file
*&---------------------------------------------------------------------*
DATA: lv_van_bancaria  TYPE xfeld.

"Export executado no programa ZFIR_REM_PGTO
IMPORT lv_van_bancaria FROM MEMORY ID 'VAN_BANCARIA'.
IF  lv_van_bancaria = abap_true
AND sy-binpt        = abap_true.
  tab_regut-dwnam = NEW zclfi_van_bancaria(  )->altera_dir_download( EXPORTING iv_laufd = tab_regut-laufd
                                                                               iv_laufi = tab_regut-laufi ).
ENDIF.
