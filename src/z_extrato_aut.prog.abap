*&---------------------------------------------------------------------*
*& Include          Z_EXTRATO_AUT
*&---------------------------------------------------------------------*
DATA: lv_auszugfile TYPE rlgrap-filename.
"IF sy-cprog = 'ZFEB_FILE_HANDLING'.
IMPORT lv_auszugfile  TO lv_auszugfile
               FROM MEMORY ID 'LV_EXTRATO'.
IF sy-uname = 'T_RSOUSA'.
 " BREAK-POINT.
ENDIF.
IF NOT lv_auszugfile IS INITIAL .
  auszugfile = lv_auszugfile.
  upload = space.
ENDIF.

FREE MEMORY ID 'LV_EXTRATO'.
"BREAK-POINT.
"ENDIF.
