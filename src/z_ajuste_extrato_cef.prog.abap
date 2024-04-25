*&---------------------------------------------------------------------*
*& Include          Z_AJUSTE_EXTRATO_CEF
*&---------------------------------------------------------------------*
DATA: lv_num TYPE bkpf-belnr.
DATA: lv_absnd(15)  TYPE c.
DATA: ls_febko      TYPE febko.
"PROGRAMA PARA CORRIGIR DADOS DO EXTRATO DA CAIXA ECONOMICA
"BREAK-POINT.
IF febko-absnd(3) = '104'.
  febko-ktonr = febko-ktonr+1(9).
  lv_num = febko-ktonr.
  PACK lv_num TO lv_num.
  CONDENSE lv_num.
  febko-ktonr = lv_num.
  lv_absnd = febko-absnd(15).
  CONCATENATE lv_absnd '+++++++++++++++' INTO lv_absnd.
  CONCATENATE lv_absnd febko-ktonr INTO febko-absnd.
  REPLACE ALL OCCURRENCES OF `+` IN febko-absnd WITH ` `.

  "BREAK-POINT.
ENDIF.
*&---------------------------------------------------------------------*
"Exportar para utilizar no programa de carregar extratos
 "BREAK-POINT.
EXPORT ls_febko FROM febko
                     TO MEMORY ID 'LS_FEBKO'.
CLEAR:ls_febko.
*&---------------------------------------------------------------------*
