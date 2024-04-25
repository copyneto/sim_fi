FUNCTION zfmfi_cheques_compensar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZI_CUSTCHEQUE_COMP_H OPTIONAL
*"     VALUE(IT_FATURAS) TYPE  ZCTGFI_CUSTCHEQ_FT OPTIONAL
*"     VALUE(IT_CHEQUES) TYPE  ZCTGFI_CUSTCHEQ_CP OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  NEW zclfi_compensa_cheques(
    it_faturas = it_faturas
    it_cheques = it_cheques
  )->main(
    IMPORTING
      et_return      = et_return
  ).


  NEW zclfi_custodia_cheque( )->get_cheque_comp(
  EXPORTING
    is_key    = is_key
  IMPORTING
    et_return = et_return
).


ENDFUNCTION.
