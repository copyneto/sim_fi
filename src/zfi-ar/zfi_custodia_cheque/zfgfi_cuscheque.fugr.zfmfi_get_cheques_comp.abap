FUNCTION zfmfi_get_cheques_comp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZI_CUSTCHEQUE_COMP_H OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  NEW zclfi_custodia_cheque( )->get_cheque_comp(
    EXPORTING
      is_key    = is_key
    IMPORTING
      et_return = et_return
  ).

ENDFUNCTION.
