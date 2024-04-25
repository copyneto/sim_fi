FUNCTION zfmfi_contab_cheque.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZTFI_CUST_CHEQUE OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  NEW zclfi_custodia_cheque( )->contab_cheque(
    EXPORTING
      is_key    = is_key
    IMPORTING
     et_return = et_return
  ).

ENDFUNCTION.
