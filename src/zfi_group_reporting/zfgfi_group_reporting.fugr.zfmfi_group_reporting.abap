FUNCTION zfmfi_group_reporting.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_RLDNR) TYPE  ACDOCU-RLDNR
*"     VALUE(IV_RDIMEN) TYPE  ACDOCU-RDIMEN
*"     VALUE(IV_GJAHR) TYPE  ACDOCU-RYEAR
*"     VALUE(IV_RVERS) TYPE  ACDOCU-RVERS
*"     VALUE(IV_POPER) TYPE  ACDOCU-POPER
*"     VALUE(IV_RITCLG) TYPE  ACDOCU-RITCLG
*"     VALUE(IV_RBUNIT) TYPE  ACDOCU-RBUNIT
*"  EXPORTING
*"     VALUE(ET_ACDOCU) TYPE  ZCTFI_ACDOCU
*"----------------------------------------------------------------------
  SELECT * FROM acdocu
  WHERE rldnr = @iv_rldnr
  AND       rdimen = @iv_rdimen
   AND      ryear = @iv_gjahr
    AND     rvers = @iv_rvers
    AND     poper = @iv_poper
   AND      ritclg  = @iv_ritclg
    AND     rbunit = @iv_rbunit
    INTO  TABLE @et_acdocu.



ENDFUNCTION.
