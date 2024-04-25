FUNCTION zfmfi_dinheiro_cheque.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZI_CUSTCHEQUE_COMP_ICHEQUES OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"--------------------------------------------------------------------

  UPDATE ztfi_custcheq_cp
      SET dinheiro = is_key-dinheiro
      WHERE bukrs = is_key-bukrs
        AND kunnr = is_key-kunnr
        AND ncheque = is_key-ncheque.

  COMMIT WORK AND WAIT.

ENDFUNCTION.
