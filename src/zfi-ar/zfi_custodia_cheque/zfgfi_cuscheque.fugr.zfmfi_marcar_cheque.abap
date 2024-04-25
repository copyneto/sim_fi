FUNCTION zfmfi_marcar_cheque.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZI_CUSTCHEQUE_COMP_ICHEQUES OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  SELECT SINGLE atribuido
    FROM ztfi_custcheq_cp
    INTO @DATA(lv_atribuido)
    WHERE bukrs = @is_key-bukrs
      AND kunnr = @is_key-kunnr
      AND ncheque = @is_key-ncheque.

  IF sy-subrc = 0.

    IF lv_atribuido = abap_true.

      UPDATE ztfi_custcheq_cp
          SET atribuido = abap_false
          WHERE bukrs = is_key-bukrs
      AND kunnr = is_key-kunnr
      AND ncheque = is_key-ncheque.

    ELSE.

      UPDATE ztfi_custcheq_cp
     SET atribuido = abap_true
         WHERE bukrs = is_key-bukrs
     AND kunnr = is_key-kunnr
     AND ncheque = is_key-ncheque.

    ENDIF.

    COMMIT WORK AND WAIT.

  ENDIF.


ENDFUNCTION.
