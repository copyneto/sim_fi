FUNCTION zfmfi_marcar_fatura.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZI_CUSTCHEQUE_COMP_IFAT OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  SELECT SINGLE atribuido
    FROM ztfi_custcheq_ft
    INTO @DATA(lv_atribuido)
    WHERE bukrs = @is_key-bukrs
      AND kunnr = @is_key-kunnr
      AND raizcnpj = @is_key-raizcnpj
      AND doc        = @is_key-doc
      AND gjahr      = @is_key-gjahr
      AND buzei       = @is_key-buzei.

  IF sy-subrc = 0.

    IF lv_atribuido = abap_true.

      UPDATE ztfi_custcheq_ft
          SET atribuido = @abap_false
          WHERE bukrs = @is_key-bukrs
      AND bukrs = @is_key-bukrs
      AND kunnr = @is_key-kunnr
      AND raizcnpj = @is_key-raizcnpj
      AND doc        = @is_key-doc
      AND gjahr      = @is_key-gjahr
      AND buzei       = @is_key-buzei.

    ELSE.

      UPDATE ztfi_custcheq_ft
     SET atribuido = @abap_true
         WHERE bukrs = @is_key-bukrs
     AND bukrs = @is_key-bukrs
      AND kunnr = @is_key-kunnr
      AND raizcnpj = @is_key-raizcnpj
      AND doc        = @is_key-doc
      AND gjahr      = @is_key-gjahr
      AND buzei       = @is_key-buzei.

    ENDIF.

    COMMIT WORK AND WAIT.

  ENDIF.


ENDFUNCTION.
