FUNCTION zfmfi_envia_email.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZSFI_BOLETO_BAN_KEY OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA(lo_boleto) = NEW zclfi_boleto_util( ).
  lo_boleto->envia_email(
       EXPORTING
         is_key  = is_key
       IMPORTING
         et_msg  = et_return
     ).

  COMMIT WORK AND WAIT.

ENDFUNCTION.
