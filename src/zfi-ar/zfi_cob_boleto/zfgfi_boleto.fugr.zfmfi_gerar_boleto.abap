FUNCTION zfmfi_gerar_boleto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZSFI_BOLETO_BAN_KEY OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  DATA(lo_boleto) = NEW zclfi_boleto_util( ).

  lo_boleto->gerar_boleto(
    EXPORTING
      is_key = is_key
    IMPORTING
      et_msg     = et_return
  ).


ENDFUNCTION.
