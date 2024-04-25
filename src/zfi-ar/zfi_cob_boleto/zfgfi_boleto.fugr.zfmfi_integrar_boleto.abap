FUNCTION zfmfi_integrar_boleto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_KEY) TYPE  ZSFI_BOLETO_BAN_KEY OPTIONAL
*"     VALUE(IS_BOLETO_INFO) TYPE  ZI_FI_BOLETO OPTIONAL
*"     VALUE(IV_DOCNUM) TYPE  J_1BNFDOC-DOCNUM OPTIONAL
*"     VALUE(IT_CONTENT) TYPE  BCST_ATTACHMENT OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  DATA(lt_content) = it_content.
  data(ls_info_boleto) = is_boleto_info.

  TYPES:
    BEGIN OF ty_file,
      filename TYPE string,
      value    TYPE string,
    END OF ty_file,

    ty_t_file TYPE TABLE OF ty_file WITH DEFAULT KEY,

    BEGIN OF ty_boleto,
      boletoDoumento TYPE string,
    END OF ty_boleto,

    BEGIN OF ty_payload,
      mandante             TYPE string,
      documentoTipo        TYPE string,
      data                 TYPE string,
      hora                 TYPE string,
      bp                   TYPE string,
      empresa              TYPE string,
      filial               TYPE string,
      documentoContabil    TYPE string,
      documentoContabilAno TYPE string,
      ordemVenda           TYPE string,
      valor                TYPE string,
      boleto               TYPE ty_boleto,
      file                 TYPE ty_t_file,
    END OF ty_payload.

  IF ( it_content IS INITIAL ).
    DATA(lo_boleto) = NEW zclfi_boleto_util( ).

    SELECT *
      FROM zi_fi_boleto
      INTO @ls_info_boleto
     WHERE Documento     = @is_key-belnr
       AND Exercicio     = @is_key-gjahr
       AND Empresa       = @is_key-bukrs
        OR BR_NotaFiscal = @iv_docnum
        ORDER BY Parcela.

      CALL METHOD lo_boleto->visualizar_boleto_app
        EXPORTING
          is_boletos     = VALUE #(
                bukrs = ls_info_boleto-Empresa
                belnr = ls_info_boleto-Documento
                gjahr = ls_info_boleto-Exercicio
                buzei = ls_info_boleto-Parcela
              )
        IMPORTING
          ev_pdf_file    = DATA(lv_file_content)
          ev_boleto_name = DATA(lv_file_name).

      CALL METHOD cl_http_utility=>if_http_utility~encode_x_base64
        EXPORTING
          unencoded = lv_file_content
        RECEIVING
          encoded   = DATA(lv_boleto_base64).

      APPEND VALUE #(
        filename     = lv_file_name
        contents_txt = lv_boleto_base64
      ) TO lt_content.
    ENDSELECT.
  ENDIF.

  IF ls_info_boleto IS INITIAL.
    SELECT SINGLE *
      FROM zi_fi_boleto
      INTO @ls_info_boleto
     WHERE Documento     = @is_key-belnr
       AND Exercicio     = @is_key-gjahr
       AND Empresa       = @is_key-bukrs
        OR BR_NotaFiscal = @iv_docnum.
  ENDIF.

  CHECK lt_content IS NOT INITIAL.

  DATA(lo_cpi) = NEW zclca_cpi( ).
  DATA(ls_payload) = VALUE ty_payload(
    mandante               = sy-mandt
    documentotipo          = ls_info_boleto-TipoDoc
    data                   = |{ ls_info_boleto-authdate DATE = USER }|
    hora                   = |{ ls_info_boleto-authtime TIME = USER }|
    bp                     = ls_info_boleto-Cliente
    empresa                = ls_info_boleto-Empresa
    filial                 = ls_info_boleto-BusinessPlace
    documentocontabil      = ls_info_boleto-Documento
    documentocontabilano   = ls_info_boleto-Exercicio
    ordemvenda             = ls_info_boleto-SalesOrder
    valor                  = ls_info_boleto-Valor
    boleto                 = VALUE #( boletodoumento = ls_info_boleto-NossoNumero )
    file                   = VALUE #( FOR ls_content IN lt_content (
      filename = ls_content-filename value = ls_content-contents_txt
    ) )
  ).

  CALL METHOD lo_cpi->send
    EXPORTING
      iv_processo  = gc_cpi-processo
      iv_metodo    = gc_cpi-method_post
      is_structure = ls_payload
    IMPORTING
      ev_result    = DATA(lv_return)
      et_return    = DATA(lt_return_cpi).

  IF ( lt_return_cpi IS NOT INITIAL ).
    APPEND VALUE #(
      id         = gc_message_id
      type       = 'E'
      number     = 000
      message_v1 = ls_info_boleto-Documento
      message_v2 = ls_info_boleto-Parcela
    ) TO et_return.

  ELSE.

    APPEND VALUE #(
      id         = gc_message_id
      type       = 'S'
      number     = 001
      message_v1 = ls_info_boleto-Documento
      message_v2 = ls_info_boleto-Parcela
    ) TO et_return.

  ENDIF.

  "@ Save logs
  data(lv_log_id) = conv balnrext( ls_info_boleto-NossoNumero ).
  CALL FUNCTION 'ZFMCA_ADD_LOG'
    EXPORTING
      iv_ext_number = lv_log_id
      iv_object     = gc_log_object
      iv_subobject  = gc_log_subobject
      iv_old_delete = ls_info_boleto-LogHandle
      it_return     = et_return.

  "@ Save CPI logs
  DATA(lo_cpi_monitor) = NEW zclca_monitor_cpi( ).
  DATA(lv_json_in) = /ui2/cl_json=>serialize( data = ls_payload ).

  CALL METHOD lo_cpi_monitor->started_process
    EXPORTING
      iv_processo  = gc_cpi-processo
      iv_metodo    = gc_cpi-method_post
      iv_chave_ref = CONV #( ls_info_boleto-NossoNumero )
      iv_json      = lv_json_in.

  CALL METHOD lo_cpi_monitor->save_log
    EXPORTING
      iv_processo     = gc_cpi-processo
      iv_metodo       = gc_cpi-method_post
      iv_json_retorno = lv_return
      iv_json         = lv_json_in
      it_return       = et_return.
ENDFUNCTION.
