CLASS zclfi_form_boleto DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Construtor
    "! @parameter is_doc | Documentos a processar
    METHODS constructor
      IMPORTING
        !is_doc    TYPE zsfi_boleto_banc_form
        iv_printer TYPE rspopname OPTIONAL.

    "! Executa o processamento
    "! @parameter rt_otf | OTF do form
    METHODS execute
      IMPORTING
        !iv_teste     TYPE char1 OPTIONAL
      RETURNING
        VALUE(rt_otf) TYPE tsfotf .
    METHODS teste .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Constante para local de pagamento
    CONSTANTS gc_local TYPE string VALUE 'Pagável em qualquer banco' ##NO_TEXT.
    "! Dados do boleto
    DATA gs_boleto TYPE zsfi_boleto .
    "! Dados do documento
    DATA gs_doc TYPE zsfi_boleto_banc_form .
    "! Especie
    CONSTANTS gc_especie_doc TYPE char2 VALUE 'DM' ##NO_TEXT.
    "! Moeda
    CONSTANTS gc_especie TYPE char2 VALUE 'R$' ##NO_TEXT.
    "! Aceite negativo
    CONSTANTS gc_aceite TYPE char1 VALUE 'N' ##NO_TEXT.

    DATA gv_form    TYPE tdsfname .
    DATA gv_printer TYPE rsponame.

    "! Formata CNPJ
    "! @parameter iv_cnpj | CNPJ
    "! @parameter rv_cnpj | CNPJ formatado
    METHODS format_cnpj
      IMPORTING
        !iv_cnpj       TYPE j_1bcgc
      RETURNING
        VALUE(rv_cnpj) TYPE char18 .
    "! Busca dados do cliente
    "! @parameter iv_cliente | Cliente
    METHODS get_customer_data
      IMPORTING
        !iv_cliente TYPE kunnr .
    "! Converte data
    "! @parameter iv_datum | Data
    "! @parameter rv_data  | Data convertida
    METHODS conv_data
      IMPORTING
        !iv_datum      TYPE datum
      RETURNING
        VALUE(rv_data) TYPE char10 .
    "! Formata linha digitavel
    "! @parameter is_linha | Linha dig.
    "! @parameter rv_linha | Linha dig. convertida
    METHODS format_linhadig
      IMPORTING
        !is_linha       TYPE ze_linha_dig
      RETURNING
        VALUE(rv_linha) TYPE char60 .
    "! Busca dados
    METHODS get_data .
    "! Busca função
    "! @parameter rv_func | Retorna a função
    METHODS get_function
      RETURNING
        VALUE(rv_func) TYPE rs38l_fnam .
    METHODS call_form_test
      RETURNING
        VALUE(rt_otf) TYPE tsfotf .
    "! Executa o formulário
    "! @parameter rt_otf | Retorna o OTF
    METHODS call_form
      RETURNING
        VALUE(rt_otf) TYPE tsfotf .
    "! Busca dados da companhia
    METHODS get_company_data .
    "! Atribui dac mod11
    "! @parameter iv_xref3 | Nosso nro
    "! @parameter rv_dac   | DAC
    METHODS set_dac_mod11
      IMPORTING
        !iv_xref3     TYPE char12       "zsfi_boleto_banc_form-nossonumero
      RETURNING
        VALUE(rv_dac) TYPE char1 .
    "! Atribui Nosso nro com 12 caracteres
    "! @parameter iv_nn | Nosso nro
    "! @parameter rv_nn_12   | Nosso nro com 12 caracteres
    METHODS set_nn_12
      IMPORTING
        iv_nn           TYPE zsfi_boleto_banc_form-nossonumero
      RETURNING
        VALUE(rv_nn_12) TYPE char12 .
    METHODS get_instrucao_bol.
ENDCLASS.



CLASS ZCLFI_FORM_BOLETO IMPLEMENTATION.


  METHOD call_form.

    DATA:
      ls_ctrlop TYPE ssfctrlop,
      ls_compop TYPE ssfcompop,
      ls_otf    TYPE ssfcrescl.

    DATA(lv_function) = get_function( ).

    "//Get OTF data
    ls_ctrlop-getotf    = 'X'.
    ls_ctrlop-no_dialog = 'X'.
    ls_compop-tdnoprev  = 'X'.
    ls_compop-tddest    = 'LP01'.
    ls_compop-tdnewid   = 'X'.

    CALL FUNCTION lv_function
      EXPORTING
        control_parameters = ls_ctrlop
        output_options     = ls_compop
        user_settings      = ' '
        is_boleto          = gs_boleto
      IMPORTING
        job_output_info    = ls_otf
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc = 0.
      rt_otf = ls_otf-otfdata[].                          "#EC CI_SUBRC
    ENDIF.

    "//Send boleto directly to local printer
    IF me->gv_printer IS NOT INITIAL.
      CLEAR: ls_compop, ls_ctrlop.

      ls_compop-tdimmed   = abap_true.
      ls_compop-tddest    = me->gv_printer.
      ls_ctrlop-no_dialog = 'X'.
      ls_ctrlop-device    = 'PRINTER'.
      ls_ctrlop-no_dialog = 'X'.

      CALL FUNCTION lv_function
        EXPORTING
          control_parameters = ls_ctrlop
          output_options     = ls_compop
          user_settings      = ' '
          is_boleto          = gs_boleto
        IMPORTING
          job_output_info    = ls_otf
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD call_form_test.

*    DATA: ls_ctrlop TYPE ssfctrlop,
*          ls_compop TYPE ssfcompop,
*          ls_otf    TYPE ssfcrescl.
*
*    DATA(lv_function) = get_function( ).
*
**    ls_ctrlop-getotf    = 'X'.
**    ls_ctrlop-no_dialog = 'X'.
**    ls_compop-tdnoprev  = 'X'.
*    ls_compop-tddest    = 'LP01'.
*    ls_compop-tdnewid   = 'X'.
*
*    CALL FUNCTION lv_function
*      EXPORTING
*        control_parameters = ls_ctrlop
*        output_options     = ls_compop
*        user_settings      = ' '
*        is_boleto          = gs_boleto
**      IMPORTING
**       job_output_info    = ls_otf
*      EXCEPTIONS
*        formatting_error   = 1
*        internal_error     = 2
*        send_error         = 3
*        user_canceled      = 4
*        OTHERS             = 5.
*
*    IF sy-subrc = 0.
**      rt_otf = ls_otf-otfdata[].
*    ENDIF.
    RETURN.
  ENDMETHOD.


  METHOD constructor.

    gs_doc = is_doc.
    gv_printer = iv_printer.

  ENDMETHOD.


  METHOD conv_data.

    rv_data = |{ iv_datum+6(2) }/{ iv_datum+4(2) }/{ iv_datum(4) }|.

  ENDMETHOD.


  METHOD execute.

    get_data( ).

    IF iv_teste IS INITIAL.
      rt_otf = call_form( ).
    ELSE.
      call_form_test( ).
    ENDIF.

  ENDMETHOD.


  METHOD format_cnpj.

    rv_cnpj = |{ iv_cnpj(2) }.{ iv_cnpj+2(3) }.{ iv_cnpj+5(3) }/{ iv_cnpj+8(4) }-{ iv_cnpj+12(2) }|.

  ENDMETHOD.


  METHOD format_linhadig.

    rv_linha = |{ is_linha(5) }.{ is_linha+5(5) } { is_linha+10(5) }.{ is_linha+15(6) } { is_linha+21(5) }| &&
               |.{ is_linha+26(6) } { is_linha+32(1) } { is_linha+33(14) }|.

  ENDMETHOD.


  METHOD get_company_data.

    DATA ls_addr TYPE bapi0002_3.
    DATA lv_cnpj TYPE j_1bwfield-cgc_number.

    CALL FUNCTION 'BAPI_COMPANYCODE_GETDETAIL'
      EXPORTING
        companycodeid       = gs_doc-bukrs
      IMPORTING
        companycode_address = ls_addr.

    CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
      EXPORTING
        bukrs      = gs_doc-bukrs
      IMPORTING
        cgc_number = lv_cnpj.


    gs_boleto-nome_cedente   = ls_addr-name.
    gs_boleto-cnpj_cedente   = format_cnpj( lv_cnpj ).
    gs_boleto-end_cedente    = ls_addr-street .
    gs_boleto-numero_cedente = ls_addr-house_no.
    gs_boleto-bairro_cedente = ls_addr-district.
    gs_boleto-cep_cedente    = ls_addr-postl_cod1.
    gs_boleto-cidade_cedente = ls_addr-city.
    gs_boleto-estado_cedente = ls_addr-region.

  ENDMETHOD.


  METHOD get_customer_data.

    DATA lv_customer TYPE bapicustomer_id-customer.
    DATA lv_cnpj TYPE j_1bcgc.
    DATA ls_addr TYPE bapicustomer_04.
    DATA ls_gen TYPE bapicustomer_kna1.

    lv_customer = iv_cliente.

    CALL FUNCTION 'BAPI_CUSTOMER_GETDETAIL2'
      EXPORTING
        customerno            = lv_customer
        companycode           = gs_doc-bukrs
      IMPORTING
        customeraddress       = ls_addr
        customergeneraldetail = ls_gen.

    lv_cnpj = ls_gen-tax_no_1.

    gs_boleto-nome_sacado   = ls_addr-name.
    gs_boleto-cnpj_sacado   = format_cnpj( lv_cnpj ).
    gs_boleto-end_sacado    = ls_addr-street.
    gs_boleto-bairro_sacado = ls_addr-district.
    gs_boleto-cidade_sacado = ls_addr-city.
    gs_boleto-estado_sacado = ls_addr-region.
    gs_boleto-cep_sacado    = ls_addr-postl_code.

  ENDMETHOD.


  METHOD get_data.
    DATA:
      lr_local_pagamento TYPE rsis_t_range,
      lv_dac             TYPE char1,
      lv_12              TYPE char12.

    CONSTANTS:
      BEGIN OF lc_boleto,
        itau       TYPE char3 VALUE 'ITA' ##NO_TEXT,
        cef        TYPE char3 VALUE 'CEF' ##NO_TEXT,
        banrisul   TYPE hbkid VALUE 'BRS' ##NO_TEXT,
        bbrasil    TYPE hbkid VALUE 'BB0' ##NO_TEXT,
        bsantander TYPE hbkid VALUE 'SAN' ##NO_TEXT,
        sicoob     TYPE hbkid VALUE 'SCB' ##NO_TEXT,
        modulo     TYPE zi_ca_param_val-Modulo VALUE 'FI-AR',
        chave1     TYPE zi_ca_param_val-chave1 VALUE 'BOLETO',
        local_pgto TYPE zi_ca_param_val-Chave2 VALUE 'LOCALPAGAMENTO',
      END OF lc_boleto.

    TRY.
        NEW zclca_tabela_parametros( )->m_get_range(
          EXPORTING
            iv_modulo = lc_boleto-modulo "zclfi_boleto_util=>gc_boleto-modulo
            iv_chave1 = lc_boleto-chave1 "zclfi_boleto_util=>gc_boleto-chave1
            iv_chave2 = lc_boleto-local_pgto "zclfi_boleto_util=>gc_boleto-chave_local_pgto
          IMPORTING
            et_range  = lr_local_pagamento
        ).
      CATCH zcxca_tabela_parametros.
    ENDTRY.

    SELECT SINGLE * FROM zi_fi_dados_boleto
      INTO @DATA(ls_dados)
      WHERE empresa   = @gs_doc-bukrs
        AND documento = @gs_doc-belnr
        AND exercicio = @gs_doc-gjahr
        AND item      = @gs_doc-buzei
        AND codbanco  = @gs_doc-banco.

    IF sy-subrc = 0.

*      gs_boleto-chave_banco     = ls_dados-chavebanco.
      gs_boleto-loc_pagamento   = VALUE #( lr_local_pagamento[ low = ls_dados-Empresa && ls_dados-CodBanco ]-high OPTIONAL ).
      IF gs_boleto-loc_pagamento IS INITIAL.
        gs_boleto-loc_pagamento = gc_local.
      ENDIF.

      gs_boleto-vencimento      = conv_data( ls_dados-vencimento ).

*      IF gs_doc-banco(3) = 'ITA'.
*        DATA(lv_pos) = strlen( ls_dados-agenciasemcod ) - 4.
*        gs_boleto-agencia_cod     = |{ ls_dados-agenciasemcod+lv_pos(4) }/{ ls_dados-conta }|.
*        gs_boleto-nosso_numero    = |{ ls_dados-carteira }/{ gs_doc-nossonumero }| .
*      ELSE.
*        gs_boleto-agencia_cod     = |{ ls_dados-agencia }/{ ls_dados-conta }|.
*        gs_boleto-nosso_numero    = gs_doc-nossonumero.
*      ENDIF.


      CASE gs_doc-banco(3).

        WHEN lc_boleto-itau.

          SELECT SINGLE portfolio,
                        register_form,
                        document_type
                  FROM idfipaym_ccha
                  INTO @DATA(ls_ccha)
                  WHERE bukrs = @ls_dados-empresa
                  AND   hbkid = @ls_dados-codbanco
                  AND   hktid = @ls_dados-carteira
                  AND   rzawe = 'E'.

          gs_boleto-carteira        = |{ ls_ccha-portfolio }{ ls_ccha-register_form }{ ls_ccha-document_type }|.

          gs_boleto-chave_banco     = ls_dados-chavebanco.
          DATA(lv_pos) = strlen( ls_dados-agenciasemcod ) - 4.

          IF ls_dados-conta CS '-'.

            DATA(lv_conta) = ls_dados-conta.

          ELSE.

            lv_conta = ls_dados-conta && '-' && ls_dados-digito+1(1).

          ENDIF.

          gs_boleto-agencia_cod     = |{ ls_dados-agenciasemcod+lv_pos(4) }/{ lv_conta }|.
          gs_boleto-nosso_numero    = |{ ls_dados-carteira }/{ gs_doc-nossonumero }| && '-' && gs_doc-cod_barras+30(1). " linha_dig+9(1).

        WHEN lc_boleto-bsantander.

***          DATA(lv_tvarv) = get_tvarv( gs_doc-bukrs ).

          gs_boleto-chave_banco     = ls_dados-chavebanco.
*          gs_boleto-agencia_cod     = |{ ls_dados-agenciasemcod+lv_pos(4) }/{ ls_dados-conta }|.
          gs_boleto-agencia_cod     = |{ ls_dados-agencia }/{ ls_dados-contadig }|.
          gs_boleto-nosso_numero    = gs_doc-nossonumero.

        WHEN lc_boleto-bbrasil.

*          lv_12 = set_nn_12( gs_doc-nossonumero ).
*          lv_dac = set_dac_mod11( lv_12 ).

          SELECT SINGLE portfolio,
                        register_form,
                        document_type
                  FROM idfipaym_ccha
                  INTO @ls_ccha
                  WHERE bukrs = @ls_dados-empresa
                  AND   hbkid = @ls_dados-codbanco
                  AND   hktid = @ls_dados-carteira
                  AND   rzawe = 'E'.

          gs_boleto-carteira        = |{ ls_ccha-portfolio }{ ls_ccha-register_form }|.

          gs_boleto-chave_banco     = ls_dados-chavebanco.
          gs_boleto-agencia_cod     = |{ ls_dados-agencia }/{ ls_dados-contadig }|.
          gs_boleto-nosso_numero    = gs_doc-nossonumero.

        WHEN lc_boleto-sicoob.

          gs_boleto-chave_banco     = ls_dados-chavebanco.
          gs_boleto-agencia_cod     = |{ ls_dados-agencia }/782084|.
          gs_boleto-nosso_numero    = gs_doc-cod_barras+33(08)."gs_doc-nossonumero.
          gs_boleto-valor_cobrado   = gs_doc-valor.

        WHEN lc_boleto-banrisul.

          SELECT SINGLE portfolio,
                        register_form,
                        document_type
                  FROM idfipaym_ccha
                  INTO @ls_ccha
                  WHERE bukrs = @ls_dados-empresa
                  AND   hbkid = @ls_dados-codbanco
                  AND   hktid = @ls_dados-carteira
                  AND   rzawe = 'E'.

          gs_boleto-carteira        = ls_ccha-portfolio.

          gs_boleto-chave_banco     = ls_dados-chavebanco.

          "--- Convenio ------
          "Select BSEG realizado para buscar forma de pagamento - 25.02.2022
          SELECT SINGLE zlsch
            FROM bseg
            INTO @DATA(lv_zlsch)
            WHERE bukrs EQ @ls_dados-empresa
              AND belnr EQ @ls_dados-documento
              AND gjahr EQ @ls_dados-exercicio
              AND buzei EQ @ls_dados-item.

          SELECT SINGLE dtaid
            FROM t045t
            INTO @DATA(lv_convenio)
            WHERE bukrs = @ls_dados-empresa
              AND hbkid = @ls_dados-codbanco
              AND zlsch EQ @lv_zlsch.

          IF sy-subrc = 0.
            gs_boleto-agencia_cod     = |{ ls_dados-agencia }{ lv_convenio+4 }|.
          ENDIF.


          gs_boleto-nosso_numero    = gs_doc-nossonumero.

        WHEN lc_boleto-cef.

          SELECT SINGLE portfolio,
                        register_form,
                        document_type
                  FROM idfipaym_ccha
                  INTO @ls_ccha
                  WHERE bukrs = @ls_dados-empresa
                  AND   hbkid = @ls_dados-codbanco
                  AND   hktid = @ls_dados-carteira
                  AND   rzawe = 'E'.

          gs_boleto-carteira        = ls_ccha-portfolio.

          gs_boleto-chave_banco     = ls_dados-chavebanco.
          gs_boleto-agencia_cod     = |{ ls_dados-agencia }{ ls_dados-conta }|.
          gs_boleto-nosso_numero    = gs_doc-nossonumero.

        WHEN OTHERS.

          gs_boleto-chave_banco     = ls_dados-chavebanco.
          gs_boleto-agencia_cod     = |{ ls_dados-agencia }{ ls_dados-conta }|.
          gs_boleto-nosso_numero    = gs_doc-nossonumero.

      ENDCASE.

      CASE gs_doc-banco(3).
        WHEN '041'.

        WHEN '001'.

        WHEN '104'.

        WHEN '341'.

      ENDCASE.


      gs_boleto-linha_digitavel = format_linhadig( gs_doc-linha_dig ).
      gs_boleto-data_doc        = conv_data( ls_dados-datadocumento ).
      gs_boleto-data_process    = conv_data( sy-datum ).
      gs_boleto-valor_doc       = gs_doc-valor.
      gs_boleto-valor_cobrado   = gs_doc-valor - gs_doc-valorabatimento.
      gs_boleto-desconto_abat   = gs_doc-valorabatimento.
      gs_boleto-cod_barras      = gs_doc-cod_barras.
      gs_boleto-especie_doc     = gc_especie_doc.
      gs_boleto-aceite          = gc_aceite.
      gs_boleto-especie         = gc_especie.
*      gs_boleto-carteira        = ls_dados-carteira.

      SPLIT gs_doc-xblnr AT '-' INTO DATA(lv_xblnr) DATA(lv_rest).

      DATA(lv_lenx) = strlen( lv_xblnr ).

*      IF lv_lenx = 9.
*        gs_boleto-num_doc = lv_xblnr && gs_doc-buzei.
*      ELSEIF lv_lenx > 9.
*        gs_boleto-num_doc = lv_xblnr(9) && gs_doc-buzei.
*      ELSE.
*        gs_boleto-num_doc = lv_xblnr(lv_lenx) && gs_doc-buzei.
*      ENDIF.

****      gs_boleto-num_doc = lv_xblnr && lv_rest.
      gs_boleto-num_doc = lv_xblnr.

    ENDIF.

    get_company_data( ).
    get_customer_data( ls_dados-cliente ).


    get_instrucao_bol(  ).




  ENDMETHOD.


  METHOD get_function.

    CONSTANTS lc_form TYPE tdsfname VALUE 'ZSFFI_BOLETO'.
    CONSTANTS lc_form_brs TYPE tdsfname VALUE 'ZSFFI_BOLETO_BANRISUL'.
    CONSTANTS lc_form_bb TYPE tdsfname VALUE 'ZSFFI_BOLETO_BB'.

    IF gs_boleto-cod_barras(3) = '041'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = lc_form_brs
        IMPORTING
          fm_name            = rv_func
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ELSEIF gs_boleto-cod_barras(3) = '001'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = lc_form_bb
        IMPORTING
          fm_name            = rv_func
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ELSE.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = lc_form
        IMPORTING
          fm_name            = rv_func
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD set_dac_mod11.

    DATA: lv_string TYPE string,
          lv_dac    TYPE string.

    DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

    lv_string = iv_xref3.
    lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = lv_dac CHANGING cv_calc = lv_string ).

    rv_dac = lv_dac.

  ENDMETHOD.


  METHOD set_nn_12.

    DATA lv_12 TYPE char12.

    lv_12 = iv_nn.

    SHIFT lv_12 RIGHT DELETING TRAILING space.

    rv_nn_12 = |{ lv_12  ALPHA = IN }|.


  ENDMETHOD.


  METHOD teste.

*    DATA: is_boleto TYPE zsfi_boleto_banc_form.
*
*    is_boleto-bukrs       = '1410'.
*    is_boleto-belnr       = '1800000091'.
*    is_boleto-gjahr       = '2022'.
*    is_boleto-buzei       = '001'.
*    is_boleto-nossonumero = '40353-9'.
*    is_boleto-cod_barras  = '34194890200098623671090004035391625403504000'.
*    is_boleto-linha_dig   = '341910900804035391624 54035040002489020009862367'.
*    is_boleto-empresatxt  = ''.
*    is_boleto-valor       = '98623.67'.
*    is_boleto-zlsch       = ''.
*    is_boleto-xblnr       = ''.
*    is_boleto-banco       = 'ITA02'.
*
*    NEW zclfi_form_boleto( is_boleto )->execute( abap_true ).

    RETURN.

  ENDMETHOD.


  METHOD get_instrucao_bol.

    DATA lv_juros TYPE dmbtr.
    DATA lv_valor_jur TYPE dmbtr.
    DATA lv_valor_escrito TYPE char30.
    DATA lr_texto TYPE RANGE OF ztca_param_val-low.

    CONSTANTS: lc_modulo_fi TYPE ztca_param_mod-modulo VALUE 'FI-AR',
               lc_chave1    TYPE    ztca_param_par-chave1 VALUE 'BOLETO',
               lc_chave2    TYPE    ztca_param_par-chave2 VALUE 'TEXTOINSTRUCAO',
               lc_chave2_2  TYPE    ztca_param_par-chave2 VALUE 'JUROS',
               lc_chave3    TYPE    ztca_param_par-chave2 VALUE 'EMPRESA',
               lc_replace   TYPE    char3 VALUE 'R$&'.

    DATA lv_chave3 TYPE  ztca_param_par-chave2.

    lv_chave3 = lc_chave3 && gs_doc-bukrs.

    DATA(lr_param) = zclca_tabela_parametros=>get_instance( ).

    TRY.
        lr_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_fi
            iv_chave1 = lc_chave1
            iv_chave2 = lc_chave2_2
            iv_chave3 = lv_chave3
          IMPORTING
            ev_param  = lv_juros
        ).

      CATCH zcxca_tabela_parametros.
        "handle exception
    ENDTRY.



    SELECT modulo , chave1, chave2, chave3, low, descricao, createdat
    FROM zi_ca_param_val
    INTO TABLE @DATA(lt_param)
    WHERE modulo = @lc_modulo_fi
      AND chave1 = @lc_chave1
      AND chave2 = @lc_chave2
      AND chave3 = @lv_chave3.

    IF sy-subrc = 0.

      SORT: lt_param BY descricao createdat.

      CLEAR: gs_boleto-instrut1,
             gs_boleto-instrut2,
             gs_boleto-instrut3,
             gs_boleto-instrut4.


      LOOP AT lt_param ASSIGNING FIELD-SYMBOL(<fs_param>).

        IF <fs_param>-descricao = 1.

          gs_boleto-instrut1 = gs_boleto-instrut1 && <fs_param>-low.
        ENDIF.

        IF <fs_param>-descricao = 2.
          gs_boleto-instrut2 = gs_boleto-instrut2 && <fs_param>-low.
        ENDIF.

        IF <fs_param>-descricao = 3.
          gs_boleto-instrut3 = gs_boleto-instrut3 && <fs_param>-low.
        ENDIF.

        IF <fs_param>-descricao = 4.
          gs_boleto-instrut4 = gs_boleto-instrut4 && <fs_param>-low.
        ENDIF.

      ENDLOOP.

    ENDIF.

    IF  lv_juros IS NOT INITIAL.
      lv_valor_jur = gs_boleto-valor_doc * ( lv_juros / 100 ).
      lv_valor_jur = lv_valor_jur / 30.

      WRITE lv_valor_jur CURRENCY 'BRL' TO lv_valor_escrito.
      CONDENSE lv_valor_escrito NO-GAPS.
      CONCATENATE 'R$' lv_valor_escrito INTO lv_valor_escrito SEPARATED BY space.

      REPLACE lc_replace IN gs_boleto-instrut1 WITH lv_valor_escrito.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
