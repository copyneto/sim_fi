CLASS zclfi_boleto_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      "! Constante para dados do boleto
      BEGIN OF gc_boleto,
        doctype          TYPE bcss_attachment-doctype     VALUE 'PDF',    "NO_TEXT"
        filename         TYPE bcss_attachment-filename    VALUE 'Boleto.PDF', "NO_TEXT"
        bb               TYPE hbkid      VALUE 'BB001',
        cef              TYPE char3      VALUE 'CEF',
        itau             TYPE char3      VALUE 'ITA',
        data             TYPE dats       VALUE '20000703',
        bbrasil          TYPE hbkid      VALUE 'BB0',
        fidc             TYPE hbkid      VALUE 'FID',
        banrisul         TYPE hbkid      VALUE 'BRS',
        bsantander       TYPE hbkid      VALUE 'SAN',
        sicredi          TYPE hbkid      VALUE 'SIC',
        bsicoob          TYPE hbkid      VALUE 'SCB',
        seis_zero        TYPE char6      VALUE '000000',
        onze_zeros       TYPE char11     VALUE '00000000000',
        nove             TYPE char1      VALUE '9',
        tvarv_banco      TYPE tvarv-name VALUE 'Z_FI_BANKSANT_',
        livre            TYPE char2      VALUE '17',
        um               TYPE char1      VALUE '1',
        zero             TYPE char1      VALUE '0',
        cinco_zero       TYPE char5      VALUE '00000',
        parcela          TYPE char3      VALUE '001',
        codbb            TYPE char3      VALUE '001',
        coditau          TYPE char3      VALUE '341',
        codcaixa         TYPE char3      VALUE '104',
        codbanri         TYPE char3      VALUE '041',
        codsantander     TYPE char3      VALUE '033',
        codsicred        TYPE char3      VALUE '748',
        modulo           TYPE ze_param_modulo VALUE 'FI-AR',
        chave1           TYPE ze_param_chave1 VALUE 'BOLETO',
        chave2           TYPE ze_param_chave1 VALUE 'DIRETORIO',
        chave3           TYPE ze_param_chave1 VALUE 'PDF',
        chave_local_pgto TYPE ze_param_chave2 VALUE 'LOCALPAGAMENTO',
        pdf              TYPE char4     VALUE '.PDF',    "NO_TEXT"
      END OF gc_boleto .

    METHODS constructor
      IMPORTING
        !iv_printer TYPE rspopname OPTIONAL.

    METHODS envia_email
      IMPORTING
        !it_keys TYPE zctgfi_boleto_ban_key OPTIONAL
        !is_key  TYPE zsfi_boleto_ban_key OPTIONAL
      EXPORTING
        !et_msg  TYPE bapiret2_tab .

    METHODS gerar_boleto
      IMPORTING
        !is_key TYPE zsfi_boleto_ban_key OPTIONAL
      EXPORTING
        !et_msg TYPE bapiret2_tab .

    METHODS gerar_boleto_fat
      IMPORTING
        !is_key TYPE zsfi_boleto_ban_key OPTIONAL
      EXPORTING
        !et_msg TYPE bapiret2_tab .

    METHODS msg .

    METHODS nosso_numero
      CHANGING
        !cs_boletos TYPE zsfi_boleto_banc_nn
      EXCEPTIONS
        not_update
        not_add_number .

    METHODS codigo_barra
      CHANGING
        !cs_boletos TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        not_found_t012
        not_found_acdoca
        not_found_t012k .

    METHODS linha_dig
      CHANGING
        !cs_boletos TYPE zsfi_boleto_banc_ld
      EXCEPTIONS
        not_found_t012
        not_found_t045t
        not_found_t012k .

    METHODS seleciona_dado_cb
      CHANGING
        !cs_boletos_cb TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        not_found_t012
        not_found_acdoca
        not_found_t012k .

    METHODS validar_nossonum
      CHANGING
        !cs_boletos_num TYPE zsfi_boleto_banc_nn
      EXCEPTIONS
        not_update
        not_add_number .

    METHODS carteira_nn_dac
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb .

    METHODS visualizar_boleto_app
      IMPORTING
        !is_boletos     TYPE zsfi_boleto_ban_key
      EXPORTING
        !ev_pdf_file    TYPE xstring
        !ev_boleto_name TYPE char50.


  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      "! Dados do boleto
      BEGIN OF ty_bol,
        email TYPE ad_smtpadr,
        name1 TYPE name1,
        butxt TYPE butxt,
        budat TYPE budat,
        osl   TYPE fis_osl,
      END OF ty_bol .

    DATA gv_pdf_file TYPE xstring .

    DATA gv_boleto_name TYPE char50.

    DATA gv_email_beh TYPE boolean.

    DATA gv_string TYPE string.

    DATA gv_dac TYPE string .

    DATA gt_otf TYPE tt_itcoo .

    DATA gv_printer TYPE rspopname.

    DATA gs_t012 TYPE t012 .
    "! Nosso nro
    DATA gv_nossonumero TYPE fis_xref3 .
    "! Código de barras
    DATA gv_cod_barras TYPE ze_cod_barra .
    "! Linha digitável
    DATA gv_linha_dig TYPE ze_linha_dig.
    "! String para mensagens
    DATA gv_dummy TYPE string .

    DATA gt_msg TYPE bapiret2_tab.

    DATA gs_boleto_info TYPE zi_fi_boleto_info .

*    DATA gs_boleto_lote TYPE zi_fi_lote_nossonumero.

    DATA gs_bol TYPE ty_bol .

    METHODS get_boleto_info
      IMPORTING
        iv_belnr TYPE bseg-belnr
        iv_gjahr TYPE bseg-gjahr
        iv_bukrs TYPE bseg-bukrs
        iv_buzei TYPE bseg-buzei OPTIONAL .
    METHODS processa_boleto_infor.

    METHODS cod_banco
      EXPORTING
        !ev_field TYPE string
      CHANGING
        !cv_hbkid TYPE fis_hbkid
        !cs_key   TYPE zsfi_boleto_ban_key
      EXCEPTIONS
        not_found_t012
        not_found_t042a_bseg_pcec .

    METHODS seleciona_dado_ld
      CHANGING
        !cs_boletos_ld TYPE zsfi_boleto_banc_ld
      EXCEPTIONS
        not_found_t012
        not_found_t045t
        not_found_t012k .

    METHODS executa_fluxo
      CHANGING
*        !cs_boleto_banc TYPE pcec
        !cs_boleto TYPE zsfi_boleto_banc_nn
      EXCEPTIONS
        not_update
        not_add_number .

    METHODS executa_fluxo_bb
      IMPORTING
        !is_boleto      TYPE zsfi_boleto_banc_nn
      CHANGING
        !cs_boleto_banc TYPE pcec
      EXCEPTIONS
        not_update
        not_add_number .

    METHODS add_numero
      IMPORTING
        !is_boleto      TYPE zsfi_boleto_banc_nn
      CHANGING
        !cs_boleto_banc TYPE pcec
      EXCEPTIONS
        not_add_number
        not_update .

    METHODS alterar_doc
      IMPORTING
        !is_boleto_banc TYPE pcec OPTIONAL
        !is_boleto_xref TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        not_update
        not_found_acdoca .

    METHODS update
      IMPORTING
        !is_boleto TYPE pcec .

    METHODS cod_moeda
      CHANGING
        !cv_field TYPE string .

    METHODS fat_vencimento
      IMPORTING
        !iv_bldat TYPE bldat
      CHANGING
        !cv_field TYPE string .

    METHODS valor_tit
      IMPORTING
        !iv_dmbr  TYPE dmbtr
      CHANGING
        !cv_field TYPE string
      EXCEPTIONS
        not_found_bseg .

    METHODS carteira_nn_dac_cb
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        not_found_t045t .

    METHODS agencia_cc_dac
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        not_found_t012k .

    METHODS dac_cod_barra
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb .

    "! Busca formulário
    METHODS get_formulario_otf
      IMPORTING
        !iv_printer TYPE rspopname OPTIONAL.

    METHODS convert_otf
      IMPORTING
        !it_otf TYPE tt_itcoo
      EXCEPTIONS
        not_convert_otf .

    METHODS execute_bb_linha_dig
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_ld
      EXCEPTIONS
        error .

    METHODS execute_banrisul_linha_dig
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_ld
      EXCEPTIONS
        error .

    METHODS set_dac_bb_ld_mod10
      IMPORTING
        !iv_key       TYPE string
      RETURNING
        VALUE(rv_dac) TYPE char1 .

    METHODS set_bb_nn_dac
      IMPORTING
        !is_key TYPE zsfi_boleto_banc_cb .

    METHODS execute_bb
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        error .

    METHODS execute_cef
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        error .

    METHODS execute_banrisul
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        error .

    METHODS execute_sicredi
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        error .

    METHODS execute_itau
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb
      EXCEPTIONS
        error .

    METHODS set_bra_convenio
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb .

    METHODS set_nn
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb .

    METHODS set_free
      CHANGING
        cs_key TYPE zsfi_boleto_banc_cb .

    METHODS envia_email_single
      IMPORTING
        is_key  TYPE zsfi_boleto_ban_key
      EXPORTING
        !et_msg TYPE bapiret2_tab .

    METHODS enviar_email
      IMPORTING
        !is_boletos     TYPE zsfi_boleto_banc_em
        !it_lista_email TYPE safm_apt_pp_email OPTIONAL
      EXCEPTIONS
        not_found_email
        not_convert_otf .

    METHODS seleciona_dado_em
      IMPORTING
        !is_boletos_em TYPE zsfi_boleto_banc_em
      EXCEPTIONS
        not_found_email .

    METHODS set_com_copia
      RETURNING
        VALUE(rv_com_copia) TYPE bcs_address.

    METHODS get_noreply
      RETURNING
        VALUE(rv_email) TYPE ad_smtpadr.
    METHODS set_bb_ag_conta
      CHANGING
        cs_key TYPE zsfi_boleto_banc_cb.
    METHODS get_convenio
      IMPORTING
        is_key             TYPE zsfi_boleto_banc_nn
      RETURNING
        VALUE(rv_convenio) TYPE dtaid_045t.
    METHODS save_boleto_al11
      IMPORTING
        is_key TYPE zsfi_boleto_ban_key.

    METHODS add_numero_bb
      CHANGING
*        !cs_boleto_banc TYPE pcec
        !cs_boleto TYPE zsfi_boleto_banc_nn
      EXCEPTIONS
        not_add_number
        not_update .

    METHODS add_numero_banrisul
      CHANGING
*        !cs_boleto_banc TYPE pcec
        !cs_boleto TYPE zsfi_boleto_banc_nn
      EXCEPTIONS
        not_add_number
        not_update .

    METHODS add_numero_sicredi
      CHANGING
*        !cs_boleto_banc TYPE pcec
        !cs_boleto TYPE zsfi_boleto_banc_nn
      EXCEPTIONS
        not_add_number
        not_update .

    METHODS add_numero_itau
      CHANGING
*        !cs_boleto_banc TYPE pcec
        !cs_boleto TYPE zsfi_boleto_banc_nn
      EXCEPTIONS
        not_add_number
        not_update .

    METHODS add_numero_cef
      CHANGING
        !cs_boleto TYPE zsfi_boleto_banc_nn
      EXCEPTIONS
        not_add_number
        not_update .

    METHODS set_nn_bb_17
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb .

    METHODS set_nn_bb_12
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb .

    METHODS set_nn_ita_12
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb .

    METHODS set_age_itau
      CHANGING
        !cs_key TYPE zsfi_boleto_banc_cb .

    METHODS get_carteira
      IMPORTING
        is_key             TYPE zsfi_boleto_banc_nn
      RETURNING
        VALUE(rv_carteira) TYPE fclm_bam_refzl.
    METHODS process_boleto_name.

ENDCLASS.



CLASS zclfi_boleto_util IMPLEMENTATION.


  METHOD envia_email.

    IF is_key IS NOT INITIAL.

      gv_email_beh = abap_false.

      envia_email_single(
        EXPORTING
          is_key = is_key
        IMPORTING
          et_msg = et_msg
      ).

    ENDIF.

  ENDMETHOD.


  METHOD gerar_boleto.

    CLEAR gt_msg.

    "@@Busca dados basicos
    get_boleto_info(
      EXPORTING
        iv_belnr = is_key-belnr
        iv_gjahr = is_key-gjahr
        iv_bukrs = is_key-bukrs
        iv_buzei = is_key-buzei   ).

    IF gs_boleto_info IS NOT INITIAL.

      "@@ Processa dados
      processa_boleto_infor(   ).

    ELSE.
      "Erro ao tentar buscar dados básicos do documento
      MESSAGE e004 WITH is_key-belnr is_key-gjahr is_key-bukrs  INTO gv_dummy.
      msg(  ).
    ENDIF.

    et_msg = gt_msg.

  ENDMETHOD.


  METHOD get_boleto_info.

    CLEAR: gs_boleto_info.

    DATA(lv_belnr) = |{ iv_belnr ALPHA = IN }|.

    SELECT SINGLE *
    FROM zi_fi_boleto_info
    INTO @gs_boleto_info
    WHERE empresa  = @iv_bukrs
     AND documento = @lv_belnr
     AND ano       = @iv_gjahr
     AND item      = @iv_buzei.                      "#EC CI_SEL_NESTED

*    IF sy-subrc = 0.
*
*      CLEAR: gs_boleto_lote.
*
*      SELECT SINGLE *
*      FROM zi_fi_lote_nossonumero
*      WHERE zbukr = @gs_boleto_info-empresa
*         AND hbkid = @gs_boleto_info-banco
*         AND hktid = @gs_boleto_info-housebankaccount
*         AND valido = @abap_true
*         INTO @gs_boleto_lote.                       "#EC CI_SEL_NESTED
*
*    ENDIF.


  ENDMETHOD.


  METHOD processa_boleto_infor.


    DATA(ls_nn) = VALUE zsfi_boleto_banc_nn(
    bukrs = gs_boleto_info-empresa
    belnr = gs_boleto_info-documento
    gjahr = gs_boleto_info-ano
    buzei = gs_boleto_info-item
    xref3 = gs_boleto_info-xref3
    hbkid = gs_boleto_info-banco
*    stapl = gs_boleto_lote-stapl
    ).


    nosso_numero(
      CHANGING
        cs_boletos                = ls_nn
      EXCEPTIONS
        not_update                = 1
        not_add_number            = 2
        OTHERS                    = 3
    ).

    CASE sy-subrc.

      WHEN 1.
        "Erro ao tentar atualizar nosso numero no documento
        MESSAGE e000 WITH TEXT-017 INTO gv_dummy.
        msg( ).
        RETURN.

      WHEN 2 OR 3.
        "Erro ao tentar gerar nosso numero
        MESSAGE e000 WITH TEXT-018 INTO gv_dummy.
        msg( ).
        RETURN.

    ENDCASE.


    DATA(ls_cb) = VALUE  zsfi_boleto_banc_cb(
        bukrs = gs_boleto_info-empresa
        belnr = gs_boleto_info-documento
        gjahr = gs_boleto_info-ano
        buzei = gs_boleto_info-item
        xref3 = ls_nn-xref3
        hbkid = gs_boleto_info-banco
        dmbtr = gs_boleto_info-valor
        netdt = gs_boleto_info-netdt
         ).

    codigo_barra(
      CHANGING
        cs_boletos       = ls_cb
      EXCEPTIONS
        not_found_t012   = 1
        not_found_acdoca = 2
        not_found_t012k  = 3
        OTHERS           = 4
    ).

    CASE sy-subrc.

      WHEN 1.
        "Dados não localizados: &1 &2 &3
        MESSAGE e002 WITH 'T012' INTO gv_dummy ##MG_MISSING.
        msg( ).
        RETURN.

      WHEN 2.
        "Dados não localizados: &1 &2 &3
        MESSAGE e002 WITH 'ACDOCA' INTO gv_dummy ##MG_MISSING.
        msg( ).
        RETURN.

      WHEN 3.
        "Dados não localizados: &1 &2 &3
        MESSAGE e002 WITH 'T012K' INTO gv_dummy ##MG_MISSING.
        msg( ).
        RETURN.

      WHEN 4.
        "Erro ao tentar gerar código de barras
        MESSAGE e000 WITH TEXT-019 INTO gv_dummy ##MG_MISSING.
        msg( ).
        RETURN.

    ENDCASE.


    DATA(ls_ld) = VALUE zsfi_boleto_banc_ld(

    bukrs = gs_boleto_info-empresa
    belnr = gs_boleto_info-documento
    gjahr = gs_boleto_info-ano
    buzei = gs_boleto_info-item
    xref3 = ls_nn-xref3
    hbkid = gs_boleto_info-banco
    cod_barra = ls_cb-cod_barra
    dmbtr = gs_boleto_info-valor
    netdt = gs_boleto_info-netdt     ).


    linha_dig(
      CHANGING
        cs_boletos                = ls_ld
      EXCEPTIONS
        not_found_t012            = 1
        not_found_t045t           = 2
        not_found_t012k           = 3
        OTHERS                    = 4
    ).
    CASE sy-subrc.

      WHEN 1.
        "Dados não localizados: &1 &2 &3
        MESSAGE e002 WITH 'T012' INTO gv_dummy ##MG_MISSING.
        msg( ).
        RETURN.

      WHEN 2.
        "Dados não localizados: &1 &2 &3
        MESSAGE e002 WITH 'T045T' INTO gv_dummy ##MG_MISSING.
        msg( ).
        RETURN.

      WHEN 3.
        "Dados não localizados: &1 &2 &3
        MESSAGE e002 WITH 'T012K' INTO gv_dummy ##MG_MISSING.
        msg( ).
        RETURN.

      WHEN 4.
        "Erro ao tentar gerar linha digitável
        MESSAGE e000 WITH TEXT-020 INTO gv_dummy ##MG_MISSING.
        msg( ).
        RETURN.

    ENDCASE.

    CLEAR: gv_cod_barras, gv_nossonumero, gv_linha_dig.

    gv_nossonumero = ls_nn-xref3.
    gv_cod_barras = ls_cb-cod_barra.
    gv_linha_dig = ls_ld-linha_dig.

    "Boleto gerado com sucesso
    MESSAGE s000 WITH TEXT-022 INTO gv_dummy.
    msg( ).


  ENDMETHOD.


  METHOD msg.

    DATA: ls_msg TYPE bapiret2.

    ls_msg-id = sy-msgid.
    ls_msg-type = sy-msgty.
    ls_msg-number = sy-msgno.
    ls_msg-message_v1 = sy-msgv1.
    ls_msg-message_v2 = sy-msgv2.
    ls_msg-message_v3 = sy-msgv3.
    ls_msg-message_v4 = sy-msgv4.
    APPEND ls_msg TO gt_msg.

  ENDMETHOD.


  METHOD codigo_barra.

    CLEAR: gs_t012.

    me->seleciona_dado_cb(
       CHANGING
         cs_boletos_cb = cs_boletos
       EXCEPTIONS
         not_found_t012   = 1
         not_found_t012k  = 2
         OTHERS = 3
    ).

    CASE sy-subrc.
      WHEN 1.
        RAISE not_found_t012.
      WHEN 2.
        RAISE not_found_t012k.
    ENDCASE.

  ENDMETHOD.


  METHOD linha_dig.

    CLEAR: gs_t012.

    me->seleciona_dado_ld(
    CHANGING
      cs_boletos_ld = cs_boletos
    EXCEPTIONS
      not_found_t012            = 1
      not_found_t045t           = 2
      not_found_t012k           = 3
      OTHERS                    = 4
    ).

    CASE sy-subrc.
      WHEN 1.
        RAISE not_found_t012.
      WHEN 2.
        RAISE not_found_t045t.
      WHEN 3.
        RAISE not_found_t012k.
    ENDCASE.

  ENDMETHOD.


  METHOD seleciona_dado_cb.

    DATA: ls_key       TYPE zsfi_boleto_ban_key,
          lv_cod_barra TYPE string.

    ls_key = CORRESPONDING #( cs_boletos_cb ).

    me->cod_banco(
        IMPORTING
          ev_field = lv_cod_barra
        CHANGING
          cv_hbkid = cs_boletos_cb-hbkid
          cs_key   = ls_key
        EXCEPTIONS
          not_found_t012 = 1
    ).

    IF sy-subrc EQ 1.
      RAISE not_found_t012.
    ENDIF.

    me->cod_moeda( CHANGING cv_field = lv_cod_barra ).

    me->fat_vencimento( EXPORTING iv_bldat = cs_boletos_cb-netdt CHANGING cv_field = lv_cod_barra ).

    me->valor_tit( EXPORTING iv_dmbr = cs_boletos_cb-dmbtr CHANGING cv_field = lv_cod_barra ).

    cs_boletos_cb-cod_barra = lv_cod_barra.


    CASE cs_boletos_cb-hbkid(3).


      WHEN  gc_boleto-banrisul.

        execute_banrisul( CHANGING cs_key = cs_boletos_cb
                       EXCEPTIONS
                         error  = 1 ).


      WHEN  gc_boleto-cef.

        execute_cef( CHANGING cs_key = cs_boletos_cb
                       EXCEPTIONS
                         error  = 1 ).

      WHEN gc_boleto-itau.

        execute_itau( CHANGING cs_key = cs_boletos_cb
                       EXCEPTIONS
                         error  = 1 ).

      WHEN gc_boleto-bbrasil OR gc_boleto-fidc .


        execute_bb( CHANGING cs_key = cs_boletos_cb
                       EXCEPTIONS
                         error  = 1 ).

      WHEN gc_boleto-sicredi.

        execute_sicredi( CHANGING cs_key = cs_boletos_cb
                        EXCEPTIONS
                          error  = 1 ).

      WHEN gc_boleto-bsantander.
**
**        set_sant_nn_dac( is_key = cs_boletos_cb ).
**
**        execute_santander( CHANGING cs_key = cs_boletos_cb
**                    EXCEPTIONS
**                      error  = 1 ).
**
      WHEN gc_boleto-bsicoob.
**
**        me->execute_sicoob( CHANGING   cs_key = cs_boletos_cb
**                            EXCEPTIONS error  = 1 ).
**
**
    ENDCASE.

*    me->carteira_nn_dac( CHANGING cs_key = cs_boletos_cb ).
*
*    me->agencia_cc_dac(
*    CHANGING
*      cs_key = cs_boletos_cb
*    EXCEPTIONS
*      not_found_t012k = 2
*    ).
*
*    IF sy-subrc EQ 2.
*      RAISE not_found_t012k.
*    ENDIF.

    me->dac_cod_barra( CHANGING cs_key = cs_boletos_cb ).

    CLEAR: gv_string.

  ENDMETHOD.


  METHOD cod_banco.

    DATA: ls_bol TYPE zsfi_boleto_ban_key,
          ls_ret TYPE pcec.

    CLEAR: gs_t012.

    SELECT SINGLE * FROM t012
    WHERE bukrs = @cs_key-bukrs
      AND hbkid = @cv_hbkid
      AND banks = 'BR'
      INTO @gs_t012.

    IF sy-subrc NE 0.
      RAISE not_found_t012.
    ELSE.
      ev_field = gs_t012-bankl(3).
    ENDIF.

    CLEAR: ls_bol, ls_ret.

  ENDMETHOD.


  METHOD seleciona_dado_ld.

    DATA: ls_key      TYPE zsfi_boleto_ban_key,
          lv_linha_dg TYPE string,
          lv_dtaid    TYPE dtaid_045t.

    ls_key = CORRESPONDING #( cs_boletos_ld ).

    me->cod_banco(
    IMPORTING
      ev_field = lv_linha_dg
    CHANGING
      cv_hbkid = cs_boletos_ld-hbkid
      cs_key   = ls_key
    EXCEPTIONS
      not_found_t012            = 1
    ).

    IF sy-subrc EQ 1.
      RAISE not_found_t012.
    ENDIF.

    cs_boletos_ld-linha_dig = lv_linha_dg.

    IF cs_boletos_ld-hbkid(3) = gc_boleto-banrisul.

      execute_banrisul_linha_dig( CHANGING   cs_key = cs_boletos_ld
                       EXCEPTIONS error  = 1 ).

    ELSE.

      execute_bb_linha_dig( CHANGING   cs_key = cs_boletos_ld
                          EXCEPTIONS error  = 1 ).
    ENDIF.

**    CASE cs_boletos_ld-hbkid(3).
**
**      WHEN gc_boleto-itau.
**
**        execute_bb_linha_dig( CHANGING   cs_key = cs_boletos_ld
**                        EXCEPTIONS error  = 1 ).
**
**      WHEN gc_boleto-cef.
**
**        execute_bb_linha_dig( CHANGING   cs_key = cs_boletos_ld
**                      EXCEPTIONS error  = 1 ).
**
***        execute_itau_linha_dig( CHANGING   cs_key = cs_boletos_ld
***                                EXCEPTIONS error  = 1 ).
**
**      WHEN gc_boleto-bbrasil.
**
**        execute_bb_linha_dig( CHANGING   cs_key = cs_boletos_ld
**                              EXCEPTIONS error  = 1 ).
**
**      WHEN gc_boleto-bsantander.
**
***        execute_sant_linha_dig( CHANGING   cs_key = cs_boletos_ld
***                                EXCEPTIONS error  = 1 ).
**
**      WHEN gc_boleto-bsicoob.
**
***        execute_scb_linha_dig( CHANGING   cs_key = cs_boletos_ld
***                               EXCEPTIONS error  = 1 ).
**
**    ENDCASE.

*    me->carteira_nn_dac_ld(                                                                                         " Bloco 1 - DAC
*    IMPORTING
*      ev_dtaid = lv_dtaid
*    CHANGING
*      cs_key = cs_boletos_ld
*    EXCEPTIONS
*      not_found_t045t = 2
*    ).
*
*    IF sy-subrc EQ 2.
*      RAISE not_found_t045t.
*    ENDIF.
*
*    me->agencia_nn_car_dac_ld( EXPORTING iv_dtaid =  lv_dtaid CHANGING cs_key = cs_boletos_ld ).
*    me->tres_pri_digitos( CHANGING cs_key = cs_boletos_ld ).                                                       " Bloco 2 - DAC
*
*    me->bloco_3(                                                                                                   " Bloco 3 - DAC
*    CHANGING
*     cs_key = cs_boletos_ld
*    EXCEPTIONS
*     not_found_t012k = 3
*    ).
*
*    IF sy-subrc EQ 3.
*      RAISE not_found_t012k.
*    ENDIF.
*
*    me->bloco_4( CHANGING cs_key = cs_boletos_ld ).                                                                " Bloco 4 - DAC CB

    CLEAR: gv_string, lv_dtaid.

  ENDMETHOD.


  METHOD nosso_numero.

    me->validar_nossonum(
      CHANGING
        cs_boletos_num            = cs_boletos
      EXCEPTIONS
        not_update                = 1
        not_add_number            = 2
        OTHERS                    = 3
    ).

    CASE sy-subrc.
      WHEN 1.
        RAISE not_update.
      WHEN 2.
        RAISE not_add_number.
    ENDCASE.

  ENDMETHOD.


  METHOD validar_nossonum.

    DATA: ls_pcec TYPE pcec,
          ls_bol  TYPE zsfi_boleto_ban_key.

    CHECK cs_boletos_num-xref3 IS INITIAL.

    me->executa_fluxo(
    CHANGING
*        cs_boleto_banc = ls_pcec
        cs_boleto = cs_boletos_num
    EXCEPTIONS
        not_update     = 1
        not_add_number = 2
        OTHERS         = 3
     ).

    CASE  sy-subrc.
      WHEN 1.
        RAISE not_update.
      WHEN 2.
        RAISE not_add_number.
    ENDCASE.

    cs_boletos_num-xref3 = cs_boletos_num-xref3.

  ENDMETHOD.


  METHOD executa_fluxo.

*    MOVE-CORRESPONDING gs_boleto_lote TO cs_boleto_banc.

*    IF cs_boleto_banc-checl IS NOT INITIAL.

    CASE gs_boleto_info-codbanco.

      WHEN gc_boleto-coditau.

        me->add_numero_itau(
               CHANGING
*                 cs_boleto_banc = cs_boleto_banc
               cs_boleto = cs_boleto
               EXCEPTIONS
               not_update     = 1
               not_add_number = 2
               OTHERS         = 3
          ).

        CASE sy-subrc.
          WHEN 1.
            RAISE not_update.
          WHEN 2.
            RAISE not_add_number.
        ENDCASE.

      WHEN gc_boleto-codbb.

        me->add_numero_bb(
              CHANGING
*                cs_boleto_banc = cs_boleto_banc
              cs_boleto = cs_boleto
              EXCEPTIONS
              not_update     = 1
              not_add_number = 2
              OTHERS         = 3
         ).

        CASE sy-subrc.
          WHEN 1.
            RAISE not_update.
          WHEN 2.
            RAISE not_add_number.
        ENDCASE.

      WHEN gc_boleto-codbanri.

        me->add_numero_banrisul(
              CHANGING
*                cs_boleto_banc = cs_boleto_banc
              cs_boleto = cs_boleto
              EXCEPTIONS
              not_update     = 1
              not_add_number = 2
              OTHERS         = 3
         ).

        CASE sy-subrc.
          WHEN 1.
            RAISE not_update.
          WHEN 2.
            RAISE not_add_number.
        ENDCASE.

      WHEN gc_boleto-codsicred.

        me->add_numero_sicredi(
                CHANGING
*                  cs_boleto_banc = cs_boleto_banc
                cs_boleto = cs_boleto
                EXCEPTIONS
                not_update     = 1
                not_add_number = 2
                OTHERS         = 3
           ).

        CASE sy-subrc.
          WHEN 1.
            RAISE not_update.
          WHEN 2.
            RAISE not_add_number.
        ENDCASE.

      WHEN gc_boleto-codcaixa.

        me->add_numero_cef(
           CHANGING
           cs_boleto = cs_boleto
           EXCEPTIONS
           not_update     = 1
           not_add_number = 2
           OTHERS         = 3
      ).

        CASE sy-subrc.
          WHEN 1.
            RAISE not_update.
          WHEN 2.
            RAISE not_add_number.
        ENDCASE.


      WHEN gc_boleto-codsantander.

    ENDCASE.

*    ELSEIF gs_boleto_info = gc_boleto-codcaixa .
*
*      me->add_numero_cef(
*         CHANGING
*         cs_boleto = cs_boleto
*         EXCEPTIONS
*         not_update     = 1
*         not_add_number = 2
*         OTHERS         = 3
*    ).
*
*      CASE sy-subrc.
*        WHEN 1.
*          RAISE not_update.
*        WHEN 2.
*          RAISE not_add_number.
*      ENDCASE.
*
*    ELSE.
*      RAISE not_add_number.
*    ENDIF.


  ENDMETHOD.


  METHOD executa_fluxo_bb.


    IF cs_boleto_banc-checl IS NOT INITIAL.

      me->add_numero(
      EXPORTING
        is_boleto = is_boleto
      CHANGING
        cs_boleto_banc = cs_boleto_banc
        EXCEPTIONS
        not_update     = 1
        not_add_number = 2
        OTHERS         = 3
      ).

      CASE sy-subrc.
        WHEN 1.
          RAISE not_update.
        WHEN 2.
          RAISE not_add_number.
      ENDCASE.

    ELSE.
      RAISE not_add_number.
    ENDIF.

  ENDMETHOD.


  METHOD add_numero.

    DATA: lv_dgv TYPE string.

    CALL FUNCTION 'ADD_N_TO_CHECK_NUMBER'
      EXPORTING
        i_pcec      = cs_boleto_banc
      IMPORTING
        e_pcec      = cs_boleto_banc
      EXCEPTIONS
        not_filled  = 1
        not_found   = 2
        not_numeric = 3
        not_valid   = 4
        OTHERS      = 5.

    IF sy-subrc NE 0.
      RAISE not_add_number.
    ELSE.

      DATA(ls_key) = VALUE zsfi_boleto_banc_cb(
      bukrs = gs_boleto_info-empresa
      belnr = gs_boleto_info-documento
      gjahr = gs_boleto_info-ano
      buzei = gs_boleto_info-item
      hbkid = cs_boleto_banc-hbkid
      xref3 = cs_boleto_banc-checl
    ).


      DATA(lv_dtaid) = get_convenio( is_boleto ).

      IF sy-subrc = 0.

        gv_string = lv_dtaid(6) && ls_key-xref3 .

        DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).
        lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = lv_dgv CHANGING cv_calc = gv_string ).

        ls_key-xref3 = lv_dtaid(6) && ls_key-xref3 && lv_dgv.

      ENDIF.


      me->alterar_doc(
      EXPORTING
        is_boleto_banc = cs_boleto_banc
        is_boleto_xref = ls_key
      EXCEPTIONS
        not_update = 1
      ).

      IF sy-subrc EQ 1.
        RAISE not_update.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD add_numero_bb.

    DATA: ls_boleto_nn TYPE zsfi_boleto_banc_nn.

    DATA: lv_dgv      TYPE string,
          lv_tamanho  TYPE i,
          lv_nr_range TYPE inri-object,
          lv_nn_get   TYPE ze_nn_bb.

    CONSTANTS lc_zbrs TYPE char3 VALUE 'ZBB'.

    lv_nr_range = lc_zbrs && gs_boleto_info-empresa.
    CONDENSE lv_nr_range.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '1'
        object                  = lv_nr_range
      IMPORTING
        number                  = lv_nn_get
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      RAISE not_add_number.
    ELSE.
***    CALL FUNCTION 'ADD_N_TO_CHECK_NUMBER'
***      EXPORTING
***        i_pcec      = cs_boleto_banc
***      IMPORTING
***        e_pcec      = cs_boleto_banc
***      EXCEPTIONS
***        not_filled  = 1
***        not_found   = 2
***        not_numeric = 3
***        not_valid   = 4
***        OTHERS      = 5.
***
***    IF sy-subrc NE 0.
***      RAISE not_add_number.
***    ELSE.


      DATA(ls_key) = VALUE zsfi_boleto_banc_cb(
        bukrs = gs_boleto_info-empresa
        belnr = gs_boleto_info-documento
        gjahr = gs_boleto_info-ano
        buzei = gs_boleto_info-item
        hbkid = gs_boleto_info-Banco
        xref3 = lv_nn_get
      ).

      "Buscar convenio
      ls_boleto_nn = VALUE #( bukrs = cs_boleto-bukrs
                              belnr = cs_boleto-belnr
                              gjahr = cs_boleto-gjahr
                              buzei = cs_boleto-buzei
                              hbkid = cs_boleto-hbkid ) .
      DATA(lv_dtaid) = get_convenio( ls_boleto_nn ).

      IF lv_dtaid IS NOT INITIAL.

        CONDENSE lv_dtaid NO-GAPS.
        lv_tamanho = strlen( lv_dtaid ).

        CASE lv_tamanho.

          WHEN 4 OR  6.

            ls_key-xref3 = lv_dtaid && lv_nn_get. "cs_boleto_banc-checl.

            "Calcular o digito verificador
            DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).
            gv_string = ls_key-xref3 .
            lo_calc_mod11->execute( EXPORTING iv_mod10 = abap_true iv_modulo = abap_false IMPORTING ev_dac = lv_dgv CHANGING cv_calc = gv_string ).
            ls_key-xref3 = gv_string && lv_dgv.

          WHEN 7.

            ls_key-xref3 = lv_dtaid && lv_nn_get. "cs_boleto_banc-checl.

        ENDCASE.

      ENDIF.

      cs_boleto-xref3 = ls_key-xref3.

      me->alterar_doc(
      EXPORTING
**        is_boleto_banc = cs_boleto_banc
        is_boleto_xref = ls_key
      EXCEPTIONS
        not_update = 1
      ).

      IF sy-subrc EQ 1.
        RAISE not_update.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD add_numero_banrisul.

    DATA: lv_dgv1     TYPE string,
          lv_dgv      TYPE string,
          lv_tamanho  TYPE i,
          lv_nossonum TYPE char10,
          lv_nr_range TYPE inri-object,
          lv_nn_get   TYPE ze_nn_banrisul.

    CONSTANTS lc_zbrs TYPE char4 VALUE 'ZBRS'.

    lv_nr_range = lc_zbrs && gs_boleto_info-empresa.
    CONDENSE lv_nr_range.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '1'
        object                  = lv_nr_range
      IMPORTING
        number                  = lv_nn_get
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      RAISE not_add_number.
    ELSE.

**    CALL FUNCTION 'ADD_N_TO_CHECK_NUMBER'
**      EXPORTING
**        i_pcec      = cs_boleto_banc
**      IMPORTING
**        e_pcec      = cs_boleto_banc
**      EXCEPTIONS
**        not_filled  = 1
**        not_found   = 2
**        not_numeric = 3
**        not_valid   = 4
**        OTHERS      = 5.
**
**    IF sy-subrc NE 0.
**      RAISE not_add_number.
**    ELSE.


      DATA(ls_key) = VALUE zsfi_boleto_banc_cb(
      bukrs = gs_boleto_info-empresa
      belnr = gs_boleto_info-documento
      gjahr = gs_boleto_info-ano
      buzei = gs_boleto_info-item
      hbkid = gs_boleto_info-banco "cs_boleto_banc-hbkid
      xref3 = lv_nn_get
    ).

      DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

      lv_nossonum = ls_key-xref3.
      gv_string = lv_nossonum .
      lo_calc_mod11->execute( EXPORTING iv_mod10 = abap_true iv_modulo = abap_false IMPORTING ev_dac = lv_dgv1 CHANGING cv_calc = gv_string ).


      gv_string = gv_string && lv_dgv1 .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gv_string
        IMPORTING
          output = lv_nossonum.


      gv_string = lv_nossonum.
      CONDENSE gv_string NO-GAPS.


      lo_calc_mod11->execute(
        EXPORTING
          iv_modulo   = abap_true
          iv_barinsul = abap_true
          iv_dig1 = lv_dgv1
        IMPORTING
          ev_dac      = lv_dgv
        CHANGING
          cv_calc     = gv_string   ).

      gv_string = gv_string && lv_dgv .

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gv_string
        IMPORTING
          output = lv_nossonum.

      ls_key-xref3 = cs_boleto-xref3 = lv_nossonum.

      me->alterar_doc(
      EXPORTING
***        is_boleto_banc = cs_boleto_banc
        is_boleto_xref = ls_key
      EXCEPTIONS
        not_update = 1
      ).

      IF sy-subrc EQ 1.
        RAISE not_update.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD add_numero_sicredi.

    DATA: lv_dgv      TYPE string,
          lv_tamanho  TYPE i,

          lv_nr_range TYPE inri-object,
          lv_nn_get   TYPE ze_nn_banrisul.

    CONSTANTS lc_zbrs TYPE char4 VALUE 'ZSID'.


    lv_nr_range = lc_zbrs && gs_boleto_info-empresa.
    CONDENSE lv_nr_range.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '1'
        object                  = lv_nr_range
      IMPORTING
        number                  = lv_nn_get
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      RAISE not_add_number.
    ELSE.

**    CALL FUNCTION 'ADD_N_TO_CHECK_NUMBER'
**      EXPORTING
**        i_pcec      = cs_boleto_banc
**      IMPORTING
**        e_pcec      = cs_boleto_banc
**      EXCEPTIONS
**        not_filled  = 1
**        not_found   = 2
**        not_numeric = 3
**        not_valid   = 4
**        OTHERS      = 5.
**
**    IF sy-subrc NE 0.
**      RAISE not_add_number.
**    ELSE.

      DATA(ls_key) = VALUE zsfi_boleto_banc_cb(
      bukrs = gs_boleto_info-empresa
      belnr = gs_boleto_info-documento
      gjahr = gs_boleto_info-ano
      buzei = gs_boleto_info-item
      hbkid = gs_boleto_info-banco "cs_boleto_banc-hbkid
      xref3 = lv_nn_get
    ).

      DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

      gv_string = sy-datum+2(2) &&
                  ls_key-xref3 .

      CONDENSE gv_string NO-GAPS.
      lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = lv_dgv CHANGING cv_calc = gv_string ).

      gv_string = gv_string && lv_dgv .
      ls_key-xref3 = cs_boleto-xref3 = gv_string.

      me->alterar_doc(
      EXPORTING
**        is_boleto_banc = cs_boleto_banc
        is_boleto_xref = ls_key
      EXCEPTIONS
        not_update = 1
      ).

      IF sy-subrc EQ 1.
        RAISE not_update.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD add_numero_itau.

    DATA: lv_dgv         TYPE string,
          lv_tamanho     TYPE i,
          lv_calc_itau   TYPE c LENGTH 20,
          lv_digito_itau TYPE c,
          lv_nr_range    TYPE inri-object,
          lv_nn_get      TYPE ze_nn_banrisul.

    CONSTANTS lc_zbrs TYPE char4 VALUE 'ZITA'.

    lv_nr_range = lc_zbrs && gs_boleto_info-empresa.
    CONDENSE lv_nr_range.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '1'
        object                  = lv_nr_range
      IMPORTING
        number                  = lv_nn_get
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      RAISE not_add_number.
    ELSE.

**    CALL FUNCTION 'ADD_N_TO_CHECK_NUMBER'
**      EXPORTING
**        i_pcec      = cs_boleto_banc
**      IMPORTING
**        e_pcec      = cs_boleto_banc
**      EXCEPTIONS
**        not_filled  = 1
**        not_found   = 2
**        not_numeric = 3
**        not_valid   = 4
**        OTHERS      = 5.
**
**    IF sy-subrc NE 0.
**      RAISE not_add_number.
**    ELSE.


      DATA(ls_key) = VALUE zsfi_boleto_banc_cb(
      bukrs = gs_boleto_info-empresa
      belnr = gs_boleto_info-documento
      gjahr = gs_boleto_info-ano
      buzei = gs_boleto_info-item
      hbkid = gs_boleto_info-banco "cs_boleto_banc-hbkid
      xref3 = lv_nn_get
    ).


      DATA(lv_carteira) = get_carteira( cs_boleto ).


      IF lv_carteira IS INITIAL.
        lv_carteira = '999'.
      ENDIF.


      SELECT SINGLE * FROM t012
        WHERE bukrs = @gs_boleto_info-empresa
          AND hbkid = @gs_boleto_info-banco
          AND banks = 'BR'
          INTO @gs_t012.

      IF sy-subrc = 0.

        SELECT SINGLE bankn , bkont
               FROM t012k
               WHERE bukrs EQ @gs_boleto_info-empresa
                 AND hbkid EQ @gs_boleto_info-banco "cs_boleto_banc-hbkid
               INTO @DATA(ls_t012k).

        IF sy-subrc = 0.

          DATA(lv_pos) = strlen( gs_t012-bankl ) - 4.
          gv_string = gs_t012-bankl+lv_pos(4) && ls_t012k-bankn(5)." && ls_t012k-bkont+1(1).

          lv_calc_itau = gv_string && lv_carteira(3) && ls_key-xref3(8).

          CALL FUNCTION 'CALCULATE_CHECK_DIGIT_MOD10'
            EXPORTING
              number_part = lv_calc_itau
            IMPORTING
              check_digit = lv_digito_itau.


          ls_key-xref3 = lv_carteira(3) && ls_key-xref3(8) && lv_digito_itau.
          cs_boleto-xref3 = ls_key-xref3.

        ENDIF.
      ENDIF.

      me->alterar_doc(
      EXPORTING
**        is_boleto_banc = cs_boleto_banc
        is_boleto_xref = ls_key
      EXCEPTIONS
        not_update = 1
      ).

      IF sy-subrc EQ 1.
        RAISE not_update.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD add_numero_cef.

    DATA: lv_dgv     TYPE string,
          lv_tamanho TYPE i,
          lv_numero  TYPE ze_nn_caixa.

    CONSTANTS: lc_range_nr TYPE inri-nrrangenr VALUE '1',
               lc_object   TYPE inri-object VALUE 'ZCEFNN'.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = lc_range_nr
        object                  = lc_object
      IMPORTING
        number                  = lv_numero
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc NE 0.
      RAISE not_add_number.
    ELSE.

      DATA(ls_key) = VALUE zsfi_boleto_banc_cb(
      bukrs = gs_boleto_info-empresa
      belnr = gs_boleto_info-documento
      gjahr = gs_boleto_info-ano
      buzei = gs_boleto_info-item
      hbkid = cs_boleto-hbkid
      xref3 = lv_numero
    ).

      DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

      DATA(lv_carteira) = get_carteira( cs_boleto ).

      IF lv_carteira IS NOT INITIAL.
        CONDENSE lv_carteira NO-GAPS.
        gv_string =  ls_key-xref3 .
        lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = lv_dgv CHANGING cv_calc = gv_string ).
        ls_key-xref3 = lv_carteira && ls_key-xref3 && lv_dgv.
        cs_boleto-xref3 = ls_key-xref3.
      ELSE.
        cs_boleto-xref3 = '99' && ls_key-xref3.
      ENDIF.


      me->alterar_doc(
      EXPORTING
        is_boleto_xref = ls_key
      EXCEPTIONS
        not_update = 1
      ).

      IF sy-subrc EQ 1.
        RAISE not_update.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD alterar_doc.

    DATA(ls_chave) = VALUE zsfi_boleto_ban_key(
        bukrs = gs_boleto_info-empresa
        belnr = gs_boleto_info-documento
        gjahr = gs_boleto_info-ano
        buzei = gs_boleto_info-item
    ).

    DATA(lo_atu_doc) = NEW zclfi_atualiza_doc(  ).
    lo_atu_doc->atualiza_xref3_bank(
      EXPORTING
        iv_chave  = ls_chave
        iv_bank   = is_boleto_xref-hbkid
        iv_xref3  = is_boleto_xref-xref3
      EXCEPTIONS
        not_update = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      RAISE not_update.
    ELSE.

      IF is_boleto_banc IS NOT INITIAL.

        me->update( EXPORTING is_boleto = is_boleto_banc ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD update.

    UPDATE pcec SET
           checl = is_boleto-checl
          WHERE zbukr = is_boleto-zbukr
            AND hbkid = is_boleto-hbkid
            AND hktid = is_boleto-hktid
            AND stapl = is_boleto-stapl.

    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD cod_moeda.
    cv_field = cv_field && '9_'.
  ENDMETHOD.


  METHOD fat_vencimento.

    DATA: lv_dias TYPE i.

*    lv_dias = iv_bldat - gc_boleto-data.

    lv_dias = ( iv_bldat - gc_boleto-data ) + 1000.

    IF lv_dias IS NOT INITIAL.

      cv_field = cv_field && lv_dias.
      CONDENSE cv_field NO-GAPS.

    ENDIF.

  ENDMETHOD.


  METHOD valor_tit.

    DATA: lv_dmb TYPE char10.
*    DATA: lv_dmb TYPE char12.

    lv_dmb = iv_dmbr.

    REPLACE ',' IN lv_dmb WITH ''.
    REPLACE '.' IN lv_dmb WITH ''.
    CONDENSE lv_dmb NO-GAPS.

    UNPACK lv_dmb TO lv_dmb.

    cv_field = cv_field && lv_dmb.

    CONDENSE cv_field NO-GAPS.

  ENDMETHOD.


  METHOD carteira_nn_dac_cb.

    IF cs_key-hbkid NE gc_boleto-bb.

      SELECT SINGLE dtaid
      FROM t045t
      INTO @DATA(lv_dtaid)
      WHERE bukrs = @cs_key-bukrs
        AND hbkid = @cs_key-hbkid.

      gv_string = cs_key-xref3.

    ELSE.

      gv_string = lv_dtaid(3) && cs_key-xref3(8).

    ENDIF.

    IF sy-subrc NE 0.
      RAISE not_found_t045t.
    ELSE.

      DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

      lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = gv_dac CHANGING cv_calc = gv_string ).

      cs_key-cod_barra = cs_key-cod_barra && gv_string && gv_dac.

      CLEAR: gv_dac, gv_string.

    ENDIF.
  ENDMETHOD.


  METHOD carteira_nn_dac.

    DATA: lv_calc_itau TYPE c LENGTH 20.
    DATA: lv_digito_itau TYPE c.

    SELECT SINGLE referenceinfo
        FROM i_housebankaccountlinkage
        INTO @DATA(lv_carteira)
        WHERE companycode = @cs_key-bukrs
          AND housebank = @cs_key-hbkid.

    IF sy-subrc = 0.

      DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

      "@@ Itau
      IF cs_key-hbkid(3) = gc_boleto-itau.

        SELECT SINGLE bankn ,bkont FROM t012k
        WHERE bukrs EQ @cs_key-bukrs
          AND hbkid EQ @cs_key-hbkid
        INTO @DATA(ls_t012k).

        DATA(lv_pos) = strlen( gs_t012-bankl ) - 4.
        gv_string = gs_t012-bankl+lv_pos(4) && ls_t012k-bankn(5)." && ls_t012k-bkont+1(1).

        lv_calc_itau = gv_string && lv_carteira && cs_key-xref3(8).

        CALL FUNCTION 'CALCULATE_CHECK_DIGIT_MOD10'
          EXPORTING
            number_part = lv_calc_itau
          IMPORTING
            check_digit = lv_digito_itau.

        gv_string = lv_carteira && cs_key-xref3(8) && lv_digito_itau.


*        "@@ Diferente de BB
*      ELSEIF cs_key-hbkid NE gc_boleto-bb.
*
*        gv_string = ls_carteira-carteira && cs_key-xref3(8).
*        lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = gv_dac CHANGING cv_calc = gv_string ).
*
*      ELSE.
*
*        gv_string = cs_key-xref3.
*        lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = gv_dac CHANGING cv_calc = gv_string ).
*
      ENDIF.

    ELSE.


    ENDIF.

**    IF cs_key-hbkid NE gc_boleto-bb.
**
**      SELECT SINGLE dtaid
**        FROM t045t
**        INTO @DATA(lv_dtaid)
**        WHERE bukrs EQ @cs_key-bukrs
**        AND hbkid EQ @cs_key-hbkid    .
**
**      gv_string = lv_dtaid(3) && cs_key-xref3(8).
**
**    ELSE.
**
**      gv_string = cs_key-xref3.
**
**    ENDIF.

    CONDENSE gv_dac NO-GAPS.
    cs_key-cod_barra = cs_key-cod_barra  && gv_string && gv_dac .

    CLEAR: gv_dac, gv_string.

  ENDMETHOD.


  METHOD agencia_cc_dac.

    SELECT SINGLE bankn ,bkont FROM t012k
    WHERE bukrs EQ @cs_key-bukrs
      AND hbkid EQ @cs_key-hbkid
    INTO @DATA(ls_t012k).

    IF sy-subrc NE 0.
      RAISE not_found_t012k.
    ELSE.

      DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

      DATA(lv_pos) = strlen( gs_t012-bankl ) - 4.

      gv_string = gs_t012-bankl+lv_pos(4) && ls_t012k-bankn(5) && ls_t012k-bkont+1(1).

      cs_key-cod_barra = cs_key-cod_barra  && gv_string && gv_dac && '000'.

      CONDENSE cs_key-cod_barra.

      CLEAR: gv_dac, gv_string.

    ENDIF.
  ENDMETHOD.


  METHOD dac_cod_barra.

    DATA lv_cod_barra TYPE ze_cod_barra.

    DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

    lv_cod_barra = gv_string = cs_key-cod_barra.

    REPLACE '_' IN gv_string WITH space.
    CONDENSE gv_string.

    IF cs_key-hbkid(3) = gc_boleto-banrisul.
      lo_calc_mod11->execute( EXPORTING
                                iv_modulo = abap_true
                                iv_barinsul = abap_true
                                iv_dac = abap_true
                              IMPORTING
                                ev_dac = gv_dac
                              CHANGING
                                cv_calc = gv_string ).
    ELSE.
      lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = gv_dac CHANGING cv_calc = gv_string ).
    ENDIF.

    CONDENSE gv_dac NO-GAPS.
    REPLACE '_' IN lv_cod_barra WITH gv_dac.
    CONDENSE lv_cod_barra NO-GAPS.

    cs_key-cod_barra = lv_cod_barra.
  ENDMETHOD.


  METHOD visualizar_boleto_app.


    "@@Busca dados basicos
    get_boleto_info(
      EXPORTING
        iv_belnr = is_boletos-belnr
        iv_gjahr = is_boletos-gjahr
        iv_bukrs = is_boletos-bukrs
        iv_buzei = is_boletos-buzei    ).

    CHECK gs_boleto_info IS NOT INITIAL.

    "@@ Processa dados
    processa_boleto_infor(   ).

    "@@ Imprimi formulario para anexo
    get_formulario_otf(   ).

    convert_otf(
    EXPORTING
      it_otf = gt_otf
    EXCEPTIONS
      not_convert_otf = 2
      OTHERS          = 3
    ).

    process_boleto_name(  ).
    CONDENSE gv_boleto_name NO-GAPS.

    ev_boleto_name = gv_boleto_name.
    ev_pdf_file = gv_pdf_file.

  ENDMETHOD.


  METHOD get_formulario_otf.

    DATA(ls_dados) = VALUE zsfi_boleto_banc_form(
      bukrs = gs_boleto_info-empresa
      belnr = gs_boleto_info-documento
      gjahr = gs_boleto_info-ano
      buzei = gs_boleto_info-item
      nossonumero = gv_nossonumero
      cod_barras = gv_cod_barras
      linha_dig = gv_linha_dig
      empresatxt = gs_boleto_info-empresatxt
      valor = gs_boleto_info-valor
      valorabatimento = gs_boleto_info-valorabatimento
      xblnr = gs_boleto_info-xblnr
      zlsch = gs_boleto_info-formpgto
      banco = gs_boleto_info-banco
   ).

    gt_otf = NEW zclfi_form_boleto( is_doc = ls_dados iv_printer = iv_printer )->execute( ).


  ENDMETHOD.


  METHOD convert_otf.


    DATA lt_pdf                TYPE tlinet.
    DATA lv_pdf_filesize       TYPE i.

    CLEAR: gv_pdf_file.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = lv_pdf_filesize
        bin_file              = gv_pdf_file
      TABLES
        otf                   = it_otf[]
        lines                 = lt_pdf[]
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    IF sy-subrc NE 0.
      RAISE not_convert_otf.
    ENDIF.


  ENDMETHOD.


  METHOD execute_bb_linha_dig.
    DATA: ls_key      TYPE zsfi_boleto_ban_key,
          lv_key      TYPE string,
          lv_linha_dg TYPE string,
          lv_dtaid    TYPE dtaid_045t.


    SELECT SINGLE referenceinfo
       FROM i_housebankaccountlinkage
       INTO @DATA(lv_carteira)
       WHERE companycode = @cs_key-bukrs
         AND housebank = @cs_key-hbkid.

    IF lv_carteira IS INITIAL.
*      RAISE error
      lv_carteira = '999'. " Testes
    ENDIF.

    "bloco 1
*    cs_key-linha_dig(3)     = lv_carteira.
    cs_key-linha_dig+3(1)   = gc_boleto-nove.
    cs_key-linha_dig+4(5)  = gc_boleto-cinco_zero.
    lv_key = cs_key-linha_dig(9).
    cs_key-linha_dig+9(1) = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).

    "bloco 2
    cs_key-linha_dig+10(10) = cs_key-cod_barra+24(10).
    lv_key = cs_key-linha_dig+10(10).
    cs_key-linha_dig+20(1) = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).


    "bloco 3
    cs_key-linha_dig+21(10) = cs_key-cod_barra+34(10). "cs_key-xref3+3(8) && gc_boleto-livre.
*    set_dac_bb_bloco3( changing cs_key = cs_key ).
    lv_key = cs_key-linha_dig+21(10).
    cs_key-linha_dig+31(1) = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).
    cs_key-linha_dig+32(1) = cs_key-cod_barra+4(1).

    "Bloco 4
    CLEAR lv_key.
    fat_vencimento( EXPORTING iv_bldat = cs_key-netdt CHANGING cv_field = lv_key ).
    cs_key-linha_dig+33(4) = lv_key.

    CLEAR lv_key.
    valor_tit( EXPORTING iv_dmbr = cs_key-dmbtr CHANGING cv_field = lv_key ).
    cs_key-linha_dig+37(10) = lv_key.
  ENDMETHOD.

  METHOD execute_banrisul_linha_dig.

    DATA: ls_key       TYPE zsfi_boleto_ban_key,
          ls_boleto_nn TYPE zsfi_boleto_banc_nn,
          lv_key       TYPE string,
          lv_linha_dg  TYPE string,
          lv_dtaid     TYPE dtaid_045t.

    DATA: lv_agencia      TYPE char4,
          lv_2espaco      TYPE char2,
          lv_ncontrole    TYPE char7,
          lv_nossonum     TYPE char8,
          lv_nctrxx       TYPE char2,
          lv_dac          TYPE char1,
          lv_fatorvenc    TYPE char4,
          lv_valornominal TYPE char10,
          lv_calcdg_1     TYPE char9,
          lv_calcdg_2     TYPE char10,
          lv_calcdg_3     TYPE char10,
          lv_dgt1         TYPE char1,
          lv_dgt2         TYPE char1,
          lv_dgt3         TYPE char1.


    ls_boleto_nn = VALUE #( bukrs = cs_key-bukrs
                        belnr = cs_key-belnr
                        gjahr = cs_key-gjahr
                        buzei = cs_key-buzei
                        hbkid = cs_key-hbkid ) .

    lv_dtaid = get_convenio( ls_boleto_nn ).

    lv_agencia = cs_key-cod_barra+21(4).
    lv_ncontrole = cs_key-cod_barra+25(7).
    lv_nossonum = cs_key-cod_barra+32(8).
    lv_nctrxx = cs_key-cod_barra+42(2).
    lv_dac = cs_key-cod_barra+4(1).
    lv_fatorvenc = cs_key-cod_barra+5(4).
    lv_valornominal = cs_key-cod_barra+9(10).

    lv_calcdg_1 = cs_key-linha_dig(3) && 921 && lv_agencia(3).
    lv_key = lv_calcdg_1.
    lv_dgt1 = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).

    lv_calcdg_2 = lv_agencia+3(1) && lv_ncontrole && lv_nossonum(2).
    lv_key = lv_calcdg_2.
    lv_dgt2 = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).

    lv_calcdg_3 = lv_nossonum+2(6) && 40 && lv_nctrxx.
    lv_key = lv_calcdg_3.
    lv_dgt3 = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).



    cs_key-linha_dig = lv_calcdg_1 && lv_dgt1 &&
                       lv_calcdg_2 && lv_dgt2 &&
                       lv_calcdg_3 && lv_dgt3 && lv_dac && lv_fatorvenc && lv_valornominal.


*    "bloco 1
*    cs_key-linha_dig+3(1)   = gc_boleto-nove.
*    cs_key-linha_dig+4(1)   = 2.
*    cs_key-linha_dig+5(1)   = 1.
*    cs_key-linha_dig+6(3)  = cs_key-cod_barra+21(3).
*    lv_key = cs_key-linha_dig(9).
*    cs_key-linha_dig+9(1) = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).
*
*    "bloco 2
*    cs_key-linha_dig+10(2)  = 00.
*    cs_key-linha_dig+12(1)  = cs_key-cod_barra+24(1).
*    cs_key-linha_dig+13(7) = cs_key-cod_barra+25(7).
*    cs_key-linha_dig+17(2) = cs_key-cod_barra+32(2).
*    lv_key = cs_key-linha_dig+10(10).
*    cs_key-linha_dig+20(1) = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).
*
*    "bloco 3
*    cs_key-linha_dig+21(2) = 00.
*    cs_key-linha_dig+23(8) = cs_key-cod_barra+34(8).
*    lv_key = cs_key-linha_dig+21(10).
*    cs_key-linha_dig+31(1) = set_dac_bb_ld_mod10( EXPORTING iv_key = lv_key ).
*    cs_key-linha_dig+32(1) = cs_key-cod_barra+4(1).
*
*    "Bloco 4
*    CLEAR lv_key.
*    fat_vencimento( EXPORTING iv_bldat = cs_key-netdt CHANGING cv_field = lv_key ).
*    cs_key-linha_dig+33(4) = lv_key.
*
*    CLEAR lv_key.
*    valor_tit( EXPORTING iv_dmbr = cs_key-dmbtr CHANGING cv_field = lv_key ).
*    cs_key-linha_dig+37(10) = lv_key.

  ENDMETHOD.


  METHOD set_dac_bb_ld_mod10.

    gv_string = iv_key.

    DATA(lo_calc_mod10) = NEW zclfi_calc_modulo( ).

    lo_calc_mod10->execute( EXPORTING iv_modulo = abap_false
                                      iv_mod10  = abap_true
                            IMPORTING ev_dac = gv_dac
                            CHANGING cv_calc = gv_string ).

    rv_dac = gv_dac(1).

*   lo_calc_mod10->execute( EXPORTING iv_modulo = abap_false IMPORTING ev_dac = gv_dac CHANGING cv_calc = gv_string ).

*    cs_key-linha_dig = gv_string && gv_dac && COND #( WHEN cs_key-hbkid NE gc_boleto-bb THEN cs_key-xref3+2 ELSE cs_key-xref3+5 ).

*    CONDENSE  cs_key-linha_dig NO-GAPS.
    CLEAR: gv_dac, gv_string.


  ENDMETHOD.


  METHOD set_bb_nn_dac.

    DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

    "Select BSEG realizado para buscar forma de pagamento - 25.02.2022
    SELECT SINGLE zlsch
      FROM bseg
      INTO @DATA(lv_zlsch)
      WHERE bukrs EQ @is_key-bukrs
        AND belnr EQ @is_key-belnr
        AND gjahr EQ @is_key-gjahr
        AND buzei EQ @is_key-buzei.

    SELECT SINGLE dtaid
      FROM t045t
      INTO @DATA(lv_dtaid)
      WHERE bukrs = @is_key-bukrs
        AND hbkid = @is_key-hbkid
        AND zlsch EQ @lv_zlsch.

    CHECK sy-subrc = 0.


    gv_string = lv_dtaid && is_key-xref3.
    lo_calc_mod11->execute( EXPORTING iv_modulo = abap_true IMPORTING ev_dac = gv_dac CHANGING cv_calc = gv_string ).

  ENDMETHOD.


  METHOD execute_bb.

    DATA: ls_boleto_nn TYPE zsfi_boleto_banc_nn,
          lv_tamanho   TYPE i.

    ls_boleto_nn = VALUE #( bukrs = cs_key-bukrs
                            belnr = cs_key-belnr
                            gjahr = cs_key-gjahr
                            buzei = cs_key-buzei
                            hbkid = cs_key-hbkid ) .

    DATA(lv_dtaid) = get_convenio( ls_boleto_nn ).

    IF lv_dtaid IS NOT INITIAL.

      CONDENSE lv_dtaid NO-GAPS.
      lv_tamanho = strlen( lv_dtaid ).

      CASE lv_tamanho.

        WHEN 4 OR 6.
          set_nn( CHANGING cs_key = cs_key ).
          set_bb_ag_conta( CHANGING cs_key = cs_key  ).
          set_free( CHANGING cs_key = cs_key ).

        WHEN 7.
          cs_key-cod_barra+19(6) =  gc_boleto-seis_zero.
          cs_key-cod_barra+25(7) =  lv_dtaid.
          cs_key-cod_barra+32(10) =  cs_key-xref3+7(10).
          set_free( CHANGING cs_key = cs_key ).

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD execute_itau.

    set_nn_ita_12( CHANGING cs_key = cs_key ).
    set_age_itau( CHANGING cs_key = cs_key ) .

  ENDMETHOD.


  METHOD execute_cef.

    DATA: ls_boleto_nn TYPE zsfi_boleto_banc_nn,
          lv_tamanho   TYPE i.

    DATA: lv_string       TYPE string,
          lv_beneficiario TYPE string,
          lv_dgv          TYPE string.

    ls_boleto_nn = VALUE #( bukrs = cs_key-bukrs
                            belnr = cs_key-belnr
                            gjahr = cs_key-gjahr
                            buzei = cs_key-buzei
                            hbkid = cs_key-hbkid ) .

    DATA(lv_dtaid) = get_convenio( ls_boleto_nn ).
    DATA(lv_conv) = strlen( lv_dtaid ).
    DATA(lv_dif) = 7 - lv_conv .

    lv_beneficiario = lv_dtaid.

    IF lv_dif > 0.

      DO lv_dif TIMES.
        lv_beneficiario = 0 && lv_beneficiario.
      ENDDO.

    ENDIF.

    lv_string =
                lv_beneficiario &&
                cs_key-xref3+2(3) &&
                cs_key-xref3(1) &&
                cs_key-xref3+5(3) &&
                cs_key-xref3+1(1) &&
                cs_key-xref3+8(9) &&
                cs_key-xref3+17(1).

    cs_key-cod_barra = cs_key-cod_barra && lv_string .

  ENDMETHOD.


  METHOD execute_banrisul.

    DATA: ls_boleto_nn TYPE zsfi_boleto_banc_nn,
          lv_tamanho   TYPE i.

    DATA: lv_string       TYPE string,
          lv_age          TYPE string,
          lv_nn           TYPE char10,
          lv_dg1          TYPE string,
          lv_dg2          TYPE string,
          lv_beneficiario TYPE string,
          lv_dgv          TYPE string.


    DATA(lv_pos) = strlen( gs_t012-bankl ) - 4.
    lv_age = gs_t012-bankl+lv_pos(4).

    ls_boleto_nn = VALUE #( bukrs = cs_key-bukrs
                            belnr = cs_key-belnr
                            gjahr = cs_key-gjahr
                            buzei = cs_key-buzei
                            hbkid = cs_key-hbkid ) .

    DATA(lv_dtaid) = get_convenio( ls_boleto_nn ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = cs_key-xref3
      IMPORTING
        output = lv_nn.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_nn
      IMPORTING
        output = lv_nn.

    lv_string = 2 &&
                1 && "constante
                lv_age &&
                lv_dtaid+4(7) &&
                lv_nn(8) &&
                40 .

    DATA(lo_calc_mod11) = NEW zclfi_calc_modulo( ).

    lo_calc_mod11->execute(
                    EXPORTING
                        iv_mod10 = abap_true
                        iv_modulo = abap_false
                        iv_barinsul = abap_true
                     IMPORTING
                        ev_dac = lv_dg1
                     CHANGING cv_calc = lv_string ).

    lv_string = lv_string && lv_dg1 .

    CONDENSE lv_string NO-GAPS.
    lo_calc_mod11->execute(
                    EXPORTING
                        iv_modulo = abap_true
                        iv_barinsul = abap_true
                        iv_dig1 = lv_dg1
                     IMPORTING
                        ev_dac = lv_dgv
                     CHANGING cv_calc = lv_string
                     ).

    lv_string = lv_string && lv_dgv .

    cs_key-cod_barra = cs_key-cod_barra && lv_string .

  ENDMETHOD.


  METHOD execute_sicredi.

    DATA: ls_boleto_nn TYPE zsfi_boleto_banc_nn,
          lv_tamanho   TYPE i.

    DATA: lv_string       TYPE string,
          lv_age          TYPE string,
          lv_beneficiario TYPE string,
          lv_dgv          TYPE string.


    DATA(lv_pos) = strlen( gs_t012-bankl ) - 4.
    lv_age = gs_t012-bankl+lv_pos(4).

    ls_boleto_nn = VALUE #( bukrs = cs_key-bukrs
                            belnr = cs_key-belnr
                            gjahr = cs_key-gjahr
                            buzei = cs_key-buzei
                            hbkid = cs_key-hbkid ) .

    DATA(lv_dtaid) = get_convenio( ls_boleto_nn ).

    lv_string = 1 &&
                1 && "constante
                cs_key-xref3(9) &&
                lv_age &&
                99 &&
                lv_dtaid(5) &&
                1 &&
                0 .

    cs_key-cod_barra = cs_key-cod_barra && lv_string .

  ENDMETHOD.


  METHOD set_bra_convenio.

    "Select BSEG realizado para buscar forma de pagamento - 25.02.2022
    SELECT SINGLE zlsch
      FROM bseg
      INTO @DATA(lv_zlsch)
      WHERE bukrs EQ @cs_key-bukrs
        AND belnr EQ @cs_key-belnr
        AND gjahr EQ @cs_key-gjahr
        AND buzei EQ @cs_key-buzei.

    SELECT SINGLE dtaid
      FROM t045t
      INTO @DATA(lv_dtaid)
      WHERE bukrs = @cs_key-bukrs
        AND hbkid = @cs_key-hbkid
        AND zlsch EQ @lv_zlsch.

    CHECK sy-subrc = 0.

    cs_key-cod_barra+19(6) = lv_dtaid(6).

  ENDMETHOD.


  METHOD set_nn.
    cs_key-cod_barra+19(11) = cs_key-xref3(11).
  ENDMETHOD.


  METHOD set_nn_bb_17.
    cs_key-cod_barra+25(17) = cs_key-xref3(17).
  ENDMETHOD.


  METHOD set_nn_bb_12.
    cs_key-cod_barra+30(12) = cs_key-xref3(12).
  ENDMETHOD.


  METHOD set_nn_ita_12.
    cs_key-cod_barra+19(12) = cs_key-xref3(12).
  ENDMETHOD.


  METHOD set_age_itau.

    SELECT SINGLE bankn ,bkont
    FROM t012k
        WHERE bukrs EQ @cs_key-bukrs
          AND hbkid EQ @cs_key-hbkid
        INTO @DATA(ls_t012k).

    IF sy-subrc = 0.

      DATA(lv_pos) = strlen( gs_t012-bankl ) - 4.

      gv_string = gs_t012-bankl+lv_pos(4) && ls_t012k-bankn(5) && ls_t012k-bkont+1(1).

      cs_key-cod_barra = cs_key-cod_barra  && gv_string && gv_dac && '000'.

      CONDENSE cs_key-cod_barra.

      CLEAR: gv_dac, gv_string.

    ENDIF.

  ENDMETHOD.


  METHOD set_free.
    cs_key-cod_barra+42(2) = gc_boleto-livre.
  ENDMETHOD.


  METHOD envia_email_single.

    get_boleto_info(
      EXPORTING
        iv_belnr = is_key-belnr
        iv_gjahr = is_key-gjahr
        iv_bukrs = is_key-bukrs
        iv_buzei = is_key-buzei
    ).

    IF gs_boleto_info IS NOT INITIAL.

      processa_boleto_infor( ).

      "@@ Imprimi formulario para anexo
      get_formulario_otf(  ).

      DATA(ls_email) = VALUE zsfi_boleto_banc_em(
            bukrs = gs_boleto_info-empresa
            belnr = gs_boleto_info-documento
            gjahr = gs_boleto_info-ano
            buzei = gs_boleto_info-item
            dmbtr = gs_boleto_info-valor
            kunnr = gs_boleto_info-cliente
            netdt = gs_boleto_info-netdt  ).

      enviar_email(
        EXPORTING
          is_boletos      = ls_email
        EXCEPTIONS
          not_found_email = 1
          not_convert_otf = 2
          OTHERS          = 3
      ).

      CASE sy-subrc.
        WHEN 1.
          "Email não cadastrado para o cliente
          MESSAGE e000 WITH TEXT-013 INTO gv_dummy.
          APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 ) TO et_msg.

        WHEN 2.
          "Erro na impressão do formulário
          MESSAGE e000 WITH TEXT-014 INTO gv_dummy.
          APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 ) TO et_msg.

        WHEN 3.
          "Erro no envio de email
          MESSAGE e000 WITH TEXT-015 INTO gv_dummy.
          APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 ) TO et_msg.

        WHEN OTHERS.
          "Email enviado com sucesso
          MESSAGE s000 WITH TEXT-016 INTO gv_dummy.
          APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 ) TO et_msg.
      ENDCASE.

    ELSE.

      "Dados não localizados
      MESSAGE e000 WITH TEXT-017 is_key-belnr INTO gv_dummy.
      APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 ) TO et_msg.
    ENDIF.


  ENDMETHOD.


  METHOD enviar_email.


    DATA: lv_data       TYPE char12,
          lv_valor      TYPE char30,
          lv_desc       TYPE bcs_description,
          lv_noreply    TYPE bcs_visname,
          lv_corpoemail TYPE string.

    me->seleciona_dado_em(
    EXPORTING
      is_boletos_em   = is_boletos
    EXCEPTIONS
      not_found_email = 1
      OTHERS          = 3
     ).

    IF sy-subrc EQ 1.
      RAISE not_found_email.
    ENDIF.

    me->convert_otf(
    EXPORTING
      it_otf = gt_otf
    EXCEPTIONS
      not_convert_otf = 2
      OTHERS          = 3
    ).

    IF sy-subrc EQ 2.
      RAISE not_convert_otf.
    ENDIF.

    DATA(lv_com_copia) = set_com_copia( ).

    TRY.

        DATA(lo_msg) = NEW cl_bcs_message( ).

        lo_msg->set_subject( CONV #( TEXT-001 )  ).

        IF it_lista_email IS INITIAL.
          lo_msg->add_recipient(
            EXPORTING
              iv_address      = CONV bcs_address( gs_bol-email )
              iv_visible_name = CONV bcs_visname( gs_bol-email )
          ).
        ELSE.
          LOOP AT it_lista_email ASSIGNING FIELD-SYMBOL(<fs_lista_email>). "INTO DATA(lv_lista_email).
            DATA(lv_lista_email) = <fs_lista_email>.
            lo_msg->add_recipient(
              EXPORTING
                iv_address      = CONV bcs_address( lv_lista_email )
                iv_visible_name = CONV bcs_visname( lv_lista_email )
            ).
          ENDLOOP.
        ENDIF.

*        lo_msg->set_reply_to(
*          EXPORTING
*            iv_address      = CONV bcs_address( lv_com_copia )
*            iv_visible_name = CONV bcs_visname( lv_com_copia )
*        ).
        IF lv_com_copia IS NOT INITIAL.

          lo_msg->add_recipient(
          EXPORTING
            iv_address      = lv_com_copia
            iv_visible_name = lv_com_copia
            iv_copy         = abap_true
          ).

        ENDIF.

        MOVE TEXT-026 TO lv_noreply.


        DATA lv_address TYPE bcs_visname.
        DATA(lv_email) = get_noreply( ).
        MOVE lv_email TO lv_address.


        lo_msg->set_sender(
          EXPORTING
            iv_address      = lv_address
            iv_visible_name = lv_noreply " gc_boleto-visname
        ).

        WRITE gs_boleto_info-budat TO lv_data USING EDIT MASK '__/__/____'.
        WRITE is_boletos-dmbtr TO lv_valor CURRENCY 'BRL'.

        CONCATENATE TEXT-002  gs_bol-name1 TEXT-027 INTO DATA(lv_cliente) SEPARATED BY space.
*        CONCATENATE TEXT-003 gs_boleto_info-xblnr TEXT-010 INTO DATA(lv_nf) SEPARATED BY space.

        CONCATENATE TEXT-003 gs_boleto_info-xblnr TEXT-010 INTO DATA(lv_nf) SEPARATED BY space.
        CONCATENATE TEXT-011 lv_data '-' gs_boleto_info-empresatxt '-' 'R$' lv_valor INTO DATA(lv_empresa) SEPARATED BY space.


        CASE is_boletos-bukrs.

          WHEN '2000'.

            lo_msg->set_main_doc(
                        EXPORTING
                    iv_contents_txt = |<p><strong><em>|      &&
                                      lv_cliente             &&
                                      |</em></strong></p>|   &&
                                      |<p><em>|     &&
                                      TEXT-029      &&
                                      |</em></p>|   &&
                                      |<p><em>|     &&
                                      TEXT-028      &&
                                      |</em></p>|   &&
                                      |<p><em>|     &&
                                      TEXT-035      &&
                                      |</em></p>|   &&
                                      |<p><em>|     &&
                                      TEXT-005      &&
                                      |</em></p>|   &&
                                      |<p><strong><em>|      &&
                                      TEXT-006
                                    iv_doctype      = 'htm'    " Document Category
                                ).

          WHEN '2500'.

            lo_msg->set_main_doc(
                     EXPORTING
                 iv_contents_txt = |<p><strong><em>|      &&
                                   lv_cliente             &&
                                   |</em></strong></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-029      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-030      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-035      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-005      &&
                                   |</em></p>|   &&
                                   |<p><strong><em>|      &&
                                   TEXT-006
                                 iv_doctype      = 'htm'    " Document Category
                             ).


          WHEN '3000'.

            lo_msg->set_main_doc(
         EXPORTING
           iv_contents_txt = |<p><strong><em>|      &&
                             lv_cliente             &&
                             |</em></strong></p>|   &&
                             |<p><em>|     &&
                             TEXT-029      &&
                             |</em></p>|   &&
                             |<p><em>|     &&
                             TEXT-036      &&
                             |</em></p>|   &&
                             |<p><em>|     &&
                             TEXT-035      &&
                             |</em></p>|   &&
                             |<p><em>|     &&
                             TEXT-005      &&
                             |</em></p>|   &&
                             |<p><strong><em>|      &&
                             TEXT-006               &&
                             |</em></strong></p>|
                             iv_doctype      = 'htm'    " Document Category
       ).

          WHEN '4000'.

            lo_msg->set_main_doc(
                     EXPORTING
                 iv_contents_txt = |<p><strong><em>|      &&
                                   lv_cliente             &&
                                   |</em></strong></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-029      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-031      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-035      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-005      &&
                                   |</em></p>|   &&
                                   |<p><strong><em>|      &&
                                   TEXT-006
                                 iv_doctype      = 'htm'    " Document Category
                             ).


          WHEN '4500'.

            lo_msg->set_main_doc(
                     EXPORTING
                 iv_contents_txt = |<p><strong><em>|      &&
                                   lv_cliente             &&
                                   |</em></strong></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-029      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-032      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-033      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-034      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-035      &&
                                   |</em></p>|   &&
                                   |<p><em>|     &&
                                   TEXT-005      &&
                                   |</em></p>|   &&
                                   |<p><strong><em>|      &&
                                   TEXT-006
                                 iv_doctype      = 'htm'    " Document Category
                             ).

        ENDCASE.


        MOVE TEXT-023 TO lv_desc.



        DATA(lv_filename) = TEXT-023 && |_| &&
                         |{ gs_boleto_info-empresa  ALPHA = OUT }| && |_| &&
                         |{ gs_boleto_info-cliente  ALPHA = OUT }| && |_| &&
                         |{ gs_boleto_info-documento }| && |_| &&
                         |{ gs_boleto_info-item ALPHA = OUT }| .

        lo_msg->add_attachment(
            EXPORTING
            iv_doctype      = gc_boleto-doctype        "'PDF'
            iv_description  =  lv_filename "gc_boleto-description
            iv_filename     = lv_filename
            iv_contents_bin = gv_pdf_file
        ).


        IF gv_email_beh = abap_true.
          lo_msg->set_send_immediately( abap_true ).
        ELSE.
          lo_msg->set_update_task( abap_true ).
        ENDIF.

        lo_msg->send( ).

      CATCH cx_bcs_send INTO DATA(ls_erro).

        RAISE  not_found_email.

    ENDTRY.

  ENDMETHOD.


  METHOD seleciona_dado_em.


    CLEAR: gs_bol.

    SELECT SINGLE smtp_addr, name1 FROM kna1 AS k
        INNER JOIN adr6 AS a
        ON k~adrnr = a~addrnumber
      WHERE kunnr EQ @is_boletos_em-kunnr
        INTO ( @gs_bol-email, @gs_bol-name1 ).       "#EC CI_SEL_NESTED

    IF sy-subrc NE 0.
      RAISE  not_found_email.
    ENDIF.


  ENDMETHOD.


  METHOD set_com_copia.

    RETURN.

**    CONSTANTS lc_com_copia TYPE tvarv-name VALUE 'Z_SD_EMAILFATURAMENTO'.
**
**    rv_com_copia = zclca_stvarv=>get_value( lc_com_copia ).

  ENDMETHOD.


  METHOD get_noreply.

    RETURN.

**    CONSTANTS lc_noreply TYPE tvarvc-name VALUE 'Z_CA_EMAIL_META_NOREPLY'.
**
**    rv_email = zclca_stvarv=>get_value( lc_noreply ).

  ENDMETHOD.


  METHOD set_bb_ag_conta.

    DATA: lv_conta_8 TYPE n LENGTH 8 .

    SELECT SINGLE banknumber, bankaccount
    INTO ( @DATA(lv_agen), @DATA(lv_conta) )
    FROM i_housebankaccountlinkage
    WHERE companycode = @cs_key-bukrs
       AND housebank = @cs_key-hbkid.

    IF sy-subrc = 0.

      lv_conta_8 = lv_conta.

      cs_key-cod_barra+30(4) = lv_agen+4(4).
      cs_key-cod_barra+34(8) = lv_conta_8.

    ENDIF.

  ENDMETHOD.


  METHOD get_convenio.

    "--- Convenio ------
    "Select BSEG realizado para buscar forma de pagamento - 25.02.2022
    SELECT SINGLE zlsch
      FROM bseg
      INTO @DATA(lv_zlsch)
      WHERE bukrs EQ @is_key-bukrs
        AND belnr EQ @is_key-belnr
        AND gjahr EQ @is_key-gjahr
        AND buzei EQ @is_key-buzei.

    SELECT SINGLE dtaid
      FROM t045t
      INTO @rv_convenio
      WHERE bukrs = @is_key-bukrs
        AND hbkid = @is_key-hbkid
        AND zlsch EQ @lv_zlsch.

  ENDMETHOD.


  METHOD get_carteira.

    SELECT SINGLE referenceinfo
    FROM i_housebankaccountlinkage
    INTO @rv_carteira
    WHERE companycode = @is_key-bukrs
      AND housebank = @is_key-hbkid.

    CASE is_key-hbkid.

      WHEN gc_boleto-itau.

        SELECT SINGLE portfolio,
                      register_form,
                      document_type
                FROM idfipaym_ccha
                INTO @DATA(ls_ccha)
                WHERE bukrs = @is_key-bukrs
                AND   hbkid = @is_key-hbkid
                AND   hktid = @rv_carteira
                AND   rzawe = 'E'.

        rv_carteira        = |{ ls_ccha-portfolio }{ ls_ccha-register_form }{ ls_ccha-document_type }|.

      WHEN gc_boleto-bbrasil.

        SELECT SINGLE portfolio,
                      register_form,
                      document_type
                FROM idfipaym_ccha
                INTO @ls_ccha
                WHERE bukrs = @is_key-bukrs
                AND   hbkid = @is_key-hbkid
                AND   hktid = @rv_carteira
                AND   rzawe = 'E'.

        rv_carteira        = |{ ls_ccha-portfolio }{ ls_ccha-register_form }|.

      WHEN gc_boleto-banrisul.

        SELECT SINGLE portfolio,
                      register_form,
                      document_type
                FROM idfipaym_ccha
                INTO @ls_ccha
                WHERE bukrs = @is_key-bukrs
                AND   hbkid = @is_key-hbkid
                AND   hktid = @rv_carteira
                AND   rzawe = 'E'.

        rv_carteira        = ls_ccha-portfolio.

      WHEN gc_boleto-cef.

        SELECT SINGLE portfolio,
                      register_form,
                      document_type
                FROM idfipaym_ccha
                INTO @ls_ccha
                WHERE bukrs = @is_key-bukrs
                AND   hbkid = @is_key-hbkid
                AND   hktid = @rv_carteira
                AND   rzawe = 'E'.

        rv_carteira        = ls_ccha-portfolio.

      WHEN OTHERS.

    ENDCASE.


  ENDMETHOD.


  METHOD gerar_boleto_fat.

    CLEAR gt_msg.

    "@@Busca dados basicos
    get_boleto_info(
      EXPORTING
        iv_belnr = is_key-belnr
        iv_gjahr = is_key-gjahr
        iv_bukrs = is_key-bukrs
        iv_buzei = is_key-buzei   ).

    IF gs_boleto_info IS NOT INITIAL.

      "@@ Processa dados
      processa_boleto_infor(   ).

      "@@ Imprimi formulario para anexo
      get_formulario_otf( me->gv_printer ).

      save_boleto_al11(
        EXPORTING
                is_key = is_key
          ).

      envia_email_single(
              EXPORTING
                is_key = is_key
              IMPORTING
                et_msg = et_msg
            ).


    ELSE.
      "Erro ao tentar buscar dados básicos do documento
      MESSAGE e004 WITH is_key-belnr is_key-gjahr is_key-bukrs  INTO gv_dummy.
      msg(  ).
    ENDIF.

    et_msg = gt_msg.

  ENDMETHOD.


  METHOD save_boleto_al11.

    DATA: lv_filename     TYPE char120,
          lv_diretorio    TYPE char50,
          lv_pdf_filesize TYPE i.

    DATA: ls_pdf TYPE char80,
          lt_pdf TYPE TABLE OF char80.

    me->convert_otf(
    EXPORTING
      it_otf = gt_otf
    EXCEPTIONS
      not_convert_otf = 2
      OTHERS          = 3
    ).

    DATA(lo_param) = NEW zclca_tabela_parametros( ).
    TRY.


        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_boleto-modulo
            iv_chave1 = gc_boleto-chave1
            iv_chave2 = gc_boleto-chave2
            iv_chave3 = gc_boleto-chave3
          IMPORTING
            ev_param  = lv_diretorio
       ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
    ENDTRY.

    " convert xstring to binary
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = gv_pdf_file
      TABLES
        binary_tab = lt_pdf.

    process_boleto_name(  ).
    CONDENSE gv_boleto_name NO-GAPS.

    CONCATENATE lv_diretorio '/' gv_boleto_name INTO lv_filename.

    OPEN DATASET lv_filename FOR OUTPUT IN BINARY MODE.

    IF sy-subrc IS INITIAL.

      LOOP AT lt_pdf INTO ls_pdf.
        TRANSFER ls_pdf TO lv_filename
          NO END OF LINE.
      ENDLOOP.

      CLOSE DATASET lv_filename.

    ENDIF.

  ENDMETHOD.


  METHOD process_boleto_name.

    DATA: lv_cliente TYPE string,
          lv_item    TYPE string.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_boleto_info-cliente
      IMPORTING
        output = lv_cliente.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_boleto_info-item
      IMPORTING
        output = lv_item.

    CLEAR: gv_boleto_name.
    CONCATENATE TEXT-023
              '_'
              gs_boleto_info-empresa
              '_'
              lv_cliente
              '_'
              gs_boleto_info-documento
              '_'
              lv_item
              gc_boleto-pdf
              INTO gv_boleto_name.


  ENDMETHOD.

  METHOD constructor.
    me->gv_printer = iv_printer.
  ENDMETHOD.

ENDCLASS.
