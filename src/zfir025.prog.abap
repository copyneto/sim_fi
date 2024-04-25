*****************************************************************************************
*                                                                                       *
*                     ********************************************                      *
*                     *                                          *                      *
*                     *                                          *                      *
*                     *        CONFIDENCIAL E PROPRIETÁRIO       *                      *
*                     *       TODOS OS DIREITOS RESERVADOS       *                      *
*                     ********************************************                      *
*                                                                                       *
*****************************************************************************************
* NOME DO PROGRAMA    : ZFIR025                                                         *
* TRANSAÇÃO           : ZXXXXX                                                          *
* TÍTULO DO PROGRAMA  : Integração Cotação de Moedas X SAP                              *
* DESCRIÇÃO           : Atualização de taxas de cambio                                  *
* PROGRAMADOR         : Rafael Fernandes                                                 *
*                                                                                       *
*---------------------------------------------------------------------------------------*
* HISTÓRICO DE MUDANÇAS:                                                                *
* MUD |DATA       |AUTOR              |REQUEST #    |DESCRIÇÃO                          *


REPORT  zfir025.
CONSTANTS: c_b VALUE 'B'.

DATA: lv_data TYPE sy-datum.

TABLES: adr6.", zfit004.
SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_backg     AS CHECKBOX DEFAULT 'X',
              p_data      TYPE sy-datum DEFAULT lv_data,
              p_proxy     TYPE string DEFAULT '147.204.152.42', "thttp-proxy,
              p_user      TYPE string DEFAULT '3128', "thttp-puser,
              p_pass      TYPE thttp-ppassword NO-DISPLAY,
              p_euro(128) TYPE c DEFAULT 'https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml' NO-DISPLAY,
              p_bace(128) TYPE c DEFAULT 'https://www4.bcb.gov.br/Download/fechamento/'.

  SELECT-OPTIONS s_email FOR adr6-smtp_addr NO INTERVALS.

SELECTION-SCREEN END OF BLOCK a3.

DATA: v_file(128)   TYPE c,
      v_data        TYPE d,
      v_email_corpo TYPE string,
      taxa          TYPE string,
      v_tx          TYPE p DECIMALS 6,
      v_urlbcb      TYPE string,
      v_urlbce      TYPE string.

DATA: response_headers(80) OCCURS 0 WITH HEADER LINE.
DATA: response_entity_body(120) OCCURS 0 WITH HEADER LINE.
DATA: v_response_entity_body(120) TYPE c.

TYPES: BEGIN OF y_linhas,
         linha TYPE string,
       END OF y_linhas.

TYPES: BEGIN OF y_bcb,
         data(10)           TYPE c,
         codmoeda(3)        TYPE c,
         tipo(1)            TYPE c,
         moeda(3)           TYPE c,
         taxacompra(15)     TYPE c,
         taxavenda(15)      TYPE c,
         paridadecompra(15) TYPE c,
         paridadevenda(15)  TYPE c,
       END OF y_bcb.

TYPES: BEGIN OF y_curr,
         count TYPE i,
         ukurs TYPE tcurr-ukurs,
       END OF y_curr.

DATA: BEGIN OF t_linhas OCCURS 1,
        linha(500) TYPE c,
      END OF t_linhas.

DATA: t_exch_rate_list  TYPE STANDARD TABLE OF bapi1093_0 WITH HEADER LINE.

DATA: t_zfit004     TYPE STANDARD TABLE OF zfit004 WITH HEADER LINE.
DATA: t_zfit004_brl TYPE STANDARD TABLE OF zfit004 WITH HEADER LINE.

DATA: wa_return TYPE bapiret2,
      wa_curr   TYPE y_curr.

DATA:  t_bcb    TYPE STANDARD TABLE OF y_bcb WITH HEADER LINE.

DATA: v_linha(60000)   TYPE c,
      v_csv(60000)     TYPE c,
      v_xml(60000)     TYPE c,
      v_status_text(2) TYPE c,
      v_usd_eur        TYPE string,
      data             TYPE sy-datum,
      v_data_email(10) TYPE c.

DATA: v_date    TYPE scal-date,
      v_facdate TYPE scal-date.

TYPES: BEGIN OF y_tcurr.
         INCLUDE STRUCTURE tcurr.
TYPES: END OF   y_tcurr.

TYPES: BEGIN OF y_tcurw,
         kurst TYPE tcurw-kurst,
         curvw TYPE tcurw-curvw,
       END OF   y_tcurw.

DATA: t_tcurr TYPE SORTED TABLE OF y_tcurr WITH UNIQUE KEY kurst fcurr tcurr.
DATA: t_tcurw TYPE SORTED TABLE OF y_tcurw WITH UNIQUE KEY kurst.

DATA: wa_tcurr TYPE y_tcurr.
DATA: wa_tcurw TYPE y_tcurw.

DATA : v_begda_ex(10)         TYPE c,
       v_endda_ex(10)         TYPE c,
       v_begda                TYPE d,
       v_endda                TYPE d,
       v_prox_dia             TYPE d,
       v_ultimodiautil        TYPE d,
       v_ultimodiautil_ex(10) TYPE c,
       v_diautil(1)           TYPE c.

*- INI - HSP --------------------------------------------------------*
"// Solução para atualização automática da cotação
INCLUDE zfir025_auto.
*- FIM - HSP --------------------------------------------------------*

INITIALIZATION.
  lv_data = sy-datum - 1.
  p_data = lv_data.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK screen-name EQ 'P_PASS'.
    screen-invisible = 1.
    MODIFY SCREEN.
  ENDLOOP.

*INITIALIZATION.

**-------------------------------------------------------------------**
** START-OF-SELECTION.
**-------------------------------------------------------------------**
START-OF-SELECTION.

  PERFORM f_check_data.

  IF p_data IS INITIAL.
    p_data = sy-datum.
  ENDIF.

  v_data = p_data.
  v_date = p_data.

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option      = '-'
      date                = v_date
      factory_calendar_id = 'BR'
    IMPORTING
      date                = v_facdate
    EXCEPTIONS
      OTHERS              = 7.

  SELECT kurst curvw
    INTO TABLE t_tcurw
    FROM tcurw
   WHERE spras = sy-langu.

  IF v_facdate EQ v_date. " Verifica se é dia útil

    v_diautil = 'X'.
    PERFORM interface_bcb.

  ELSE.

    CONCATENATE v_facdate+6(2) v_facdate+4(2) v_facdate(4) INTO v_ultimodiautil.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = v_ultimodiautil
      IMPORTING
        output = v_ultimodiautil_ex.

    "Busca os valores de todos as cotações do ultimo dia útil a serem lançados nos fim de samana e feriados
    SELECT * FROM tcurr INTO TABLE t_tcurr WHERE gdatu EQ v_ultimodiautil_ex.

    IF sy-subrc EQ 0.
      " Apaga a taxa VB para não ser replicada
      DELETE t_tcurr
       WHERE kurst EQ 'VB'.

    ENDIF.


  ENDIF.

  PERFORM cabecalho_mail.

  IF  v_status_text EQ 'OK' OR v_diautil NE 'X'.

    " Categoria M
    PERFORM header_cat USING 'M'.
    PERFORM lanca_taxa_bcb  USING 'USD' 'BRL' 'M' 'A' '1'   ''  '' ''.
    PERFORM lanca_taxa_bcb  USING 'EUR' 'BRL' 'M' 'B' '1'   ''  '' ''.

    " Categoria V
   PERFORM header_cat USING 'G'.
   PERFORM lanca_taxa_bcb  USING 'USD' 'BRL' 'G' 'A' '1'   '' 'X' ''.
   PERFORM lanca_taxa_bcb  USING 'EUR' 'BRL' 'G' 'B' '1'   '' 'X' ''.



*   PERFORM lanca_taxa_bcb  USING 'HKD' 'BRL' 'M' 'A' '100' ''  '' ''.
*   PERFORM lanca_taxa_bcb  USING 'JPY' 'BRL' 'M' 'B' '100' ''  '' ''.
*   PERFORM lanca_taxa_bcb  USING 'USD' 'HKD' 'M' 'A' '1'   'X' '' ''.
*
*   PERFORM lanca_taxa_bcb  USING 'USD' 'JPY' 'M' 'B' '1' 'X' '' ''.
*   PERFORM lanca_taxa_bcb  USING 'GBP' 'BRL' 'M' 'B' '1' ''  '' ''.
*   PERFORM lanca_taxa_bcb  USING 'USD' 'GBP' 'M' 'B' '1' 'X' '' 'X'.

    PERFORM fecha_cat.

    " Categoria V
*   PERFORM header_cat USING 'V'.
*   PERFORM lanca_taxa_bcb  USING 'USD' 'BRL' 'V' 'A' '1'   '' 'X' ''.
*   PERFORM lanca_taxa_bcb  USING 'EUR' 'BRL' 'V' 'B' '1'   '' 'X' ''.
*   PERFORM lanca_taxa_bcb  USING 'HKD' 'BRL' 'V' 'A' '100' '' 'X' ''.
*   PERFORM lanca_taxa_bcb  USING 'JPY' 'BRL' 'V' 'B' '100' '' 'X' ''.
*   PERFORM fecha_cat.

    " Categoria VB
*    PERFORM header_cat USING 'VB'.
*    PERFORM lanca_taxa_bcb  USING 'USD' 'BRL' 'VB' 'A' '1' '' 'X' ''.
*    PERFORM fecha_cat.

    " Categoria H
*    PERFORM header_cat USING 'H'.
*    PERFORM lanca_taxa_bcb USING 'USD' 'BRL' 'H' 'A' '1'   '' '' ''.
*    PERFORM lanca_taxa_bcb USING 'HKD' 'BRL' 'H' 'B' '100' '' '' ''.
*    PERFORM fecha_cat.
*
    " Categoria EURX
*    PERFORM header_cat USING 'EURX'.
*    PERFORM lanca_taxa_bcb  USING 'USD' 'BRL' 'EURX' 'A' '1' '' '' ''.
*    PERFORM lanca_taxa_bcb  USING 'EUR' 'BRL' 'EURX' 'B' '1' '' '' ''.

  ENDIF.

*  PERFORM lanca_taxa_bce USING 'USD' 'EUR' 'EURX' 'A' '1' ''. " Usa Banco central Europeu
  PERFORM fecha_cat.
  PERFORM mensal.

  PERFORM salva_dados.
*- INI - HSP --------------------------------------------------------*
  PERFORM zf_auto_ini.
*- FIM - HSP --------------------------------------------------------*
  PERFORM envia_mail.


*&---------------------------------------------------------------------*
*&      Form  LOG
*&---------------------------------------------------------------------*
FORM log USING p_msgv1 p_msgv2 p_msgv3 p_msgv4.

ENDFORM.                    "LOG

*&---------------------------------------------------------------------*
*&      Form  DOLAR_REAL
*&---------------------------------------------------------------------*
FORM lanca_taxa_bcb USING p_moeda_origem p_moeda_destino p_categoria p_cor p_fator_0rigem p_paridade p_venda p_taxa_cambio.
  DATA v_moed TYPE string.

  CLEAR: wa_return, taxa.
  CONCATENATE p_moeda_origem 'X' p_moeda_destino INTO v_moed SEPARATED BY space.

  IF v_diautil EQ 'X'.

    IF p_paridade IS INITIAL.
      CLEAR: t_bcb.
      READ TABLE t_bcb WITH KEY moeda = p_moeda_origem.
      IF p_venda IS INITIAL.
        IF t_bcb-taxacompra > t_bcb-taxavenda.
          REPLACE ',' WITH '.' INTO t_bcb-taxacompra .
          taxa = t_bcb-taxacompra * p_fator_0rigem.
        ELSE.
          REPLACE ',' WITH '.' INTO t_bcb-taxavenda .
          taxa = t_bcb-taxavenda * p_fator_0rigem.
        ENDIF.
      ELSE.
        IF t_bcb-taxacompra > t_bcb-taxavenda.
          REPLACE ',' WITH '.' INTO t_bcb-taxavenda .
          taxa = t_bcb-taxavenda * p_fator_0rigem.
        ELSE.
          REPLACE ',' WITH '.' INTO t_bcb-taxacompra .
          taxa = t_bcb-taxacompra * p_fator_0rigem.
        ENDIF.

      ENDIF.
    ELSE.
      CLEAR: t_bcb.
      READ TABLE t_bcb WITH KEY moeda = p_moeda_destino.
      IF p_venda IS INITIAL.
        REPLACE ',' WITH '.' INTO t_bcb-paridadecompra.
        taxa = t_bcb-paridadecompra.
      ELSE.
        REPLACE ',' WITH '.' INTO t_bcb-paridadevenda.
        taxa = t_bcb-paridadevenda.
      ENDIF.
    ENDIF.

    IF p_taxa_cambio = 'X'.
      PERFORM grava_t_zfi004 USING '' p_moeda_destino p_moeda_origem p_categoria v_data p_fator_0rigem '1' taxa.
    ELSE.
      PERFORM grava_t_zfi004 USING taxa p_moeda_destino p_moeda_origem p_categoria v_data p_fator_0rigem '1' ''.
    ENDIF.

    IF p_cor = 'A'.
      PERFORM taxa_azul USING v_moed taxa.
    ELSEIF p_cor = 'B'.
      PERFORM taxa_branca USING v_moed taxa.
    ENDIF.

  ELSE.

    READ TABLE t_tcurr INTO wa_tcurr WITH KEY kurst = p_categoria fcurr = p_moeda_origem tcurr = p_moeda_destino.

    IF sy-subrc EQ 0.
      taxa = wa_tcurr-ukurs.
      IF p_taxa_cambio = 'X'.
        IF taxa LT 0.
          taxa = taxa * -1.
        ENDIF.
        PERFORM grava_t_zfi004 USING '' p_moeda_destino p_moeda_origem p_categoria v_data p_fator_0rigem '1' taxa.
      ELSE.
        PERFORM grava_t_zfi004 USING taxa p_moeda_destino p_moeda_origem p_categoria v_data p_fator_0rigem '1' ''.
      ENDIF.
    ELSE.

      taxa = 'Taxa do ultimo dia útil não foi cadastrada.'.
      IF p_categoria EQ 'VB'.
        taxa = 'Taxa VB não é considerada em dias não úteis.'.
      ENDIF.
    ENDIF.

    IF p_cor = 'A'.
      PERFORM taxa_azul USING v_moed taxa.
    ELSEIF p_cor = 'B'.
      PERFORM taxa_branca USING v_moed taxa.
    ENDIF.

  ENDIF.

ENDFORM.                    "DOLAR_REAL


*&---------------------------------------------------------------------*
*&      Form  envia_mail
*&---------------------------------------------------------------------*
FORM envia_mail.
  DATA: lr_mail_data    TYPE REF TO cl_crm_email_data,
        ls_struc_mail   TYPE crms_email_mime_struc,
        lv_send_request TYPE sysuuid_x,
        lv_to           TYPE crms_email_recipient,
        v_dt_rod(16)    TYPE c,
        v_hr_rod(5).

  WRITE sy-datum TO v_dt_rod DD/MM/YYYY.
  CONCATENATE sy-uzeit(2) ':' sy-uzeit+2(2) INTO v_hr_rod.
  CONCATENATE v_dt_rod v_hr_rod INTO v_dt_rod SEPARATED BY space.

*   Cria a Mensagem de E-mail
  CREATE OBJECT lr_mail_data.

*   Preenche o Remetente.
  lr_mail_data->from-address = 'cxa_igua_sap@cofer.com.br'.
  lr_mail_data->from-name    = lr_mail_data->from-address.
  lr_mail_data->from-id      = lr_mail_data->from-address.

**   Preenche os Destinatários.
  LOOP AT s_email.
    lv_to-address = s_email-low.
    lv_to-name    = lv_to-address.
    APPEND lv_to TO lr_mail_data->to.
  ENDLOOP.

****   Preenche o Remetente.
***  lv_to-address = 'cxa_igua_sap@iguasa.com.br'.
***  lv_to-name    = 'IGUA'.
***  lv_to-id      = 1.
***  APPEND lv_to TO lr_mail_data->to.
***
*****   Preenche os Destinatários.
***  LOOP AT s_email.
***    lr_mail_data->from-address = s_email-low.
***    lr_mail_data->from-name    = lr_mail_data->from-address.
***    lr_mail_data->from-id      = sy-tabix.
***  ENDLOOP.

*   Define o Assunto do E-mail
  CONCATENATE 'COTAÇÃO DE MOEDAS -' sy-datum INTO  lr_mail_data->subject SEPARATED BY space.

*   Define o Assunto do E-mail
  IF sy-sysid EQ 'R6Q'.
    CONCATENATE lr_mail_data->subject 'Ambiente Qualidade' INTO lr_mail_data->subject SEPARATED BY space.
  ELSEIF sy-sysid EQ 'R3P'.
    CONCATENATE lr_mail_data->subject 'Ambiente Produção' INTO lr_mail_data->subject SEPARATED BY space.
  ENDIF.

*  if v_diautil = 'X'.
  v_urlbcb = '<a href="http://www4.bcb.gov.br/pec/taxas/port/ptaxnpesq.asp?id=txcotacao&id=txcotacao">http://www4.bcb.gov.br/pec/taxas/port/ptaxnpesq.asp?id=txcotacao&id=txcotacao</a>'.
*  v_urlbce = '<a href="http://www.ecb.int/stats/exchange/eurofxref/html/eurofxref-graph-usd.en.html">http://www.ecb.int/stats/exchange/eurofxref/html/eurofxref-graph-usd.en.html</a>'.
*  else.
*    v_urlbcb = 'SAP - OB08'.
*    v_urlbce = 'SAP - OB08'.
*  endif.

  CONCATENATE v_email_corpo
      '<p class="MsoNormal">'
      '    <span style="font-family:&quot;Calibri&quot;,&quot;sans-serif&quot;;'
      'color:#1F497D" class="style1">Fontes: </span>&nbsp;</p>'
      '<div align="center" class="MsoNormal" style="text-align:center">'
      '    <hr align="center" size="2" width="100%" />'
      '</div>'
      '            <p class="MsoNormal" style="mso-margin-top-alt:auto;mso-margin-bottom-alt:auto">'
      '                <span lang="PT-BR" style="font-size:11.0pt;font-family:&quot;Calibri&quot;,&quot;sans-serif&quot;;'
      '  color:black;mso-ansi-language:PT-BR">Banco Central do Brasil - ' v_dt_rod ' - ' v_urlbcb '<br></br>'
      'Banco Central Europeu - ' v_dt_rod ' - ' v_urlbce '</span><span style="color:'
      '  black"><o:p></o:p></span>'
      '            </p>'
 INTO v_email_corpo SEPARATED BY space.


*   Mensagem no Corpo do E-mail.
  ls_struc_mail-mime_type     = 'text/html'.
  ls_struc_mail-file_name     = 'body.htm'.
  ls_struc_mail-content_ascii =  v_email_corpo.
  APPEND ls_struc_mail TO lr_mail_data->body.

*   Envia o E-mail
  lv_send_request = cl_crm_email_utility_base=>send_email( iv_mail_data = lr_mail_data ).

* Processa o Envio Imediato
  SUBMIT rsconn01 AND RETURN.
ENDFORM.                    "envia_mail



*&---------------------------------------------------------------------*
*&      Form  HKD_BRL_M
*&---------------------------------------------------------------------*
FORM hkd_brl_m .
  CLEAR taxa.

  CLEAR wa_return.

  IF v_diautil EQ 'X'.

    READ TABLE t_bcb WITH KEY moeda = 'HKD'.

    REPLACE ',' WITH '.' INTO t_bcb-taxacompra .

    taxa = t_bcb-taxacompra * 100.

    PERFORM grava_t_zfi004 USING taxa 'BRL' 'HKD' 'M' v_data '100' '1' ''.

    PERFORM taxa_azul USING 'HKD X BRL' taxa.
  ELSE.

    READ TABLE t_tcurr INTO wa_tcurr WITH KEY kurst = 'M' fcurr = 'HKD' tcurr = 'BRL'.

    IF sy-subrc EQ 0.

      taxa = wa_tcurr-ukurs.
      PERFORM grava_t_zfi004 USING taxa 'BRL' 'HKD' 'M' v_data '100' '1' ''.

    ELSE.

      taxa = 'Taxa do ultimo dia útil não foi cadastrada.'.

    ENDIF.

    PERFORM taxa_azul USING 'HKD X BRL' taxa.

  ENDIF.

ENDFORM.                    " HKD_BRL_M
*&---------------------------------------------------------------------*
*&      Form  JPY_BRL_M
*&---------------------------------------------------------------------*
FORM jpy_brl_m .
  CLEAR taxa.

  CLEAR wa_return.

  IF v_diautil EQ 'X'.

    READ TABLE t_bcb WITH KEY moeda = 'JPY'.

    REPLACE ',' WITH '.' INTO t_bcb-taxacompra .

    taxa = t_bcb-taxacompra * 100.
    PERFORM grava_t_zfi004 USING taxa 'BRL' 'JPY' 'M' v_data '100' '1' ''.

    PERFORM taxa_branca USING 'JPY X BRL' taxa.

  ELSE.

    READ TABLE t_tcurr INTO wa_tcurr WITH KEY kurst = 'M' fcurr = 'JPY' tcurr = 'BRL'.

    IF sy-subrc EQ 0.

      taxa = wa_tcurr-ukurs.
      PERFORM grava_t_zfi004 USING taxa 'BRL' 'JPY' 'M' v_data '100' '1' ''.

    ELSE.

      taxa = 'Taxa do ultimo dia útil não foi cadastrada.'.

    ENDIF.

    PERFORM taxa_branca USING 'JPY X BRL' taxa.

  ENDIF.
ENDFORM.                    " JPY_BRL_M
*&---------------------------------------------------------------------*
*&      Form  USD_EUR_EURX
*&---------------------------------------------------------------------*
FORM 	lanca_taxa_bce USING p_moeda_origem p_moeda_destino p_categoria p_cor p_fator_0rigem p_paridade.
  DATA: v_pos  TYPE i,
        v_moed TYPE string,
        str    TYPE string.

  CLEAR: wa_return, taxa.
  CONCATENATE p_moeda_origem 'X' p_moeda_destino INTO v_moed SEPARATED BY space.

  IF v_diautil EQ 'X'.

    CLEAR: v_status_text, response_entity_body,  response_entity_body[], response_headers, response_headers[].

    v_file = p_euro.
    TRANSLATE v_file TO LOWER CASE.

    IF p_backg  IS NOT INITIAL.

      CALL FUNCTION 'HTTP_GET'
        EXPORTING
          absolute_uri          = v_file
          rfc_destination       = 'SAPHTTPA'
          proxy                 = p_proxy
          proxy_user            = p_user
          proxy_password        = p_pass
          blankstocrlf          = 'Y' "Ehp7 by CP 23.07.2020
        IMPORTING
          status_text           = v_status_text
        TABLES
          response_entity_body  = response_entity_body
          response_headers      = response_headers
        EXCEPTIONS
          connect_failed        = 1
          timeout               = 2
          internal_error        = 3
          tcpip_error           = 4
          data_error            = 5
          system_failure        = 6
          communication_failure = 7
          OTHERS                = 8.

    ELSE.

      CALL FUNCTION 'HTTP_GET'
        EXPORTING
          absolute_uri          = v_file
          rfc_destination       = 'SAPHTTP'
          proxy                 = p_proxy
          proxy_user            = p_user
          proxy_password        = p_pass
          blankstocrlf          = 'Y' "Ehp7 by CP 23.07.2020
        IMPORTING
          status_text           = v_status_text
        TABLES
          response_entity_body  = response_entity_body
          response_headers      = response_headers
        EXCEPTIONS
          connect_failed        = 1
          timeout               = 2
          internal_error        = 3
          tcpip_error           = 4
          data_error            = 5
          system_failure        = 6
          communication_failure = 7
          OTHERS                = 8.

    ENDIF.

    IF v_status_text NE 'OK'.
      taxa = 'Erro de comunicação com o banco central europeu'.
      IF p_cor = 'A'.
        PERFORM taxa_azul USING v_moed taxa.
      ELSEIF p_cor = 'B'.
        PERFORM taxa_branca USING v_moed taxa.
      ENDIF.
    ELSE.

      LOOP AT response_entity_body.
        CONCATENATE v_xml response_entity_body INTO v_xml.
      ENDLOOP.
      CONCATENATE '<Cube currency=''' p_moeda_origem ''' rate=''' INTO str.
      SEARCH v_xml FOR str.

      v_pos = sy-fdpos + 27.

      taxa = v_xml+v_pos(6).

      REPLACE '''' WITH '' INTO taxa.
      REPLACE '/' WITH '' INTO taxa.
      REPLACE '>' WITH '' INTO taxa.
      CONDENSE taxa NO-GAPS.

      v_usd_eur = taxa. "Será utilizado no lançamento Mensal

      PERFORM grava_t_zfi004 USING '' p_moeda_destino p_moeda_origem p_categoria v_data p_fator_0rigem '1' taxa.

      IF p_cor EQ c_b.
        PERFORM taxa_branca USING v_moed taxa.
      ELSE.
        PERFORM taxa_azul USING v_moed taxa.
      ENDIF.

    ENDIF.


  ELSE.

    READ TABLE t_tcurr INTO wa_tcurr WITH KEY kurst = p_categoria fcurr = p_moeda_origem tcurr = p_moeda_destino.

    IF sy-subrc EQ 0.

      taxa = wa_tcurr-ukurs * -1.

      v_usd_eur = taxa. "Será utilizado no lançamento Mensal

      PERFORM grava_t_zfi004 USING '' p_moeda_destino p_moeda_origem p_categoria v_data p_fator_0rigem '1' taxa.

    ELSE.

      taxa = 'Taxa do ultimo dia útil não foi cadastrada.'.

    ENDIF.
    IF p_cor EQ c_b.
      PERFORM taxa_branca USING v_moed taxa.
    ELSE.
      PERFORM taxa_azul USING v_moed taxa.
    ENDIF.

  ENDIF.

ENDFORM.                    " USD_EUR_EURX
*&---------------------------------------------------------------------*
*&      Form  INTERFACE_BCB
*&---------------------------------------------------------------------*
FORM interface_bcb .


  CONCATENATE p_bace v_data+0(4) v_data+4(2) v_data+6(2) '.csv' INTO v_file.
  TRANSLATE v_file TO LOWER CASE.

  DATA: lv_url              TYPE string,
        lv_proxy_host       TYPE string,
        lv_proxy_service    TYPE string,
        lv_http_param_name  TYPE string,
        lv_http_param_value TYPE string,
        lv_body             TYPE string,
        lv_response         TYPE string,
        lv_error            TYPE string,
        lv_content          TYPE i,
        lt_string           TYPE TABLE OF string.

  DATA: lo_http_client TYPE REF TO if_http_client.

  lv_url = v_file.

  lv_proxy_host    = p_proxy.
  lv_proxy_service = p_user.

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = lv_url
      proxy_host         = lv_proxy_host
      proxy_service      = lv_proxy_service
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc IS NOT INITIAL.
    lv_error = |{ TEXT-e02 }{ sy-subrc }|. "Erro no método CREATE_BY_URL. SUBRC=
    EXIT. "Sai do DO..
  ENDIF.

  lo_http_client->request->set_method('GET').
  lo_http_client->request->set_content_type( 'application/json' ).

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'Content-Type'
      value = |application/json|.


*--------------------------------------------------------------------
* Enviar a requisição HTTP
*--------------------------------------------------------------------
  "// Send data By Http
  CALL METHOD lo_http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5.
  IF sy-subrc IS NOT INITIAL.
    lv_error = |{ TEXT-e03 }{ sy-subrc }|. "Erro no método SEND. SUBRC=
    EXIT. "Sai do DO..
  ENDIF.

  "// Get reveice from Http
  CALL METHOD lo_http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 5.
  IF sy-subrc IS NOT INITIAL.

    "// GET last HTTP error code
    CALL METHOD lo_http_client->get_last_error
      IMPORTING
        code    = DATA(lv_code_erro)
        message = DATA(lv_message_erro).

    lv_error = |{ TEXT-e04 }{ sy-subrc }. { lv_code_erro } { lv_message_erro }|. "Erro no método RECEIVE. SUBRC=
    EXIT. "Sai do DO..

  ELSE.

    v_status_text = 'OK'.

  ENDIF.

  "// Get Body from Http response
  lv_response = lo_http_client->response->get_cdata( ).


  SPLIT lv_response AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_string.

*  IF p_backg  IS NOT INITIAL.
*
*    CALL FUNCTION 'HTTP_GET'
*      EXPORTING
*        absolute_uri          = v_file
*        rfc_destination       = 'SAPHTTPA'
*        proxy                 = p_proxy
*        proxy_user            = p_user
*        proxy_password        = p_pass
*        blankstocrlf          = 'Y' "Ehp7 by CP 23.07.2020
*      IMPORTING
*        status_text           = v_status_text
*      TABLES
*        response_entity_body  = response_entity_body
*        response_headers      = response_headers
*      EXCEPTIONS
*        connect_failed        = 1
*        timeout               = 2
*        internal_error        = 3
*        tcpip_error           = 4
*        data_error            = 5
*        system_failure        = 6
*        communication_failure = 7
*        OTHERS                = 8.
*  ELSE.
*    CALL FUNCTION 'HTTP_GET'
*      EXPORTING
*        absolute_uri          = v_file
*        rfc_destination       = 'SAPHTTP'
*        proxy                 = p_proxy
*        proxy_user            = p_user
*        proxy_password        = p_pass
*        blankstocrlf          = 'Y' "Ehp7 by CP 23.07.2020
*      IMPORTING
*        status_text           = v_status_text
*      TABLES
*        response_entity_body  = response_entity_body
*        response_headers      = response_headers
*      EXCEPTIONS
*        connect_failed        = 1
*        timeout               = 2
*        internal_error        = 3
*        tcpip_error           = 4
*        data_error            = 5
*        system_failure        = 6
*        communication_failure = 7
*        OTHERS                = 8.
*  ENDIF.


  IF v_status_text NE 'OK'.
    CONCATENATE v_email_corpo '<br />' 'Erro de comunicação com o Banco Central do Brasil' INTO v_email_corpo SEPARATED BY space.
    "Caso o Arquivo não tenha sido encontrado no Banco Central
  ELSE.

* It was included the code ( if 1 = 2 ) for comment a code block below, because with Ehp7 the function HTTP_GET using parameter (blankstocrlf) for break lines automatic.
    IF 1 = 2.
      LOOP AT response_entity_body.
        CONCATENATE v_csv response_entity_body INTO v_csv.
      ENDLOOP.

      DATA: v_quebra(2).
      DATA: v_quebra_aux(1).

      CONCATENATE cl_abap_char_utilities=>cr_lf cl_abap_char_utilities=>cr_lf INTO v_quebra.
      v_quebra_aux = v_quebra+1(1).

*    SPLIT v_csv AT v_quebra INTO TABLE t_linhas.
      SPLIT v_csv AT v_quebra_aux INTO TABLE t_linhas.

      LOOP AT t_linhas INTO v_linha.
        SPLIT v_linha AT ';' INTO: t_bcb-data
                                   t_bcb-codmoeda
                                   t_bcb-tipo
                                   t_bcb-moeda
                                   t_bcb-taxacompra
                                   t_bcb-taxavenda
                                   t_bcb-paridadecompra
                                   t_bcb-paridadevenda.
        APPEND t_bcb.

      ENDLOOP.
    ENDIF.

* CP - 23.07.2020 - Begin
* New code for get currency values
*    LOOP AT response_entity_body ASSIGNING FIELD-SYMBOL(<fs_response>).
    LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<fs_response>).

      APPEND INITIAL LINE TO t_bcb ASSIGNING FIELD-SYMBOL(<fs_bcb>).

      SPLIT <fs_response> AT ';' INTO: <fs_bcb>-data
                                       <fs_bcb>-codmoeda
                                       <fs_bcb>-tipo
                                       <fs_bcb>-moeda
                                       <fs_bcb>-taxacompra
                                       <fs_bcb>-taxavenda
                                       <fs_bcb>-paridadecompra
                                       <fs_bcb>-paridadevenda.

    ENDLOOP.
* CP - 23.07.2020 - End

    SORT t_bcb BY moeda.
  ENDIF.
ENDFORM.                    " INTERFACE_BCB
*&---------------------------------------------------------------------*
*&      Form  MENSAL
*&---------------------------------------------------------------------*
FORM mensal .
  DATA: v_endda_formatado(10) TYPE c,
        v_begda_formatado(10) TYPE c.

  CLEAR: v_endda, v_endda, v_endda_ex, v_begda_ex, v_prox_dia.

  CONCATENATE v_data(4) v_data+4(2) '01' INTO v_begda.
  CONCATENATE v_data(4) v_data+4(2) v_data+6(2) INTO v_endda.

  v_prox_dia = v_endda + 1.

  IF ( v_prox_dia+6(2) EQ 01 ). "A data do processamento seja o ultimo dia do mes

    WRITE v_begda DD/MM/YYYY TO v_begda_formatado.
    WRITE v_endda DD/MM/YYYY TO v_endda_formatado.

    PERFORM mensais_mail.

    IF v_usd_eur IS NOT INITIAL.

      PERFORM header_cat USING 'M'.
      PERFORM usd_eur_m.
      PERFORM fecha_cat.

      PERFORM header_cat USING 'V'.
      PERFORM usd_eur_v.
      PERFORM fecha_cat.

      PERFORM header_cat USING 'VB'.
      PERFORM usd_eur_vb.
      PERFORM fecha_cat.

    ENDIF.
  ENDIF.

ENDFORM.                    " MENSAL
*&---------------------------------------------------------------------*
*&      Form  USD_BRL_1002
*&---------------------------------------------------------------------*
FORM taxa_media  USING p_moeda_origem p_moeda_destino p_categoria_origem p_categoria_destino p_cor p_fator_0rigem p_paridade.
  DATA v_moed TYPE string.

  CLEAR: taxa, wa_return.
  CONCATENATE p_moeda_origem 'X' p_moeda_destino INTO v_moed SEPARATED BY space.

  CLEAR wa_curr.

  IF p_moeda_origem EQ 'EUR' AND p_moeda_destino EQ 'USD'.

    SELECT COUNT(*) AVG( ukurs ) UP TO 1 ROWS
      FROM tcurr INTO wa_curr
       WHERE kurst = p_categoria_origem   AND
          fcurr = p_moeda_destino AND
          tcurr = p_moeda_origem  AND
          gdatu BETWEEN v_endda_ex AND v_begda_ex.

    taxa = wa_curr-ukurs * ( - 1 ).

  ELSE.

    SELECT COUNT(*) AVG( ukurs ) UP TO 1 ROWS
      FROM tcurr INTO wa_curr
       WHERE kurst = p_categoria_origem   AND
          fcurr = p_moeda_origem AND
          tcurr = p_moeda_destino AND
          gdatu BETWEEN v_endda_ex AND v_begda_ex.

    taxa = wa_curr-ukurs.

  ENDIF.

  IF wa_curr-count EQ v_endda+6(2).

    CLEAR wa_return.
    PERFORM grava_t_zfi004 USING taxa p_moeda_destino p_moeda_origem p_categoria_destino v_data '1' '1' ''.

    IF p_cor = 'A'.
      PERFORM taxa_azul USING v_moed taxa.
    ELSEIF p_cor = 'B'.
      PERFORM taxa_branca USING v_moed taxa.
    ENDIF.

  ELSE.
    taxa = 'Não foram lançadas todas as taxas para que se faça uma media'.
    IF p_cor = 'A'.
      PERFORM taxa_azul USING v_moed taxa.
    ELSEIF p_cor = 'B'.
      PERFORM taxa_branca USING v_moed taxa.
    ENDIF.
  ENDIF.
ENDFORM.                    " USD_BRL_1002

*&---------------------------------------------------------------------*
*&      Form  CABECALHO_MAIL
*&---------------------------------------------------------------------*
FORM cabecalho_mail.
  WRITE v_data DD/MM/YYYY TO v_data_email.
  CONCATENATE v_email_corpo
        '<b>'
        '<span style="color:#1F497D; font-family:Calibri; font-size:larger">Cotações Atualizadas em'
        v_data_email
        '<br />'
        '</span>'
        '</b>'
        '<hr />'
        '<BR />' INTO v_email_corpo SEPARATED BY space.
ENDFORM.                    "CABECALHO_MAIL


*&---------------------------------------------------------------------*
*&      Form  MENSAIS_MAIL
*&---------------------------------------------------------------------*
FORM mensais_mail.
  WRITE v_data DD/MM/YYYY TO v_data_email.
  CONCATENATE v_email_corpo
        '<br />'
        '<b>'
        '<span style="color:#1F497D; font-family:Calibri; font-size:medium">Taxas Mensais'
        '<br />'
        '</span>'
        '</b>'
        '<hr />'
        '<BR />' INTO v_email_corpo SEPARATED BY space.
ENDFORM.                    "CABECALHO_MAIL


*&---------------------------------------------------------------------*
*&      Form  HEADER_CAT
*&---------------------------------------------------------------------*
FORM header_cat USING categoria.

  CLEAR: wa_tcurw.
  READ TABLE t_tcurw INTO wa_tcurw WITH TABLE KEY kurst = categoria.

  CONCATENATE v_email_corpo
  '<table border="1" cellpadding="1" cellspacing="0" >'
  '<tr height="20" style="font-size: 11.0pt; color: white; '
  'font-weight: 700; text-decoration: none; text-underline-style: none; text-line-through: none;'
  ' font-family: Calibri; border-top: .5pt solid #95B3D7; border-right: none; border-bottom: .5pt solid #95B3D7;'
  ' border-left: .5pt solid #95B3D7; background: #4F81BD; mso-pattern: #4F81BD none">'
  '<td nowrap="nowrap" style="border-right: windowtext 1pt solid; padding-right: 5.4pt;'
  'border-top: #f0f0f0; padding-left: 5.4pt; background: #4f81bd; padding-bottom: 0in;'
  'border-left: #f0f0f0; width: 170pt; padding-top: 0in; border-bottom: windowtext 1pt solid;'
  'height: 15pt" valign="bottom">'
  '<p class="MsoNormal" style="margin: 0in 0in 0pt">'
                '<b><span style="font-size: 11pt; color: white; font-family: ''Calibri'',''sans-serif''">'
                    'Categoria' categoria
                    '<br />'
                '</span></b><b><span style="font-size: 9pt; color: white; font-family: ''Calibri'',''sans-serif''">'
                    wa_tcurw-curvw
                    '<o:p></o:p>'
                '</span></b>'
            '</p>'
  '</td>'
  '<td style="border-style: none solid solid none;'
  ' border-color: -moz-use-text-color rgb(149, 179, 215) rgb(149, 179, 215) -moz-use-text-color;'
  ' border-width: medium 1pt 1pt medium; padding: 0in 5.4pt; height: 15pt;"'
  ' nowrap="nowrap" valign="bottom" >'
  '    Taxa</td>'
  '</tr>'  INTO v_email_corpo SEPARATED BY space.
ENDFORM.                    "HEADER_CAT

*&---------------------------------------------------------------------*
*&      Form  taxa_azul
*&---------------------------------------------------------------------*
FORM taxa_azul USING moedas tax.
  CONCATENATE v_email_corpo
  '<tr height="20" style="height: 15.0pt; font-size: 11.0pt; color: black; font-weight: 400;'
  ' text-decoration: none; text-underline-style: none; text-line-through: none; font-family: Calibri;'
  ' border-top: .5pt solid #95B3D7; border-right: none; border-bottom: .5pt solid #95B3D7; border-left: .5pt solid #95B3D7;'
  ' background: #DCE6F1; mso-pattern: #DCE6F1 none">'
  ' <td width="170" style="border-style: none solid solid; border-color: -moz-use-text-color windowtext rgb(149, 179, 215) rgb(149, 179, 215); '
  'border-width: medium 1pt 1pt; padding: 0in 5.4pt; height: 15pt;" nowrap="nowrap" valign="bottom" >'
  '<p class="MsoNormal"><span style="font-size: 11pt; font-family: &quot;Calibri&quot;,&quot;sans-serif&quot;; color: black;" lang="PT-BR">'
  moedas
  '</span></p>'
  '</td>'
  '<td style="border-style: none solid solid none;'
  ' border-color: -moz-use-text-color rgb(149, 179, 215) rgb(149, 179, 215) -moz-use-text-color;'
  ' border-width: medium 1pt 1pt medium; padding: 0in 5.4pt; height: 15pt;" nowrap="nowrap" valign="bottom" >'
  '<p class="MsoNormal" style="text-align: right;" align="right">'
  '<span style="font-size: 11pt; font-family: &quot;Calibri&quot;,&quot;sans-serif&quot;; color: black;" lang="PT-BR">  '
  tax
  '<o:p></o:p></span></p>'
  '</td>'
  '</tr>'
  INTO v_email_corpo SEPARATED BY space.
ENDFORM.                    "taxa_azul

*&---------------------------------------------------------------------*
*&      Form  taxa_branca
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->MOEDAS     text
*      -->TAX        text
*----------------------------------------------------------------------*
FORM taxa_branca USING moedas tax.
  CONCATENATE v_email_corpo
  '<tr height="20" style="height: 15.0pt; font-size: 11.0pt;'
  ' color: black; font-weight: 400; text-decoration: none;'
  ' text-underline-style: none; text-line-through: none; font-family: Calibri;'
  ' border-top: .5pt solid #95B3D7; border-right: none; border-bottom: .5pt solid #95B3D7;'
  ' border-left: .5pt solid #95B3D7;  mso-pattern: #DCE6F1 none">'
  '<td width="170" style="border-style: none solid solid;'
  ' border-color: -moz-use-text-color windowtext rgb(149, 179, 215) rgb(149, 179, 215);'
  ' border-width: medium 1pt 1pt; padding: 0in 5.4pt; '
  ' height: 15pt;" nowrap="nowrap" valign="bottom" >'
  '<p class="MsoNormal"><span style="font-size: 11pt;'
  ' font-family: &quot;Calibri&quot;,&quot;sans-serif&quot;; color: black;" lang="PT-BR">'
  moedas
  '</span></p>'
  '</td>'
  '<td style="border-style: none solid solid none;'
  ' border-color: -moz-use-text-color rgb(149, 179, 215) rgb(149, 179, 215) -moz-use-text-color;'
  ' border-width: medium 1pt 1pt medium; padding: 0in 5.4pt; height: 15pt;" nowrap="nowrap" valign="bottom" >'
  '<p class="MsoNormal" style="text-align: right;" align="right"><span style="font-size: 11pt;'
  ' font-family: &quot;Calibri&quot;,&quot;sans-serif&quot;; color: black;" lang="PT-BR">'
  taxa
  '<o:p></o:p></span></p>'
  '</td>'
  '</tr>'
  INTO v_email_corpo SEPARATED BY space.
ENDFORM.                    "taxa_branca
*&---------------------------------------------------------------------*
*&      Form  TRATA_RETORNO
*&---------------------------------------------------------------------*
FORM trata_retorno .
  IF wa_return-type EQ 'E'.
    IF wa_return-message+0(57) EQ 'Linha                                                 1:'.
      taxa = wa_return-message+57.
    ELSE.
      taxa = wa_return-message.
    ENDIF.
  ELSE.
    v_tx = taxa.
    taxa = v_tx.
  ENDIF.
ENDFORM.                    " TRATA_RETORNO

*&---------------------------------------------------------------------*
*&      Form  usd_eur_m
*&---------------------------------------------------------------------*
FORM usd_eur_m .
  CLEAR taxa.

  CLEAR wa_return.
  PERFORM grava_t_zfi004 USING '' 'EUR' 'USD' 'M'  v_data '1' '1' v_usd_eur.
  taxa = v_usd_eur.
  PERFORM taxa_azul USING 'USD X EUR' taxa.

ENDFORM.                    " USD_EUR_M

*&---------------------------------------------------------------------*
*&      Form  usd_eur_v
*&---------------------------------------------------------------------*
FORM usd_eur_v.
  CLEAR taxa.

  CLEAR wa_return.
  PERFORM grava_t_zfi004 USING '' 'EUR' 'USD' 'V' v_data '1' '1' v_usd_eur.

  taxa = v_usd_eur.

  PERFORM taxa_azul USING 'USD X EUR' taxa.

ENDFORM.                    " USD_EUR_V

*&---------------------------------------------------------------------*
*&      Form  usd_eur_vb
*&---------------------------------------------------------------------*
FORM usd_eur_vb.

  CLEAR taxa.
  CLEAR wa_return.
  PERFORM grava_t_zfi004 USING '' 'EUR' 'USD' 'VB' v_data '1' '1' v_usd_eur.
  taxa = v_usd_eur.
  PERFORM taxa_azul USING 'USD X EUR' taxa.

ENDFORM.                    " USD_EUR_VB

*&---------------------------------------------------------------------*
*&      Form  FECHA_CAT
*&---------------------------------------------------------------------*
FORM fecha_cat.
  CONCATENATE v_email_corpo
  '</table>' '<br />'
    INTO v_email_corpo SEPARATED BY space.
ENDFORM.                    "FECHA_CAT

*&---------------------------------------------------------------------*
*&      Form  GRAVA_T_ZFI004
*&---------------------------------------------------------------------*
FORM grava_t_zfi004 USING p_taxa          TYPE string
                          p_moeda_destino TYPE string
                          p_moeda_origem  TYPE string
                          p_categoria     TYPE char04
                          p_v_data        TYPE d
                          p_fator_0rigem  TYPE string
                          p_fator_destino TYPE string
                          p_taxa_cambio   TYPE string.


  CLEAR t_zfit004.
  t_zfit004-rate_type  = p_categoria.
  t_zfit004-from_curr  = p_moeda_origem.
  t_zfit004-to_currncy = p_moeda_destino.
  t_zfit004-valid_from = p_v_data.

  t_zfit004_brl-rate_type  = p_categoria.
  t_zfit004_brl-from_curr  = p_moeda_destino.
  t_zfit004_brl-to_currncy = p_moeda_origem.
  t_zfit004_brl-valid_from = p_v_data.

  IF p_taxa IS NOT INITIAL.

    t_zfit004-exch_rate   = p_taxa.
    t_zfit004-from_factor = p_fator_0rigem.
    t_zfit004-to_factor   = p_fator_destino.

    t_zfit004_brl-exch_rate_v    = p_taxa .
    t_zfit004_brl-from_factor = p_fator_destino.
    t_zfit004_brl-to_factor   = p_fator_0rigem.

  ELSEIF p_taxa_cambio IS NOT INITIAL.

    t_zfit004-exch_rate_v   = p_taxa_cambio.
    t_zfit004-from_factor_v = p_fator_0rigem.
    t_zfit004-to_factor_v   = p_fator_destino.

    t_zfit004_brl-exch_rate_v   = p_taxa_cambio.
    t_zfit004_brl-from_factor_v = p_fator_destino.
    t_zfit004_brl-to_factor_v   = p_fator_0rigem.

  ENDIF.

  APPEND t_zfit004.

  " APPEND t_zfit004_brl TO t_zfit004.
  " BREAK-POINT.
  "APPEND t_zfit004_brl.


***********************************************************************
*  Ajuste Rafael para BRL X USD


ENDFORM.                    " GRAVA_T_ZFI004


*&---------------------------------------------------------------------*
*&      Form  SALVA_DADOS
*&---------------------------------------------------------------------*
FORM salva_dados .

  DATA lt_zfit004     TYPE TABLE OF zfit004.

  CLEAR t_zfit004.

  LOOP AT t_zfit004.
    IF t_zfit004-rate_type = 'M'.
      t_zfit004-rate_type = 'P'.
      APPEND t_zfit004.
    ENDIF.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM t_zfit004 COMPARING ALL FIELDS.

  APPEND LINES OF t_zfit004 TO lt_zfit004.
  MODIFY zfit004 FROM TABLE lt_zfit004.

ENDFORM.                    " SALVA_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DATA
*&---------------------------------------------------------------------*
FORM f_check_data .

*** Deixar atualizar mais de uma vez diariamente!!!
***  SELECT COUNT(*)
***    FROM zfit004
***   WHERE valid_from EQ sy-datum
***     AND usua_autor NE ''.
***
***  IF sy-subrc EQ 0.
***    MESSAGE s028(zfi) WITH 'As cotações já foram atualizadas para esta data'.
***    LEAVE PROGRAM.
***  ENDIF.

ENDFORM.                    " F_CHECK_DATA
