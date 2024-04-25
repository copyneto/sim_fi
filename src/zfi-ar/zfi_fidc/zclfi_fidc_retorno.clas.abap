CLASS zclfi_fidc_retorno DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS main .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_fi_ar TYPE ze_param_modulo VALUE 'FI-AR' ##NO_TEXT.
    CONSTANTS gc_s TYPE char1 VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_n TYPE char1 VALUE 'N' ##NO_TEXT.
    CONSTANTS gc_e TYPE char1 VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_fidc TYPE ze_param_chave1 VALUE 'FIDC' ##NO_TEXT.
    CONSTANTS gc_retorno TYPE ze_param_chave2 VALUE 'RETORNO' ##NO_TEXT.
    CONSTANTS gc_banco_emissor_rej TYPE ze_param_chave2 VALUE 'BANCO_EMISSOR_REJEITADOS' ##NO_TEXT.
    CONSTANTS gc_pasta TYPE ze_param_chave3 VALUE 'PASTA' ##NO_TEXT.
    CONSTANTS gc_pasta_processados TYPE ze_param_chave3 VALUE 'PASTA_PROCESSADOS' ##NO_TEXT.
    CONSTANTS gc_banco_3000 TYPE ze_param_chave3 VALUE '3000' ##NO_TEXT.
    DATA go_parametros TYPE REF TO zclca_tabela_parametros .
    DATA:
      gt_found_files TYPE TABLE OF salfldir .
    DATA:
      gt_file_retorno_table TYPE TABLE OF string .
    DATA:
      gt_detalhe TYPE TABLE OF zsfi_fidc_fat_ret_detalhe .
    DATA gs_header TYPE zsfi_fidc_fat_rem_header .
    DATA gs_trailer TYPE zsfi_fidc_fat_ret_trailer .
    DATA gv_dir_retorno TYPE string .
    DATA gv_dir_processados TYPE string .
    DATA gv_file_retorno TYPE string .
    DATA gv_file_retorno_with_path TYPE string .
    DATA gv_file_processados_with_path TYPE string .
    DATA:
*    DATA gv_newval type string.
      gt_fidc_contr TYPE TABLE OF ztfi_fidc_contr .
    DATA gv_newval TYPE newdata_l .

    METHODS get_fat_sap
      IMPORTING
        !it_detalhe TYPE zctgfi_fidc_ret_detalhe
      EXCEPTIONS
        not_found .
    METHODS get_parameters
      IMPORTING
        !iv_modulo TYPE ze_param_modulo
        !iv_chave1 TYPE ze_param_chave1
        !iv_chave2 TYPE ze_param_chave2
        !iv_chave3 TYPE ze_param_chave3
      CHANGING
        !cv_dir    TYPE string .
    METHODS find_file
      IMPORTING
        !iv_dir        TYPE string
      RETURNING
        VALUE(rv_file) TYPE string .
    METHODS read_al11
      IMPORTING
        !iv_file             TYPE string
      RETURNING
        VALUE(rt_file_table) LIKE gt_file_retorno_table .
    METHODS processar_arquivo
      IMPORTING
        !it_file_table LIKE gt_file_retorno_table
      CHANGING
        !cs_header     TYPE zsfi_fidc_fat_rem_header
        !cs_trailer    TYPE zsfi_fidc_fat_ret_trailer
        !ct_detalhe    TYPE zctgfi_fidc_ret_detalhe .
    METHODS move_file_dir
      IMPORTING
        !iv_dir_file     TYPE string
        !iv_dir_new_path TYPE string .
    METHODS format_file_table
      IMPORTING
        !it_files              LIKE gt_found_files
        !iv_filename_substring TYPE string
      RETURNING
        VALUE(rt_files)        LIKE gt_found_files .
    METHODS format_file_name_with_path
      IMPORTING
        !iv_file_name                 TYPE string
        !iv_dir                       TYPE string
      RETURNING
        VALUE(rv_file_name_with_path) TYPE string .
    METHODS executa_processo
      CHANGING
        !cs_header  TYPE zsfi_fidc_fat_rem_header
        !cs_trailer TYPE zsfi_fidc_fat_ret_trailer
        !ct_detalhe TYPE zctgfi_fidc_ret_detalhe .
    METHODS processa_aceito_det
      IMPORTING
        !is_det TYPE zsfi_fidc_fat_ret_detalhe .
    METHODS lancar_doc_aceito
      IMPORTING
        !is_det      TYPE zsfi_fidc_fat_ret_detalhe
      EXPORTING
        !es_key      TYPE zsfi_boleto_ban_key
      CHANGING
        !cs_fidc_ctr TYPE ztfi_fidc_contr .
    METHODS processa_rejeicao_det
      IMPORTING
        !is_det TYPE zsfi_fidc_fat_ret_detalhe .
    METHODS atualiza_fat_rej
      IMPORTING
        !is_det        TYPE zsfi_fidc_fat_ret_detalhe
      CHANGING
        !cs_fidc_contr TYPE ztfi_fidc_contr
      EXCEPTIONS
        not_update .
    METHODS save_log .
ENDCLASS.



CLASS zclfi_fidc_retorno IMPLEMENTATION.


  METHOD constructor.

    go_parametros = zclca_tabela_parametros=>get_instance( ).

    get_parameters( EXPORTING iv_modulo = gc_fi_ar
                              iv_chave1 = gc_fidc
                              iv_chave2 = gc_retorno
                              iv_chave3 = gc_pasta
                     CHANGING cv_dir    = gv_dir_retorno ).

    get_parameters( EXPORTING iv_modulo = gc_fi_ar
                              iv_chave1 = gc_fidc
                              iv_chave2 = gc_retorno
                              iv_chave3 = gc_pasta_processados
                     CHANGING cv_dir    = gv_dir_processados ).

    RETURN.
  ENDMETHOD.


  METHOD main.

    gv_file_retorno = find_file( gv_dir_retorno ).

    gv_file_retorno_with_path = format_file_name_with_path( EXPORTING iv_dir = gv_dir_retorno
                                                                      iv_file_name = gv_file_retorno ).

    gv_file_processados_with_path = format_file_name_with_path( EXPORTING iv_dir = gv_dir_processados
                                                                      iv_file_name = gv_file_retorno ).

    gt_file_retorno_table = read_al11( gv_file_retorno_with_path ).

    processar_arquivo( EXPORTING it_file_table = gt_file_retorno_table
                        CHANGING cs_header  = gs_header
                                 cs_trailer = gs_trailer
                                 ct_detalhe = gt_detalhe ).

    executa_processo(
            CHANGING
            cs_header = gs_header
            cs_trailer = gs_trailer
            ct_detalhe = gt_detalhe ).


    move_file_dir( EXPORTING iv_dir_file     = gv_file_retorno_with_path
                             iv_dir_new_path = gv_file_processados_with_path ).

    RETURN.
  ENDMETHOD.


  METHOD get_parameters.

*    DATA ls_remessa LIKE LINE OF gt_remessa.

*    ls_remessa-tipo_status = gc_erro.
*    MESSAGE e002 INTO ls_remessa-mensagem.

    TRY.

        go_parametros->m_get_single( EXPORTING iv_modulo = iv_modulo
                                               iv_chave1 = iv_chave1
                                               iv_chave2 = iv_chave2
                                               iv_chave3 = iv_chave3
                                     IMPORTING ev_param  = cv_dir ).

        IF cv_dir IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = iv_modulo
                                                       iv_chave1 = iv_chave1
                                                       iv_chave2 = iv_chave2
                                                       iv_chave3 = iv_chave3 ).

        ENDIF.

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
*        MODIFY ct_retorno FROM ls_remessa TRANSPORTING tipo_status mensagem WHERE tipo_status <> gc_erro.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD processar_arquivo.

    CONSTANTS: lc_split   TYPE c VALUE ';',
               lc_comma   TYPE c VALUE ',',
               lc_point   TYPE c VALUE '.',
               lc_curr(3) TYPE c VALUE 'BRL'.

    DATA ls_detalhe LIKE LINE OF ct_detalhe.

    DATA: lv_data_hora_aux         TYPE string,
          lv_vl_total_boleto_aux   TYPE string,
          lv_vl_nf_aux             TYPE string,
          lv_vl_cessao_aux         TYPE string,
          lv_taxa_cessao_aux       TYPE string,
          lv_vl_total_cessao_aux   TYPE string,
          lv_vl_total_desconto_aux TYPE string,
          lv_vl_total_cessao(18)   TYPE c,
          lv_vl_total_desconto(18) TYPE c.

    DATA(lv_num_lines) = lines( it_file_table ).
    DATA(lt_file_table_aux) = it_file_table.

    IF it_file_table IS NOT INITIAL.

      READ TABLE it_file_table ASSIGNING FIELD-SYMBOL(<fs_file_line>) INDEX 1.

      IF <fs_file_line> IS ASSIGNED.

        SPLIT <fs_file_line> AT lc_split INTO cs_header-tipo_arquivo lv_data_hora_aux.

        cs_header-data_hora_geracao = lv_data_hora_aux.

      ENDIF.

      UNASSIGN <fs_file_line>.

      READ TABLE it_file_table ASSIGNING <fs_file_line> INDEX lv_num_lines.

      IF <fs_file_line> IS ASSIGNED.

        SPLIT <fs_file_line> AT lc_split INTO cs_trailer-sequencial_total lv_vl_total_cessao_aux lv_vl_total_desconto_aux.

        REPLACE ALL OCCURRENCES OF lc_comma IN: lv_vl_total_cessao_aux   WITH lc_point,
                                                lv_vl_total_desconto_aux WITH lc_point.

        cs_trailer-valor_total_cessao   = lv_vl_total_cessao_aux.
        cs_trailer-valor_total_desconto = lv_vl_total_desconto_aux.

      ENDIF.

      DELETE lt_file_table_aux INDEX lv_num_lines.
      DELETE lt_file_table_aux INDEX 1.

      LOOP AT lt_file_table_aux ASSIGNING <fs_file_line>.

        TRY.

            SPLIT <fs_file_line> AT lc_split INTO ls_detalhe-nro_nf
                                                  ls_detalhe-nosso_numero
                                                  lv_vl_total_boleto_aux
                                                  ls_detalhe-nome_cliente
                                                  ls_detalhe-cnpj_cliente
                                                  ls_detalhe-dt_emissao_nf
                                                  ls_detalhe-dt_vcto_nf
                                                  lv_vl_nf_aux
                                                  ls_detalhe-cnpj_emissor
                                                  ls_detalhe-dt_envio_fidc
                                                  ls_detalhe-dt_retorno_fidc
                                                  ls_detalhe-titulo_aceito
                                                  ls_detalhe-dt_recompra
                                                  ls_detalhe-tipo_movimento
                                                  lv_vl_cessao_aux
                                                  ls_detalhe-motivo_recusa
                                                  lv_taxa_cessao_aux
                                                  ls_detalhe-chave_nf
                                                  ls_detalhe-endereco_cliente
                                                  ls_detalhe-complemento
                                                  ls_detalhe-bairro
                                                  ls_detalhe-cidade
                                                  ls_detalhe-uf
                                                  ls_detalhe-cep_cliente
                                                  ls_detalhe-sequencial.

            REPLACE ALL OCCURRENCES OF lc_comma IN: lv_vl_total_boleto_aux WITH lc_point,
                                                    lv_vl_nf_aux           WITH lc_point,
                                                    lv_vl_cessao_aux       WITH lc_point,
                                                    lv_taxa_cessao_aux     WITH lc_point.

            CONDENSE: ls_detalhe-cnpj_emissor,
                      ls_detalhe-cep_cliente,
                      ls_detalhe-sequencial.

            IF ls_detalhe-dt_recompra = ''.
              CLEAR ls_detalhe-dt_recompra.
            ENDIF.

            ls_detalhe-valor_total_boleto = lv_vl_total_boleto_aux.
            ls_detalhe-valor_nf = lv_vl_nf_aux.
            ls_detalhe-valor_cessao = lv_vl_cessao_aux.
            ls_detalhe-taxa_cessao = lv_taxa_cessao_aux.

            APPEND ls_detalhe TO ct_detalhe.
            CLEAR  ls_detalhe.

          CATCH cx_root.
        ENDTRY.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD read_al11.

    DATA lv_newline TYPE string.

    TRY.
        OPEN DATASET iv_file FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.

        DO.
          READ DATASET iv_file INTO lv_newline.

          IF sy-subrc NE 0.

            CLOSE DATASET iv_file.
            EXIT.

          ENDIF.

          IF lv_newline IS NOT INITIAL.
            DATA(lv_strlen) = strlen( lv_newline ) - 1.
            lv_newline = lv_newline(lv_strlen).

            APPEND lv_newline TO rt_file_table.
          ENDIF.
        ENDDO.

      CATCH cx_sy_file_open_mode.
    ENDTRY.

    CLOSE DATASET iv_file.

  ENDMETHOD.


  METHOD find_file.

    CONSTANTS lc_file_substring TYPE string VALUE 'FAT_RET_'.

    DATA lt_files TYPE TABLE OF salfldir.

    DATA lv_dir TYPE pfeflnamel.

    lv_dir = iv_dir.

    CALL FUNCTION 'RZL_READ_DIR'
      EXPORTING
        name           = lv_dir
      TABLES
        file_tbl       = lt_files
      EXCEPTIONS
        argument_error = 1
        not_found      = 2
        send_error     = 3
        system_failure = 4
        OTHERS         = 5.

    IF sy-subrc IS INITIAL.

      lt_files = format_file_table( EXPORTING it_files = lt_files
                                              iv_filename_substring = lc_file_substring ).

      READ TABLE lt_files ASSIGNING FIELD-SYMBOL(<fs_file>) INDEX 1.

      IF <fs_file> IS ASSIGNED.
        rv_file = <fs_file>-name.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD move_file_dir.

    DATA lv_newline TYPE string.

    TRY.

        OPEN DATASET iv_dir_file FOR INPUT IN BINARY MODE.

        OPEN DATASET iv_dir_new_path FOR OUTPUT IN BINARY MODE.

        DO.
          READ DATASET iv_dir_file INTO lv_newline.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
          TRANSFER lv_newline  TO iv_dir_new_path.
        ENDDO.

        CLOSE DATASET  iv_dir_new_path.
        CLOSE DATASET  iv_dir_file.
        DELETE DATASET iv_dir_file.

      CATCH cx_sy_file_open_mode.
    ENDTRY.

  ENDMETHOD.


  METHOD format_file_table.

    DATA lt_files_aux LIKE it_files.

    LOOP AT it_files ASSIGNING FIELD-SYMBOL(<fs_files>).

      IF <fs_files>-name CS iv_filename_substring.
        APPEND <fs_files> TO lt_files_aux.
      ENDIF.

    ENDLOOP.

    rt_files = lt_files_aux.

  ENDMETHOD.


  METHOD format_file_name_with_path.

    rv_file_name_with_path = |{ iv_dir }/{ iv_file_name }|.

  ENDMETHOD.


  METHOD executa_processo.


    get_fat_sap(
      EXPORTING
        it_detalhe = ct_detalhe
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      "Erro

    ELSE.

      LOOP AT ct_detalhe ASSIGNING FIELD-SYMBOL(<fs_det>).


        IF <fs_det>-titulo_aceito = gc_s.

          processa_aceito_det( <fs_det> ).

        ELSEIF <fs_det>-titulo_aceito = gc_n.

          processa_rejeicao_det( <fs_det> ).

        ENDIF.

      ENDLOOP.

      save_log(  ).

    ENDIF.

  ENDMETHOD.


  METHOD get_fat_sap.

    DATA: lr_nn TYPE RANGE OF ztfi_fidc_contr-nossonumero.

    CLEAR: lr_nn.
    lr_nn = VALUE #(  FOR ls_nn IN it_detalhe
                     ( sign = 'I'
                       option = 'EQ'
                       low = ls_nn-nosso_numero )  ).

    CLEAR: gt_fidc_contr.
    SELECT *
        FROM ztfi_fidc_contr
        INTO TABLE @gt_fidc_contr
        WHERE nossonumero IN @lr_nn.

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.

  ENDMETHOD.


  METHOD processa_aceito_det.

    DATA: ls_key      TYPE zsfi_boleto_ban_key,
          ls_ret_comp TYPE zsfi_retorno_comp_tit_orig,
          ls_comp     TYPE zsfi_dados_retorno.

    DATA: lt_msg TYPE bapiret2_tab.

    READ TABLE gt_fidc_contr ASSIGNING FIELD-SYMBOL(<fs_fidc_contr>) WITH KEY nossonumero = is_det-nosso_numero BINARY SEARCH.
    IF sy-subrc = 0.

      lancar_doc_aceito(
        EXPORTING
          is_det      = is_det
        IMPORTING
          es_key      = ls_key
        CHANGING
          cs_fidc_ctr = <fs_fidc_contr>
      ).
      IF <fs_fidc_contr>-titulo_cob IS NOT INITIAL.

        NEW zclfi_boleto_util(  )->gerar_boleto(
          EXPORTING
            is_key = ls_key
          IMPORTING
            et_msg = lt_msg
        ).

        IF NOT line_exists( lt_msg[ type = gc_e ] ).

          SELECT SINGLE kunnr, bupla
          FROM bseg
          INTO @DATA(ls_dados)
            WHERE belnr = @<fs_fidc_contr>-belnr
              AND bukrs = @<fs_fidc_contr>-bukrs
              AND gjahr = @<fs_fidc_contr>-gjahr
              AND buzei = @<fs_fidc_contr>-buzei.

          ls_comp-cliente = ls_dados-kunnr.
          ls_comp-empresa = <fs_fidc_contr>-bukrs.
          ls_comp-numdoc = <fs_fidc_contr>-belnr.
          ls_comp-anodoc = <fs_fidc_contr>-gjahr.
          ls_comp-item = <fs_fidc_contr>-buzei.
          ls_comp-loc_negocios = ls_dados-bupla.
          ls_comp-valor_cessao = is_det-valor_cessao.
          ls_comp-valor_lancado_desconto = is_det-valor_total_boleto - is_det-valor_cessao.

          DATA(lo_comp) = NEW zclfi_compensa_titulo_original( ls_comp ).
          lo_comp->main(
            IMPORTING
              es_retorno_msg = ls_ret_comp
          ).

          "Sucesso
          IF ls_ret_comp-tipo_status = gc_s.

            <fs_fidc_contr>-tipo_status = gc_s.
            <fs_fidc_contr>-titulo_aceito = abap_true.
            <fs_fidc_contr>-status = TEXT-s01.
            <fs_fidc_contr>-titulo_comp = ls_ret_comp-nro_compensacao.

          ELSE.

            <fs_fidc_contr>-tipo_status = gc_e.
            <fs_fidc_contr>-status = ls_ret_comp-mensagem.

          ENDIF.

        ELSE.

          "'Erro ao tentar gerar boleto aceito'
          <fs_fidc_contr>-tipo_status = gc_e.
          <fs_fidc_contr>-status = TEXT-e01.

        ENDIF.

      ELSE.

        "'Erro ao tentar lançar doc. aceito'
        <fs_fidc_contr>-tipo_status = gc_e.
        <fs_fidc_contr>-status = TEXT-e05.

      ENDIF.

      <fs_fidc_contr>-data_retorno = sy-datum.
      <fs_fidc_contr>-hora_retorno = sy-uzeit.
      IF is_det-dt_recompra IS NOT INITIAL.
        <fs_fidc_contr>-dt_recompra = is_det-dt_recompra.
      ENDIF.
      <fs_fidc_contr>-valor_cessao = is_det-valor_cessao.
      <fs_fidc_contr>-taxa_cessao = is_det-taxa_cessao.
      <fs_fidc_contr>-valor_lanc_desc = ls_comp-valor_lancado_desconto.

    ENDIF.

  ENDMETHOD.


  METHOD lancar_doc_aceito.

    CONSTANTS: lc_bkpff      TYPE awtyp VALUE 'BKPFF',
               lc_rfst       TYPE glvor VALUE 'RFST',
               lc_fd         TYPE blart VALUE 'FD',
               lc_9999       TYPE bukrs VALUE '9999',
               lc_9901       TYPE bupla VALUE '9901',
               lc_fidc       TYPE bseg-hbkid VALUE 'FIDC',
               lc_00001      TYPE bseg-hktid VALUE '00001',
               lc_e          TYPE bseg-zlsch VALUE 'E',
               lc_brl        TYPE waers VALUE 'BRL',
               lc_retorno    TYPE ze_param_chave2 VALUE 'RETORNO',
               lc_conta_fidc TYPE ze_param_chave2 VALUE 'CONTA_FIDC',
               lc_txt        TYPE string VALUE 'Lçto FID' ##NO_TEXT.


    DATA ls_documentheader    TYPE bapiache09.
    DATA ls_accountreceivable TYPE bapiacar09.
    DATA ls_currencyamount    TYPE bapiaccr09.
    DATA ls_accountgl TYPE bapiacgl09.
    DATA: lt_accountreceivable TYPE STANDARD TABLE OF bapiacar09,
          lt_currencyamount    TYPE STANDARD TABLE OF bapiaccr09,
          lt_accountgl         TYPE STANDARD TABLE OF bapiacgl09,
          lt_return            TYPE bapiret2_tab.

    DATA: lv_doc TYPE bapiache09-obj_key.


    SELECT SINGLE kunnr, kkber
        FROM bseg
        INTO @DATA(ls_doc)
        WHERE belnr = @cs_fidc_ctr-belnr
         AND bukrs = @cs_fidc_ctr-bukrs
         AND gjahr = @cs_fidc_ctr-gjahr
         AND buzei = @cs_fidc_ctr-buzei.



    "Header
    ls_documentheader-obj_type         = lc_bkpff.
*    ls_documentheader-bus_act          = lc_rfst.
    ls_documentheader-username         = sy-uname.
    ls_documentheader-header_txt       = lc_txt &&
                                         sy-datum+6(2) &&
                                         sy-datum+4(2) &&
                                         sy-datum+2(2).
    ls_documentheader-ref_doc_no       = is_det-nro_nf.
    ls_documentheader-comp_code        = lc_9999.
    ls_documentheader-doc_date         = sy-datum.
    ls_documentheader-pstng_date       = sy-datum.
    ls_documentheader-fisc_year        = sy-datum+0(4).
    ls_documentheader-fis_period       = sy-datum+4(2).
    ls_documentheader-doc_type         = lc_fd.

    ls_accountreceivable-itemno_acc    = 1.


    ls_accountreceivable-customer      = ls_doc-kunnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accountreceivable-customer
      IMPORTING
        output = ls_accountreceivable-customer.


    ls_accountreceivable-item_text     = ls_documentheader-header_txt.
    ls_accountreceivable-alloc_nmbr     = cs_fidc_ctr-belnr &&
                                          cs_fidc_ctr-gjahr &&
                                          cs_fidc_ctr-buzei.
    ls_accountreceivable-comp_code     = lc_9999.
    ls_accountreceivable-bline_date = is_det-dt_vcto_nf.
    ls_accountreceivable-c_ctr_area = ls_doc-kkber.
    ls_accountreceivable-businessplace = lc_9901.
    ls_accountreceivable-pymt_meth = lc_e.
    ls_accountreceivable-ref_key_3 = is_det-nosso_numero.
    ls_accountreceivable-bank_id = lc_fidc.
    ls_accountreceivable-housebankacctid = lc_00001 .


    APPEND ls_accountreceivable TO lt_accountreceivable.


    ls_currencyamount-itemno_acc = 1.
    ls_currencyamount-currency   = lc_brl.
    ls_currencyamount-amt_doccur = is_det-valor_total_boleto.

    APPEND ls_currencyamount TO lt_currencyamount.
    CLEAR ls_currencyamount.



    SELECT SINGLE low
        FROM zi_ca_param_val
        INTO @DATA(lv_conta)
        WHERE modulo = @gc_fi_ar
          AND chave1 = @gc_fidc
          AND chave2 = @lc_retorno
          AND chave3 = @lc_conta_fidc.

    CLEAR:ls_accountgl.
    ls_accountgl-itemno_acc = 2.

    ls_accountgl-gl_account = lv_conta .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accountgl-gl_account
      IMPORTING
        output = ls_accountgl-gl_account.


    ls_accountgl-bus_area = lc_9901.
    ls_accountgl-alloc_nmbr = ls_accountreceivable-alloc_nmbr.
    ls_accountgl-item_text = ls_accountreceivable-item_text.

    APPEND ls_accountgl TO lt_accountgl.

    ls_currencyamount-itemno_acc = 2.
    ls_currencyamount-currency   = lc_brl.
    ls_currencyamount-amt_doccur = is_det-valor_total_boleto * -1.

    APPEND ls_currencyamount TO lt_currencyamount.
    CLEAR ls_currencyamount.


    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = ls_documentheader "CABEÇALHO.
      IMPORTING
        obj_key           = lv_doc
      TABLES
        accountreceivable = lt_accountreceivable
        currencyamount    = lt_currencyamount
        accountgl         = lt_accountgl
        return            = lt_return.

    IF NOT line_exists( lt_return[ type = 'E' ] ).       "#EC CI_STDSEQ

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      cs_fidc_ctr-tipo_status = gc_s.
      cs_fidc_ctr-titulo_cob = lv_doc.
      cs_fidc_ctr-titulo_bukrs = lc_9999.
      cs_fidc_ctr-titulo_gjahr = sy-datum(4).

      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type   = 'S'
                                                 id     = 'RW'
                                                 number = '605'.

      es_key-belnr = <fs_return>-message_v2(10).
      es_key-gjahr = sy-datum(4).
      es_key-bukrs = lc_9999.
      es_key-buzei = 1.


    ELSE.
      cs_fidc_ctr-tipo_status = gc_e.
      cs_fidc_ctr-status = TEXT-e01.
    ENDIF.


  ENDMETHOD.


  METHOD processa_rejeicao_det.


    DATA: ls_key      TYPE zsfi_boleto_ban_key,
          ls_ret_comp TYPE zsfi_retorno_comp_tit_orig,
          ls_comp     TYPE zsfi_dados_retorno.

    DATA: lt_msg TYPE bapiret2_tab.

    READ TABLE gt_fidc_contr ASSIGNING FIELD-SYMBOL(<fs_fidc_contr>) WITH KEY nossonumero = is_det-nosso_numero BINARY SEARCH.
    IF sy-subrc = 0.

      atualiza_fat_rej(
        EXPORTING
          is_det        = is_det
        CHANGING
          cs_fidc_contr = <fs_fidc_contr>
        EXCEPTIONS
          not_update    = 1
          OTHERS        = 2
      ).
      IF sy-subrc <> 0.

        <fs_fidc_contr>-status = TEXT-e03.
        <fs_fidc_contr>-tipo_status = gc_e.

      ELSE.

        MOVE-CORRESPONDING <fs_fidc_contr> TO ls_key.

        NEW zclfi_boleto_util(  )->gerar_boleto(
            EXPORTING
              is_key = ls_key
            IMPORTING
              et_msg = lt_msg
          ).

        IF NOT line_exists( lt_msg[ type = gc_e ] ).

          <fs_fidc_contr>-status = TEXT-s02.
          <fs_fidc_contr>-tipo_status = gc_s.

        ENDIF.

      ENDIF.

      <fs_fidc_contr>-motivo_recusa = is_det-motivo_recusa.
      <fs_fidc_contr>-data_retorno = sy-datum.
      <fs_fidc_contr>-hora_retorno = sy-uzeit.
      IF is_det-dt_recompra IS NOT INITIAL.
        <fs_fidc_contr>-dt_recompra = is_det-dt_recompra.
      ENDIF.
      <fs_fidc_contr>-valor_cessao = is_det-valor_cessao.
      <fs_fidc_contr>-taxa_cessao = is_det-taxa_cessao.
      <fs_fidc_contr>-valor_lanc_desc = ls_comp-valor_lancado_desconto.

    ENDIF.

  ENDMETHOD.


  METHOD atualiza_fat_rej.

    CONSTANTS: lc_xref3 TYPE fieldname VALUE 'XREF3' ##NO_TEXT,
               lc_xref2 TYPE fieldname VALUE 'XREF2' ##NO_TEXT,
               lc_hbkid TYPE fieldname VALUE 'HBKID' ##NO_TEXT,
               lc_hktid TYPE fieldname VALUE 'HKTID' ##NO_TEXT.

    DATA: lt_campos TYPE TABLE OF accchg.
    DATA: lv_xref2 TYPE bseg-xref2.
    DATA: lv_newval TYPE string,
          lv_bukrs  TYPE ze_param_chave3.

    lv_bukrs = cs_fidc_contr-bukrs.

    get_parameters( EXPORTING iv_modulo = gc_fi_ar
                              iv_chave1 = gc_fidc
                              iv_chave2 = gc_banco_emissor_rej
                              iv_chave3 = lv_bukrs
                     CHANGING cv_dir    = lv_newval ).

    SELECT SINGLE awtyp, awref, aworg
        FROM acdoca
        INTO @DATA(ls_acdoca)
        WHERE rldnr = '0L'
         AND rbukrs = @cs_fidc_contr-bukrs
         AND belnr = @cs_fidc_contr-belnr
         AND ryear = @cs_fidc_contr-gjahr
         AND buzei = @cs_fidc_contr-buzei.

    APPEND VALUE accchg( fdname = lc_xref3
                         newval = '' ) TO lt_campos.

    CONCATENATE gc_fidc sy-datum+6(2) sy-datum+4(2) sy-datum+2(2) INTO lv_xref2 SEPARATED BY space.
    APPEND VALUE accchg( fdname = lc_xref2
                         newval = '' ) TO lt_campos.
*****
    APPEND VALUE accchg( fdname = lc_hbkid
                         newval = lv_newval ) TO lt_campos.

    SELECT SINGLE hktid
      FROM t012k
      INTO @DATA(lv_hktid)
      WHERE bukrs = @cs_fidc_contr-bukrs
      AND hbkid = @lv_newval.

    APPEND VALUE accchg( fdname = lc_hktid
                         newval = lv_hktid ) TO lt_campos.
*****
    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_awtyp              = ls_acdoca-awtyp
        i_awref              = ls_acdoca-awref
        i_aworg              = ls_acdoca-aworg
        i_buzei              = cs_fidc_contr-buzei
        x_lock               = abap_true
        i_upd_fqm            = abap_true
      TABLES
        t_accchg             = lt_campos
      EXCEPTIONS
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      RAISE not_update.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD save_log.

    MODIFY ztfi_fidc_contr FROM TABLE gt_fidc_contr.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
