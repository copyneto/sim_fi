CLASS zclfi_fidc_remessa DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_t_report_rem,
        empresa            TYPE bsid_view-bukrs,
        ano                TYPE bsid_view-gjahr,
        numero_documento   TYPE bsid_view-belnr,
        item               TYPE bsid_view-buzei,
        area_credito       TYPE bsid_view-kkber,
        divisao            TYPE bsid_view-gsber,
        cliente            TYPE bsid_view-kunnr,
        nf                 TYPE bsid_view-xblnr,
        nosso_numero       TYPE bsid_view-xref3,
        valor_total_boleto TYPE bsid_view-wrbtr,
        nome_cliente       TYPE kna1-name1,
        cnpj_cliente       TYPE kna1-stcd2,
        docnum             TYPE j_1bnflin-docnum,
        data_emissao_nf    TYPE bsid_view-budat,
        data_vcto_nf       TYPE bseg-netdt,
        valor_nf           TYPE j_1bnfdoc-nftot,
        cnpj_emissor       TYPE t001z-paval,
        chave_nf           TYPE j_1b_nfe_access_key_dtel44,
        endereco_cliente   TYPE j_1bnfdoc-stras,
        complemento        TYPE j_1bnfdoc-house_num2,
        bairro             TYPE j_1bnfdoc-ort02,
        cidade             TYPE j_1bnfdoc-ort01,
        uf                 TYPE j_1bnfdoc-land1,
        cep                TYPE j_1bnfdoc-pstlz,
        email_1            TYPE but020-addrnumber,
        email_2            TYPE but020-addrnumber,
        telefone_1         TYPE kna1-telf1,
        telefone_2         TYPE kna1-telf2,
      END OF ty_t_report_rem .
    TYPES:
      ty_fidc_fat_rem_detalhe TYPE TABLE OF zsfi_fidc_fat_rem_detalhe .
    TYPES:
      ty_report_rem           TYPE TABLE OF ty_t_report_rem .

    DATA gs_header TYPE zsfi_fidc_fat_rem_header .
    DATA gt_detalhe TYPE ty_fidc_fat_rem_detalhe .
    DATA gs_trailer TYPE zsfi_fidc_fat_rem_trailer .
    DATA gt_remessa TYPE zctgfi_dados_remessa_job .

    METHODS constructor
      IMPORTING
        !it_report_rem TYPE zctgfi_dados_remessa_job .
    METHODS main
      RETURNING
        VALUE(rt_remessa) TYPE zctgfi_dados_remessa_job .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_fi_ar TYPE ze_param_modulo VALUE 'FI-AR' ##NO_TEXT.
    CONSTANTS gc_fidc TYPE ze_param_chave1 VALUE 'FIDC' ##NO_TEXT.
    CONSTANTS gc_remessa TYPE ze_param_chave2 VALUE 'REMESSA' ##NO_TEXT.
    CONSTANTS gc_pasta TYPE ze_param_chave3 VALUE 'PASTA' ##NO_TEXT.
    CONSTANTS gc_titulo_aceito TYPE c VALUE 'N' ##NO_TEXT.
    CONSTANTS gc_tipo_arquivo TYPE c VALUE '1' ##NO_TEXT.
    CONSTANTS gc_tipo_movimento TYPE c VALUE 'I' ##NO_TEXT.
    CONSTANTS gc_erro TYPE c VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_sucesso TYPE c VALUE 'S' ##NO_TEXT.
    DATA go_parametros TYPE REF TO zclca_tabela_parametros .
    DATA gv_dir_remessa TYPE string .
    DATA gv_mensagem TYPE string .
    DATA gv_remessa_file TYPE string .

    METHODS get_dados
      CHANGING
        ct_report_rem TYPE zctgfi_dados_remessa_job .
    METHODS get_parameters
      CHANGING
        !ct_remessa TYPE zctgfi_dados_remessa_job .
    METHODS set_dados
      IMPORTING
        !it_report_rem TYPE zctgfi_dados_remessa_job .
    METHODS validar
      IMPORTING
        !is_detalhe_line TYPE zsfi_fidc_fat_rem_detalhe
      CHANGING
        !cs_remessa_line TYPE zsfi_dados_remessa_job .
    METHODS processar_arquivo
      IMPORTING
        !is_header       TYPE zsfi_fidc_fat_rem_header
        !it_detalhe      TYPE ty_fidc_fat_rem_detalhe
        !is_trailer      TYPE zsfi_fidc_fat_rem_trailer
      CHANGING
        !cv_remessa_file TYPE string .
    METHODS salva_al11
      IMPORTING
        !iv_remessa_file TYPE string
      CHANGING
        !ct_remessa      TYPE zctgfi_dados_remessa_job .

    METHODS verifica_erro
      IMPORTING
                !it_remessa         TYPE zctgfi_dados_remessa_job
      RETURNING VALUE(rv_flag_erro) TYPE char1.

ENDCLASS.



CLASS zclfi_fidc_remessa IMPLEMENTATION.


  METHOD constructor.

    go_parametros = zclca_tabela_parametros=>get_instance( ).

    set_dados( it_report_rem ).
    get_parameters( CHANGING ct_remessa = gt_remessa ).

  ENDMETHOD.


  METHOD get_dados.

    DATA ls_detalhe TYPE zsfi_fidc_fat_rem_detalhe.

    DATA: lv_total_arquivo TYPE zsfi_fidc_fat_rem_trailer-valor_total_arquivo,
          lv_sequencial    TYPE i.

    DATA(lv_date) = sy-datum.
    DATA(lv_time) = sy-uzeit.
    DATA(lv_timestamp) = |{ lv_date }{ lv_time }|.

    gs_header-tipo_arquivo = gc_tipo_arquivo.
    gs_header-data_hora_geracao = lv_timestamp.

*    DATA(lv_num_lines) = lines( ct_report_rem ).

    lv_sequencial = 0.

    LOOP AT ct_report_rem ASSIGNING FIELD-SYMBOL(<fs_report_rem>).

      ls_detalhe-nro_nf             = <fs_report_rem>-nf.
      ls_detalhe-nosso_numero       = <fs_report_rem>-nosso_numero.
      ls_detalhe-valor_total_boleto = <fs_report_rem>-valor_total_boleto.
      ls_detalhe-nome_cliente       = <fs_report_rem>-nome_cliente.
      ls_detalhe-cnpj_cliente       = <fs_report_rem>-cnpj_cliente.
      ls_detalhe-dt_emissao_nf      = <fs_report_rem>-data_emissao_nf.
      ls_detalhe-dt_vcto_nf         = <fs_report_rem>-data_vcto_nf.
      ls_detalhe-valor_nf           = <fs_report_rem>-valor_nf.
      ls_detalhe-cnpj_emissor       = <fs_report_rem>-cnpj_emissor.
      ls_detalhe-dt_envio_fidc      = sy-datum.
      ls_detalhe-titulo_aceito      = gc_titulo_aceito.
      ls_detalhe-dt_recompra        = ''.
      ls_detalhe-tipo_movimento     = gc_tipo_movimento.
      ls_detalhe-chave_nf           = <fs_report_rem>-chave_nf.
      ls_detalhe-endereco_cliente   = <fs_report_rem>-endereco_cliente.
      ls_detalhe-complemento        = <fs_report_rem>-complemento.
      ls_detalhe-bairro             = <fs_report_rem>-bairro.
      ls_detalhe-cidade             = <fs_report_rem>-cidade.
      ls_detalhe-uf                 = <fs_report_rem>-uf.
      ls_detalhe-cep_cliente        = replace( val = <fs_report_rem>-cep sub = '-' with = '' ).
      ls_detalhe-email_contato_1    = <fs_report_rem>-email_1.
      ls_detalhe-email_contato_2    = <fs_report_rem>-email_2.
      ls_detalhe-telefone_contato_1 = <fs_report_rem>-telefone_1.
      ls_detalhe-telefone_contato_2 = <fs_report_rem>-telefone_2.
      ls_detalhe-sequencial         = lv_sequencial + 1.


      validar( EXPORTING is_detalhe_line = ls_detalhe
      CHANGING cs_remessa_line = <fs_report_rem> ).

      IF <fs_report_rem>-tipo_status NE gc_erro.

        lv_total_arquivo = lv_total_arquivo + <fs_report_rem>-valor_total_boleto.
        APPEND ls_detalhe TO gt_detalhe.
        lv_sequencial = lv_sequencial + 1.

      ENDIF.

      CLEAR ls_detalhe.

    ENDLOOP.

    gs_trailer-sequencial_total = 2 + lv_sequencial.
    gs_trailer-valor_total_arquivo = lv_total_arquivo.

  ENDMETHOD.


  METHOD get_parameters.

    DATA ls_remessa LIKE LINE OF gt_remessa.

    ls_remessa-tipo_status = gc_erro.
    MESSAGE e002 INTO ls_remessa-mensagem.

    TRY.

        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi_ar
                                               iv_chave1 = gc_fidc
                                               iv_chave2 = gc_remessa
                                               iv_chave3 = gc_pasta
                                     IMPORTING ev_param  = gv_dir_remessa ).

        IF gv_dir_remessa IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_fi_ar
                                                       iv_chave1 = gc_fidc
                                                       iv_chave2 = gc_remessa
                                                       iv_chave3 = gc_pasta ).

        ENDIF.

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
        MODIFY ct_remessa FROM ls_remessa TRANSPORTING tipo_status mensagem WHERE tipo_status <> gc_erro.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD main.

    IF verifica_erro( EXPORTING it_remessa = gt_remessa ) NE abap_true.

      get_dados( CHANGING ct_report_rem = gt_remessa ).

      processar_arquivo( EXPORTING is_header  = gs_header
                                   is_trailer = gs_trailer
                                   it_detalhe = gt_detalhe
                          CHANGING cv_remessa_file = gv_remessa_file ).

      salva_al11( EXPORTING iv_remessa_file = gv_remessa_file
                  CHANGING ct_remessa = gt_remessa ).

    ENDIF.

    rt_remessa = gt_remessa.

  ENDMETHOD.



  METHOD validar.
    DATA lv_mensagem TYPE string.
    FIELD-SYMBOLS <fs_remessa_line> LIKE cs_remessa_line.

    ASSIGN cs_remessa_line TO <fs_remessa_line>.

    DATA lv_campos TYPE string.

    IF is_detalhe_line-nro_nf IS INITIAL.
      lv_campos = |{ lv_campos } text-c01|.
    ENDIF.
    IF is_detalhe_line-nosso_numero IS INITIAL.
      lv_campos = |{ lv_campos } text-c02|.
    ENDIF.
    IF is_detalhe_line-valor_total_boleto IS INITIAL.
      lv_campos = |{ lv_campos } text-c03|.
    ENDIF.
    IF is_detalhe_line-nome_cliente IS INITIAL.
      lv_campos = |{ lv_campos } text-c04|.
    ENDIF.
    IF is_detalhe_line-cnpj_cliente IS INITIAL.
      lv_campos = |{ lv_campos } text-c05|.
    ENDIF.
    IF is_detalhe_line-dt_emissao_nf IS INITIAL.
      lv_campos = |{ lv_campos } text-c06|.
    ENDIF.
    IF is_detalhe_line-dt_vcto_nf IS INITIAL.
      lv_campos = |{ lv_campos } text-c07|.
    ENDIF.
    IF is_detalhe_line-valor_nf IS INITIAL.
      lv_campos = |{ lv_campos } text-c08|.
    ENDIF.
    IF is_detalhe_line-cnpj_emissor IS INITIAL.
      lv_campos = |{ lv_campos } text-c09|.
    ENDIF.
    IF is_detalhe_line-titulo_aceito IS INITIAL.
      lv_campos = |{ lv_campos } text-c10|.
    ENDIF.
    IF is_detalhe_line-tipo_movimento IS INITIAL.
      lv_campos = |{ lv_campos } text-c11|.
    ENDIF.
    IF is_detalhe_line-chave_nf IS INITIAL.
      lv_campos = |{ lv_campos } text-c12|.
    ENDIF.
    IF is_detalhe_line-endereco_cliente IS INITIAL.
      lv_campos = |{ lv_campos } text-c13|.
    ENDIF.
    IF is_detalhe_line-bairro IS INITIAL.
      lv_campos = |{ lv_campos } text-c14|.
    ENDIF.
    IF is_detalhe_line-cidade IS INITIAL.
      lv_campos = |{ lv_campos } text-c15|.
    ENDIF.
    IF is_detalhe_line-uf IS INITIAL.
      lv_campos = |{ lv_campos } text-c16|.
    ENDIF.
    IF is_detalhe_line-cep_cliente IS INITIAL.
      lv_campos = |{ lv_campos } text-c17|.
    ENDIF.
    IF is_detalhe_line-sequencial IS INITIAL.
      lv_campos = |{ lv_campos } text-c18|.
    ENDIF.

    IF lv_campos IS NOT INITIAL.

      MESSAGE e000 WITH lv_campos INTO lv_mensagem.
      <fs_remessa_line>-tipo_status = gc_erro.
      <fs_remessa_line>-tipo_status = lv_mensagem.

    ELSE.
      <fs_remessa_line>-tipo_status = gc_sucesso.
    ENDIF.

  ENDMETHOD.


  METHOD set_dados.

    gt_remessa = it_report_rem.

  ENDMETHOD.


  METHOD processar_arquivo.

    CONSTANTS lc_curr(3) TYPE c VALUE 'BRL'.

    DATA: lv_valor_total_boleto_aux(18)  TYPE c,
          lv_valor_nf_aux(18)            TYPE c,
          lv_valor_total_arquivo_aux(18) TYPE c.

    cv_remessa_file = |{ is_header-tipo_arquivo };{ is_header-data_hora_geracao }{ cl_abap_char_utilities=>newline }|.

    LOOP AT it_detalhe ASSIGNING FIELD-SYMBOL(<fs_detalhe>).

      WRITE: <fs_detalhe>-valor_total_boleto TO lv_valor_total_boleto_aux NO-GROUPING CURRENCY lc_curr,
             <fs_detalhe>-valor_nf TO lv_valor_nf_aux NO-GROUPING CURRENCY lc_curr.

      cv_remessa_file = |{ cv_remessa_file }| &
                        |{ <fs_detalhe>-nro_nf };| &
                        |{ <fs_detalhe>-nosso_numero };| &
                        |{ condense( lv_valor_total_boleto_aux ) };| &
                        |{ <fs_detalhe>-nome_cliente };| &
                        |{ condense( <fs_detalhe>-cnpj_cliente ) };| &
                        |{ <fs_detalhe>-dt_emissao_nf };| &
                        |{ <fs_detalhe>-dt_vcto_nf };| &
                        |{ condense( lv_valor_nf_aux ) };| &
                        |{ condense( <fs_detalhe>-cnpj_emissor ) };| &
                        |{ <fs_detalhe>-dt_envio_fidc };| &
                        |{ <fs_detalhe>-dt_retorno_fidc };| &
                        |{ <fs_detalhe>-titulo_aceito };| &
                        |{ <fs_detalhe>-dt_recompra };| &
                        |{ <fs_detalhe>-tipo_movimento };| &
                        |{ <fs_detalhe>-chave_nf };| &
                        |{ <fs_detalhe>-endereco_cliente };| &
                        |{ <fs_detalhe>-complemento };| &
                        |{ <fs_detalhe>-bairro };| &
                        |{ <fs_detalhe>-cidade };| &
                        |{ <fs_detalhe>-uf };| &
                        |{ <fs_detalhe>-cep_cliente };| &
                        |{ <fs_detalhe>-email_contato_1 };| &
                        |{ <fs_detalhe>-email_contato_2 };| &
                        |{ <fs_detalhe>-telefone_contato_1 };| &
                        |{ <fs_detalhe>-telefone_contato_2 };| &
                        |{ condense( <fs_detalhe>-sequencial ) }| &
                        |{ cl_abap_char_utilities=>newline }|.

    ENDLOOP.

    WRITE is_trailer-valor_total_arquivo TO lv_valor_total_arquivo_aux NO-GROUPING CURRENCY lc_curr.
    cv_remessa_file = |{ cv_remessa_file }{ condense( is_trailer-sequencial_total ) };{ condense( lv_valor_total_arquivo_aux ) }|.

  ENDMETHOD.


  METHOD salva_al11.

    DATA ls_remessa LIKE LINE OF gt_remessa.

    DATA: lv_filename       TYPE string,
          lv_file_with_path TYPE string.

    lv_filename = |FAT_REM_{ gs_header-data_hora_geracao }.TXT|.

    lv_file_with_path = |{ gv_dir_remessa }/{ lv_filename }|.

    TRY.

        OPEN DATASET lv_file_with_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        TRANSFER iv_remessa_file TO lv_file_with_path.
        CLOSE DATASET lv_file_with_path.

        ls_remessa-tipo_status = gc_sucesso.
        ls_remessa-nome_arq_remessa = lv_filename.
        MESSAGE s003 WITH lv_filename INTO ls_remessa-mensagem.
        MODIFY ct_remessa FROM ls_remessa TRANSPORTING tipo_status mensagem nome_arq_remessa WHERE tipo_status <> gc_erro.

      CATCH cx_sy_file_open_mode.

        ls_remessa-tipo_status = gc_erro.
        MESSAGE e001 INTO ls_remessa-mensagem.
        MODIFY ct_remessa FROM ls_remessa TRANSPORTING tipo_status mensagem WHERE tipo_status <> gc_erro.
        RETURN.

    ENDTRY.

  ENDMETHOD.
  METHOD verifica_erro.

    rv_flag_erro = abap_true.

    LOOP AT it_remessa ASSIGNING FIELD-SYMBOL(<fs_remessa>).

      IF <fs_remessa>-tipo_status NE gc_erro.
        rv_flag_erro = abap_false.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
