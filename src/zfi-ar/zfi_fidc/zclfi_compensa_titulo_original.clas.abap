CLASS zclfi_compensa_titulo_original DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_function TYPE rfipi-funct VALUE 'C' ##NO_TEXT.
    CONSTANTS gc_mode TYPE rfpdo-allgazmd VALUE 'N' ##NO_TEXT.
    CONSTANTS gc_update TYPE rfpdo-allgvbmd VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_user TYPE apqi-userid VALUE 'SVC-JOB-USER' ##NO_TEXT.
    CONSTANTS: gc_modulo_fi     TYPE ztca_param_mod-modulo VALUE 'FI-AR',
               gc_chave1        TYPE ztca_param_par-chave1 VALUE 'FIDC',
               gc_chave2_cb     TYPE ztca_param_par-chave2 VALUE 'CONTA_BAIXA',
               gc_chave2_cc     TYPE ztca_param_par-chave2 VALUE 'CENTRO_DE_CUSTO',
               gc_chave2_ret    TYPE ztca_param_par-chave2 VALUE 'RETORNO',
               gc_chave3        TYPE ztca_param_par-chave2 VALUE 'CONTA_BAIXA_FIDC',
               gc_auglv         TYPE t041a-auglv VALUE 'UMBUCHNG',
               gc_tcode         TYPE sy-tcode VALUE 'FB05',
               gc_sgfunct       TYPE rfipi-sgfunct VALUE 'C',
               gc_agkoa         TYPE koart VALUE 'D',
               gc_xnops         TYPE xnops VALUE 'X',
               gc_belnr         TYPE fld30_f05a VALUE 'BELNR',
               gc_stype_k       TYPE ftpost-stype VALUE 'K',
               gc_stype_p       TYPE ftpost-stype VALUE 'P',
               gc_count_1       TYPE ftpost-count VALUE '001',
               gc_count_2       TYPE ftpost-count VALUE '002',
               gc_count_3       TYPE ftpost-count VALUE '003',
               gc_bkpf_budat    TYPE ftpost-fnam VALUE 'BKPF-BUDAT',
               gc_bkpf_bldat    TYPE ftpost-fnam VALUE 'BKPF-BLDAT',
               gc_bkpf_blart    TYPE ftpost-fnam VALUE 'BKPF-BLART',
               gc_bkpf_bukrs    TYPE ftpost-fnam VALUE 'BKPF-BUKRS',
               gc_bkpf_waers    TYPE ftpost-fnam VALUE 'BKPF-WAERS',
               gc_bkpf_bktxt    TYPE ftpost-fnam VALUE 'BKPF-BKTXT',
               gc_fd            TYPE ftpost-fval VALUE 'FD',
               gc_brl           TYPE ftpost-fval VALUE 'BRL',
               gc_fidc          TYPE c LENGTH 4 VALUE 'FIDC',
               gc_bseg_bupla    TYPE ftpost-fnam VALUE 'BSEG-BUPLA',
               gc_rf05a_newbs   TYPE ftpost-fnam VALUE 'RF05A-NEWBS',
               gc_rf05a_newko   TYPE ftpost-fnam VALUE 'RF05A-NEWKO',
               gc_bseg_wrbtr    TYPE ftpost-fnam VALUE 'BSEG-WRBTR',
               gc_bseg_sgtxt    TYPE ftpost-fnam VALUE 'BSEG-SGTXT',
               gc_cobl_kostl    TYPE ftpost-fnam VALUE 'COBL-KOSTL',
               gc_bseg_valut    TYPE ftpost-fnam VALUE 'BSEG-VALUT',
               gc_40            TYPE ftpost-fval VALUE '40',
               gc_receb_fidc    TYPE c LENGTH 16 VALUE 'RECEBIMENTO FIDC',
               gc_taxa_fidc     TYPE c LENGTH 9 VALUE 'TAXA FIDC',
               gc_bdcimmed      TYPE rfipi-bdcimmed VALUE 'X',
               gc_msgtyp_e      TYPE sy-msgty VALUE 'E',
               gc_msgtyp_s      TYPE sy-msgty VALUE 'S',
               gc_msgid_success TYPE sy-msgid VALUE 'F5',
               gc_msgno_success TYPE sy-msgno VALUE '312'.


    TYPES: tt_t_ftclear TYPE STANDARD TABLE OF ftclear,
           tt_t_ftpost  TYPE STANDARD TABLE OF ftpost.

    DATA: gs_dados_retorno TYPE zsfi_dados_retorno,
          go_parametros    TYPE REF TO zclca_tabela_parametros,
          gv_newko_cb      TYPE rf05a-newko,
          gv_newko_ret     TYPE rf05a-newko,
          gv_centrocusto   TYPE cobl-kostl.


    METHODS constructor
      IMPORTING
        !is_dados_retorno TYPE zsfi_dados_retorno OPTIONAL .


    METHODS main
      EXPORTING
        !es_retorno_msg TYPE zsfi_retorno_comp_tit_orig.


  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS set_data
      IMPORTING
        !is_dados_retorno TYPE zsfi_dados_retorno.

    METHODS get_parameters.

    METHODS get_data
      EXPORTING
        !et_t_ftclear TYPE tt_t_ftclear
        !et_t_ftpost  TYPE tt_t_ftpost.

    METHODS call_start
      CHANGING
        !cs_retorno_msg TYPE zsfi_retorno_comp_tit_orig.

    METHODS call_clearing
      IMPORTING
        !it_t_ftclear   TYPE tt_t_ftclear
        !it_t_ftpost    TYPE tt_t_ftpost
      CHANGING
        !cs_retorno_msg TYPE zsfi_retorno_comp_tit_orig.

    METHODS call_end
      CHANGING
        !cs_retorno_msg TYPE zsfi_retorno_comp_tit_orig.
ENDCLASS.



CLASS zclfi_compensa_titulo_original IMPLEMENTATION.

  METHOD constructor.

    set_data( is_dados_retorno ).

    go_parametros = zclca_tabela_parametros=>get_instance( ).
    get_parameters( ).

  ENDMETHOD.

  METHOD set_data.

    gs_dados_retorno = is_dados_retorno.

  ENDMETHOD.

  METHOD get_data.

    DATA: ls_t_ftclear           TYPE ftclear,
          lt_t_ftclear           TYPE STANDARD TABLE OF ftclear,
          ls_t_ftpost            TYPE ftpost,
          lt_t_ftpost            TYPE STANDARD TABLE OF ftpost,
          lv_newko_cb            TYPE rf05a-newko,
          lv_newko_ret           TYPE rf05a-newko,
          lv_vlr_cessao_char(18) TYPE c,
          lv_vlr_desc_char(18)   TYPE c.



    DATA(ls_dados_retorno) = gs_dados_retorno.

    IF ls_dados_retorno IS NOT INITIAL.

      " Montagem estrutura T_FTCLEAR
      CLEAR ls_t_ftclear.

      ls_t_ftclear-agkoa = gc_agkoa.
      ls_t_ftclear-agkon = ls_dados_retorno-cliente.
      ls_t_ftclear-agbuk = ls_dados_retorno-empresa.
      ls_t_ftclear-xnops = gc_xnops.
      ls_t_ftclear-selfd = gc_belnr.
      ls_t_ftclear-selvon = |{ ls_dados_retorno-numdoc }{ ls_dados_retorno-anodoc }{ ls_dados_retorno-item }|.
      ls_t_ftclear-selbis = |{ ls_dados_retorno-numdoc }{ ls_dados_retorno-anodoc }{ ls_dados_retorno-item }|.

      APPEND ls_t_ftclear TO lt_t_ftclear.

      " Montagem estrutura T_FTPOST -> STYPE=K COUNT=001
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bkpf_budat.
      ls_t_ftpost-fval = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bkpf_bldat.
      ls_t_ftpost-fval = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bkpf_blart.
      ls_t_ftpost-fval = gc_fd.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bkpf_bukrs.
      ls_t_ftpost-fval = ls_dados_retorno-empresa.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bkpf_waers.
      ls_t_ftpost-fval = gc_brl.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bkpf_bktxt.
      ls_t_ftpost-fval = |{ gc_fidc } { sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.





      " Montagem estrutura T_FTPOST -> STYPE=P COUNT=002
      IF ls_dados_retorno-loc_negocios IS INITIAL.
        SELECT SINGLE bupla
          FROM bseg
          INTO @DATA(lv_bupla)
          WHERE bukrs = @ls_dados_retorno-empresa
            AND belnr = @ls_dados_retorno-numdoc
            AND gjahr = @ls_dados_retorno-anodoc
            AND buzei = @ls_dados_retorno-item.

        IF lv_bupla IS NOT INITIAL.
          ls_t_ftpost-stype = gc_stype_p.
          ls_t_ftpost-count = gc_count_2.
          ls_t_ftpost-fnam = gc_bseg_bupla.
          ls_t_ftpost-fval = lv_bupla.
          APPEND ls_t_ftpost TO lt_t_ftpost.
          CLEAR ls_t_ftpost.
        ENDIF.
      ELSE.
        ls_t_ftpost-stype = gc_stype_p.
        ls_t_ftpost-count = gc_count_2.
        ls_t_ftpost-fnam = gc_bseg_bupla.
        ls_t_ftpost-fval = ls_dados_retorno-loc_negocios.
        APPEND ls_t_ftpost TO lt_t_ftpost.
        CLEAR ls_t_ftpost.
      ENDIF.

      ls_t_ftpost-stype = gc_stype_p.
      ls_t_ftpost-count = gc_count_2.
      ls_t_ftpost-fnam = gc_rf05a_newbs.
      ls_t_ftpost-fval = gc_40.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      IF sy-subrc IS INITIAL.
        ls_t_ftpost-stype = gc_stype_p.
        ls_t_ftpost-count = gc_count_2.
        ls_t_ftpost-fnam = gc_rf05a_newko.
        ls_t_ftpost-fval = gv_newko_cb.
        APPEND ls_t_ftpost TO lt_t_ftpost.
        CLEAR ls_t_ftpost.
      ENDIF.

      lv_vlr_cessao_char = ls_dados_retorno-valor_cessao.
      REPLACE '.' WITH ',' INTO lv_vlr_cessao_char.
      SHIFT lv_vlr_cessao_char LEFT DELETING LEADING space.
      ls_t_ftpost-stype = gc_stype_p.
      ls_t_ftpost-count = gc_count_2.
      ls_t_ftpost-fnam = gc_bseg_wrbtr.
      ls_t_ftpost-fval = lv_vlr_cessao_char.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_p.
      ls_t_ftpost-count = gc_count_2.
      ls_t_ftpost-fnam = gc_bseg_sgtxt.
      ls_t_ftpost-fval = |{ gc_receb_fidc } { sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_p.
      ls_t_ftpost-count = gc_count_2.
      ls_t_ftpost-fnam = gc_bseg_valut.
      ls_t_ftpost-fval = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.


      " Montagem estrutura T_FTPOST -> STYPE=P COUNT=003
      ls_t_ftpost-stype = gc_stype_p.
      ls_t_ftpost-count = gc_count_3.
      ls_t_ftpost-fnam = gc_rf05a_newbs.
      ls_t_ftpost-fval = gc_40.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      IF ls_dados_retorno-loc_negocios IS INITIAL.
        SELECT SINGLE bupla
          FROM bseg
          INTO @lv_bupla
          WHERE bukrs = @ls_dados_retorno-empresa
            AND belnr = @ls_dados_retorno-numdoc
            AND gjahr = @ls_dados_retorno-anodoc
            AND buzei = @ls_dados_retorno-item.

        IF lv_bupla IS NOT INITIAL.
          ls_t_ftpost-stype = gc_stype_p.
          ls_t_ftpost-count = gc_count_3.
          ls_t_ftpost-fnam = gc_bseg_bupla.
          ls_t_ftpost-fval = lv_bupla.
          APPEND ls_t_ftpost TO lt_t_ftpost.
          CLEAR ls_t_ftpost.
        ENDIF.
      ELSE.
        ls_t_ftpost-stype = gc_stype_p.
        ls_t_ftpost-count = gc_count_3.
        ls_t_ftpost-fnam = gc_bseg_bupla.
        ls_t_ftpost-fval = ls_dados_retorno-loc_negocios.
        APPEND ls_t_ftpost TO lt_t_ftpost.
        CLEAR ls_t_ftpost.
      ENDIF.

      IF sy-subrc IS INITIAL.
        ls_t_ftpost-stype = gc_stype_p.
        ls_t_ftpost-count = gc_count_3.
        ls_t_ftpost-fnam = gc_rf05a_newko.
        ls_t_ftpost-fval = gv_newko_ret.
        APPEND ls_t_ftpost TO lt_t_ftpost.
        CLEAR ls_t_ftpost.
      ENDIF.

      lv_vlr_desc_char = ls_dados_retorno-valor_lancado_desconto.
      REPLACE '.' WITH ',' INTO lv_vlr_desc_char.
      SHIFT lv_vlr_desc_char LEFT DELETING LEADING space.
      ls_t_ftpost-stype = gc_stype_p.
      ls_t_ftpost-count = gc_count_3.
      ls_t_ftpost-fnam = gc_bseg_wrbtr.
      ls_t_ftpost-fval = lv_vlr_desc_char.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_p.
      ls_t_ftpost-count = gc_count_3.
      ls_t_ftpost-fnam = gc_bseg_sgtxt.
      ls_t_ftpost-fval = |{ gc_taxa_fidc } { sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

      ls_t_ftpost-stype = gc_stype_p.
      ls_t_ftpost-count = gc_count_3.
      ls_t_ftpost-fnam = gc_cobl_kostl.
      ls_t_ftpost-fval = gv_centrocusto.
      APPEND ls_t_ftpost TO lt_t_ftpost.
      CLEAR ls_t_ftpost.

    ENDIF.

    et_t_ftclear = lt_t_ftclear.
    et_t_ftpost = lt_t_ftpost.

  ENDMETHOD.

  METHOD call_start.
    DATA: lv_message TYPE zsfi_retorno_comp_tit_orig-mensagem,
          ls_message TYPE zsfi_retorno_comp_tit_orig.

    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_client         = sy-mandt
        i_function       = gc_function
        i_mode           = gc_mode
        i_update         = gc_update
        i_user           = gc_user
      EXCEPTIONS
        client_incorrect = 1
        function_invalid = 2
*       GROUP_NAME_MISSING       = 3
        mode_invalid     = 4
        update_invalid   = 5
        user_invalid     = 6
        OTHERS           = 7.
    IF sy-subrc <> 0.
      MESSAGE e001(zfi_compensa_tit_ori) INTO lv_message.

      ls_message-tipo_status = gc_msgtyp_e.
      ls_message-mensagem = lv_message.

      ls_message = cs_retorno_msg.

    ENDIF.

  ENDMETHOD.

  METHOD call_clearing.

    DATA: lt_t_blntab TYPE STANDARD TABLE OF blntab,
          lt_t_fttax  TYPE STANDARD TABLE OF fttax.

    DATA: lv_message TYPE zsfi_retorno_comp_tit_orig-mensagem,
          ls_message TYPE zsfi_retorno_comp_tit_orig,
          lv_msgid   TYPE sy-msgid,
          lv_msgno   TYPE sy-msgno,
          lv_msgty   TYPE sy-msgty,
          lv_msgv1   TYPE sy-msgv1,
          lv_msgv2   TYPE sy-msgv2,
          lv_msgv3   TYPE sy-msgv3,
          lv_msgv4   TYPE sy-msgv4,
          lv_subrc   TYPE sy-subrc.

    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = gc_auglv
        i_tcode                    = gc_tcode
        i_sgfunct                  = gc_sgfunct
*       i_no_auth                  = space
*       i_xsimu                    = space
      IMPORTING
        e_msgid                    = lv_msgid
        e_msgno                    = lv_msgno
        e_msgty                    = lv_msgty
        e_msgv1                    = lv_msgv1
        e_msgv2                    = lv_msgv2
        e_msgv3                    = lv_msgv3
        e_msgv4                    = lv_msgv4
        e_subrc                    = lv_subrc
      TABLES
        t_blntab                   = lt_t_blntab
        t_ftclear                  = it_t_ftclear
        t_ftpost                   = it_t_ftpost
        t_fttax                    = lt_t_fttax
      EXCEPTIONS
        clearing_procedure_invalid = 1
        clearing_procedure_missing = 2
        table_t041a_empty          = 3
        transaction_code_invalid   = 4
        amount_format_error        = 5
        too_many_line_items        = 6
        company_code_invalid       = 7
        screen_not_found           = 8
        no_authorization           = 9
        OTHERS                     = 10.
    IF sy-subrc <> 0.

      MESSAGE e002(zfi_compensa_tit_ori) INTO lv_message.

      ls_message-tipo_status = gc_msgtyp_e.
      ls_message-mensagem = lv_message.

      ls_message = cs_retorno_msg.

    ELSE.

      IF lv_msgty = gc_msgtyp_s AND lv_msgid = gc_msgid_success AND lv_msgno = gc_msgno_success.

        MESSAGE s005(zfi_compensa_tit_ori) WITH lv_msgv1
                                                gs_dados_retorno-anodoc
                                                gs_dados_retorno-empresa
                                                INTO lv_message.

        ls_message-tipo_status = lv_msgty.
        ls_message-mensagem = lv_message.
        ls_message-nro_compensacao = lv_msgv1.

        cs_retorno_msg = ls_message.

      ELSE.

        MESSAGE e004(zfi_compensa_tit_ori) WITH lv_msgid lv_msgty lv_msgno INTO lv_message.

        ls_message-tipo_status = lv_msgty.
        ls_message-mensagem = lv_message.

        cs_retorno_msg = ls_message.

      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD call_end.
    DATA: lv_message TYPE zsfi_retorno_comp_tit_orig-mensagem,
          ls_message TYPE zsfi_retorno_comp_tit_orig.

    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXPORTING
        i_bdcimmed              = gc_bdcimmed
        i_bdcstrtdt             = sy-datum
        i_bdcstrttm             = sy-uzeit
      EXCEPTIONS
        session_not_processable = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE e003(zfi_compensa_tit_ori) INTO lv_message.

      ls_message-tipo_status = gc_msgtyp_e.
      ls_message-mensagem = lv_message.

      ls_message = cs_retorno_msg.
    ENDIF.

  ENDMETHOD.

  METHOD get_parameters.
    DATA: lv_chave3 TYPE ze_param_chave3.

    lv_chave3 = gs_dados_retorno-empresa.
    TRY.
        go_parametros->m_get_single(
          EXPORTING
            iv_modulo = gc_modulo_fi
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2_cb
            iv_chave3 = lv_chave3
          IMPORTING
            ev_param  = gv_newko_cb
        ).

        IF gv_newko_cb IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo_fi
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_chave2_cb
                                                       iv_chave3 = lv_chave3 ).

        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    TRY.
        go_parametros->m_get_single(
          EXPORTING
            iv_modulo = gc_modulo_fi
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2_ret
            iv_chave3 = gc_chave3
          IMPORTING
            ev_param  = gv_newko_ret
        ).

        IF gv_newko_ret IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo_fi
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_chave2_ret
                                                       iv_chave3 = gc_chave3 ).

        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.




    TRY.
        DATA(lr_param) = zclca_tabela_parametros=>get_instance( ).
        lv_chave3 = gs_dados_retorno-empresa.
        lr_param->m_get_single(
          EXPORTING
            iv_modulo = gc_modulo_fi
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2_cc
            iv_chave3 = lv_chave3
          IMPORTING
            ev_param  = gv_centrocusto
        ).

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.




  ENDMETHOD.

  METHOD main.

    DATA: lt_t_ftclear       TYPE STANDARD TABLE OF ftclear,
          lt_t_ftpost        TYPE STANDARD TABLE OF ftpost,
          lv_msg             TYPE char60,
          ls_retorno_msg_end TYPE zsfi_retorno_comp_tit_orig.

    get_data(
      IMPORTING
        et_t_ftclear = lt_t_ftclear
        et_t_ftpost  = lt_t_ftpost
    ).

    call_start(
      CHANGING
        cs_retorno_msg = es_retorno_msg
    ).

    IF es_retorno_msg IS INITIAL.

      call_clearing(
        EXPORTING
          it_t_ftclear   = lt_t_ftclear
          it_t_ftpost    = lt_t_ftpost
        CHANGING
          cs_retorno_msg = es_retorno_msg
      ).

      IF es_retorno_msg-tipo_status = gc_msgtyp_s.
        call_end(
          CHANGING
            cs_retorno_msg = ls_retorno_msg_end
        ).

      ENDIF.
    ENDIF.

    IF es_retorno_msg-tipo_status = gc_msgtyp_s.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ELSEIF es_retorno_msg-tipo_status = gc_msgtyp_e OR ls_retorno_msg_end IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
