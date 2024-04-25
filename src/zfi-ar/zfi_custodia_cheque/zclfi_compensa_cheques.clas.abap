CLASS zclfi_compensa_cheques DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_t_ftclear TYPE STANDARD TABLE OF ftclear .
    TYPES:
      tt_t_ftpost  TYPE STANDARD TABLE OF ftpost .

    CONSTANTS gc_function TYPE rfipi-funct VALUE 'C' ##NO_TEXT.
    CONSTANTS gc_mode TYPE rfpdo-allgazmd VALUE 'N' ##NO_TEXT.
    CONSTANTS gc_update TYPE rfpdo-allgvbmd VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_modulo_fi TYPE ztca_param_mod-modulo VALUE 'FI-AR' ##NO_TEXT.
    CONSTANTS gc_chave1 TYPE ztca_param_par-chave1 VALUE 'CUSTODIA_DE_CHEQUES' ##NO_TEXT.
    CONSTANTS gc_chave2_tipodoc TYPE ztca_param_par-chave2 VALUE 'ZCHEQUE_CLIENTE_TIPODOC' ##NO_TEXT.
    CONSTANTS gc_chave2_formapagtodoc TYPE ztca_param_par-chave2 VALUE 'ZCHEQUE_CLIENTE_FORMAPAGTODOC' ##NO_TEXT.
    CONSTANTS gc_chave2_desc TYPE ztca_param_par-chave2 VALUE 'ZCHEQUE_CLIENTE_DESC' ##NO_TEXT.
    CONSTANTS gc_chave2_contacreditocheque TYPE ztca_param_par-chave2 VALUE 'ZCHEQUE_CLIENTE_CONTACREDITOCHEQUE' ##NO_TEXT.
    CONSTANTS gc_chave2_bancoempresa TYPE ztca_param_par-chave2 VALUE 'ZCHEQUE_BANCOEMPRESA' ##NO_TEXT.
    CONSTANTS gc_chave2_ctadinheiro TYPE ztca_param_par-chave2 VALUE 'ZCHEQUE_CLIENTE_CTADINHEIRO' ##NO_TEXT.
    CONSTANTS gc_chave2_forpagto TYPE ztca_param_par-chave2 VALUE 'ZCHEQUE_CLIENTE_FORMAPAGTODOC' ##NO_TEXT.
    CONSTANTS gc_auglv TYPE t041a-auglv VALUE 'UMBUCHNG' ##NO_TEXT.
    CONSTANTS gc_tcode TYPE sy-tcode VALUE 'FB05' ##NO_TEXT.
    CONSTANTS gc_sgfunct TYPE rfipi-sgfunct VALUE 'C' ##NO_TEXT.
    CONSTANTS gc_agkoa TYPE koart VALUE 'D' ##NO_TEXT.
    CONSTANTS gc_agums TYPE koart VALUE 'D' ##NO_TEXT.
    CONSTANTS gc_xnops TYPE xnops VALUE 'X' ##NO_TEXT.
    CONSTANTS gc_belnr TYPE fld30_f05a VALUE 'BELNR' ##NO_TEXT.
    CONSTANTS gc_stype_k TYPE ftpost-stype VALUE 'K' ##NO_TEXT.
    CONSTANTS gc_stype_p TYPE ftpost-stype VALUE 'P' ##NO_TEXT.
    CONSTANTS gc_count_1 TYPE ftpost-count VALUE '001' ##NO_TEXT.
    CONSTANTS gc_count_2 TYPE ftpost-count VALUE '002' ##NO_TEXT.
    CONSTANTS gc_count_3 TYPE ftpost-count VALUE '003' ##NO_TEXT.
    CONSTANTS gc_vazio TYPE sy-msgty VALUE '' ##NO_TEXT.
    CONSTANTS gc_bkpf_budat TYPE ftpost-fnam VALUE 'BKPF-BUDAT' ##NO_TEXT.
    CONSTANTS gc_bkpf_bldat TYPE ftpost-fnam VALUE 'BKPF-BLDAT' ##NO_TEXT.
    CONSTANTS gc_bkpf_blart TYPE ftpost-fnam VALUE 'BKPF-BLART' ##NO_TEXT.
    CONSTANTS gc_bkpf_bukrs TYPE ftpost-fnam VALUE 'BKPF-BUKRS' ##NO_TEXT.
    CONSTANTS gc_bkpf_monat TYPE ftpost-fnam VALUE 'BKPF-MONAT' ##NO_TEXT.
    CONSTANTS gc_bkpf_waers TYPE ftpost-fnam VALUE 'BKPF-WAERS' ##NO_TEXT.
    CONSTANTS gc_bkpf_xblnr TYPE ftpost-fnam VALUE 'BKPF-BKTXT' ##NO_TEXT.
    CONSTANTS gc_bkpf_bktxt TYPE ftpost-fnam VALUE 'BKPF-BKTXT' ##NO_TEXT.
    CONSTANTS gc_fd TYPE ftpost-fval VALUE 'FD' ##NO_TEXT.
    CONSTANTS gc_brl TYPE ftpost-fval VALUE 'BRL' ##NO_TEXT.
    CONSTANTS gc_fidc TYPE c LENGTH 4 VALUE 'FIDC' ##NO_TEXT.
    CONSTANTS gc_bseg_bupla TYPE ftpost-fnam VALUE 'BSEG-BUPLA' ##NO_TEXT.
    CONSTANTS gc_rf05a_newbs TYPE ftpost-fnam VALUE 'RF05A-NEWBS' ##NO_TEXT.
    CONSTANTS gc_rf05a_newum TYPE ftpost-fnam VALUE 'RF05A-NEWUM' ##NO_TEXT.
    CONSTANTS gc_rf05a_newko TYPE ftpost-fnam VALUE 'RF05A-NEWKO' ##NO_TEXT.
    CONSTANTS gc_bseg_wrbtr TYPE ftpost-fnam VALUE 'BSEG-WRBTR' ##NO_TEXT.
    CONSTANTS gc_bseg_zlsch TYPE ftpost-fnam VALUE 'BSEG-ZLSCH' ##NO_TEXT.
    CONSTANTS gc_bseg_zfbdt TYPE ftpost-fnam VALUE 'BSEG-ZFBDT' ##NO_TEXT.
    CONSTANTS gc_bseg_sgtxt TYPE ftpost-fnam VALUE 'BSEG-SGTXT' ##NO_TEXT.
    CONSTANTS gc_text_conc TYPE ftpost-fnam VALUE 'Conciliacao cheque recebido' ##NO_TEXT.
    CONSTANTS gc_bseg_valut TYPE ftpost-fnam VALUE 'BSEG-VALUT' ##NO_TEXT.
    CONSTANTS gc_40 TYPE ftpost-fval VALUE '40' ##NO_TEXT.
    CONSTANTS gc_receb_fidc TYPE c LENGTH 16 VALUE 'RECEBIMENTO FIDC' ##NO_TEXT.
    CONSTANTS gc_taxa_fidc TYPE c LENGTH 9 VALUE 'TAXA FIDC' ##NO_TEXT.
    CONSTANTS gc_bdcimmed TYPE rfipi-bdcimmed VALUE 'C' ##NO_TEXT.
    CONSTANTS gc_msgtyp_e TYPE sy-msgty VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_msgtyp_s TYPE sy-msgty VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_msgid_success TYPE sy-msgid VALUE 'F5' ##NO_TEXT.
    CONSTANTS gc_msgno_success TYPE sy-msgno VALUE '312' ##NO_TEXT.
    CONSTANTS gc_status_comp TYPE ztfi_cust_cheque-status VALUE '07' ##NO_TEXT.

    DATA gs_fatura TYPE ztfi_custcheq_ft .
    DATA gt_faturas TYPE zctgfi_custcheq_ft.
    DATA gt_cheques TYPE zctgfi_custcheq_cp .
    DATA go_parametros TYPE REF TO zclca_tabela_parametros .
    DATA gv_tipodoc TYPE ftpost-fval.
    DATA gv_formapagtodoc TYPE ftpost-fval.
    DATA gv_desc TYPE ftpost-fval.
    DATA gv_contacreditocheque TYPE ftpost-fval.
    DATA gv_bancoempresa TYPE ftpost-fval.
    DATA gv_ctadinheiro TYPE ftpost-fval.
    DATA gv_formapagto TYPE ftpost-fval.
    DATA gv_count TYPE ftpost-count.
    DATA gt_msg TYPE bapiret2_tab.

    METHODS constructor
      IMPORTING
        !is_fatura  TYPE ztfi_custcheq_ft OPTIONAL
        !it_faturas TYPE zctgfi_custcheq_ft OPTIONAL
        !it_cheques TYPE zctgfi_custcheq_cp OPTIONAL.

    METHODS main
      EXPORTING
        !es_retorno_msg TYPE zsfi_retorno_comp_tit_orig
        !et_return      TYPE bapiret2_tab.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS set_data
      IMPORTING
        !is_fatura  TYPE ztfi_custcheq_ft OPTIONAL
        !it_faturas TYPE zctgfi_custcheq_ft OPTIONAL
        !it_cheques TYPE zctgfi_custcheq_cp OPTIONAL.


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

    METHODS msg.

ENDCLASS.



CLASS zclfi_compensa_cheques IMPLEMENTATION.


  METHOD constructor.

    set_data(
      is_fatura  = is_fatura
      it_faturas = it_faturas
      it_cheques = it_cheques
    ).

    go_parametros = zclca_tabela_parametros=>get_instance( ).
    get_parameters( ).                               "#EC CI_SEL_NESTED

  ENDMETHOD.


  METHOD set_data.

    gs_fatura = is_fatura.
    gt_faturas = it_faturas.
    gt_cheques = it_cheques.

  ENDMETHOD.


  METHOD get_parameters.

    TRY.
        go_parametros->m_get_single(
          EXPORTING
            iv_modulo = gc_modulo_fi
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2_tipodoc
          IMPORTING
            ev_param  = gv_tipodoc
        ).

        IF gv_tipodoc IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo_fi
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_chave2_tipodoc ).

        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    TRY.
        go_parametros->m_get_single(
          EXPORTING
            iv_modulo = gc_modulo_fi
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2_desc
          IMPORTING
            ev_param  = gv_desc
        ).

        IF gv_tipodoc IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo_fi
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_chave2_desc ).

        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    TRY.
        go_parametros->m_get_single(
          EXPORTING
            iv_modulo = gc_modulo_fi
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2_ctadinheiro
          IMPORTING
            ev_param  = gv_ctadinheiro
        ).

        IF gv_tipodoc IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo_fi
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_chave2_ctadinheiro ).

        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    TRY.
        go_parametros->m_get_single(
          EXPORTING
            iv_modulo = gc_modulo_fi
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2_forpagto
          IMPORTING
            ev_param  = gv_formapagto
        ).

        IF gv_tipodoc IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo_fi
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_chave2_ctadinheiro ).

        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.


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

      MESSAGE e002 INTO lv_message.
      msg(  ).

      ls_message-tipo_status = gc_msgtyp_e.
      ls_message-mensagem = lv_message.
      ls_message = cs_retorno_msg.

    ELSE.

      IF lv_msgty = gc_msgtyp_s AND lv_msgid = gc_msgid_success AND lv_msgno = gc_msgno_success.

        MESSAGE s005 WITH lv_msgv1
                          gs_fatura-gjahr
                          gs_fatura-bukrs
                          INTO lv_message.

        msg(  ).

        ls_message-tipo_status = lv_msgty.
        ls_message-mensagem = lv_message.
        ls_message-nro_compensacao = lv_msgv1.

        cs_retorno_msg = ls_message.

        "Atualização de status
        LOOP AT gt_cheques ASSIGNING FIELD-SYMBOL(<fs_cheques_update_sts>).
          UPDATE ztfi_cust_cheque
            SET status = gc_status_comp "07 - COMPENSADO
                doc_compensacao = ls_message-nro_compensacao
            WHERE bukrs = <fs_cheques_update_sts>-bukrs
              AND kunnr = <fs_cheques_update_sts>-kunnr
              AND ncheque = <fs_cheques_update_sts>-ncheque.

          IF sy-subrc IS INITIAL.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDLOOP.

      ELSE.
        MESSAGE e004 WITH lv_msgid lv_msgty lv_msgno INTO lv_message.
        msg(  ).

        ls_message-tipo_status = lv_msgty.
        ls_message-mensagem = lv_message.

        cs_retorno_msg = ls_message.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD call_end.
    DATA: lv_message TYPE zsfi_retorno_comp_cheques-mensagem,
          ls_message TYPE zsfi_retorno_comp_cheques.

    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXPORTING
        i_bdcimmed              = gc_bdcimmed
*       i_bdcstrtdt             = sy-datum
*       i_bdcstrttm             = sy-uzeit
      EXCEPTIONS
        session_not_processable = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE e003 INTO lv_message.

      ls_message-tipo_status = gc_msgtyp_e.
      ls_message-mensagem = lv_message.

      ls_message = cs_retorno_msg.
    ENDIF.

  ENDMETHOD.


  METHOD call_start.
    DATA: lv_message TYPE zsfi_retorno_comp_tit_orig-mensagem,
          ls_message TYPE zsfi_retorno_comp_tit_orig.

    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_function       = gc_function
        i_mode           = gc_mode
        i_user           = sy-uname
      EXCEPTIONS
*       client_incorrect = 1
        function_invalid = 2
*       GROUP_NAME_MISSING = 3
        mode_invalid     = 4
*       update_invalid   = 5
        user_invalid     = 6
        OTHERS           = 7.
    IF sy-subrc <> 0.
      MESSAGE e001 INTO lv_message.

      ls_message-tipo_status = gc_msgtyp_e.
      ls_message-mensagem = lv_message.

      ls_message = cs_retorno_msg.

    ENDIF.

  ENDMETHOD.


  METHOD get_data.

    DATA: ls_t_ftclear         TYPE ftclear,
          lt_t_ftclear         TYPE STANDARD TABLE OF ftclear,
          ls_t_ftpost          TYPE ftpost,
          lt_t_ftpost          TYPE STANDARD TABLE OF ftpost,
          lv_newko_cb          TYPE rf05a-newko,
          lv_newko_ret         TYPE rf05a-newko,
          lv_chave3            TYPE ze_param_chave3,
          lv_valor_cheques     TYPE bseg-wrbtr,
          lv_valor_faturas     TYPE bseg-wrbtr,
          lv_dinheiro_char(18) TYPE c,
          lv_vlr_desc_char(18) TYPE c.

    DATA(lr_param) = zclca_tabela_parametros=>get_instance( ).

    DATA(ls_fatura) = gs_fatura.
    DATA(lt_faturas) = gt_faturas.
    DATA(lt_cheques) = gt_cheques.

    IF lt_faturas IS NOT INITIAL.
      SORT lt_faturas BY bukrs.
      READ TABLE lt_faturas ASSIGNING FIELD-SYMBOL(<fs_faturas_bukrs>) INDEX 1.
      IF sy-subrc IS INITIAL.
        DATA(lv_bukrs) = <fs_faturas_bukrs>-bukrs.
      ENDIF.
    ENDIF.

*---------------------------------------------------------------------------------*
*             Montagem estrutura T_FTCLEAR -> CHEQUES SELECIONADOS
*---------------------------------------------------------------------------------*
    IF lt_cheques IS NOT INITIAL.

      CLEAR: lv_valor_cheques.
      LOOP AT lt_cheques ASSIGNING FIELD-SYMBOL(<fs_cheques>).
        CLEAR ls_t_ftclear.

        lv_valor_cheques = lv_valor_cheques + <fs_cheques>-valor +  <fs_cheques>-dinheiro.
        ls_t_ftclear-agkoa = gc_agkoa.
        ls_t_ftclear-agbuk = <fs_cheques>-bukrs.
        ls_t_ftclear-agkon = gc_vazio.
        ls_t_ftclear-xnops = abap_false.
        ls_t_ftclear-agums = gc_agums.
        ls_t_ftclear-selvon = |{ <fs_cheques>-doc }{ <fs_cheques>-gjahr }{ <fs_cheques>-buzei }|.
        ls_t_ftclear-selbis = |{ <fs_cheques>-doc }{ <fs_cheques>-gjahr }{ <fs_cheques>-buzei }|.
        ls_t_ftclear-selfd = gc_belnr.
        APPEND ls_t_ftclear TO lt_t_ftclear.
      ENDLOOP.
    ENDIF.

*---------------------------------------------------------------------------------*
*             Montagem estrutura T_FTCLEAR -> FATURAS SELECIONADAS
*---------------------------------------------------------------------------------*
    IF lt_faturas IS NOT INITIAL.

      CLEAR: lv_valor_faturas.
      LOOP AT lt_faturas ASSIGNING FIELD-SYMBOL(<fs_faturas>).
        CLEAR ls_t_ftclear.

        lv_valor_faturas = lv_valor_faturas + <fs_faturas>-wrbtr.
        ls_t_ftclear-agkoa = gc_agkoa.
        ls_t_ftclear-agbuk = <fs_faturas>-bukrs.
        ls_t_ftclear-agkon = gc_vazio.
        ls_t_ftclear-xnops = gc_xnops.
        ls_t_ftclear-selvon = |{ <fs_faturas>-doc }{ <fs_faturas>-gjahr }{ <fs_faturas>-buzei }|.
        ls_t_ftclear-selbis = |{ <fs_faturas>-doc }{ <fs_faturas>-gjahr }{ <fs_faturas>-buzei }|.
        ls_t_ftclear-selfd = gc_belnr.
        APPEND ls_t_ftclear TO lt_t_ftclear.
      ENDLOOP.
    ENDIF.

*---------------------------------------------------------------------------------*
*             Montagem estrutura T_FTPOST -> STYPE=K COUNT=001
*---------------------------------------------------------------------------------*
    CLEAR ls_t_ftpost.

    ls_t_ftpost-stype = gc_stype_k.
    ls_t_ftpost-count = gc_count_1.
    ls_t_ftpost-fnam = gc_bkpf_bldat.
    ls_t_ftpost-fval = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
    APPEND ls_t_ftpost TO lt_t_ftpost.
    CLEAR ls_t_ftpost.

    ls_t_ftpost-stype = gc_stype_k.
    ls_t_ftpost-count = gc_count_1.
    ls_t_ftpost-fnam = gc_bkpf_budat.
    ls_t_ftpost-fval = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
    APPEND ls_t_ftpost TO lt_t_ftpost.
    CLEAR ls_t_ftpost.

    ls_t_ftpost-stype = gc_stype_k.
    ls_t_ftpost-count = gc_count_1.
    ls_t_ftpost-fnam = gc_bkpf_blart.
    ls_t_ftpost-fval = gv_tipodoc.
    APPEND ls_t_ftpost TO lt_t_ftpost.
    CLEAR ls_t_ftpost.

    ls_t_ftpost-stype = gc_stype_k.
    ls_t_ftpost-count = gc_count_1.
    ls_t_ftpost-fnam = gc_bkpf_bukrs.
    ls_t_ftpost-fval = lv_bukrs.
    APPEND ls_t_ftpost TO lt_t_ftpost.
    CLEAR ls_t_ftpost.

    ls_t_ftpost-stype = gc_stype_k.
    ls_t_ftpost-count = gc_count_1.
    ls_t_ftpost-fnam = gc_bkpf_monat.
    ls_t_ftpost-fval = sy-datum+4(2).
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
    ls_t_ftpost-fval = gv_desc.
    APPEND ls_t_ftpost TO lt_t_ftpost.
    CLEAR ls_t_ftpost.


    IF lv_valor_cheques > lv_valor_faturas.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_rf05a_newbs.
      ls_t_ftpost-fval = '19'.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_bupla.
      ls_t_ftpost-fval = lv_bukrs(2) && '01'.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_rf05a_newum.
      ls_t_ftpost-fval = 'D'.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_rf05a_newko.
      ls_t_ftpost-fval = <fs_faturas_bukrs>-kunnr .
      APPEND ls_t_ftpost TO lt_t_ftpost.


      lv_dinheiro_char = lv_valor_cheques - lv_valor_faturas.
      REPLACE '.' WITH ',' INTO lv_dinheiro_char.
      SHIFT lv_dinheiro_char LEFT DELETING LEADING space.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_wrbtr.
      ls_t_ftpost-fval = lv_dinheiro_char.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_zlsch.
      ls_t_ftpost-fval = gv_formapagto.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_zfbdt.
      ls_t_ftpost-fval = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|..
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_sgtxt.
      ls_t_ftpost-fval = gc_text_conc.
      APPEND ls_t_ftpost TO lt_t_ftpost.

    ELSEIF lv_valor_faturas > lv_valor_cheques .

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bkpf_blart.
      ls_t_ftpost-fval = 'DR'.
      APPEND ls_t_ftpost TO lt_t_ftpost.


      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_rf05a_newbs.
      ls_t_ftpost-fval = '01'.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_bupla.
      ls_t_ftpost-fval = lv_bukrs(2) && '01'.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_rf05a_newko.
      ls_t_ftpost-fval = <fs_faturas_bukrs>-kunnr .
      APPEND ls_t_ftpost TO lt_t_ftpost.

      lv_dinheiro_char = lv_valor_faturas - lv_valor_cheques.
      REPLACE '.' WITH ',' INTO lv_dinheiro_char.
      SHIFT lv_dinheiro_char LEFT DELETING LEADING space.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_wrbtr.
      ls_t_ftpost-fval = lv_dinheiro_char.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_zlsch.
      ls_t_ftpost-fval = gv_formapagto.
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_zfbdt.
      ls_t_ftpost-fval = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|..
      APPEND ls_t_ftpost TO lt_t_ftpost.

      ls_t_ftpost-stype = gc_stype_k.
      ls_t_ftpost-count = gc_count_1.
      ls_t_ftpost-fnam = gc_bseg_sgtxt.
      ls_t_ftpost-fval = gc_text_conc.
      APPEND ls_t_ftpost TO lt_t_ftpost.


    ENDIF.



*---------------------------------------------------------------------------------*
*             Montagem estrutura T_FTPOST -> STYPE=P COUNT=002
*---------------------------------------------------------------------------------*
    gv_count = gc_count_2.
    LOOP AT lt_cheques ASSIGNING FIELD-SYMBOL(<fs_cheques_ftpost>) WHERE dinheiro IS NOT INITIAL.
      IF <fs_cheques_ftpost>-dinheiro IS NOT INITIAL.
        ls_t_ftpost-stype = gc_stype_p.
        ls_t_ftpost-count = gv_count.
        ls_t_ftpost-fnam = gc_rf05a_newbs.
        ls_t_ftpost-fval = gc_40.
        APPEND ls_t_ftpost TO lt_t_ftpost.
        CLEAR ls_t_ftpost.

        ls_t_ftpost-stype = gc_stype_p.
        ls_t_ftpost-count = gv_count.
        ls_t_ftpost-fnam = gc_rf05a_newko.
        ls_t_ftpost-fval = gv_ctadinheiro.
        APPEND ls_t_ftpost TO lt_t_ftpost.
        CLEAR ls_t_ftpost.

        lv_dinheiro_char = <fs_cheques_ftpost>-dinheiro.
        REPLACE '.' WITH ',' INTO lv_dinheiro_char.
        SHIFT lv_dinheiro_char LEFT DELETING LEADING space.
        ls_t_ftpost-stype = gc_stype_p.
        ls_t_ftpost-count = gv_count.
        ls_t_ftpost-fnam = gc_bseg_wrbtr.
        ls_t_ftpost-fval = lv_dinheiro_char.
        APPEND ls_t_ftpost TO lt_t_ftpost.
        CLEAR ls_t_ftpost.

        gv_count += 1.
      ENDIF.

    ENDLOOP.

    et_t_ftclear = lt_t_ftclear.
    et_t_ftpost = lt_t_ftpost.

  ENDMETHOD.


  METHOD main.

    DATA: lt_t_ftclear       TYPE STANDARD TABLE OF ftclear,
          lt_t_ftpost        TYPE STANDARD TABLE OF ftpost,
          lv_msg             TYPE char60,
          ls_retorno_msg_end TYPE zsfi_retorno_comp_cheques.

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
      MESSAGE s006 INTO lv_msg.
      msg(  ).
    ELSEIF es_retorno_msg-tipo_status = gc_msgtyp_e OR ls_retorno_msg_end IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE e007 INTO lv_msg.
      msg(  ).
    ENDIF.

    et_return = gt_msg.
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
ENDCLASS.
