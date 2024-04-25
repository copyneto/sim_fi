CLASS zclfi_lanc_f47 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:

      BEGIN OF ty_bkpf,
        bukrs TYPE bkpf-bukrs,
      END OF ty_bkpf,

      BEGIN OF ty_bseg,
        dmbtr TYPE bseg-dmbtr,
        waers TYPE t001-waers,
        hbkid TYPE bseg-hbkid,
        zlsch TYPE bseg-zlsch,
        zfbdt TYPE bseg-zfbdt,
      END OF ty_bseg,

      ty_t_bseg TYPE STANDARD TABLE OF ty_bseg.

    CONSTANTS:

      BEGIN OF gc_param_raiz_cnpj,
        modulo TYPE ztca_param_mod-modulo VALUE 'FI-AP'             ##NO_TEXT,
        chave1 TYPE ztca_param_par-chave1 VALUE 'BOMBEIO'           ##NO_TEXT,
        chave2 TYPE ztca_param_par-chave2 VALUE 'RAIZCNPJ'          ##NO_TEXT,
        chave3 TYPE ztca_param_par-chave3 VALUE 'FORNECEDORMATRIZ'  ##NO_TEXT,
      END OF gc_param_raiz_cnpj,

      BEGIN OF gc_solicitacao,
        obj_type_bkpff             TYPE bapiache09-obj_type   VALUE 'BKPFF'            ##NO_TEXT,
        header_txt_bombeio         TYPE bapiache09-header_txt VALUE 'SOL.BOMBEIO'      ##NO_TEXT,
        doc_type_ka                TYPE bapiache09-doc_type   VALUE 'KA'               ##NO_TEXT,  " Documento fornecedor
        ref_doc_no_bombeio         TYPE bapiache09-ref_doc_no VALUE 'BOMBEIO'          ##NO_TEXT,

        bus_area_01                TYPE char02                VALUE '01'               ##NO_TEXT,
        pay_meth_a                 TYPE dzlsch                VALUE 'A'                ##NO_TEXT,  " AP - TED
        pmt_block_a                TYPE dzlspr                VALUE 'A'                ##NO_TEXT,  " Bloqueado p/pgto.
        sp_gl_ind_d                TYPE umskz                 VALUE 'D'                ##NO_TEXT,  " Descontos
        alloc_nmbr_bombeio         TYPE char07                VALUE 'BOMBEIO'          ##NO_TEXT,
        bank_id_san01              TYPE hbkid                 VALUE 'SAN01'            ##NO_TEXT,  " Banco Santander
        item_text_sugestao_bombeio TYPE bktxt                 VALUE 'SUGESTAO-BOMBEIO' ##NO_TEXT,

        bstat_s                    TYPE accit-bstat           VALUE 'S'                ##NO_TEXT, " Partida-memo
        bschl_39                   TYPE accit-bschl           VALUE '39'               ##NO_TEXT,
        umskz_f                    TYPE accit-umskz           VALUE 'F'                ##NO_TEXT, " Solicitação de adiantamento
        zumsk_a                    TYPE accit-zumsk           VALUE 'A'                ##NO_TEXT, " Adiantamento
        shkzg_h                    TYPE accit-shkzg           VALUE 'H'                ##NO_TEXT, " Crédito

        currency_brl               TYPE bapiaccr09-currency   VALUE 'BRL'              ##NO_TEXT,
      END OF gc_solicitacao,

      BEGIN OF gc_extensao,
        gsber TYPE strng250 VALUE 'GSBER' ##NO_TEXT,
        bstat TYPE strng250 VALUE 'BSTAT' ##NO_TEXT,
        bschl TYPE strng250 VALUE 'BSCHL' ##NO_TEXT,
        umskz TYPE strng250 VALUE 'UMSKZ' ##NO_TEXT,
        zumsk TYPE strng250 VALUE 'ZUMSK' ##NO_TEXT,
        shkzg TYPE strng250 VALUE 'SHKZG' ##NO_TEXT,
        bupla TYPE strng250 VALUE 'BUPLA' ##NO_TEXT,
      END OF gc_extensao,

      BEGIN OF gc_log,
        log_id    TYPE balobj_d  VALUE 'ZFI_SOLADTBOMB' ##NO_TEXT,
        log_subid TYPE balsubobj VALUE 'LANC_F47' ##NO_TEXT,
      END OF gc_log.

    METHODS criar_documento
      IMPORTING is_bkpf   TYPE ty_bkpf
                it_bseg   TYPE ty_t_bseg
                iv_commit TYPE abap_boolean DEFAULT abap_true
                iv_ballog TYPE abap_boolean DEFAULT abap_false
      EXPORTING es_key    TYPE bkpf_key
                et_return TYPE bapiret2_t.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS save_bal_log
      IMPORTING it_return TYPE bapiret2_t
      EXPORTING et_return TYPE bapiret2_t.

ENDCLASS.


CLASS zclfi_lanc_f47 IMPLEMENTATION.

  METHOD criar_documento.

    DATA: lv_obj_type  TYPE awtyp,
          lv_obj_key   TYPE awkey,
          lv_obj_sys   TYPE awsys,
          lv_raiz_cnpj TYPE lfa1-stcd1,
          lv_message   TYPE bapiret2-message,
          lv_item      TYPE char04 VALUE '0001'.

    FREE: et_return, es_key.

* ---------------------------------------------------------------------------
* Recupera parâmetro - Raiz do CNPJ do Fornecedor
* ---------------------------------------------------------------------------
    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).

    TRY.
        lo_param->m_get_single( EXPORTING iv_modulo = gc_param_raiz_cnpj-modulo
                                          iv_chave1 = gc_param_raiz_cnpj-chave1
                                          iv_chave2 = gc_param_raiz_cnpj-chave2
                                          iv_chave3 = gc_param_raiz_cnpj-chave3
                                IMPORTING ev_param  = lv_raiz_cnpj ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_param_cx).

        lv_message = lo_param_cx->get_longtext( ).

        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = zclfi_cockpit_bombeio_event=>gc_message-id
                                              number     = zclfi_cockpit_bombeio_event=>gc_message-no_000
                                              message_v1 = lv_message+0(50)
                                              message_v2 = lv_message+50(50)
                                              message_v3 = lv_message+100(50)
                                              message_v4 = lv_message+150(50)
                                              message    = lv_message ) ).
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------------
* Recupera código do fornecedor da Matriz (Raiz CNPJ)
* ---------------------------------------------------------------------------
    SELECT SINGLE lifnr, stcd1
        FROM fndei_lfa1_filter
        WHERE stcd1 EQ @lv_raiz_cnpj
        INTO @DATA(ls_lfa1).

    IF sy-subrc NE 0.
      " Fornecedor não encontrado para raiz &1.
      et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                            id         = zclfi_cockpit_bombeio_event=>gc_message-id
                                            number     = zclfi_cockpit_bombeio_event=>gc_message-no_019
                                            message_v1 = lv_raiz_cnpj ) ).
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Prepara os dados para criar a solicitação de adiantamento
* ---------------------------------------------------------------------------
    DATA(ls_documentheader)     = VALUE bapiache09( obj_type      = gc_solicitacao-obj_type_bkpff
                                                    username      = sy-uname
                                                    header_txt    = gc_solicitacao-header_txt_bombeio
                                                    comp_code     = is_bkpf-bukrs
                                                    doc_date      = sy-datum
                                                    pstng_date    = sy-datum
                                                    fisc_year     = sy-datum+0(4)
                                                    fis_period    = sy-datum+4(2)
                                                    doc_type      = gc_solicitacao-doc_type_ka
                                                    ref_doc_no    = gc_solicitacao-ref_doc_no_bombeio ).

    DATA(lt_accountpayable)   = VALUE bapiacap09_tab( FOR ls_bseg_ IN it_bseg INDEX INTO lv_index_ (
                                                      itemno_acc = lv_index_
                                                      vendor_no  = ls_lfa1-lifnr
                                                      comp_code  = is_bkpf-bukrs
                                                      bus_area   = |{ is_bkpf-bukrs+0(2) }{ gc_solicitacao-bus_area_01 }|
                                                      bline_date = COND #( WHEN ls_bseg_-zfbdt IS NOT INITIAL
                                                                           THEN ls_bseg_-zfbdt
                                                                           ELSE sy-datum )
                                                      pymt_meth  = COND #( WHEN ls_bseg_-zlsch IS NOT INITIAL
                                                                           THEN ls_bseg_-zlsch
                                                                           ELSE gc_solicitacao-pay_meth_a )
                                                      pmnt_block = gc_solicitacao-pmt_block_a
                                                      sp_gl_ind  = gc_solicitacao-sp_gl_ind_d
                                                      alloc_nmbr = |{ gc_solicitacao-alloc_nmbr_bombeio } { sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|
                                                      bank_id    = COND #( WHEN ls_bseg_-hbkid IS NOT INITIAL
                                                                           THEN ls_bseg_-hbkid
                                                                           ELSE gc_solicitacao-bank_id_san01 )
                                                      item_text  = gc_solicitacao-item_text_sugestao_bombeio
                                                      ) ).

    DATA(lt_currencyamount)   = VALUE bapiaccr09_tab( FOR ls_bseg_ IN it_bseg INDEX INTO lv_index_ (
                                                      itemno_acc = lv_index_
                                                      currency   = COND #( WHEN ls_bseg_-waers IS NOT INITIAL
                                                                           THEN ls_bseg_-waers
                                                                           ELSE gc_solicitacao-currency_brl )
                                                      amt_doccur = ls_bseg_-dmbtr * -1
                                                      ) ).

    DATA(lt_extension1)       = VALUE bapiacextc_tab( FOR ls_bseg_ IN it_bseg INDEX INTO lv_index_

                                                      ( field1 = |{ lv_item }{ CONV posnr_acc( lv_index_ ) }|
                                                        field2 = gc_extensao-gsber
                                                        field3 = |{ is_bkpf-bukrs+0(2) }{ gc_solicitacao-bus_area_01 }| )

                                                      ( field1 = |{ lv_item }{ CONV posnr_acc( lv_index_ ) }|
                                                        field2 = gc_extensao-bupla
                                                        field3 = |{ is_bkpf-bukrs+0(2) }{ gc_solicitacao-bus_area_01 }| )

                                                      ( field1 = |{ lv_item }{ CONV posnr_acc( lv_index_ ) }|
                                                        field2 = gc_extensao-bstat
                                                        field3 = gc_solicitacao-bstat_s )

                                                      ( field1 = |{ lv_item }{ CONV posnr_acc( lv_index_ ) }|
                                                        field2 = gc_extensao-bschl
                                                        field3 = gc_solicitacao-bschl_39 )

                                                      ( field1 = |{ lv_item }{ CONV posnr_acc( lv_index_ ) }|
                                                        field2 = gc_extensao-umskz
                                                        field3 = gc_solicitacao-umskz_f )

                                                      ( field1 = |{ lv_item }{ CONV posnr_acc( lv_index_ ) }|
                                                        field2 = gc_extensao-zumsk
                                                        field3 = gc_solicitacao-zumsk_a )

                                                      ( field1 = |{ lv_item }{ CONV posnr_acc( lv_index_ ) }|
                                                        field2 = gc_extensao-shkzg
                                                        field3 = gc_solicitacao-shkzg_h )
                                                      ).

    " Preenchemos este campo para pular a validação da EXIT.
    sy-tcode = zclfi_cockpit_bombeio_event=>gc_transaction-bombeio.

* ---------------------------------------------------------------------------
* Chama BAPI de crição da solicitação de adiantamento
* ---------------------------------------------------------------------------
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_documentheader
      IMPORTING
        obj_type       = lv_obj_type
        obj_key        = lv_obj_key
        obj_sys        = lv_obj_sys
      TABLES
        accountpayable = lt_accountpayable
        currencyamount = lt_currencyamount
        extension1     = lt_extension1
        return         = et_return.


    IF line_exists( et_return[ type = if_xo_const_message=>error ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      " Armazena o log na transação SLG0
      IF iv_ballog EQ abap_true.

        me->save_bal_log( EXPORTING it_return = et_return
                          IMPORTING et_return = DATA(lt_return_ballog) ).

      ENDIF.

    ELSE.

      IF iv_commit EQ abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.

    ENDIF.

* ---------------------------------------------------------------------------
* Devolve a chave criada
* ---------------------------------------------------------------------------
    es_key = VALUE #( bukrs = lv_obj_key+10(4)
                      belnr = lv_obj_key+0(10)
                      gjahr = lv_obj_key+14(4) ).

  ENDMETHOD.


  METHOD save_bal_log.

    DATA: lv_log_handle  TYPE balloghndl .

    FREE: et_return.

    DATA(ls_log) = VALUE bal_s_log( object    = gc_log-log_id
                                    subobject = gc_log-log_subid
                                    aluser    = sy-uname
                                    alprog    = sy-repid ).

* ---------------------------------------------------------------------------
* Cria Log de aplicação
* ---------------------------------------------------------------------------
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc NE 0.
      et_return = VALUE #( BASE et_return ( type       = sy-msgty
                                            id         = sy-msgid
                                            number     = sy-msgno
                                            message_v1 = sy-msgv1
                                            message_v2 = sy-msgv2
                                            message_v3 = sy-msgv3
                                            message_v4 = sy-msgv4 ) ).
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Armazena log
* ---------------------------------------------------------------------------
    LOOP AT it_return REFERENCE INTO DATA(ls_return).

      DATA(ls_msg) = VALUE bal_s_msg( msgty = ls_return->type
                                      msgid = ls_return->id
                                      msgno = ls_return->number
                                      msgv1 = CONV syst_msgv( ls_return->message_v1 )
                                      msgv2 = CONV syst_msgv( ls_return->message_v1 )
                                      msgv3 = CONV syst_msgv( ls_return->message_v1 )
                                      msgv4 = CONV syst_msgv( ls_return->message_v1 ) ).

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = lv_log_handle
          i_s_msg          = ls_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

      IF sy-subrc NE 0.
        et_return = VALUE #( BASE et_return ( type       = sy-msgty
                                              id         = sy-msgid
                                              number     = sy-msgno
                                              message_v1 = sy-msgv1
                                              message_v2 = sy-msgv2
                                              message_v3 = sy-msgv3
                                              message_v4 = sy-msgv4 ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
