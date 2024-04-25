CLASS zclfi_custodia_cheque DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS upload_file
      IMPORTING
        !iv_file     TYPE xstring
        !iv_filename TYPE string
      EXPORTING
        !et_return   TYPE bapiret2_t .

    METHODS contab_cheque
      IMPORTING
        is_key     TYPE ztfi_cust_cheque
      EXPORTING
        !et_return TYPE bapiret2_t.

    METHODS contab_cheque_devo
      IMPORTING
        is_key     TYPE ztfi_cust_cheque
      EXPORTING
        !et_return TYPE bapiret2_t.

    METHODS estorno_cheque
      IMPORTING
        is_key     TYPE ztfi_cust_cheque
      EXPORTING
        !et_return TYPE bapiret2_t.

    METHODS get_cheque_comp
      IMPORTING
        is_key     TYPE zi_custcheque_comp_h
      EXPORTING
        !et_return TYPE bapiret2_t .


  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_fi_ar TYPE ze_param_modulo VALUE 'FI-AR' ##NO_TEXT.
    CONSTANTS gc_cheque TYPE ze_param_chave1 VALUE 'CUSTODIA_DE_CHEQUES' ##NO_TEXT.
    CONSTANTS gc_tipodoc TYPE ze_param_chave2 VALUE 'ZCHEQUE_CLIENTE_TIPODOC' ##NO_TEXT.
    CONSTANTS gc_formapagto TYPE ze_param_chave2 VALUE 'ZCHEQUE_CLIENTE_FORMAPAGTODOC' ##NO_TEXT.
    CONSTANTS gc_clientedesc TYPE ze_param_chave2 VALUE 'ZCHEQUE_CLIENTE_DESC' ##NO_TEXT.
    CONSTANTS gc_conta TYPE ze_param_chave2 VALUE 'ZCHEQUE_CLIENTE_CONTACREDITOCHEQUE' ##NO_TEXT.
    CONSTANTS gc_conta_dev TYPE ze_param_chave2 VALUE 'ZCHEQUE_CLIENTE_CONTADEVCHEQUE' ##NO_TEXT.
    CONSTANTS gc_titulo_aceito TYPE c VALUE 'N' ##NO_TEXT.
    CONSTANTS gc_tipo_arquivo TYPE c VALUE '1' ##NO_TEXT.
    CONSTANTS gc_tipo_movimento TYPE c VALUE 'I' ##NO_TEXT.
    CONSTANTS gc_erro TYPE c VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_sucesso TYPE c VALUE 'S' ##NO_TEXT.

    DATA go_parametros TYPE REF TO zclca_tabela_parametros .
    DATA gv_tipodoc TYPE blart.
    DATA gv_formapagto TYPE schzw_bseg.
    DATA gv_clientedesc TYPE string.
    DATA gv_conta TYPE hkont.
    DATA gv_conta_dev TYPE hkont.
    DATA gv_dummy TYPE string.

    DATA gt_msg TYPE bapiret2_tab.

    METHODS get_param.
    METHODS valida_param.
    METHODS msg.
    METHODS lanc_cont
      IMPORTING
        is_key TYPE ztfi_cust_cheque.
    METHODS lanc_cont_devo
      IMPORTING
        is_key TYPE ztfi_cust_cheque.

ENDCLASS.



CLASS zclfi_custodia_cheque IMPLEMENTATION.


  METHOD upload_file.
    DATA: lt_file       TYPE TABLE OF zsfi_cust_cheque_file.
    "DATA: lt_file       TYPE TABLE OF zctgsd_preco_arquivo.
    DATA: lt_cust_cheque  TYPE TABLE OF ztfi_cust_cheque.
    DATA: ls_cust_cheque  TYPE zsfi_cust_cheque_file.
* ---------------------------------------------------------------------------
* Converte arquivo excel para tabela
* ---------------------------------------------------------------------------
    DATA(lo_excel) = NEW zclca_excel( iv_filename = iv_filename
                                      iv_file     = iv_file ).
    "lo_excel->gv_quant = abap_true.
    lo_excel->get_sheet( IMPORTING et_return = DATA(lt_return)              " Ignorar validação durante carga
                         CHANGING  ct_table  = lt_file[] ).

    IF line_exists( lt_return[ type = 'E' ] ).           "#EC CI_STDSEQ
      et_return = lt_return.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Prepara dados para salvar
* ---------------------------------------------------------------------------
    DELETE lt_file WHERE bukrs   IS INITIAL
                      OR kunnr   IS INITIAL
                      OR ncheque IS INITIAL.

    lt_cust_cheque = VALUE #( FOR ls_file IN lt_file ( client = sy-mandt
                                                       ncontrato = ls_file-ncontrato
                                                       ncontratojuridico = ls_file-ncontratojuridico
                                                       ncheque = ls_file-ncheque
                                                       kunnr = ls_file-kunnr
                                                       budat = ls_file-budat
                                                       bukrs = ls_file-bukrs
                                                       bupla = ls_file-bupla
                                                       zterm    = ls_file-zterm
                                                       nchamado = ls_file-nchamado
                                                       valor    = ls_file-valor
                                                       zcmc7 = ls_file-zcmc7
                                                       hktid = ls_file-hktid
                                                       bankn = ls_file-bankn
                                                       bkont = ls_file-bkont
                                                       zcamara = ls_file-zcamara
                                                       moeda    = 'BRL'
                                                       status   = '01' ) ).

    IF lt_cust_cheque IS NOT INITIAL.

      MODIFY ztfi_cust_cheque FROM TABLE lt_cust_cheque.

      IF sy-subrc IS INITIAL.
        et_return[] = VALUE #( BASE et_return ( type = 'S' id = 'ZCA_EXCEL' number = '000' ) ).
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD contab_cheque.

    CLEAR gt_msg.

    valida_param( ).

    IF NOT line_exists( gt_msg[ type = gc_erro ] ).

      lanc_cont( is_key ).

    ENDIF.

    et_return = gt_msg.

  ENDMETHOD.

  METHOD contab_cheque_devo.

    CLEAR gt_msg.

    valida_param( ).

    IF NOT line_exists( gt_msg[ type = gc_erro ] ).

      lanc_cont_devo( is_key ).

    ENDIF.

    et_return = gt_msg.

  ENDMETHOD.

  METHOD estorno_cheque.
    CONSTANTS: lc_stgrd TYPE stgrd VALUE '01'.

    DATA: ls_cust_cheque TYPE ztfi_cust_cheque.

    DATA: lv_budat TYPE budat,
          lv_monat TYPE monat.

    CLEAR gt_msg.

    valida_param( ).

    IF NOT line_exists( gt_msg[ type = gc_erro ] ).

      CALL FUNCTION 'CALL_FB08'
        EXPORTING
          i_bukrs      = is_key-bukrs
          i_belnr      = is_key-doc
          i_gjahr      = is_key-gjahr
          i_stgrd      = lc_stgrd
        IMPORTING
          e_budat      = lv_budat
          e_monat      = lv_monat
          "e_xsofo      = lv_xsofo
        EXCEPTIONS
          not_possible = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO gv_dummy.
        msg( ).

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        msg( ).
        ls_cust_cheque = is_key.
        ls_cust_cheque-doc_estorno = sy-msgv1.
        MODIFY ztfi_cust_cheque FROM ls_cust_cheque.

        DELETE FROM ztfi_custcheq_cp WHERE bukrs = is_key-bukrs
                                       AND kunnr = is_key-kunnr
                                       AND ncheque = is_key-ncheque.
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

      ENDIF.
    ENDIF.

    et_return = gt_msg.

  ENDMETHOD.

  METHOD constructor.

    get_param( ).

  ENDMETHOD.


  METHOD get_param.

    go_parametros = zclca_tabela_parametros=>get_instance( ).

    TRY.

        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi_ar
                                               iv_chave1 = gc_cheque
                                               iv_chave2 = gc_tipodoc
                                     IMPORTING ev_param  = gv_tipodoc ).

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros

    ENDTRY.
    TRY.

        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi_ar
                                              iv_chave1 = gc_cheque
                                              iv_chave2 = gc_clientedesc
                                    IMPORTING ev_param  = gv_clientedesc ).

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros

    ENDTRY.

    TRY.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi_ar
                                               iv_chave1 = gc_cheque
                                               iv_chave2 = gc_conta
                                     IMPORTING ev_param  = gv_conta ).

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros


    ENDTRY.

    TRY.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi_ar
                                               iv_chave1 = gc_cheque
                                               iv_chave2 = gc_conta_dev
                                     IMPORTING ev_param  = gv_conta_dev ).

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros


    ENDTRY.

    TRY.

        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi_ar
                                              iv_chave1 = gc_cheque
                                              iv_chave2 = gc_formapagto
                                    IMPORTING ev_param  = gv_formapagto ).


      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros

    ENDTRY.

  ENDMETHOD.


  METHOD valida_param.

    IF gv_tipodoc IS INITIAL.
      MESSAGE e001(zfi_cust_cheque) WITH gc_tipodoc INTO gv_dummy.
      msg( ).
    ELSEIF gv_formapagto IS INITIAL.
      MESSAGE e001(zfi_cust_cheque) WITH gc_formapagto INTO gv_dummy.
      msg( ).
    ELSEIF gv_clientedesc IS INITIAL.
      MESSAGE e001(zfi_cust_cheque) WITH gc_clientedesc INTO gv_dummy.
      msg( ).
    ENDIF.

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


  METHOD lanc_cont.

    CONSTANTS: lc_bkpff      TYPE awtyp VALUE 'BKPFF',
               lc_rfst       TYPE glvor VALUE 'RFST',
               lc_xblnr      TYPE xblnr VALUE 'Cheque Recebido' ##NO_TEXT,
               lc_fd         TYPE blart VALUE 'FD',
               lc_9999       TYPE bukrs VALUE '9999',
               lc_9901       TYPE bupla VALUE '9901',
               lc_brl        TYPE waers VALUE 'BRL',
               lc_d          TYPE char1 VALUE 'D',
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


    "Header
    ls_documentheader-obj_type         = lc_bkpff.
    ls_documentheader-username         = sy-uname.
    ls_documentheader-header_txt       = |{ gv_clientedesc }| && |{ is_key-ncheque }| .
    ls_documentheader-ref_doc_no       = lc_xblnr.
    ls_documentheader-comp_code        = is_key-bukrs.
    ls_documentheader-doc_date         = sy-datum.
    ls_documentheader-pstng_date       = sy-datum.
    ls_documentheader-fisc_year        = sy-datum+0(4).
    ls_documentheader-fis_period       = sy-datum+4(2).
    ls_documentheader-doc_type         = gv_tipodoc.



    ls_accountreceivable-itemno_acc    = 1.
    ls_accountreceivable-customer      = is_key-kunnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accountreceivable-customer
      IMPORTING
        output = ls_accountreceivable-customer.


    ls_accountreceivable-item_text  = ls_documentheader-header_txt.
    ls_accountreceivable-comp_code  = is_key-bukrs.
    ls_accountreceivable-alloc_nmbr = is_key-ncheque.
    ls_accountreceivable-pymt_meth  = gv_formapagto.
    ls_accountreceivable-bus_area   = is_key-bupla.
    ls_accountreceivable-bline_date = sy-datum.
    ls_accountreceivable-businessplace = is_key-bupla.
    ls_accountreceivable-sp_gl_ind = lc_d.

    APPEND ls_accountreceivable TO lt_accountreceivable.


    ls_currencyamount-itemno_acc = 1.
    ls_currencyamount-currency   = lc_brl.
    ls_currencyamount-amt_doccur = is_key-valor * -1.

    APPEND ls_currencyamount TO lt_currencyamount.
    CLEAR ls_currencyamount.



    CLEAR:ls_accountgl.
    ls_accountgl-itemno_acc = 2.

    ls_accountgl-gl_account = gv_conta .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accountgl-gl_account
      IMPORTING
        output = ls_accountgl-gl_account.


    ls_accountgl-bus_area = is_key-bupla.
    ls_accountgl-alloc_nmbr = ls_accountreceivable-alloc_nmbr.
    ls_accountgl-item_text = ls_accountreceivable-item_text.

    APPEND ls_accountgl TO lt_accountgl.

    ls_currencyamount-itemno_acc = 2.
    ls_currencyamount-currency   = lc_brl.
    ls_currencyamount-amt_doccur = is_key-valor .

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

      SORT: lt_return BY type id number.
      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type   = 'S'
                                                 id     = 'RW'
                                                 number = '605' BINARY SEARCH.

      "Documento contabilizado: &1 &2 &3
      MESSAGE s006(zfi_cust_cheque) WITH <fs_return>-message_v2(10) sy-datum(4) is_key-bukrs  INTO gv_dummy.
      msg( ).

      UPDATE ztfi_cust_cheque SET doc = <fs_return>-message_v2(10)
                                  gjahr = sy-datum(4)
                                  buzei = '01'
                               WHERE bukrs = is_key-bukrs
                                AND kunnr = is_key-kunnr
                                AND ncheque = is_key-ncheque.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.

    ELSE.

      DELETE lt_return WHERE type <> gc_erro.
      APPEND LINES OF lt_return TO gt_msg.

      "Erro ao tentar contabilizar Cheque: &1
      MESSAGE e007(zfi_cust_cheque) WITH is_key-ncheque  INTO gv_dummy.
      msg( ).

    ENDIF.

  ENDMETHOD.

  METHOD lanc_cont_devo.

    CONSTANTS: lc_bkpff      TYPE awtyp VALUE 'BKPFF',
               lc_rfst       TYPE glvor VALUE 'RFST',
               lc_xblnr      TYPE xblnr VALUE 'Cheque Recebido' ##NO_TEXT,
               lc_fd         TYPE blart VALUE 'FD',
               lc_9999       TYPE bukrs VALUE '9999',
               lc_9901       TYPE bupla VALUE '9901',
               lc_brl        TYPE waers VALUE 'BRL',
               lc_d          TYPE char1 VALUE 'D',
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


    "Header
    ls_documentheader-obj_type         = lc_bkpff.
    ls_documentheader-username         = sy-uname.
    ls_documentheader-header_txt       = |{ gv_clientedesc }| && |{ is_key-ncheque }| .
    ls_documentheader-ref_doc_no       = lc_xblnr.
    ls_documentheader-comp_code        = is_key-bukrs.
    ls_documentheader-doc_date         = sy-datum.
    ls_documentheader-pstng_date       = sy-datum.
    ls_documentheader-fisc_year        = sy-datum+0(4).
    ls_documentheader-fis_period       = sy-datum+4(2).
    ls_documentheader-doc_type         = gv_tipodoc.



    CLEAR:ls_accountgl.
    ls_accountgl-itemno_acc = 1.

    ls_accountgl-gl_account = gv_conta .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accountgl-gl_account
      IMPORTING
        output = ls_accountgl-gl_account.


    ls_accountgl-bus_area = is_key-bupla.
    ls_accountgl-alloc_nmbr = ls_accountreceivable-alloc_nmbr.
    ls_accountgl-item_text = ls_accountreceivable-item_text.

    APPEND ls_accountgl TO lt_accountgl.

    ls_currencyamount-itemno_acc = 2.
    ls_currencyamount-currency   = lc_brl.
    ls_currencyamount-amt_doccur = is_key-valor .

    APPEND ls_currencyamount TO lt_currencyamount.
    CLEAR ls_currencyamount.



    CLEAR:ls_accountgl.
    ls_accountgl-itemno_acc = 2.

    ls_accountgl-gl_account = gv_conta_dev .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accountgl-gl_account
      IMPORTING
        output = ls_accountgl-gl_account.

    ls_accountgl-bus_area = is_key-bupla.
    ls_accountgl-alloc_nmbr = ls_accountreceivable-alloc_nmbr.
    ls_accountgl-item_text = ls_accountreceivable-item_text.

    APPEND ls_accountgl TO lt_accountgl.

    ls_currencyamount-itemno_acc = 1.
    ls_currencyamount-currency   = lc_brl.
    ls_currencyamount-amt_doccur = is_key-valor * -1.

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

      SORT: lt_return BY type id number.
      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type   = 'S'
                                                 id     = 'RW'
                                                 number = '605' BINARY SEARCH.

      "Documento contabilizado: &1 &2 &3
      MESSAGE s006(zfi_cust_cheque) WITH <fs_return>-message_v2(10) sy-datum(4) is_key-bukrs  INTO gv_dummy.
      msg( ).

      UPDATE ztfi_cust_cheque SET doc_devolucao = <fs_return>-message_v2(10)
                                  status = '06' "DEVOLVIDO
                               WHERE bukrs = is_key-bukrs
                                AND kunnr = is_key-kunnr
                                AND ncheque = is_key-ncheque.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.

    ELSE.

      DELETE lt_return WHERE type <> gc_erro.
      APPEND LINES OF lt_return TO gt_msg.

      "Erro ao tentar contabilizar Cheque: &1
      MESSAGE e007(zfi_cust_cheque) WITH is_key-ncheque  INTO gv_dummy.
      msg( ).

    ENDIF.

  ENDMETHOD.

  METHOD get_cheque_comp.

    DATA: lt_custcheq_cp  TYPE TABLE OF ztfi_custcheq_cp,
          ls_custcheq_cp  TYPE ztfi_custcheq_cp,
          lt_custcheq_fat TYPE TABLE OF ztfi_custcheq_ft,
          ls_custcheq_fat TYPE ztfi_custcheq_ft.


    CLEAR: gt_msg.

    SELECT         *
            FROM zi_fi_cheques_p_comp
            INTO TABLE @DATA(lt_cheques)
            WHERE bukrs = @is_key-bukrs
               AND kunnr = @is_key-kunnr.     "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc = 0.


      DELETE FROM ztfi_custcheq_cp WHERE bukrs = is_key-bukrs
                                    AND kunnr = is_key-kunnr.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.

      lt_custcheq_cp = VALUE #( FOR ls_cheque IN lt_cheques
                                ( CORRESPONDING #( BASE ( ls_custcheq_cp ) ls_cheque  ) ) ).

      MODIFY ztfi_custcheq_cp FROM TABLE lt_custcheq_cp.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
        "Dados cheques atualizado
        MESSAGE s003(zfi_cust_cheque) INTO gv_dummy.
        msg( ).
      ELSE.
        "Dados cheques não localizado
        MESSAGE e004(zfi_cust_cheque) INTO gv_dummy.
        msg( ).
      ENDIF.

    ENDIF.

    SELECT *
        FROM zi_fi_fat_raizcnpj_group
        INTO TABLE @DATA(lt_faturas)
            WHERE bukrs = @is_key-bukrs
               AND kunnr = @is_key-kunnr.

    IF sy-subrc = 0.

      LOOP AT lt_faturas ASSIGNING FIELD-SYMBOL(<fs_fat>).

        APPEND INITIAL LINE TO lt_custcheq_fat ASSIGNING FIELD-SYMBOL(<fs_fat_i>).
        <fs_fat_i>-bukrs = <fs_fat>-bukrs.
        <fs_fat_i>-kunnr = is_key-kunnr.
        <fs_fat_i>-raizcnpj = is_key-raizcnpj.
        <fs_fat_i>-doc = <fs_fat>-belnr.
        <fs_fat_i>-gjahr = <fs_fat>-gjahr.
        <fs_fat_i>-buzei = <fs_fat>-buzei.
        <fs_fat_i>-wrbtr = <fs_fat>-wrbtr.
        <fs_fat_i>-moeda = <fs_fat>-waers.
        <fs_fat_i>-cliente_fat = <fs_fat>-kunnr.

      ENDLOOP.

      IF lt_custcheq_fat IS NOT INITIAL.

        DELETE FROM ztfi_custcheq_ft WHERE bukrs = is_key-bukrs
                                    AND kunnr = is_key-kunnr.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        MODIFY ztfi_custcheq_ft FROM TABLE lt_custcheq_fat.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          "Dados faturas atualizado
          MESSAGE s008(zfi_cust_cheque) INTO gv_dummy.
          msg( ).
        ELSE.
          "Dados faturas não atualizado
          MESSAGE e009(zfi_cust_cheque) INTO gv_dummy.
          msg( ).
        ENDIF.

      ENDIF.

    ENDIF.

    et_return = gt_msg.

  ENDMETHOD.

ENDCLASS.
