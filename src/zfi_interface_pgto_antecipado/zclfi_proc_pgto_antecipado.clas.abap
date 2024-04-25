CLASS zclfi_proc_pgto_antecipado DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_xo_const_message .

    TYPES:
      BEGIN OF ty_payload_devedor,
        cnpj TYPE string,
        nome TYPE string,
      END OF ty_payload_devedor,

      BEGIN OF ty_payload_valor,
        original             TYPE string,
        modalidade_alteracao TYPE string,
      END OF ty_payload_valor,

      BEGIN OF ty_payload_calendar,
        expiracao TYPE i,
      END OF ty_payload_calendar,
      "! Types para payload da interface inbound
      BEGIN OF ty_payload_oubound,
        empresa             TYPE string,
        devedor             TYPE ty_payload_devedor,
        valor               TYPE ty_payload_valor,
        calendario          TYPE ty_payload_calendar,
        "modalidade_alteracao       TYPE string,
        chave               TYPE string,
        solicitacao_pagador TYPE string,
        "expiracao                  TYPE string,
        txid                TYPE string,
        pix_copia_e_cola    TYPE string,
      END OF ty_payload_oubound,
      "! Type para tabela ztfi_adto_ov
      ty_t_ztfi_adto_ov TYPE TABLE OF ztfi_adto_ov WITH DEFAULT KEY.

    CONSTANTS:
      "! Constantes para integração CPI
      BEGIN OF gc_cpi,
        envio_pagamento   TYPE ze_processo   VALUE 'ZFI_SOLIC_PGTO_ANTECIPADO' ##NO_TEXT,
        retorno_pagamento TYPE ze_processo   VALUE 'ZFI_SOLIC_PGTO_ANTECIPADO_RET' ##NO_TEXT,
        method_post       TYPE ze_method_api VALUE 'POST' ##NO_TEXT,
      END OF gc_cpi .
    CONSTANTS:
      "! Status das solicitações
      BEGIN OF gc_status,
        erro_adiantamento   TYPE c VALUE '1',
        erro_envio_a27      TYPE c VALUE '2',
        enviado_a27         TYPE c VALUE '3',
        erro_compensacao    TYPE c VALUE '4',
        erro_desbl_ordem    TYPE c VALUE '5',
        ordem_desbloqueada  TYPE c VALUE '6',
        erro_compens_fatura TYPE c VALUE '7',
        processado          TYPE c VALUE '8',
        pix_expirado        TYPE c VALUE '9',
      END OF gc_status .
    "! Constante objeto de log
    CONSTANTS gc_log_object TYPE balobj_d VALUE 'ZFI_PGTO_ANTECIPADO' ##NO_TEXT.
    "! Constante subobjeto de log
    CONSTANTS gc_log_subobj TYPE balsubobj VALUE 'INTERFACE' ##NO_TEXT.
    "! Constante classe de mensagem
    CONSTANTS gc_msg_id TYPE t100-arbgb VALUE 'ZFI_SOLIC_PGTO_ANTEC' ##NO_TEXT.
    "! Constante parâmetro módulo
    CONSTANTS gc_param_modulo TYPE ze_param_modulo VALUE 'FI-AR' ##NO_TEXT.
    "! Constante parâmetro chave 1
    CONSTANTS gc_param_a27 TYPE ze_param_chave1 VALUE 'A27' ##NO_TEXT.
    "! Constante parâmetro tpdoc compensação
    CONSTANTS gc_param_tpdoc_compensacao TYPE ze_param_chave2 VALUE 'TPDOC_COMPENSACAO' ##NO_TEXT.
    "! Constante parâmetro tpdoc adto
    CONSTANTS gc_param_tpdoc_adto TYPE ze_param_chave2 VALUE 'TPDOC_ADTO' ##NO_TEXT.
    "! Constante parâmetro solicitacao pagador
    CONSTANTS gc_param_solicita_pag TYPE ze_param_chave2 VALUE 'SOLICITACAOPAGADOR' ##NO_TEXT.
    "! Constante parâmetro modalidade alteração
    CONSTANTS gc_param_mod_alteracao TYPE ze_param_chave2 VALUE 'MODALIDADEALTERACAO' ##NO_TEXT.
    "! Constante parâmetro forma de pagamento adto
    CONSTANTS gc_form_pag_adto TYPE ze_param_chave2 VALUE 'FORMADEPAGAMENTO_ADTO' ##NO_TEXT.
    "! Constante parâmetro expiração
    CONSTANTS gc_param_expiracao TYPE ze_param_chave2 VALUE 'EXPIRACAO' ##NO_TEXT.
    "! Constante parâmetro conta compensação
    CONSTANTS gc_param_conta_comp TYPE ze_param_chave2 VALUE 'CONTA_COMPENSACAO' ##NO_TEXT.
    "! Emissor e-mail
    CONSTANTS gc_param_email_from TYPE ze_param_chave2 VALUE 'EMAIL_ENVIO' ##NO_TEXT.
    "! E-mail cópia
    CONSTANTS gc_param_email_cc TYPE ze_param_chave2 VALUE 'EMAIL_CC' ##NO_TEXT.
    "! Tipo documento compensação fatura
    CONSTANTS gc_param_tpdoc_comp_fat TYPE ze_param_chave2 VALUE 'TPDOC_COMP_FATXADTO' ##NO_TEXT.
    "! Motivo recusa pix
    CONSTANTS gc_param_motivo_Recusa_pix   TYPE ze_param_chave2 VALUE 'MOTIVO_RECUSA_PIX_EXPIRADO' ##NO_TEXT.
    "! E-mail cópia
    CONSTANTS gc_param_banco_empresa TYPE ze_param_chave2 VALUE 'BANCO_EMPRESA' ##NO_TEXT.

    "! Executar processamento da interface outbound
    "! ir_salesorder | Range com as ordens de venda
    "! ev_count_success | Total de ordens processadas
    "! ev_count_Error | Total de ordens com erro
    "! rt_Return | Mensagens de erro
    METHODS process
      IMPORTING
        !ir_salesorder   TYPE rsis_t_range OPTIONAL
        !ir_txid         TYPE rsis_t_range OPTIONAL
        !is_payment_data TYPE zclfi_solic_pgto_ant_mpc=>ts_solicitacaotype OPTIONAL
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .
    METHODS process_so_adv_clearing
      IMPORTING
        !ir_salesorder   TYPE rsis_t_range OPTIONAL
        !ir_comp_code    TYPE rsis_t_range OPTIONAL
        !is_payment_data TYPE zclfi_solic_pgto_ant_mpc=>ts_solicitacaotype OPTIONAL
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .
    METHODS process_estorno_solicitacao
      IMPORTING
        !ir_salesorder TYPE rsis_t_range OPTIONAL.
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      gt_sales_order TYPE TABLE OF zi_fi_ordens_pgto_antecipado .
    "! Tipagem do parâmetro tpdoc compensação
    DATA gv_param_tpdoc_compensacao TYPE bkpf-blart .
    "! Tipagem do parâmetro tpdoc adto
    DATA gv_param_tpdoc_adto TYPE bkpf-blart .
    "! Tipagem do parâmetro modakidade alteração
    DATA gv_param_mod_alteracao TYPE char100 .
    "! Tipagem do parâmetro solicitação pagamento
    DATA gv_param_solicita_pag TYPE char100 .
    "! Tipagem do parâmetro forma de pagamento
    DATA gv_param_forma_pag TYPE bseg-zlsch .
    "! Tipagem do parâmetro expiração
    DATA gv_param_expiracao TYPE i.
    "! Tipagem do parâmetro conta compensação
    DATA gt_param_conta_compensacao TYPE rsis_t_range .
    "! Email from
    DATA gt_param_email_from TYPE rsis_t_range .
    "! Email cópia
    DATA gt_param_email_cc TYPE rsis_t_range .
    "! Tipo doc. compensação fatura
    DATA gv_param_tpdoc_comp_fatxadto TYPE bkpf-blart .
    "! Motivo recusa pix
    DATA gv_param_motivo_recusa_pix TYPE vbap-abgru.
    "! Email cópia
    DATA gt_param_banco_empresa TYPE rsis_t_range .

    "! Executar processamento da interface outbound
    "! ir_salesorder | Range com as ordens de venda
    "! ev_count_success | Total de ordens processadas
    "! ev_count_Error | Total de ordens com erro
    "! rt_Return | Mensagens de erro
    METHODS process_outbound
      IMPORTING
        !is_order_data   TYPE zi_fi_ordens_pgto_antecipado
      CHANGING
        !cs_table        TYPE ztfi_adto_ov
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .
    "! Processa dados de entrada para solicitações de adiantamento.
    "! is_payload | Estrutura de dados contendo informações da transação
    "! rt_Return | Mensagens de erro
    METHODS process_inbound
      IMPORTING
        !is_payment_data TYPE zclfi_solic_pgto_ant_mpc=>ts_solicitacaotype
        !is_order_data   TYPE zi_fi_ordens_pgto_antecipado
      CHANGING
        !cs_table        TYPE ztfi_adto_ov
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .

    "! Recupera os dados associados a uma transação de pagamento antecipado
    "! iv_txid | Identificador da transação.
    "! rs_table | Dados da transação recuperados.
    METHODS get_payment_request_data
      IMPORTING
        !iv_txid        TYPE ztfi_adto_ov-txid
      RETURNING
        VALUE(rs_table) TYPE ztfi_adto_ov
      RAISING
        zclca_msg_erro .
    "! Cria uma solicitação de adiantamento
    "! is_ordem | Dados da ordem de venda.
    "! ev_documento | Número do documento adiantamento
    "! rt_Return | Mensagens de erro
    METHODS post_down_payment_request
      IMPORTING
        !is_ordem     TYPE zi_fi_ordens_pgto_antecipado
      EXPORTING
        !ev_documento TYPE awkey
        !et_return    TYPE bapiret2_t
      CHANGING
        !cs_table     TYPE ztfi_adto_ov .
    "! Compensar solicitação de adiantamento
    "! is_ordem | Dados da ordem de venda.
    "! ev_documento | Número do documento compensação
    "! rt_Return | Mensagens de erro
    METHODS post_down_payment_clearing
      IMPORTING
        !is_ordem     TYPE zi_fi_ordens_pgto_antecipado
      EXPORTING
        !ev_documento TYPE awkey
        !et_return    TYPE bapiret2_t
      CHANGING
        !cs_table     TYPE ztfi_adto_ov .
    "! Desbloqueia uma ordem de venda no sistema
    "! iv_salesorder | Ordem de venda
    "! rt_Return | Mensagens de erro
    METHODS unlock_sales_order
      IMPORTING
        !iv_salesorder TYPE zi_fi_ordens_pgto_antecipado-salesorder
      EXPORTING
        !et_return     TYPE bapiret2_t
      CHANGING
        !cs_table      TYPE ztfi_adto_ov .
    METHODS send_to_barramento
      IMPORTING
        !is_data   TYPE zi_fi_ordens_pgto_antecipado
      EXPORTING
        !et_return TYPE bapiret2_t
      CHANGING
        !cs_table  TYPE ztfi_adto_ov .
    "! Carrega parâmetros relevantes do sistema
    METHODS load_parameters
      RAISING
        zclca_msg_erro .
    "! Envia e-mail
    "! is_order | Dados da ordem de venda.
    "! is_table | Dados da tabela
    METHODS send_email
      IMPORTING
        !is_order TYPE zi_fi_ordens_pgto_antecipado
        !is_table TYPE ztfi_adto_ov .

    "! Lançar compensação fatura x adiantamento
    "! is_order | Dados da ordem de venda.
    "! ev_documento | Documento compensação gerado
    "! et_return | Dados retorno
    "! cs_table | Tabela de banco
    METHODS post_sales_order_adv_clearing
      IMPORTING
        !is_ordem     TYPE zi_fi_info_pgto_antecipado
      EXPORTING
        !ev_documento TYPE awkey
        !et_return    TYPE bapiret2_t
      CHANGING
        !cs_table     TYPE ztfi_adto_ov .

    "! Lock na ordem de venda em processamento
    "! iv_salesorder | Dados da ordem de venda.
    METHODS lock_object
      IMPORTING
        iv_salesorder TYPE zi_fi_ordens_pgto_antecipado-salesorder
      EXCEPTIONS
        object_already_locked.

    "! Unlock na ordem de venda em processamento
    "! iv_salesorder | Dados da ordem de venda.
    METHODS unlock_object
      IMPORTING
        iv_salesorder TYPE zi_fi_ordens_pgto_antecipado-salesorder.

    "! Persistir dados no banco
    "! it_adto_ov | Tipo de tabela do banco
    METHODS persist
      IMPORTING
        it_adto_ov TYPE ty_t_ztfi_adto_ov.

ENDCLASS.



CLASS zclfi_proc_pgto_antecipado IMPLEMENTATION.


  METHOD process.
    DATA lt_adto_ov TYPE STANDARD TABLE OF ztfi_adto_ov.
    DATA lt_return  TYPE bapiret2_t.

    "@ Load parameters
    TRY.
        me->load_parameters( ).
      CATCH zclca_msg_erro INTO DATA(lo_cx_error_param).
        APPEND VALUE #(
          type       = lo_cx_error_param->if_abap_behv_message~m_severity
          id         = lo_cx_error_param->if_t100_message~t100key-msgid
          number     = lo_cx_error_param->if_t100_message~t100key-msgno
          message_v1 = lo_cx_error_param->if_t100_message~t100key-attr1
        ) TO rt_return.
        RETURN.
    ENDTRY.

    "@ Select sales orders to process
    SELECT
         salesorder,
         billingcompanycode,
         soldtoparty,
         transactioncurrency,
         totalnetamount,
         businessarea,
         cnpjcliente,
         customername,
         docadiantamento,
         doccompensacao,
         transactionidentification,
         outboundstep,
         inboundstep,
         isprocessed
    FROM zi_fi_ordens_pgto_antecipado
   WHERE salesorder                IN @ir_salesorder
     AND transactionidentification IN @ir_txid
     AND iscanceled                EQ @abap_false
     AND isprocessed               EQ @abap_false
     AND isexpired                 EQ @abap_false
    INTO CORRESPONDING FIELDS OF TABLE @gt_sales_order.

    IF sy-subrc IS NOT INITIAL.
      APPEND VALUE #(
        type       = if_xo_const_message~error
        id         = gc_msg_id
        number     = 007
      ) TO rt_return.
      RETURN.
    ENDIF.

    "//Select documents already saved
    SELECT *                                       "#EC CI_NO_TRANSFORM
      FROM ztfi_adto_ov
      INTO TABLE @lt_adto_ov
   FOR ALL ENTRIES IN @gt_sales_order
     WHERE vbeln = @gt_sales_order-salesorder.

    SORT gt_sales_order BY SalesOrder.
    SORT lt_adto_ov     BY vbeln.

    "//Process
    LOOP AT gt_sales_order ASSIGNING FIELD-SYMBOL(<fs_sales>).
      TRY.
          me->lock_object(
            EXPORTING
              iv_salesorder         = <fs_sales>-SalesOrder
            EXCEPTIONS
              object_already_locked = 1
              OTHERS                = 2
          ).

          IF sy-subrc <> 0.
            APPEND VALUE #(
              type       = if_xo_const_message~error
              id         = gc_msg_id
              number     = 006
              message_v1 = <fs_sales>-SalesOrder
              message_v2 = <fs_sales>-TransactionIdentification
            ) TO lt_return.

            APPEND LINES OF lt_return TO rt_return.
            RAISE EXCEPTION TYPE zclca_msg_erro.
          ENDIF.

          READ TABLE lt_adto_ov ASSIGNING FIELD-SYMBOL(<fs_solic_adiantamento>)
            WITH KEY vbeln = <fs_sales>-salesorder BINARY SEARCH.

          IF sy-subrc <> 0.
            APPEND VALUE #(
             bukrs           = <fs_sales>-billingcompanycode
             vbeln           = <fs_sales>-salesorder
             kunnr           = <fs_sales>-soldtoparty
             criado_em       = sy-datum
             hora_registro   = sy-uzeit
             usuario         = sy-uname
            ) TO lt_adto_ov ASSIGNING <fs_solic_adiantamento>.
          ENDIF.

          IF ( <fs_sales>-outboundstep = abap_true ).
            CALL METHOD me->process_outbound
              EXPORTING
                is_order_data = <fs_sales>
              CHANGING
                cs_table      = <fs_solic_adiantamento>
              RECEIVING
                rt_return     = lt_return.

            APPEND LINES OF lt_return TO rt_return.
          ENDIF.

          IF ( <fs_sales>-inboundstep = abap_true ).

            IF is_payment_data IS INITIAL AND <fs_sales>-haserror IS INITIAL.
              CONTINUE.
            ENDIF.

            CALL METHOD me->process_inbound
              EXPORTING
                is_payment_data = is_payment_data
                is_order_data   = <fs_sales>
              CHANGING
                cs_table        = <fs_solic_adiantamento>
              RECEIVING
                rt_return       = lt_return.

            APPEND LINES OF lt_return TO rt_return.
          ENDIF.

        CATCH zclca_msg_erro.
      ENDTRY.

      IF line_exists( lt_return[ type = if_xo_const_message~error ] ). "#EC CI_STDSEQ
        DATA(lv_external_id) = CONV balnrext( <fs_sales>-salesorder ).

        CALL FUNCTION 'ZFMCA_ADD_LOG'
          EXPORTING
            iv_ext_number = lv_external_id
            iv_object     = gc_log_object
            iv_subobject  = gc_log_subobj
            it_return     = lt_return.
      ENDIF.

      CLEAR lt_return.
    ENDLOOP.

    SORT lt_adto_ov BY expira_em.
    DELETE lt_adto_ov WHERE expira_em IS INITIAL.

    "@ Update table
    me->persist( lt_adto_ov ).

  ENDMETHOD.


  METHOD process_outbound.
    DATA lt_return      TYPE bapiret2_t.

    IF cs_table-expira_em IS INITIAL.
      GET TIME STAMP FIELD DATA(lv_timestamp_current).

      CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
        EXPORTING
          timestamp_in    = lv_timestamp_current
          duration        = me->gv_param_expiracao
        IMPORTING
          timestamp_out   = cs_table-expira_em
        EXCEPTIONS
          timestamp_error = 1
          OTHERS          = 2.

      CHECK sy-subrc IS INITIAL.
    ENDIF.

    "@ Create down payment request (f-37)
    IF is_order_data-docadiantamento IS INITIAL.
      CALL METHOD me->post_down_payment_request
        EXPORTING
          is_ordem     = is_order_data
        IMPORTING
          ev_documento = DATA(lv_accounting_document)
          et_return    = DATA(lt_return_adto)
        CHANGING
          cs_table     = cs_table.

      APPEND LINES OF lt_return_adto TO rt_return.
    ELSE.
      lv_accounting_document = is_order_data-docadiantamento.
    ENDIF.

    "@ Send data to barramento interface through CPI
    IF lv_accounting_document IS NOT INITIAL AND is_order_data-transactionidentification IS INITIAL.
      CALL METHOD me->send_to_barramento
        EXPORTING
          is_data   = is_order_data
        IMPORTING
          et_return = DATA(lt_return_intf)
        CHANGING
          cs_table  = cs_table.

      APPEND LINES OF lt_return_intf TO rt_return.
    ENDIF.

    IF cs_table-pix_copia_cola IS NOT INITIAL.
      send_email( is_order = is_order_data is_table = cs_table ).
    ENDIF.

  ENDMETHOD.


  METHOD process_inbound.
    IF is_payment_data IS NOT INITIAL.
      cs_table-info_pagamento = is_payment_data-infopagador.
      cs_table-flag_pago      = abap_true.
      cs_table-hora_pagamento = is_payment_data-horario.
    ENDIF.

    IF is_order_data-doccompensacao IS INITIAL.
      CALL METHOD me->post_down_payment_clearing
        EXPORTING
          is_ordem     = is_order_data
        IMPORTING
          ev_documento = DATA(lv_accounting_document)
          et_return    = DATA(lt_return_clearing)
        CHANGING
          cs_table     = cs_table.

      APPEND LINES OF lt_return_clearing TO rt_return.
    ELSE.
      lv_accounting_document = is_order_data-doccompensacao.
    ENDIF.

    IF lv_accounting_document IS NOT INITIAL.

      CALL METHOD me->unlock_sales_order
        EXPORTING
          iv_salesorder = is_order_data-salesorder
        IMPORTING
          et_return     = DATA(lt_return_unlck_ov)
        CHANGING
          cs_table      = cs_table.

      APPEND LINES OF lt_return_unlck_ov TO rt_return.
    ENDIF.

  ENDMETHOD.


  METHOD post_down_payment_request.
    DATA ls_documentheader    TYPE bapiache09.
    DATA ls_accountreceivable TYPE bapiacar09.
    DATA ls_currencyamount    TYPE bapiaccr09.
    DATA lt_accountreceivable TYPE STANDARD TABLE OF bapiacar09.
    DATA lt_currencyamount    TYPE STANDARD TABLE OF bapiaccr09.

    CONSTANTS:
      lc_obj_type  TYPE char5 VALUE 'BKPFF',
      lc_bus_act   TYPE char4 VALUE 'RFST',
      lc_sp_gl_ind TYPE char1 VALUE 'F'.

    READ TABLE me->gt_param_banco_empresa ASSIGNING FIELD-SYMBOL(<fs_param_banco_empresa>)
      WITH KEY low = is_ordem-BillingCompanyCode.

    "PREENCHENDO DADOS DO CABEÇALHO.
    ls_documentheader-obj_type         = lc_obj_type.
    ls_documentheader-bus_act          = lc_bus_act.
    ls_documentheader-username         = sy-uname.
    ls_documentheader-header_txt       = gc_acc_document_txt.
    ls_documentheader-ref_doc_no       = is_ordem-salesorder.
    ls_documentheader-comp_code        = is_ordem-billingcompanycode.
    ls_documentheader-doc_date         = sy-datum.
    ls_documentheader-pstng_date       = sy-datum.
    ls_documentheader-fisc_year        = sy-datum+0(4).
    ls_documentheader-fis_period       = sy-datum+4(2).
    ls_documentheader-doc_type         = me->gv_param_tpdoc_adto.

    "PREENCHENDO DADOS CONTAS A RECEBER
    ls_accountreceivable-itemno_acc    = 1.
    ls_accountreceivable-customer      = is_ordem-soldtoparty.
    ls_accountreceivable-comp_code     = is_ordem-billingcompanycode.
    ls_accountreceivable-bus_area      = is_ordem-businessarea.
    ls_accountreceivable-businessplace = is_ordem-businessarea.
    ls_accountreceivable-sp_gl_ind     = lc_sp_gl_ind.
    ls_accountreceivable-bline_date    = sy-datum + 1.
    ls_accountreceivable-pymt_meth     = me->gv_param_forma_pag.
    ls_accountreceivable-bank_id       = <fs_param_banco_empresa>-high.

    APPEND ls_accountreceivable TO lt_accountreceivable.
    CLEAR ls_accountreceivable.

    "PREENCHENDO DADOS VALOR MOEDA
    ls_currencyamount-itemno_acc = 1.
    ls_currencyamount-currency   = is_ordem-transactioncurrency.
    ls_currencyamount-amt_doccur = is_ordem-totalnetamount.

    APPEND ls_currencyamount TO lt_currencyamount.
    CLEAR ls_currencyamount.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = ls_documentheader "CABEÇALHO.
      IMPORTING
        obj_key           = ev_documento
      TABLES
        accountreceivable = lt_accountreceivable
        currencyamount    = lt_currencyamount
        return            = et_return.

    IF NOT line_exists( et_return[ type = if_xo_const_message~error ] ). "#EC CI_STDSEQ
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      cs_table-status = gc_status-erro_adiantamento.
      CLEAR ev_documento.
    ENDIF.

    cs_table-doc_adiantamento = ev_documento.
  ENDMETHOD.


  METHOD post_down_payment_clearing.
    DATA lt_blntab  TYPE STANDARD TABLE OF blntab.
    DATA lt_ftclear TYPE STANDARD TABLE OF ftclear.
    DATA lt_ftpost  TYPE STANDARD TABLE OF ftpost.
    DATA lt_fttax   TYPE STANDARD TABLE OF fttax.

    CONSTANTS:
      lc_agkoa         TYPE char1     VALUE 'D',
      lc_agums         TYPE char100   VALUE 'F',
      lc_selfd         TYPE char30    VALUE 'BELNR',
      lc_selvon_selbis TYPE numc3     VALUE '001',
      lc_stype_k       TYPE char1     VALUE 'K',
      lc_stype_p       TYPE char1     VALUE 'P',
      lc_count_001     TYPE numc3     VALUE '001',
      lc_count_002     TYPE numc3     VALUE '002',
      lc_fval_pag      TYPE bdc_fval  VALUE 'PAGAMENTO SOLIC. ADTO',
      lc_fval_40       TYPE bdc_fval  VALUE '40',
      lc_function      TYPE char1     VALUE 'C',
      lc_mode          TYPE char1     VALUE 'N',
      lc_update        TYPE char1     VALUE 'S',
      lc_low           TYPE rvari_val VALUE '*',
      lc_auglv         TYPE char8     VALUE 'UMBUCHNG',
      lc_tcode         TYPE char20    VALUE 'FB05',
      lc_sgfunct       TYPE char1     VALUE 'C'.

    TRY.
        DATA(lv_conta_compensacao) = me->gt_param_conta_compensacao[ low = is_ordem-billingcompanycode ]-high. "#EC CI_STDSEQ
      CATCH cx_sy_itab_line_not_found.
        lv_conta_compensacao = me->gt_param_conta_compensacao[ low = lc_low ]-high. "#EC CI_STDSEQ
    ENDTRY.

    lt_ftclear = VALUE #( (
      agkoa  = lc_agkoa
      agkon  = cs_table-kunnr
      agbuk  = cs_table-bukrs
      xnops  = abap_true
      agums  = lc_agums
      selfd  = lc_selfd
      selvon = cs_table-doc_adiantamento(10) && cs_table-criado_em(4) && lc_selvon_selbis
      selbis = cs_table-doc_adiantamento(10) && cs_table-criado_em(4) && lc_selvon_selbis
    ) ).

    lt_ftpost = VALUE #(
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_budat  fval = |{ sy-datum DATE = USER }|     )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_bldat  fval = |{ sy-datum DATE = USER }|     )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_blart  fval = me->gv_param_tpdoc_compensacao )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_bukrs  fval = cs_table-bukrs                 )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_waers  fval = is_ordem-transactioncurrency   )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_bktxt  fval = lc_fval_pag        )
      ( stype = lc_stype_p count = lc_count_002 fnam = gc_field_bseg_bupla  fval = is_ordem-businessarea          )
      ( stype = lc_stype_p count = lc_count_002 fnam = gc_field_rf05a_newbs fval = lc_fval_40                           )
      ( stype = lc_stype_p count = lc_count_002 fnam = gc_field_rf05a_newko fval = lv_conta_compensacao           )
      ( stype = lc_stype_p count = lc_count_002 fnam = gc_field_bseg_wrbtr  fval = |{ is_ordem-totalnetamount NUMBER = USER }| )
      ( stype = lc_stype_p count = lc_count_002 fnam = gc_field_bseg_valut  fval = |{ sy-datum DATE = USER }|     )
    ).

    TRY.
        CALL FUNCTION 'POSTING_INTERFACE_START' "#EC CI_SUBRC
          EXPORTING
            i_function         = lc_function
            i_mode             = lc_mode
            i_update           = lc_update
            i_user             = sy-uname
          EXCEPTIONS ##FM_SUBRC_OK
            client_incorrect   = 1
            function_invalid   = 2
            group_name_missing = 3
            mode_invalid       = 4
            update_invalid     = 5
            user_invalid       = 6
            OTHERS             = 7.

        IF sy-subrc <> 0.
          APPEND VALUE #(
            type       = if_xo_const_message~error
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4
          ) TO et_return.

          RAISE EXCEPTION TYPE zclca_msg_erro.
        ENDIF.

        CALL FUNCTION 'POSTING_INTERFACE_CLEARING' "#EC CI_SUBRC
          EXPORTING
            i_auglv                    = lc_auglv
            i_tcode                    = lc_tcode
            i_sgfunct                  = lc_sgfunct
          IMPORTING
            e_msgid                    = sy-msgid
            e_msgno                    = sy-msgno
            e_msgty                    = sy-msgty
            e_msgv1                    = sy-msgv1
            e_msgv2                    = sy-msgv2
            e_msgv3                    = sy-msgv3
            e_msgv4                    = sy-msgv4
          TABLES
            t_blntab                   = lt_blntab
            t_ftclear                  = lt_ftclear
            t_ftpost                   = lt_ftpost
            t_fttax                    = lt_fttax
          EXCEPTIONS ##FM_SUBRC_OK
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
          APPEND VALUE #(
            type       = if_xo_const_message~error
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4
          ) TO et_return.

          RAISE EXCEPTION TYPE zclca_msg_erro.
        ENDIF.

        CALL FUNCTION 'POSTING_INTERFACE_END'
          EXPORTING
            i_bdcimmed              = abap_true
          EXCEPTIONS
            session_not_processable = 1
            OTHERS                  = 2.

        IF sy-subrc <> 0.
          APPEND VALUE #(
            type       = if_xo_const_message~error
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4
          ) TO et_return.

          RAISE EXCEPTION TYPE zclca_msg_erro.
        ENDIF.

      CATCH zclca_msg_erro.
    ENDTRY.

    IF sy-subrc IS INITIAL AND sy-msgty = if_xo_const_message~success.
      ev_documento = sy-msgv1 && sy-msgv2 && sy-datum(4).
    ELSE.
      APPEND VALUE #(
        type       = if_xo_const_message~error
        id         = sy-msgid
        number     = sy-msgno
        message_v1 = sy-msgv1
        message_v2 = sy-msgv2
        message_v3 = sy-msgv3
        message_v4 = sy-msgv4
      ) TO et_return.

      cs_table-status = gc_status-erro_compensacao.
      CLEAR ev_documento.
    ENDIF.

    cs_table-doc_compensacao = ev_documento.
  ENDMETHOD.


  METHOD unlock_sales_order.
    DATA ls_headerx TYPE bapisdh1x.

*    ls_headerx-updateflag = 'U'.
*    ls_headerx-bill_block = abap_true.
*
*    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*      EXPORTING
*        salesdocument    = iv_salesorder
*        order_header_inx = ls_headerx
*      TABLES
*        return           = et_return.

    CALL FUNCTION 'SD_ORDER_CREDIT_RELEASE'
      EXPORTING
        vbeln       = iv_salesorder
        if_synchron = abap_true.
*
*    IF NOT line_exists( et_return[ type = 'E' ] ).       "#EC CI_STDSEQ
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = abap_true.

    cs_table-status = gc_status-ordem_desbloqueada.
*    ELSE.
*      cs_table = gc_status-erro_desbl_ordem.
*    ENDIF.
  ENDMETHOD.


  METHOD send_to_barramento.
    DATA ls_payload TYPE ty_payload_oubound.

    DATA(lo_cpi) = NEW zclca_cpi( ).
    DATA(lo_cpi_monitor) = NEW zclca_monitor_cpi( ).

    ls_payload-empresa              = is_data-BillingCompanyCode.
    ls_payload-devedor-cnpj         = is_data-cnpjcliente.
    ls_payload-devedor-nome         = is_data-customername.
    ls_payload-valor-original       = is_data-totalnetamount.
    ls_payload-valor-modalidade_alteracao = me->gv_param_mod_alteracao.
    ls_payload-chave                = ''.
    ls_payload-solicitacao_pagador  = me->gv_param_solicita_pag.
    ls_payload-calendario-expiracao = me->gv_param_expiracao.

    DATA(lv_payload_str) = lo_cpi->conv_data_to_json( iv_data = ls_payload ).

    CALL METHOD lo_cpi->send
      EXPORTING
        iv_processo  = gc_cpi-envio_pagamento
        iv_metodo    = gc_cpi-method_post
        is_structure = ls_payload
      IMPORTING
        ev_result    = DATA(lv_result)
        et_return    = et_return.

    IF ( et_return IS NOT INITIAL ).
      cs_table-status = gc_status-erro_envio_a27.

      lo_cpi_monitor->started_process(
        EXPORTING
          iv_processo  = gc_cpi-envio_pagamento
          iv_metodo    = gc_cpi-method_post
          iv_json      = lv_payload_str
          iv_chave_ref = CONV #( is_data-salesorder )
      ).

      lo_cpi_monitor->save_log(
        EXPORTING
          iv_processo     = gc_cpi-envio_pagamento
          iv_metodo       = gc_cpi-method_post
          iv_json_retorno = lv_result
          iv_json         = lv_payload_str
          it_return       = et_return
      ).
    ELSE.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = lv_result
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data             = ls_payload
      ).

      cs_table-status         = gc_status-enviado_a27.
      cs_table-txid           = ls_payload-txid.
      cs_table-pix_copia_cola = ls_payload-pix_copia_e_cola.
    ENDIF.
  ENDMETHOD.


  METHOD load_parameters.
    DATA(lo_param) = NEW zclca_tabela_parametros( ).

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_tpdoc_compensacao
          IMPORTING
            ev_param  = me->gv_param_tpdoc_compensacao
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_tpdoc_compensacao
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_tpdoc_comp_fat
          IMPORTING
            ev_param  = me->gv_param_tpdoc_comp_fatxadto
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_tpdoc_comp_fat
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_tpdoc_adto
          IMPORTING
            ev_param  = me->gv_param_tpdoc_adto
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_tpdoc_adto
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_solicita_pag
          IMPORTING
            ev_param  = me->gv_param_solicita_pag
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_solicita_pag
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_mod_alteracao
          IMPORTING
            ev_param  = me->gv_param_mod_alteracao
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_mod_alteracao
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_form_pag_adto
          IMPORTING
            ev_param  = me->gv_param_forma_pag
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_form_pag_adto
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_expiracao
          IMPORTING
            ev_param  = me->gv_param_expiracao
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_expiracao
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_range(
          EXPORTING
              iv_modulo = gc_param_modulo
              iv_chave1 = gc_param_a27
              iv_chave2 = gc_param_conta_comp
          IMPORTING
            et_range  = me->gt_param_conta_compensacao
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_conta_comp
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_range(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_email_from
          IMPORTING
            et_range  = me->gt_param_email_from
        ).

        SORT gt_param_email_from BY low.
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_email_from
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_range(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_email_cc
          IMPORTING
            et_range  = me->gt_param_email_cc
        ).

        SORT gt_param_email_cc BY low.
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_email_cc
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_range(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_banco_empresa
          IMPORTING
            et_range  = me->gt_param_banco_empresa
        ).

        SORT gt_param_banco_empresa BY low.
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_banco_empresa
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_tpdoc_comp_fat
          IMPORTING
            ev_param  = me->gv_param_tpdoc_comp_fatxadto
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_tpdoc_comp_fat
            ).
    ENDTRY.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = gc_param_modulo
            iv_chave1 = gc_param_a27
            iv_chave2 = gc_param_motivo_recusa_pix
          IMPORTING
            ev_param  = me->gv_param_motivo_recusa_pix
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = if_abap_behv_message=>severity-error
            textid   = VALUE #(
              msgid = gc_msg_id
              msgno = 001
              attr1 = gc_param_tpdoc_comp_fat
            ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_payment_request_data.

    SELECT SINGLE *
      FROM ztfi_adto_ov
      INTO @rs_table
     WHERE txid = @iv_txid.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zclca_msg_erro
        EXPORTING
          severity = if_abap_behv_message=>severity-error
          textid   = VALUE #(
            msgid = gc_msg_id
            msgno = 002
            attr1 = iv_txid
          ).
    ENDIF.

  ENDMETHOD.


  METHOD send_email.
    CONSTANTS: lc_type      TYPE so_obj_tp VALUE 'HTM',
               lc_extension TYPE so_obj_tp VALUE 'bmp'.

    DATA: lt_cc   TYPE TABLE OF REF TO cl_cam_address_bcs,
          lt_dest TYPE TABLE OF REF TO cl_cam_address_bcs.

    SELECT DISTINCT ihbbusinesspartner AS businesspartner,
                    ihbemailaddress    AS emailaddress
      FROM i_ihbbuspartneremailaddress
      WHERE ihbbusinesspartner EQ @is_order-soldtoparty
      INTO TABLE @DATA(lt_cliente).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    TRY.
        DATA(lv_email_from) = CONV ad_smtpadr( me->gt_param_email_from[ low = is_table-bukrs ]-high ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    DELETE gt_param_email_cc WHERE low <> is_table-bukrs.
    DATA(lt_qr_code) = cl_rmps_disposal=>convert_128_to_255( zclca_qr_code=>generate( CONV #( is_table-pix_copia_cola ) ) ).

    DATA(lv_subject) = CONV so_obj_des( |{ TEXT-001 } { is_order-salesorder ALPHA = OUT }| ).

    DATA(lt_html) = VALUE soli_tab(
      ( line = |{ TEXT-h01 }| )
      ( line = |{ TEXT-h02 } { is_order-customername },{ TEXT-h00 }{ TEXT-h00 }| )
      ( line = |{ TEXT-h03 } { is_order-salesorder ALPHA = OUT }.{ TEXT-h00 }| )
      ( line = |{ TEXT-h04 } { TEXT-h05 }{ TEXT-h00 }{ TEXT-h00 }| )
      ( line = |{ is_table-pix_copia_cola }{ TEXT-h00 }{ TEXT-h00 }| )
      ( line = |{ TEXT-h06 } { TEXT-h07 } { TEXT-h00 }{ TEXT-h00 }| )
      ( line = |{ TEXT-h08 } { TEXT-h00 }| )
      ( line = |{ is_order-CompanyCodeName }| )
      ( line = |{ TEXT-h09 }| ) ).

    TRY.
        DATA(lo_doc_bcs) = cl_document_bcs=>create_document( i_type    = lc_type
                                                             i_text    = lt_html
                                                             i_subject = lv_subject ).

        lo_doc_bcs->add_attachment( i_attachment_type    = lc_extension
                                    i_attachment_subject = TEXT-002
                                    i_att_content_hex    = lt_qr_code ).

      CATCH cx_document_bcs INTO DATA(lo_document_bcs).
        RETURN.
    ENDTRY.

    TRY.
        DATA(lo_sender) = cl_cam_address_bcs=>create_internet_address( lv_email_from ).

        lt_cc = VALUE #( FOR ls_email_cc IN gt_param_email_cc (
          cl_cam_address_bcs=>create_internet_address( i_address_string = CONV #( ls_email_cc-high ) ) ) ).

        lt_dest = VALUE #( FOR ls_cliente IN lt_cliente (
          cl_cam_address_bcs=>create_internet_address( i_address_string = ls_cliente-emailaddress ) ) ).

      CATCH cx_address_bcs INTO DATA(lo_address_bcs).
        RETURN.
    ENDTRY.

    TRY.
        DATA(lo_bcs) = cl_bcs=>create_persistent( ).

        lo_bcs->set_document( lo_doc_bcs ).

        lo_bcs->set_sender( i_sender = lo_sender ).

        LOOP AT lt_cc ASSIGNING FIELD-SYMBOL(<fs_cc>).
          lo_bcs->add_recipient( i_recipient = <fs_cc> i_copy = abap_true ).
        ENDLOOP.

        LOOP AT lt_dest ASSIGNING FIELD-SYMBOL(<fs_dest>).
          lo_bcs->add_recipient( i_recipient = <fs_dest> i_copy = abap_false ).
        ENDLOOP.

        lo_bcs->send( ).

      CATCH cx_send_req_bcs INTO DATA(lo_send_req_bcs).
        RETURN.
    ENDTRY.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD post_sales_order_adv_clearing.
    DATA lt_blntab  TYPE STANDARD TABLE OF blntab.
    DATA lt_ftclear TYPE STANDARD TABLE OF ftclear.
    DATA lt_ftpost  TYPE STANDARD TABLE OF ftpost.
    DATA lt_fttax   TYPE STANDARD TABLE OF fttax.

    CONSTANTS:
      lc_agums         TYPE char100   VALUE 'A',
      lc_agkoa         TYPE char1     VALUE 'D',
      lc_selfd         TYPE char30    VALUE 'BELNR',
      lc_selvon_selbis TYPE char3     VALUE '001',
      lc_stype_k       TYPE char1     VALUE 'K',
      lc_stype_p       TYPE char1     VALUE 'P',
      lc_count_001     TYPE numc3     VALUE '001',
      lc_count_002     TYPE numc3     VALUE '002',
      lc_fval_conc     TYPE bdc_fval  VALUE 'CONCILIAÇÃO PIX QRCODE',
      lc_fval_40       TYPE bdc_fval  VALUE '40',
      lc_function      TYPE char1     VALUE 'C',
      lc_mode          TYPE char1     VALUE 'N',
      lc_update        TYPE char1     VALUE 'S',
      lc_low           TYPE rvari_val VALUE '*',
      lc_auglv         TYPE char8     VALUE 'UMBUCHNG',
      lc_tcode         TYPE char20    VALUE 'FB05',
      lc_sgfunct       TYPE char1     VALUE 'C'.

    lt_ftclear = VALUE #( ( agkoa  = lc_agkoa
                            agkon  = is_ordem-kunnr
                            agbuk  = is_ordem-bukrs
                            xnops  = abap_true
                            selfd  = lc_selfd
                            selvon = is_ordem-doccontabilfatura && is_ordem-fiscalperiod && lc_count_001
                            selbis = is_ordem-doccontabilfatura && is_ordem-fiscalperiod && lc_count_001 )
                          ( agkoa  = lc_agkoa
                            agkon  = is_ordem-kunnr
                            agbuk  = is_ordem-bukrs
                            agums  = lc_agums
                            selfd  = lc_selfd
                            selvon = is_ordem-Doc_compensacao && is_ordem-fiscalperiod && lc_count_002
                            selbis = is_ordem-Doc_compensacao && is_ordem-fiscalperiod && lc_count_002 ) ).

    lt_ftpost = VALUE #(
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_budat  fval = |{ sy-datum DATE = USER }|       )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_bldat  fval = |{ sy-datum DATE = USER }|       )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_blart  fval = me->gv_param_tpdoc_comp_fatxadto )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_bukrs  fval = cs_table-bukrs                   )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_waers  fval = is_ordem-transactioncurrency     )
      ( stype = lc_stype_k count = lc_count_001 fnam = gc_field_bkpf_bktxt  fval = lc_fval_conc    )
      ).

    TRY.
        CALL FUNCTION 'POSTING_INTERFACE_START' "#EC CI_SUBRC
          EXPORTING
            i_function         = lc_function
            i_mode             = lc_mode
            i_update           = lc_update
            i_user             = sy-uname
          EXCEPTIONS ##FM_SUBRC_OK
            client_incorrect   = 1
            function_invalid   = 2
            group_name_missing = 3
            mode_invalid       = 4
            update_invalid     = 5
            user_invalid       = 6
            OTHERS             = 7.

        IF sy-subrc <> 0.
          APPEND VALUE #(
            type       = if_xo_const_message~error
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4
          ) TO et_return.

          RAISE EXCEPTION TYPE zclca_msg_erro.
        ENDIF.

        CALL FUNCTION 'POSTING_INTERFACE_CLEARING' "#EC CI_SUBRC
          EXPORTING
            i_auglv                    = lc_auglv
            i_tcode                    = lc_tcode
            i_sgfunct                  = lc_sgfunct
          IMPORTING
            e_msgid                    = sy-msgid
            e_msgno                    = sy-msgno
            e_msgty                    = sy-msgty
            e_msgv1                    = sy-msgv1
            e_msgv2                    = sy-msgv2
            e_msgv3                    = sy-msgv3
            e_msgv4                    = sy-msgv4
          TABLES
            t_blntab                   = lt_blntab
            t_ftclear                  = lt_ftclear
            t_ftpost                   = lt_ftpost
            t_fttax                    = lt_fttax
          EXCEPTIONS ##FM_SUBRC_OK
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
          APPEND VALUE #(
            type       = if_xo_const_message~error
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4
          ) TO et_return.

          RAISE EXCEPTION TYPE zclca_msg_erro.
        ENDIF.

        CALL FUNCTION 'POSTING_INTERFACE_END'
          EXPORTING
            i_bdcimmed              = abap_true
            i_bdcstrtdt             = sy-datum
            i_bdcstrttm             = sy-uzeit
          EXCEPTIONS
            session_not_processable = 1
            OTHERS                  = 2.

        IF sy-subrc <> 0.
          APPEND VALUE #(
            type       = if_xo_const_message~error
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4
          ) TO et_return.

          RAISE EXCEPTION TYPE zclca_msg_erro.
        ENDIF.

      CATCH zclca_msg_Erro.
    ENDTRY.

    IF sy-subrc IS INITIAL AND sy-msgty = if_xo_const_message~success.
      ev_documento = sy-msgv1 && sy-msgv2 && sy-datum(4).
      cs_table-status = gc_status-processado.
    ELSE.
      APPEND VALUE #(
        type       = if_xo_const_message~error
        id         = sy-msgid
        number     = sy-msgno
        message_v1 = sy-msgv1
        message_v2 = sy-msgv2
        message_v3 = sy-msgv3
        message_v4 = sy-msgv4
      ) TO et_return.

      cs_table-status = gc_status-erro_compens_fatura.
      CLEAR ev_documento.
    ENDIF.

    cs_table-doc_compensacao_fatura = ev_documento.
  ENDMETHOD.


  METHOD process_so_adv_clearing.
    DATA lt_adto_ov TYPE TABLE OF ztfi_adto_ov.
    DATA lt_return  TYPE bapiret2_t.

    "@ Load parameters
    TRY.
        me->load_parameters( ).
      CATCH zclca_msg_erro INTO DATA(lo_cx_error_param).
        APPEND VALUE #(
          type       = lo_cx_error_param->if_abap_behv_message~m_severity
          id         = lo_cx_error_param->if_t100_message~t100key-msgid
          number     = lo_cx_error_param->if_t100_message~t100key-msgno
          message_v1 = lo_cx_error_param->if_t100_message~t100key-attr1
        ) TO rt_return.
        RETURN.
    ENDTRY.

    "@ Select sales orders to process
    SELECT  vbeln,
            bukrs,
            kunnr,
            valor,
            transactioncurrency,
            criado_em,
            hora_registro,
            usuario,
            doc_adiantamento,
            ano_adiantamento,
            doc_compensacao,
            doccompensacaofatura,
            status,
            txid,
            flag_cancelamento,
            docfatura,
            doccontabilfatura,
            fiscalperiod,
            doccontabilfatdocdate,
            doccontabilfatpstdate,
            doccontabilfattexthead,
            doccontabilfatcurrency,
            doccontabilfattype,
            status_txt,
            customername,
            objpagetitle,
            status_pgto,
            IsExpired
    FROM zi_fi_info_pgto_antecipado
   WHERE vbeln                     IN @ir_salesorder
     AND bukrs                     IN @ir_comp_code
     AND docfatura                 <> @space
     AND txid                      <> @space
     AND doccompensacaofatura      =  @space
     AND flag_cancelamento         =  @abap_false
    INTO TABLE @DATA(lt_sales_order).

    CHECK sy-subrc IS INITIAL.

    SELECT *                                       "#EC CI_NO_TRANSFORM
      FROM ztfi_adto_ov
      INTO TABLE @lt_adto_ov
   FOR ALL ENTRIES IN @lt_sales_order
     WHERE vbeln = @lt_sales_order-vbeln.

    SORT lt_sales_order BY vbeln.
    SORT lt_adto_ov     BY vbeln.

    LOOP AT lt_sales_order ASSIGNING FIELD-SYMBOL(<fs_sales>).
      READ TABLE lt_adto_ov ASSIGNING FIELD-SYMBOL(<fs_solic_adiantamento>)
        WITH KEY vbeln = <fs_sales>-vbeln BINARY SEARCH.

      CHECK sy-subrc = 0.

      TRY.
          me->lock_object(
            EXPORTING
              iv_salesorder         = <fs_sales>-vbeln
            EXCEPTIONS
              object_already_locked = 1
              OTHERS                = 2
          ).

          IF sy-subrc <> 0.
            APPEND VALUE #(
              type       = if_xo_const_message~error
              id         = gc_msg_id
              number     = 006
              message_v1 = <fs_sales>-vbeln
              message_v2 = <fs_sales>-Txid
            ) TO lt_return.

            APPEND LINES OF lt_return TO rt_return.
            RAISE EXCEPTION TYPE zclca_msg_erro.
          ENDIF.

          CALL METHOD me->post_sales_order_adv_clearing
            EXPORTING
              is_ordem  = <fs_sales>
            IMPORTING
              et_return = DATA(lt_return_clearing)
            CHANGING
              cs_table  = <fs_solic_adiantamento>.

          APPEND LINES OF lt_return_clearing TO rt_return.
        CATCH zclca_msg_erro.
      ENDTRY.

      IF line_exists( lt_return[ type = if_xo_const_message~error ] ). "#EC CI_STDSEQ
        DATA(lv_external_id) = CONV balnrext( <fs_sales>-vbeln ).

        CALL FUNCTION 'ZFMCA_ADD_LOG'
          EXPORTING
            iv_ext_number = lv_external_id
            iv_object     = gc_log_object
            iv_subobject  = gc_log_subobj
            it_return     = lt_return.
      ENDIF.

      CLEAR lt_return.
    ENDLOOP.

    "@ Update table
    me->persist( lt_adto_ov ).

  ENDMETHOD.

  METHOD process_estorno_solicitacao.
    DATA lt_return  TYPE bapiret2_t.
    DATA lt_adto_ov TYPE TABLE OF ztfi_adto_ov.

    CONSTANTS:
      lc_stgrd       TYPE char2  VALUE '01',
      lc_monat_voidr TYPE numc2  VALUE '00',
      lc_f5          TYPE char20 VALUE 'F5'.

    "@ Load parameters
    TRY.
        me->load_parameters( ).
      CATCH zclca_msg_erro INTO DATA(lo_cx_error_param).
        APPEND VALUE #(
          type       = lo_cx_error_param->if_abap_behv_message~m_severity
          id         = lo_cx_error_param->if_t100_message~t100key-msgid
          number     = lo_cx_error_param->if_t100_message~t100key-msgno
          message_v1 = lo_cx_error_param->if_t100_message~t100key-attr1
        ) TO lt_return.
        RETURN.
    ENDTRY.

    "@ Select sales orders to process
    SELECT
      vbeln,
      bukrs,
      Doc_Adiantamento,
      Ano_Adiantamento,
      Txid
    FROM zi_fi_info_pgto_antecipado
   WHERE Vbeln     IN @ir_salesorder
     AND IsExpired EQ @abap_true
     AND Status    NE @gc_status-pix_expirado
    INTO TABLE @DATA(lt_sales_order).

    CHECK sy-subrc IS INITIAL.

    SELECT *                                       "#EC CI_NO_TRANSFORM
      FROM ztfi_adto_ov
      INTO TABLE @lt_adto_ov
   FOR ALL ENTRIES IN @lt_sales_order
     WHERE vbeln = @lt_sales_order-Vbeln.

    SORT lt_sales_order BY vbeln.
    SORT lt_adto_ov     BY vbeln.

    LOOP AT lt_sales_order ASSIGNING FIELD-SYMBOL(<fs_sales>).
      READ TABLE lt_adto_ov ASSIGNING FIELD-SYMBOL(<fs_solic_adiantamento>)
        WITH KEY vbeln = <fs_sales>-vbeln BINARY SEARCH.

      CHECK sy-subrc = 0.

      TRY.
          me->lock_object(
            EXPORTING
              iv_salesorder         = <fs_sales>-vbeln
            EXCEPTIONS
              object_already_locked = 1
              OTHERS                = 2
          ).

          IF sy-subrc <> 0.
            APPEND VALUE #(
              type       = if_xo_const_message~error
              id         = gc_msg_id
              number     = 006
              message_v1 = <fs_sales>-vbeln
              message_v2 = <fs_sales>-Txid
            ) TO lt_return.

            RAISE EXCEPTION TYPE zclca_msg_erro.
          ENDIF.

          CALL FUNCTION 'CALL_FB08'
            EXPORTING
              i_bukrs      = <fs_sales>-Bukrs
              i_belnr      = <fs_sales>-Doc_Adiantamento
              i_gjahr      = <fs_sales>-Ano_Adiantamento
              i_stgrd      = lc_stgrd
              i_voidr      = lc_monat_voidr
              i_monat      = lc_monat_voidr
            EXCEPTIONS
              not_possible = 1
              OTHERS       = 2.

          IF sy-subrc <> 0.
            IF NOT ( sy-msgid = lc_f5 AND sy-msgno = 361 ). "//Documento já estornado não exibe como erro
              APPEND VALUE #(
                type       = if_xo_const_message~error
                id         = sy-msgid
                number     = sy-msgno
                message_v1 = sy-msgv1
                message_v2 = sy-msgv2
                message_v3 = sy-msgv3
                message_v4 = sy-msgv4
              ) TO lt_return.
            ENDIF.
          ENDIF.

          IF NOT line_exists( lt_return[ type = if_xo_const_message~error ] ).
            lt_return = NEW zclsd_cancel_ped_vda( is_canc_ped_vda = VALUE #(
              mandante                = sy-mandt
              salesorder              = <fs_sales>-Vbeln
              salesdocumentrjcnreason = me->gv_param_motivo_recusa_pix
            )  )->process(  ).
          ENDIF.

        CATCH zclca_msg_erro.
      ENDTRY.

      IF line_exists( lt_return[ type = if_xo_const_message~error ] ). "#EC CI_STDSEQ
        DATA(lv_external_id) = CONV balnrext( <fs_sales>-Vbeln ).

        CALL FUNCTION 'ZFMCA_ADD_LOG'
          EXPORTING
            iv_ext_number = lv_external_id
            iv_object     = gc_log_object
            iv_subobject  = gc_log_subobj
            it_return     = lt_return.
      ELSE.
        <fs_solic_adiantamento>-status = gc_status-pix_expirado.
      ENDIF.

      CLEAR lt_return.
    ENDLOOP.

    "@ Update table
    me->persist( lt_adto_ov ).
  ENDMETHOD.

  METHOD lock_object.

    CALL FUNCTION 'ENQUEUE_EZ_PGANTECIPADO'
      EXPORTING
        mode_ztfi_adto_ov = 'X'
        mandt             = sy-mandt
        vbeln             = iv_salesorder
        x_vbeln           = space
        _scope            = '2'
        _wait             = space
        _collect          = ' '
      EXCEPTIONS
        foreign_lock      = 1
        system_failure    = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      RAISE object_already_locked.
    ENDIF.

  ENDMETHOD.

  METHOD unlock_object.

    CALL FUNCTION 'DEQUEUE_EZ_PGANTECIPADO'
      EXPORTING
        mode_ztfi_adto_ov = 'X'
        mandt             = sy-mandt
        vbeln             = iv_salesorder
        x_vbeln           = space
        _scope            = '3'
        _synchron         = space
        _collect          = ' '.

  ENDMETHOD.

  METHOD persist.

    LOOP AT it_adto_ov ASSIGNING FIELD-SYMBOL(<Fs_adto_ov>).
      me->unlock_object( <Fs_adto_ov>-vbeln ).
    ENDLOOP.

    "@ Update table
    MODIFY ztfi_adto_ov FROM TABLE it_adto_ov.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

ENDCLASS.
