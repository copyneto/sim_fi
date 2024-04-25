" ***********************************************************************
" ***                           © REDE SIM                            ***
" ***********************************************************************
" ***                                                                   *
" *** DESCRIÇÃO: Cockpit Adiantamentos de Bombeio - Eventos             *
" *** AUTOR : Jong Silva – META                                         *
" *** FUNCIONAL: Lia Fantin Da Rocha – META                             *
" *** DATA : 19.02.2024                                                 *
" ***********************************************************************
" *** HISTÓRICO DAS MODIFICAÇÕES                                        *
" ***-------------------------------------------------------------------*
" *** DATA      | AUTOR             | DESCRIÇÃO                         *
" ***-------------------------------------------------------------------*
" ***           |                   |                                   *
" ***********************************************************************

CLASS zclfi_cockpit_bombeio_event DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_behv
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_cds,
        bombeio                 TYPE string VALUE 'BOMBEIO'         ##NO_TEXT,  " Bombeio
        solicitacaoadiantamento TYPE string VALUE 'BOMBEIO_SOL_ADI' ##NO_TEXT,  " Bombeio - Solicitação de  Adiantamento
      END OF gc_cds,

      BEGIN OF gc_field,
        HouseBank             TYPE string    VALUE 'HOUSEBANK'     ##NO_TEXT,
        PaymentMethod         TYPE string    VALUE 'PAYMENTMETHOD' ##NO_TEXT,
        NetDueDate            TYPE string    VALUE 'NETDUEDATE'    ##NO_TEXT,
        Amount                TYPE string    VALUE 'AMOUNT'        ##NO_TEXT,
        PaymentBlockingReason TYPE fieldname VALUE 'ZLSPR'         ##NO_TEXT,
      END OF gc_field,

      BEGIN OF gc_bloqueio_pagamento,
        disponivel TYPE bseg-zlspr VALUE ''  ##NO_TEXT,                 " Dispon.p/pagamento
        bloqueio   TYPE bseg-zlspr VALUE 'A' ##NO_TEXT,                 " Bloqueado p/pgto.
      END OF gc_bloqueio_pagamento,

      BEGIN OF gc_documento_estorno,
        desconhecido  TYPE bkpf-xreversal VALUE '' ##NO_TEXT,           " Desconhecido (doc.antigo do release <4.70 R/3 Enterprise)
        estornado     TYPE bkpf-xreversal VALUE '1' ##NO_TEXT,          " Documento estornado
        estorno       TYPE bkpf-xreversal VALUE '2' ##NO_TEXT,          " Documento de estorno
        estornado_var TYPE bkpf-xreversal VALUE '3' ##NO_TEXT,          " Documento estornado
        estorno_var   TYPE bkpf-xreversal VALUE '4' ##NO_TEXT,          " Documento de estorno
      END OF gc_documento_estorno,

      BEGIN OF gc_motivo_estorno,
        lancamento_incorreto TYPE bkpf-stgrd VALUE '01' ##NO_TEXT,      " Lançamento incorreto
        periodo_incorreto    TYPE bkpf-stgrd VALUE '02' ##NO_TEXT,      " Período incorreto
        preco_cond_incorreto TYPE bkpf-stgrd VALUE '03' ##NO_TEXT,      " Preços e condições incorretos
        devolucao            TYPE bkpf-stgrd VALUE '04' ##NO_TEXT,      " Devolução
        estorno_automatico   TYPE bkpf-stgrd VALUE '05' ##NO_TEXT,      " Estorno automático
        estorno_mod_imob     TYPE bkpf-stgrd VALUE 'A0' ##NO_TEXT,      " Estorno modificação ClC imobilizado
      END OF gc_motivo_estorno,

      BEGIN OF gc_message,
        id             TYPE symsgid     VALUE 'ZFI_BOMBEIO',
        textformat_asc TYPE bapi_tfrmt  VALUE 'ASC',
        no_000         TYPE symsgno     VALUE '000',
        no_001         TYPE symsgno     VALUE '001',
        no_002         TYPE symsgno     VALUE '002',
        no_003         TYPE symsgno     VALUE '003',
        no_004         TYPE symsgno     VALUE '004',
        no_005         TYPE symsgno     VALUE '005',
        no_006         TYPE symsgno     VALUE '006',
        no_007         TYPE symsgno     VALUE '007',
        no_008         TYPE symsgno     VALUE '008',
        no_009         TYPE symsgno     VALUE '009',
        no_010         TYPE symsgno     VALUE '010',
        no_011         TYPE symsgno     VALUE '011',
        no_012         TYPE symsgno     VALUE '012',
        no_013         TYPE symsgno     VALUE '013',
        no_014         TYPE symsgno     VALUE '014',
        no_015         TYPE symsgno     VALUE '015',
        no_016         TYPE symsgno     VALUE '016',
        no_017         TYPE symsgno     VALUE '017',
        no_018         TYPE symsgno     VALUE '018',
        no_019         TYPE symsgno     VALUE '019',
        no_020         TYPE symsgno     VALUE '020',
        no_021         TYPE symsgno     VALUE '021',
        no_022         TYPE symsgno     VALUE '022',
        no_023         TYPE symsgno     VALUE '023',
        no_024         TYPE symsgno     VALUE '024',
        no_025         TYPE symsgno     VALUE '025',
        no_026         TYPE symsgno     VALUE '026',
        no_027         TYPE symsgno     VALUE '027',
        no_028         TYPE symsgno     VALUE '028',
        no_029         TYPE symsgno     VALUE '029',
        no_030         TYPE symsgno     VALUE '030',
        no_031         TYPE symsgno     VALUE '031',
        no_032         TYPE symsgno     VALUE '032',
        no_033         TYPE symsgno     VALUE '033',
        no_034         TYPE symsgno     VALUE '034',
        no_035         TYPE symsgno     VALUE '035',
        no_036         TYPE symsgno     VALUE '036',
        no_037         TYPE symsgno     VALUE '037',
        no_038         TYPE symsgno     VALUE '038',
        no_039         TYPE symsgno     VALUE '039',
        no_040         TYPE symsgno     VALUE '040',
      END OF gc_message,

      BEGIN OF gc_range,
        include TYPE char01 VALUE 'I',
        equal   TYPE char02 VALUE 'EQ',
      END OF gc_range,

      BEGIN OF gc_transaction,
        bombeio TYPE string VALUE 'ZAPPBOMBEIO',
      END OF gc_transaction,

      BEGIN OF gc_reported,
        msg     TYPE string VALUE '%msg',
        element TYPE string VALUE '%element',
      END OF gc_reported.

    TYPES:
      ty_reported TYPE RESPONSE FOR REPORTED EARLY zi_fi_cockpit_bombeio,

      BEGIN OF ty_bkpf,
        bukrs      TYPE bkpf-bukrs,
        belnr      TYPE bkpf-belnr,
        gjahr      TYPE bkpf-gjahr,
        stblg      TYPE bkpf-stblg,
        stjah      TYPE bkpf-stjah,
        stgrd      TYPE bkpf-stgrd,
        xreversal  TYPE bkpf-xreversal,
        xreversing TYPE bkpf-xreversing,
        xreversed  TYPE bkpf-xreversed,
      END OF ty_bkpf,

      ty_t_bkpf TYPE SORTED TABLE OF ty_bkpf
                    WITH NON-UNIQUE KEY bukrs belnr gjahr,

      BEGIN OF ty_bseg,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
        buzei TYPE bseg-buzei,
        zlspr TYPE bseg-zlspr,
      END OF ty_bseg,

      ty_t_bseg TYPE SORTED TABLE OF ty_bseg
                    WITH NON-UNIQUE KEY bukrs belnr gjahr buzei.

    "! Recupera instância atual
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zclfi_cockpit_bombeio_event.

    "! Recupera dados do documento contábil
    METHODS get_documento_contabil
      IMPORTING it_key       TYPE fdm_t_bseg_key
                iv_bloqueado TYPE flag OPTIONAL
      EXPORTING et_bkpf      TYPE ty_t_bkpf
                et_bseg      TYPE ty_t_bseg.

    "! Botão - Liberar bloqueio de pagamento do documento de solicitação
    METHODS action_liberar_solicitacao
      IMPORTING it_key    TYPE fdm_t_bseg_key
      EXPORTING et_return TYPE bapiret2_t.

    "! Botão - Estornar documento de solicitação
    METHODS action_estornar_solicitacao
      IMPORTING it_key    TYPE fdm_t_bseg_key
      EXPORTING et_return TYPE bapiret2_t.

    "! Botão - Ajustar documento de solicitação
    METHODS action_ajustar_solicitacao
      IMPORTING it_key    TYPE fdm_t_bseg_key
                is_popup  TYPE zi_fi_bombeio_popup_ajuste_sol
      EXPORTING et_return TYPE bapiret2_t.

    "! Lógica para liberar bloqueio de pagamento do documento de solicitação
    METHODS event_liberar_solicitacao
      IMPORTING it_key    TYPE fdm_t_bseg_key
                iv_commit TYPE abap_boolean DEFAULT abap_true
      EXPORTING et_return TYPE bapiret2_t.

    "! Lógica para estornar documento de solicitação
    METHODS event_estornar_solicitacao
      IMPORTING it_key    TYPE fdm_t_bseg_key
                iv_commit TYPE abap_boolean DEFAULT abap_true
      EXPORTING et_return TYPE bapiret2_t.

    "! Lógica para ajustar documento de solicitação
    METHODS event_ajustar_solicitacao
      IMPORTING it_key    TYPE fdm_t_bseg_key
                is_popup  TYPE zi_fi_bombeio_popup_ajuste_sol
                iv_commit TYPE abap_boolean DEFAULT abap_true
      EXPORTING et_key    TYPE fins_t_bkpf_key
                et_return TYPE bapiret2_t.

    "! Valida os campos informados no pop-up
    METHODS valida_ajustar_solicitacao
      IMPORTING it_key    TYPE fdm_t_bseg_key
                is_popup  TYPE zi_fi_bombeio_popup_ajuste_sol
      EXPORTING et_return TYPE bapiret2_t.

    "! Constrói mensagens retorno do aplicativo
    METHODS build_reported
      IMPORTING
        !it_return   TYPE bapiret2_t
      EXPORTING
        !es_reported TYPE ty_reported.

    "! Prepara mensagens de retorno
    METHODS prepare_return
      CHANGING ct_return TYPE bapiret2_t.

    "! Método para gerenciar retorno de chamadas RFC
    METHODS setup_messages
      IMPORTING
        !p_task TYPE clike.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA: go_instance  TYPE REF TO zclfi_cockpit_bombeio_event.

    DATA: gv_wait   TYPE abap_boolean,
          gt_return TYPE bapiret2_t.

ENDCLASS.



CLASS zclfi_cockpit_bombeio_event IMPLEMENTATION.

  METHOD get_instance.

* ---------------------------------------------------------------------------
* Caso a instância não exista, criar uma nova
* ---------------------------------------------------------------------------
    IF NOT go_instance IS BOUND.
      go_instance = NEW zclfi_cockpit_bombeio_event( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD get_documento_contabil.

    DATA: lr_zlspr TYPE RANGE OF bseg-zlspr.

    FREE: et_bkpf, et_bseg.

* ---------------------------------------------------------------------------
* Se parâmetro informado, recuperar apenas os documentos bloqueados
* ---------------------------------------------------------------------------
    IF iv_bloqueado IS NOT INITIAL.
      lr_zlspr = VALUE #( ( sign = gc_range-include option = gc_range-equal low = gc_bloqueio_pagamento-bloqueio ) ).
    ENDIF.

* ---------------------------------------------------------------------------
* Recupera os documentos de contabilidade
* ---------------------------------------------------------------------------
    IF it_key IS NOT INITIAL.

      " Cabeçalho do documento contabilidade financeira
      SELECT bukrs, belnr, gjahr,
             stblg, stjah, stgrd, xreversal, xreversing, xreversed
          FROM bkpf
          FOR ALL ENTRIES IN @it_key
          WHERE bukrs EQ @it_key-bukrs
            AND belnr EQ @it_key-belnr
            AND gjahr EQ @it_key-gjahr
          INTO CORRESPONDING FIELDS OF TABLE @et_bkpf.

      IF sy-subrc NE 0.
        FREE et_bkpf.
      ENDIF.
    ENDIF.

    IF it_key IS NOT INITIAL.

      " Segmento do documento contabilidade financeira
      SELECT bukrs, belnr, gjahr, buzei, zlspr
          FROM bseg
          FOR ALL ENTRIES IN @it_key
          WHERE bukrs EQ @it_key-bukrs
            AND belnr EQ @it_key-belnr
            AND gjahr EQ @it_key-gjahr
            AND buzei EQ @it_key-buzei
            AND zlspr IN @lr_zlspr
          INTO CORRESPONDING FIELDS OF TABLE @et_bseg.

      IF sy-subrc NE 0.
        FREE et_bseg.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD action_liberar_solicitacao.

    FREE: et_return, gt_return, gv_wait.

* ----------------------------------------------------------------------
* Realiza a liberação dos documentos contábeis com bloqueio de pagamento
* ----------------------------------------------------------------------
    CALL FUNCTION 'ZFMFI_BOMBEIO_LIBERAR_SOLICITA'
      STARTING NEW TASK 'BOMBEIO_LIBERAR_SOLICITA'
      CALLING setup_messages ON END OF TASK
      EXPORTING
        it_key = it_key.

    WAIT UNTIL gv_wait EQ abap_true.
    et_return = gt_return.

    me->prepare_return( CHANGING ct_return = et_return ).

  ENDMETHOD.


  METHOD action_estornar_solicitacao.

    FREE: et_return, gt_return, gv_wait.

* ----------------------------------------------------------------------
* Realiza a liberação dos documentos contábeis com bloqueio de pagamento
* ----------------------------------------------------------------------
    CALL FUNCTION 'ZFMFI_BOMBEIO_ESTORNA_SOLICITA'
      STARTING NEW TASK 'BOMBEIO_ESTORNA_SOLICITA'
      CALLING setup_messages ON END OF TASK
      EXPORTING
        it_key = it_key.

    WAIT UNTIL gv_wait EQ abap_true.
    et_return = gt_return.

    me->prepare_return( CHANGING ct_return = et_return ).

  ENDMETHOD.


  METHOD action_ajustar_solicitacao.

    FREE: et_return, gt_return, gv_wait.

* ----------------------------------------------------------------------
* Valida dados do pop-up
* ----------------------------------------------------------------------
    me->valida_ajustar_solicitacao( EXPORTING it_key    = it_key
                                              is_popup  = is_popup
                                    IMPORTING et_return = et_return ).

    IF et_return IS NOT INITIAL.
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
* Realiza processo de ajuste da solicitação de adiantamento
* ----------------------------------------------------------------------
    CALL FUNCTION 'ZFMFI_BOMBEIO_AJUSTAR_SOLICITA'
      STARTING NEW TASK 'BOMBEIO_AJUSTAR_SOLICITA'
      CALLING setup_messages ON END OF TASK
      EXPORTING
        it_key   = it_key
        is_popup = is_popup.

    WAIT UNTIL gv_wait EQ abap_true.
    et_return = gt_return.

    me->prepare_return( CHANGING ct_return = et_return ).

  ENDMETHOD.


  METHOD event_liberar_solicitacao.

    DATA: lt_accchg TYPE fdm_t_accchg,
          lt_return TYPE bapiret2_t.

    FREE: et_return.

* ---------------------------------------------------------------------------
* Recupera os documentos de contabilidade (somente os bloqueados)
* ---------------------------------------------------------------------------
    me->get_documento_contabil( EXPORTING it_key       = it_key
                                          iv_bloqueado = abap_true
                                IMPORTING et_bkpf      = DATA(lt_bkpf)
                                          et_bseg      = DATA(lt_bseg) ).

* ---------------------------------------------------------------------------
* Realiza a liberação dos documentos contábeis com bloqueio de pagamento
* ---------------------------------------------------------------------------
    LOOP AT it_key REFERENCE INTO DATA(ls_key).

      READ TABLE lt_bkpf REFERENCE INTO DATA(ls_bkpf) WITH TABLE KEY bukrs = ls_key->bukrs
                                                                     belnr = ls_key->belnr
                                                                     gjahr = ls_key->gjahr.
      IF sy-subrc NE 0.
        " Documento &1 Empresa &2 Exercício &3 não encontrado.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_009
                                              message_v1 = |{ ls_key->belnr ALPHA = OUT }|
                                              message_v2 = ls_key->bukrs
                                              message_v3 = ls_key->gjahr ) ).
        CONTINUE.
      ENDIF.

      FREE: lt_return.

      LOOP AT lt_bseg REFERENCE INTO DATA(ls_bseg) WHERE bukrs = ls_key->bukrs
                                                     AND belnr = ls_key->belnr
                                                     AND gjahr = ls_key->gjahr.

        " Prepara para atualizar campo de bloqueio de pagamento para 'Disponível'.
        lt_accchg = VALUE #( ( fdname = gc_field-PaymentBlockingReason
                               oldval = ls_bseg->zlspr
                               newval = abap_false ) ).

        " Preenchemos este campo para pular a validação da EXIT.
        sy-tcode = gc_transaction-bombeio.

        " Atualiza documento contábil (por item)
        CALL FUNCTION 'FI_DOCUMENT_CHANGE'
          EXPORTING
            i_buzei              = ls_bseg->buzei
            i_bukrs              = ls_bseg->bukrs
            i_belnr              = ls_bseg->belnr
            i_gjahr              = ls_bseg->gjahr
            i_upd_fqm            = abap_false
          TABLES
            t_accchg             = lt_accchg
          EXCEPTIONS
            no_reference         = 1
            no_document          = 2
            many_documents       = 3
            wrong_input          = 4
            overwrite_creditcard = 5
            OTHERS               = 6.

        IF sy-subrc NE 0.
          lt_return = VALUE #( BASE lt_return ( type       = sy-msgty
                                                id         = sy-msgid
                                                number     = sy-msgno
                                                message_v1 = sy-msgv1
                                                message_v2 = sy-msgv2
                                                message_v3 = sy-msgv3
                                                message_v4 = sy-msgv4 ) ).
        ENDIF.

      ENDLOOP.

* ---------------------------------------------------------------------------
* Cenário 1 - Todos os itens já liberados.
* ---------------------------------------------------------------------------
      IF sy-subrc NE 0.

        " Pgto já liberado para Doc. Sol. Adiantamento &1 Empresa &2 Exercício &3.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>info
                                              id         = gc_message-id
                                              number     = gc_message-no_008
                                              message_v1 = |{ ls_key->belnr ALPHA = OUT }|
                                              message_v2 = ls_key->bukrs
                                              message_v3 = ls_key->gjahr ) ).

      ENDIF.

* ---------------------------------------------------------------------------
* Cenário 2 - Erro durante a liberação do bloqueio de pagamento.
* ---------------------------------------------------------------------------
      IF lt_return IS NOT INITIAL.

        INSERT LINES OF lt_return INTO TABLE et_return.
        " Falha ao liberar bloqueio pgto p/ Sol. Adiantam. &1 Empresa &2 Exerc. &3.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_006
                                              message_v1 = |{ ls_key->belnr ALPHA = OUT }|
                                              message_v2 = ls_key->bukrs
                                              message_v3 = ls_key->gjahr ) ).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

* ---------------------------------------------------------------------------
* Cenário 3 - Liberação realizado com sucesso.
* ---------------------------------------------------------------------------
      ELSE.

        " Pgto Liberado para Doc. Sol. Adiantamento &1 Empresa &2 Exercício &3.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>success
                                              id         = gc_message-id
                                              number     = gc_message-no_007
                                              message_v1 = |{ ls_key->belnr ALPHA = OUT }|
                                              message_v2 = ls_key->bukrs
                                              message_v3 = ls_key->gjahr ) ).

        IF iv_commit EQ abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

      ENDIF.

    ENDLOOP.

    me->prepare_return( CHANGING ct_return = et_return ).

  ENDMETHOD.


  METHOD event_estornar_solicitacao.

    DATA: lt_return TYPE bapiret2_t,
          lv_budat  TYPE budat,
          lv_monat  TYPE monat.

    FREE: et_return.

* ---------------------------------------------------------------------------
* Recupera os documentos de contabilidade
* ---------------------------------------------------------------------------
    me->get_documento_contabil( EXPORTING it_key       = it_key
                                IMPORTING et_bkpf      = DATA(lt_bkpf)
                                          et_bseg      = DATA(lt_bseg) ).

* ---------------------------------------------------------------------------
* Inicia processo de estorno
* ---------------------------------------------------------------------------
    LOOP AT it_key REFERENCE INTO DATA(ls_key).

      FREE: lt_return.

      READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH TABLE KEY bukrs = ls_key->bukrs
                                                           belnr = ls_key->belnr
                                                           gjahr = ls_key->gjahr.
      IF sy-subrc NE 0.
        " Documento &1 Empresa &2 Exercício &3 não encontrado.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_009
                                              message_v1 = |{ ls_key->belnr ALPHA = OUT }|
                                              message_v2 = ls_key->bukrs
                                              message_v3 = ls_key->gjahr ) ).
        CONTINUE.
      ENDIF.

      " Verifica se o documento atual é um "Documento estornado"
      IF ls_bkpf-stblg IS NOT INITIAL
      AND ( ls_bkpf-xreversal EQ gc_documento_estorno-desconhecido
         OR ls_bkpf-xreversal EQ gc_documento_estorno-estornado
         OR ls_bkpf-xreversal EQ gc_documento_estorno-estornado_var ).

        " Documento Sol. Adiantamento &1 já estornado por &2.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>info
                                              id         = gc_message-id
                                              number     = gc_message-no_010
                                              message_v1 = |{ ls_key->belnr ALPHA = OUT }/{ ls_key->bukrs }/{ ls_key->gjahr }|
                                              message_v2 = |{ ls_bkpf-stblg ALPHA = OUT }/{ ls_key->bukrs }/{ ls_bkpf-stjah }| ) ).
        CONTINUE.
      ENDIF.


      " Verifica se o documento atual é um "Documento de estorno"
      IF ls_bkpf-stblg IS NOT INITIAL
      AND ( ls_bkpf-xreversal EQ gc_documento_estorno-estorno
         OR ls_bkpf-xreversal EQ gc_documento_estorno-estorno_var ).

        " Documento &1 é um documento de estorno para Sol. Adiantamento &2.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>info
                                              id         = gc_message-id
                                              number     = gc_message-no_011
                                              message_v1 = |{ ls_bkpf-stblg ALPHA = OUT }/{ ls_key->bukrs }/{ ls_bkpf-stjah }|
                                              message_v2 = |{ ls_key->belnr ALPHA = OUT }/{ ls_key->bukrs }/{ ls_key->gjahr }| ) ).
        CONTINUE.
      ENDIF.

      " Realiza processo de estorno do Doc. Sol. Adiantamento
      CALL FUNCTION 'CALL_FB08'
        EXPORTING
          i_bukrs      = ls_key->bukrs
          i_belnr      = ls_key->belnr
          i_gjahr      = ls_key->gjahr
          i_stgrd      = gc_motivo_estorno-lancamento_incorreto
        IMPORTING
          e_budat      = lv_budat
          e_monat      = lv_monat
        EXCEPTIONS
          not_possible = 1
          OTHERS       = 2.

      IF sy-subrc NE 0.
        lt_return = VALUE #( BASE lt_return ( type       = sy-msgty
                                              id         = sy-msgid
                                              number     = sy-msgno
                                              message_v1 = sy-msgv1
                                              message_v2 = sy-msgv2
                                              message_v3 = sy-msgv3
                                              message_v4 = sy-msgv4 ) ).
      ENDIF.

* ---------------------------------------------------------------------------
* Cenário 1 - Erro durante o estorno do documento
* ---------------------------------------------------------------------------
      IF lt_return IS NOT INITIAL.

        INSERT LINES OF lt_return INTO TABLE et_return.

        " Falha ao estornar Doc. Sol. Adiantamento &1 Empresa &2 Exercício &3.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_018
                                              message_v1 = |{ ls_key->belnr ALPHA = OUT }|
                                              message_v2 = ls_key->bukrs
                                              message_v3 = ls_key->gjahr ) ).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

* ---------------------------------------------------------------------------
* Cenário 2 - Estorno realizado com sucesso.
* ---------------------------------------------------------------------------
      ELSE.

        " Estornado Doc. Sol. Adiantamento &1 Empresa &2 Exercício &3.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>success
                                              id         = gc_message-id
                                              number     = gc_message-no_017
                                              message_v1 = |{ ls_key->belnr ALPHA = OUT }|
                                              message_v2 = ls_key->bukrs
                                              message_v3 = ls_key->gjahr ) ).

        IF iv_commit EQ abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

      ENDIF.

    ENDLOOP.

    me->prepare_return( CHANGING ct_return = et_return ).

  ENDMETHOD.


  METHOD event_ajustar_solicitacao.

    DATA: lt_bseg_f47 TYPE zclfi_lanc_f47=>ty_t_bseg,
          ls_bkpf_f47 TYPE zclfi_lanc_f47=>ty_bkpf,
          lv_obj_type TYPE awtyp,
          lv_obj_key  TYPE awkey,
          lv_obj_sys  TYPE awsys.

    FREE: et_return, et_key.

* ---------------------------------------------------------------------------
* Recupera os documentos de contabilidade
* ---------------------------------------------------------------------------
    me->get_documento_contabil( EXPORTING it_key       = it_key
                                IMPORTING et_bkpf      = DATA(lt_bkpf)
                                          et_bseg      = DATA(lt_bseg) ).

    DATA(lo_lanc_f47) = NEW zclfi_lanc_f47( ).

* ---------------------------------------------------------------------------
* OBS: Devido à algumas limitações, no procesos de ajuste devemos estornar
*      a solicitação de adiantamento (original) e criar uma (nova) solicitação
* ---------------------------------------------------------------------------
    LOOP AT it_key REFERENCE INTO DATA(ls_key).

* ---------------------------------------------------------------------------
* Realiza processo de criação do Doc. Sol. Adiantamento (novo)
* ---------------------------------------------------------------------------
      ls_bkpf_f47 = VALUE #( bukrs = ls_key->bukrs ).

      lt_bseg_f47 = VALUE #( ( dmbtr = is_popup-AmountNew
                               hbkid = is_popup-HouseBank
                               zlsch = is_popup-PaymentMethod
                               zfbdt = is_popup-NetDueDateNew ) ).

      " OBS: Temos que realizar um commit durante a criação.
      "      A chamada posterior do estorno (FB08) faz um Rollback da criação deste documento
      lo_lanc_f47->criar_documento( EXPORTING is_bkpf   = ls_bkpf_f47
                                              it_bseg   = lt_bseg_f47
                                              iv_commit = abap_true
                                              iv_ballog = abap_false
                                    IMPORTING es_key    = DATA(ls_new_key)
                                              et_return = DATA(lt_return_s) ).

      et_key = VALUE #( BASE et_key ( ls_new_key ) ).

* ---------------------------------------------------------------------------
* Realiza processo de estorno do Doc. Sol. Adiantamento (original)
* ---------------------------------------------------------------------------
      IF NOT line_exists( lt_return_s[ type = if_xo_const_message=>error ] ) .

        me->event_estornar_solicitacao( EXPORTING it_key    = VALUE #( ( ls_key->* ) )
                                                  iv_commit = abap_false
                                        IMPORTING et_return = DATA(lt_return_e) ).

      ENDIF.

* ---------------------------------------------------------------------------
* Se o estorno do Doc. Sol. Adiantamento (original) falhar, temos que estornar
* o Doc. Sol. Adiantamento (novo)
* ---------------------------------------------------------------------------
      IF line_exists( lt_return_e[ type = if_xo_const_message=>error ] ).

        me->event_estornar_solicitacao( EXPORTING it_key    = VALUE #( ( CORRESPONDING #( ls_new_key ) ) )
                                                  iv_commit = abap_false
                                        IMPORTING et_return = DATA(lt_return_se) ).

      ENDIF.

* ---------------------------------------------------------------------------
* Cenário 1 - Erro durante criação da nova solicitação
* ---------------------------------------------------------------------------
      IF line_exists( lt_return_s[ type = if_xo_const_message=>error ] )
      OR line_exists( lt_return_e[ type = if_xo_const_message=>error ] ).

        INSERT LINES OF lt_return_e  INTO TABLE et_return.
        INSERT LINES OF lt_return_s  INTO TABLE et_return.
        INSERT LINES OF lt_return_se INTO TABLE et_return.

        " Falha ao criar Doc. Sol. Adiantamento.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>error
                                              id         = gc_message-id
                                              number     = gc_message-no_016 ) ).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

* ---------------------------------------------------------------------------
* Cenário 2 - Nova solicitação criada com sucesso
* ---------------------------------------------------------------------------
      ELSE.

        " Adiciona mensagem do processo de estorno
        INSERT LINES OF lt_return_e INTO TABLE et_return.

        " Criado Doc. Sol. Adiantamento &1 Empresa &2 Exercício &3.
        et_return = VALUE #( BASE et_return ( type       = if_xo_const_message=>success
                                              id         = gc_message-id
                                              number     = gc_message-no_015
                                              message_v1 = |{ ls_new_key-belnr ALPHA = OUT }|
                                              message_v2 = ls_new_key-bukrs
                                              message_v3 = ls_new_key-gjahr ) ).

        IF iv_commit EQ abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

      ENDIF.

    ENDLOOP.

    me->prepare_return( CHANGING ct_return = et_return ).

  ENDMETHOD.


  METHOD valida_ajustar_solicitacao.

    FREE: et_return.

* ----------------------------------------------------------------------
* Valida campo informado: Banco da Empresa
* ----------------------------------------------------------------------

    IF is_popup-HouseBank IS INITIAL.
      " Favor informar Banco da Empresa.
      et_return = VALUE #( BASE et_return ( type = if_xo_const_message=>info id = gc_message-id number = gc_message-no_028 ) ).
    ENDIF.

    IF is_popup-HouseBank IS NOT INITIAL.

      DATA(lt_key) = it_key.
      SORT lt_key BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_key COMPARING bukrs.

      IF lt_key IS NOT INITIAL.

        SELECT bukrs, hbkid, banks, bankl, text1
             FROM zi_fi_vh_bank_id
             FOR ALL ENTRIES IN @lt_key
             WHERE bukrs = @lt_key-bukrs
               AND hbkid = @is_popup-HouseBank
             INTO TABLE @DATA(lt_banks).

        IF sy-subrc NE 0.
          " Banco &1 não existe para a(s) empresa(s) selecionada(s).
          et_return = VALUE #( BASE et_return ( type = if_xo_const_message=>info id = gc_message-id number = gc_message-no_034 message_v1 = is_popup-HouseBank ) ).
        ENDIF.
      ENDIF.
    ENDIF.

* ----------------------------------------------------------------------
* Valida campo informado: Forma de Pagamento
* ----------------------------------------------------------------------

    IF is_popup-PaymentMethod IS INITIAL.
      " Favor informar Forma de Pagamento.
      et_return = VALUE #( BASE et_return ( type = if_xo_const_message=>info id = gc_message-id number = gc_message-no_029 ) ).
    ENDIF.

    IF is_popup-PaymentMethod IS NOT INITIAL.

      SELECT SINGLE zlsch, text1
          FROM zi_fi_vh_pymt_meth
          WHERE zlsch = @is_popup-PaymentMethod
          INTO @DATA(ls_pymt_meth).

      IF sy-subrc NE 0.
        " Forma de pagamento &1 não existe.
        et_return = VALUE #( BASE et_return ( type = if_xo_const_message=>info id = gc_message-id number = gc_message-no_035 message_v1 = is_popup-PaymentMethod ) ).
      ENDIF.
    ENDIF.

* ----------------------------------------------------------------------
* Valida campo informado: Novo Vencimento
* ----------------------------------------------------------------------

    IF is_popup-NetDueDateNew IS INITIAL.
      " Favor informar Novo Vencimento.
      et_return = VALUE #( BASE et_return ( type = if_xo_const_message=>info id = gc_message-id number = gc_message-no_030 ) ).
    ENDIF.

    IF is_popup-NetDueDateNew < sy-datum.
      " Vencimento informado está com uma data no passado.
      et_return = VALUE #( BASE et_return ( type = if_xo_const_message=>info id = gc_message-id number = gc_message-no_033 ) ).
    ENDIF.

* ----------------------------------------------------------------------
* Valida campo informado: Novo Montante
* ----------------------------------------------------------------------

    IF is_popup-AmountNew IS INITIAL.
      " Favor informar Novo Montante.
      et_return = VALUE #( BASE et_return ( type = if_xo_const_message=>info id = gc_message-id number = gc_message-no_031 ) ).
    ENDIF.

    IF is_popup-AmountNew < 0.
      " Montante informado com valor negativo.
      et_return = VALUE #( BASE et_return ( type = if_xo_const_message=>info id = gc_message-id number = gc_message-no_032 ) ).
    ENDIF.

  ENDMETHOD.


  METHOD prepare_return.

* ----------------------------------------------------------------------
* Prepara mensagem de retorno
* ----------------------------------------------------------------------
    LOOP AT ct_return REFERENCE INTO DATA(ls_return).

      CHECK ls_return->message IS INITIAL.

      TRY.
          CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
            EXPORTING
              id         = ls_return->id
              number     = ls_return->number
              textformat = gc_message-textformat_asc
              message_v1 = ls_return->message_v1
              message_v2 = ls_return->message_v2
              message_v3 = ls_return->message_v3
              message_v4 = ls_return->message_v4
            IMPORTING
              message    = ls_return->message.
        CATCH cx_root.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD setup_messages.

    CASE p_task.

      WHEN 'BOMBEIO_LIBERAR_SOLICITA'.

        RECEIVE RESULTS FROM FUNCTION 'ZFMFI_BOMBEIO_LIBERAR_SOLICITA'
         IMPORTING
           et_return      = gt_return.

      WHEN 'BOMBEIO_ESTORNA_SOLICITA'.

        RECEIVE RESULTS FROM FUNCTION 'ZFMFI_BOMBEIO_ESTORNA_SOLICITA'
         IMPORTING
           et_return      = gt_return.

      WHEN 'BOMBEIO_AJUSTAR_SOLICITA'.

        RECEIVE RESULTS FROM FUNCTION 'ZFMFI_BOMBEIO_AJUSTAR_SOLICITA'
         IMPORTING
           et_return      = gt_return.

    ENDCASE.

    gv_wait = abap_true.

  ENDMETHOD.


  METHOD build_reported.

    DATA: lo_dataref         TYPE REF TO data,
          ls_bombeio         TYPE zi_fi_cockpit_bombeio,
          ls_bombeio_sol_adi TYPE zi_fi_cockpit_bombeio_sol_adi.

    FIELD-SYMBOLS: <fs_cds>  TYPE any.

    FREE: es_reported.

    LOOP AT it_return INTO DATA(ls_return).

* ---------------------------------------------------------------------------
* Determina tipo de estrutura CDS
* ---------------------------------------------------------------------------
      CASE ls_return-parameter.
        WHEN gc_cds-bombeio.
          CREATE DATA lo_dataref TYPE LINE OF ty_reported-bombeio.
        WHEN gc_cds-solicitacaoadiantamento.
          CREATE DATA lo_dataref TYPE LINE OF ty_reported-solicitacaoadiantamento.
        WHEN OTHERS.
          CREATE DATA lo_dataref TYPE LINE OF ty_reported-bombeio.
      ENDCASE.

      ASSIGN lo_dataref->* TO <fs_cds>.

* ---------------------------------------------------------------------------
* Converte mensagem
* ---------------------------------------------------------------------------
      ASSIGN COMPONENT gc_reported-msg OF STRUCTURE <fs_cds> TO FIELD-SYMBOL(<fs_msg>).

      IF sy-subrc EQ 0.
        TRY.
            <fs_msg>  = new_message( id       = ls_return-id
                                     number   = ls_return-number
                                     v1       = ls_return-message_v1
                                     v2       = ls_return-message_v2
                                     v3       = ls_return-message_v3
                                     v4       = ls_return-message_v4
                                     severity = CONV #( ls_return-type ) ).
          CATCH cx_root.
        ENDTRY.
      ENDIF.

* ---------------------------------------------------------------------------
* Marca o campo com erro
* ---------------------------------------------------------------------------
      IF ls_return-field IS NOT INITIAL.
        ASSIGN COMPONENT |{ gc_reported-element }-{ ls_return-field }| OF STRUCTURE <fs_cds> TO FIELD-SYMBOL(<fs_field>).

        IF sy-subrc EQ 0.
          TRY.
              <fs_field> = if_abap_behv=>mk-on.
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ENDIF.

* ---------------------------------------------------------------------------
* Adiciona o erro na CDS correspondente
* ---------------------------------------------------------------------------
      CASE ls_return-parameter.
        WHEN gc_cds-bombeio.
          es_reported-bombeio[]                 = VALUE #( BASE es_reported-bombeio[] ( CORRESPONDING #( <fs_cds> ) ) ).
        WHEN gc_cds-solicitacaoadiantamento.
          es_reported-solicitacaoadiantamento[] = VALUE #( BASE es_reported-solicitacaoadiantamento[] ( CORRESPONDING #( <fs_cds> ) ) ).
        WHEN OTHERS.
          es_reported-bombeio[]                 = VALUE #( BASE es_reported-bombeio[] ( CORRESPONDING #( <fs_cds> ) ) ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
