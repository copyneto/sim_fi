CLASS lcl_Bombeio DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Bombeio RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ Bombeio RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Bombeio.

    METHODS rba_Solicitacaoadiantamento FOR READ
      IMPORTING keys_rba FOR READ Bombeio\_Solicitacaoadiantamento FULL result_requested RESULT result LINK association_links.

    METHODS rba_Pedido FOR READ
      IMPORTING keys_rba FOR READ Bombeio\_Pedido FULL result_requested RESULT result LINK association_links.

    METHODS rba_Adiapagos FOR READ
      IMPORTING keys_rba FOR READ Bombeio\_Adiapagos FULL result_requested RESULT result LINK association_links.

    METHODS rba_Devolucaocancelamento FOR READ
      IMPORTING keys_rba FOR READ Bombeio\_Devolucaocancelamento FULL result_requested RESULT result LINK association_links.

    METHODS rba_Faturadrc FOR READ
      IMPORTING keys_rba FOR READ Bombeio\_Faturadrc FULL result_requested RESULT result LINK association_links.

    METHODS rba_Faturas FOR READ
      IMPORTING keys_rba FOR READ Bombeio\_Faturas FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_Bombeio IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD rba_Solicitacaoadiantamento.
    RETURN.
  ENDMETHOD.

  METHOD rba_Pedido.
    RETURN.
  ENDMETHOD.

  METHOD rba_Adiapagos.
    RETURN.
  ENDMETHOD.

  METHOD rba_Devolucaocancelamento.
    RETURN.
  ENDMETHOD.

  METHOD rba_Faturadrc.
    RETURN.
  ENDMETHOD.

  METHOD rba_Faturas.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_SolicitacaoAdiantamento DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ SolicitacaoAdiantamento RESULT result.

    METHODS rba_Bombeio FOR READ
      IMPORTING keys_rba FOR READ SolicitacaoAdiantamento\_Bombeio FULL result_requested RESULT result LINK association_links.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR SolicitacaoAdiantamento RESULT result.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR SolicitacaoAdiantamento RESULT result.

    METHODS ajustar FOR MODIFY
      IMPORTING keys FOR ACTION SolicitacaoAdiantamento~ajustar.

    METHODS estornar FOR MODIFY
      IMPORTING keys FOR ACTION SolicitacaoAdiantamento~estornar.

    METHODS liberar FOR MODIFY
      IMPORTING keys FOR ACTION SolicitacaoAdiantamento~liberar.

ENDCLASS.

CLASS lcl_SolicitacaoAdiantamento IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Bombeio.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.


  METHOD get_instance_features.

* ---------------------------------------------------------------------------
* Recupera todas as linhas selecionadas
* ---------------------------------------------------------------------------
    IF keys IS NOT INITIAL.

      SELECT *
          FROM zi_fi_cockpit_bombeio_sol_adi
          INTO TABLE @DATA(lt_bombeio_sol)
          FOR ALL ENTRIES IN @keys
          WHERE CompanyCode            = @keys-CompanyCode
            AND FiscalYear             = @keys-FiscalYear
            AND AccountingDocument     = @keys-AccountingDocument
            AND AccountingDocumentItem = @keys-AccountingDocumentItem.

      IF sy-subrc EQ 0.
        SORT lt_bombeio_sol BY CompanyCode FiscalYear AccountingDocument AccountingDocumentItem.
      ENDIF.
    ENDIF.

* ---------------------------------------------------------------------------
* Habilita/desabilita funcionalidades
* ---------------------------------------------------------------------------
    LOOP AT keys REFERENCE INTO DATA(ls_keys).

      READ TABLE lt_bombeio_sol REFERENCE INTO DATA(ls_bombeio_sol) WITH KEY CompanyCode            = ls_keys->CompanyCode
                                                                             FiscalYear             = ls_keys->FiscalYear
                                                                             AccountingDocument     = ls_keys->AccountingDocument
                                                                             AccountingDocumentItem = ls_keys->AccountingDocumentItem
                                                                             BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      result = VALUE #( BASE result (
                        %tky                   = ls_keys->%tky
                        CompanyCode            = ls_keys->CompanyCode
                        FiscalYear             = ls_keys->FiscalYear
                        AccountingDocument     = ls_keys->AccountingDocument
                        AccountingDocumentItem = ls_keys->AccountingDocumentItem

                        " Desabilitar quando não houver bloqueio
                        %action-liberar        = COND #( WHEN ls_bombeio_sol->PaymentBlockingReason IS NOT INITIAL
                                                         THEN if_abap_behv=>fc-o-enabled
                                                         ELSE if_abap_behv=>fc-o-disabled )

                        " Desabilitar quando documento é de estorno ou já foi estornado
                        %action-estornar       = COND #( WHEN ls_bombeio_sol->IsReversal IS INITIAL
                                                          AND ls_bombeio_sol->IsReversed IS INITIAL
                                                         THEN if_abap_behv=>fc-o-enabled
                                                         ELSE if_abap_behv=>fc-o-disabled )

                        %action-ajustar        = if_abap_behv=>fc-o-enabled

                        ) ).

    ENDLOOP.


  ENDMETHOD.


  METHOD liberar.

* ----------------------------------------------------------------------
* Prepara as chaves
* ----------------------------------------------------------------------
    DATA(lt_key)   = VALUE fdm_t_bseg_key( FOR ls_keys_ IN keys ( bukrs = ls_keys_-CompanyCode
                                                                  belnr = ls_keys_-AccountingDocument
                                                                  gjahr = ls_keys_-FiscalYear
                                                                  buzei = ls_keys_-AccountingDocumentItem
                                                                  ) ).

* ----------------------------------------------------------------------
* Realiza a liberação dos documentos contábeis com bloqueio de pagamento
* ----------------------------------------------------------------------
    DATA(lo_event) = zclfi_cockpit_bombeio_event=>get_instance( ).

    lo_event->action_liberar_solicitacao( EXPORTING it_key    = lt_key
                                          IMPORTING et_return = DATA(lt_return) ).

* ---------------------------------------------------------------------------
* Retornar mensagens
* ---------------------------------------------------------------------------
    lo_event->build_reported( EXPORTING it_return   = lt_return
                              IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD estornar.

* ----------------------------------------------------------------------
* Prepara as chaves
* ----------------------------------------------------------------------
    DATA(lt_key)   = VALUE fdm_t_bseg_key( FOR ls_keys_ IN keys ( bukrs = ls_keys_-CompanyCode
                                                                  belnr = ls_keys_-AccountingDocument
                                                                  gjahr = ls_keys_-FiscalYear
                                                                  buzei = ls_keys_-AccountingDocumentItem
                                                                  ) ).

* ----------------------------------------------------------------------
* Realiza a liberação dos documentos contábeis com bloqueio de pagamento
* ----------------------------------------------------------------------
    DATA(lo_event) = zclfi_cockpit_bombeio_event=>get_instance( ).

    lo_event->action_estornar_solicitacao( EXPORTING it_key    = lt_key
                                           IMPORTING et_return = DATA(lt_return) ).

* ---------------------------------------------------------------------------
* Retornar mensagens
* ---------------------------------------------------------------------------
    lo_event->build_reported( EXPORTING it_return   = lt_return
                              IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD ajustar.

* ----------------------------------------------------------------------
* Prepara as chaves
* ----------------------------------------------------------------------
    DATA(lt_key)   = VALUE fdm_t_bseg_key( FOR ls_keys_ IN keys ( bukrs = ls_keys_-CompanyCode
                                                                  belnr = ls_keys_-AccountingDocument
                                                                  gjahr = ls_keys_-FiscalYear
                                                                  buzei = ls_keys_-AccountingDocumentItem
                                                                  ) ).

    DATA(ls_popup) = VALUE zi_fi_bombeio_popup_ajuste_sol( keys[ 1 ]-%param OPTIONAL ).

* ----------------------------------------------------------------------
* Realiza a liberação dos documentos contábeis com bloqueio de pagamento
* ----------------------------------------------------------------------
    DATA(lo_event) = zclfi_cockpit_bombeio_event=>get_instance( ).

    lo_event->action_ajustar_solicitacao( EXPORTING it_key    = lt_key
                                                    is_popup  = ls_popup
                                          IMPORTING et_return = DATA(lt_return) ).

* ---------------------------------------------------------------------------
* Retornar mensagens
* ---------------------------------------------------------------------------
    lo_event->build_reported( EXPORTING it_return   = lt_return
                              IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_pedido DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ Pedido RESULT result.

    METHODS rba_Bombeio FOR READ
      IMPORTING keys_rba FOR READ Pedido\_Bombeio FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_pedido IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Bombeio.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_ZI_FI_COCKPIT_BOMBEIO DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_adiantamentopagamento DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ AdiantamentoPagamento RESULT result.

    METHODS rba_Bombeio FOR READ
      IMPORTING keys_rba FOR READ AdiantamentoPagamento\_Bombeio FULL result_requested RESULT result LINK association_links.

ENDCLASS.
CLASS lcl_devolucaocancelamento DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ DevolucaoCancelamento RESULT result.

    METHODS rba_Bombeio FOR READ
      IMPORTING keys_rba FOR READ DevolucaoCancelamento\_Bombeio FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_devolucaocancelamento IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Bombeio.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_adiantamentopagamento IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Bombeio.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_faturadrc DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ FaturaDRC RESULT result.

    METHODS rba_Bombeio FOR READ
      IMPORTING keys_rba FOR READ FaturaDRC\_Bombeio FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_faturas DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS read FOR READ
      IMPORTING keys FOR READ Faturas RESULT result.

    METHODS rba_Bombeio FOR READ
      IMPORTING keys_rba FOR READ Faturas\_Bombeio FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lcl_faturas IMPLEMENTATION.

  METHOD read.
  RETURN.
  ENDMETHOD.

  METHOD rba_Bombeio.
  RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_faturadrc IMPLEMENTATION.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD rba_Bombeio.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_ZI_FI_COCKPIT_BOMBEIO IMPLEMENTATION.

  METHOD finalize.
    RETURN.
  ENDMETHOD.

  METHOD check_before_save.
    RETURN.
  ENDMETHOD.

  METHOD save.
    RETURN.
  ENDMETHOD.

  METHOD cleanup.
    RETURN.
  ENDMETHOD.

  METHOD cleanup_finalize.
    RETURN.
  ENDMETHOD.

ENDCLASS.
