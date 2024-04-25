CLASS zclfi_solic_pgto_ant_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zclfi_solic_pgto_ant_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS solicitacaoset_create_entity
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zclfi_solic_pgto_ant_dpc_ext IMPLEMENTATION.


  METHOD solicitacaoset_create_entity.

    TRY.
        io_data_provider->read_entry_data(
          IMPORTING
            es_data = er_entity
        ).

        NEW zclfi_proc_pgto_antecipado( )->process(
          EXPORTING
            ir_txid         = VALUE #( ( sign = 'I' option = 'EQ' low = to_upper( er_entity-txid ) ) )
            is_payment_data = er_entity
          RECEIVING
            rt_return       = DATA(lt_return)
        ).

        SELECT SINGLE SalesOrder
          FROM zi_fi_ordens_pgto_antecipado
          INTO @DATA(lv_sales_order_ref)
         WHERE TransactionIdentification = @er_entity-txid.

        IF sy-subrc IS INITIAL.
          DATA(lv_chave_ref) = CONV char50( lv_sales_order_ref ).
        ENDIF.

        LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<Fs_ret>).
          MESSAGE ID <Fs_ret>-id TYPE <Fs_ret>-type NUMBER <fs_ret>-number WITH <Fs_ret>-message_v1 <Fs_ret>-message_v2 <Fs_ret>-message_v3 <Fs_ret>-message_v4
          INTO <Fs_ret>-message.
        ENDLOOP.

        DATA(lo_cpi_monitor) = NEW zclca_monitor_cpi( ).
        DATA(lv_payload)     = /ui2/cl_json=>serialize( data = er_entity ).
        DATA(lv_result)      = /ui2/cl_json=>serialize( data = lt_return ).

        lo_cpi_monitor->started_process(
          EXPORTING
            iv_processo  = zclfi_proc_pgto_antecipado=>gc_cpi-retorno_pagamento
            iv_metodo    = zclfi_proc_pgto_antecipado=>gc_cpi-method_post
            iv_json      = lv_payload
            iv_chave_ref = lv_chave_ref
        ).

        lo_cpi_monitor->save_log(
          EXPORTING
            iv_processo     = zclfi_proc_pgto_antecipado=>gc_cpi-retorno_pagamento
            iv_metodo       = zclfi_proc_pgto_antecipado=>gc_cpi-method_post
            iv_json_retorno = lv_result
            iv_json         = lv_payload
            it_return       = lt_return
        ).

        CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
          RECEIVING
            ro_message_container = DATA(lo_msg).

        lo_msg->add_messages_from_bapi(
          EXPORTING
            it_bapi_messages = lt_return
        ).

        IF line_Exists( lt_return[ type = zclfi_proc_pgto_antecipado=>if_xo_const_message~error ] ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exceptioN
            EXPORTING
              message_container = lo_msg.
        ENDIF.

      CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
