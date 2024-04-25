CLASS zclfi_van_ban_remessa DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .

  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Leitura de filtros para aplicativo fiori
    "! it_filters | Lista de seleções
    METHODS get_filters
      IMPORTING
        it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    DATA gr_laufd  TYPE hrdepbsnvt_range_laufd.

ENDCLASS.

CLASS zclfi_van_ban_remessa IMPLEMENTATION.
  METHOD if_rap_query_provider~select.
    TRY.
        IF io_request->is_data_requested(  ).

          DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
          DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
          DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_page_size ) .



          TRY.
              TRY.
                  DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ). "#EC CI_CONV_OK
                CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
                  DATA(lv_exp_msg) = lo_ex_filter->get_longtext( ).
              ENDTRY.

              IF NOT lt_filters IS INITIAL.

                me->get_filters( lt_filters ).

                NEW zclfi_van_bancaria(  )->processa_remessa_pgto(
                                 EXPORTING
                                   ir_laufd  = gr_laufd
                                 RECEIVING
                                   rt_return = DATA(lt_return)
                               ).

              ENDIF.

              io_response->set_total_number_of_records( lines( lt_return ) ).
              io_response->set_data( lt_return ).

            CATCH cx_rap_query_filter_no_range INTO DATA(lv_range).
              DATA(lv_msg) = lv_range->get_text( ).
          ENDTRY.

        ENDIF.
      CATCH cx_rap_query_provider.
    ENDTRY.
  ENDMETHOD.

  METHOD get_filters.

    LOOP AT it_filters ASSIGNING FIELD-SYMBOL(<fs_filters>).

      gr_laufd     = CORRESPONDING #( <fs_filters>-range ).

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
