CLASS zclfi_van_ban_retorno DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider .
    INTERFACES if_xo_const_message.
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Leitura de filtros para aplicativo fiori
    "! it_filters | Lista de seleções
    METHODS get_filters
      IMPORTING
        it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    DATA gv_variant       TYPE variant.

ENDCLASS.



CLASS ZCLFI_VAN_BAN_RETORNO IMPLEMENTATION.


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
                DATA(lo_van) = NEW zclfi_van_bancaria(  ).

                lo_van->processa_ret_extrato(
                  EXPORTING
                    iv_variant = gv_variant
                  RECEIVING
                    rt_return  = DATA(lt_return)
                ).

                IF lo_van->gv_seqnr > 20.
                  IF line_exists( lt_return[ msgty = if_xo_const_message~error ] )."#EC CI_STDSEQ
                    MESSAGE ID zclfi_van_bancaria=>gc_msgid TYPE if_xo_const_message~error NUMBER 025 INTO DATA(lv_dummy).

                    lt_return = VALUE #( (
                      variant = gv_variant
                      data    = sy-datum
                      time    = sy-uzeit
                      files   = |{ TEXT-001 } { lo_van->gv_seqnr }|
                      msgty   = if_xo_const_message~error
                      msgid   = zclfi_van_bancaria=>gc_msgid
                      msgno   = 025
                      seqnr   = 1
                      message = lv_dummy
                    ) ).
                  ELSE.
                    MESSAGE ID zclfi_van_bancaria=>gc_msgid TYPE if_xo_const_message~error NUMBER 026 INTO lv_dummy.

                    lt_return = VALUE #( (
                      variant = gv_variant
                      data    = sy-datum
                      time    = sy-uzeit
                      files   = |{ TEXT-001 } { lo_van->gv_seqnr }|
                      msgty   = if_xo_const_message~success
                      msgid   = zclfi_van_bancaria=>gc_msgid
                      msgno   = 026
                      seqnr   = 1
                      message = lv_dummy
                    ) ).
                  ENDIF.
                ENDIF.

              ENDIF.

              io_response->set_total_number_of_records( CONV #( lo_van->gv_seqnr ) ).
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

      gv_variant     =  VALUE #( <fs_filters>-range[ 1 ]-low OPTIONAL ).


    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
