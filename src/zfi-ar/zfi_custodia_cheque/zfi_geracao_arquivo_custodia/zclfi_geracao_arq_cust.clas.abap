CLASS zclfi_geracao_arq_cust DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider.

    TYPES: BEGIN OF ty_filters,
             bukrs   TYPE RANGE OF bukrs,
             kunnr   TYPE RANGE OF kunnr,
             ncheque TYPE RANGE OF ze_ncheque,
             tipo    TYPE RANGE OF char3,
           END OF ty_filters.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: gs_filters    TYPE ty_filters.

    METHODS build
      IMPORTING
                it_filters    TYPE if_rap_query_filter=>tt_name_range_pairs
      RETURNING VALUE(rv_pdf) TYPE ze_stream_arq.

    METHODS get_data
      EXPORTING
        et_cheque TYPE zctgfi_relat_cheque.

    METHODS get_filters
      IMPORTING it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    METHODS get_adobeform
      IMPORTING
                it_cheque            TYPE zctgfi_relat_cheque
      RETURNING VALUE(rt_formoutput) TYPE tfpcontent.

ENDCLASS.

CLASS zclfi_geracao_arq_cust IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA: lt_tab                   TYPE TABLE OF zi_fi_geracao_arq_cust_ce.
    DATA: lt_master_keys           TYPE cl_somu_form_services=>ty_gt_key.
    DATA: lv_content               TYPE  xstring.
    DATA: lo_cl_somu_form_services TYPE REF TO cl_somu_form_services,
          lt_keys                  TYPE cl_somu_form_services=>ty_gt_key.


    TRY.
        "Requested data
        IF io_request->is_data_requested(  ).

          "Paginacao
          DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
          DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
          DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited
                                      THEN 0 ELSE lv_page_size )  .

          "Recupera filtros
          TRY.
              TRY.
                  DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ). "#EC CI_CONV_OK
                  DATA(lt_parameters) = io_request->get_parameters( ).
                CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
                  DATA(lv_exp_msg) = lo_ex_filter->get_longtext( ).
              ENDTRY.
              "Busca os parametros da custom entity
*              DATA(lv_salesorder)     =   VALUE #( lt_parameters[ parameter_name =  'P_HANDLINGUNITEXTERNALID' ]-value OPTIONAL ).
              "Cria instancia

              me->get_filters( lt_filters ).

              DATA(lo_dados_txt) = zclfi_arq_cust_txt=>get_instance( ).

              IF 'TXT' IN gs_filters-tipo.
                lo_dados_txt->build(
                  EXPORTING
                    is_filters = gs_filters
                  IMPORTING
                    et_txt     = DATA(lt_txt)
                ).

                lt_tab = VALUE #( FOR ls_txt IN lt_txt ( txt = ls_txt ) ).

              ELSE.
                lt_tab = VALUE #( ( stream_data = me->build( it_filters = lt_filters ) ) ).
              ENDIF.
              io_response->set_total_number_of_records( 1 ).

*  " -------------- Send the response back to UI------------
              io_response->set_data( lt_tab ).

            CATCH cx_rap_query_filter_no_range INTO DATA(lv_range).
              DATA(lv_msg) = lv_range->get_text( ).
          ENDTRY.


        ENDIF.
      CATCH cx_rap_query_provider.
    ENDTRY.

  ENDMETHOD.

  METHOD build.
    DATA lt_formoutput TYPE tfpcontent.

    "CHECK gs_parameters IS NOT INITIAL.

    me->get_data(
      IMPORTING
        et_cheque   = DATA(lt_cheque)
    ).


    "CHECK ls_header IS NOT INITIAL AND lt_item[] IS NOT INITIAL.
    lt_formoutput = me->get_adobeform( it_cheque = lt_cheque ).

    CHECK lt_formoutput IS NOT INITIAL.

    rv_pdf = lt_formoutput[ 1 ].
*   rv_pdf = me->get_smartform( is_header = ls_header
*                               it_item   = lt_item ).

  ENDMETHOD.

  METHOD get_data.

    DATA: lv_total_vol      TYPE j_1bcte_vol_transp,
          lv_total_peso     TYPE ntgew_15,
          lv_numero_seq     TYPE char10,
          lv_transportadora TYPE char100.

*     Seleciona os itens da tabela
    SELECT bukrs        AS empresa,
           kunnr        AS cliente,
           ncheque      AS cheque,
           Doc          AS documento,
           Gjahr        AS exercicio,
*                       AS tipo_doc,
*                       AS atribuicao,
*                       AS referencia,
           budat        AS dt_lancamento,
*                       AS vencimento,
           valor        AS montante,
           moeda,
           status       AS status,
           Hktid        AS ag_cheque,
           Bankn        AS conta_corrente
        FROM zi_fi_geracao_arq_cust
        INTO CORRESPONDING FIELDS OF TABLE @et_cheque
        WHERE bukrs IN @gs_filters-bukrs
          AND kunnr IN @gs_filters-kunnr
          AND ncheque IN @gs_filters-ncheque.

  ENDMETHOD.

  METHOD get_filters.

    LOOP AT it_filters ASSIGNING FIELD-SYMBOL(<fs_filters>).

      CASE <fs_filters>-name.
        WHEN 'BUKRS'.
          gs_filters-bukrs = CORRESPONDING #( <fs_filters>-range ).

        WHEN 'KUNNR'.
          gs_filters-kunnr = CORRESPONDING #( <fs_filters>-range ).

        WHEN 'NCHEQUE'.
          gs_filters-ncheque = CORRESPONDING #( <fs_filters>-range ).
        WHEN 'TIPO'.
          gs_filters-tipo = CORRESPONDING #( <fs_filters>-range[] ).

      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_adobeform.

    DATA: lv_fm_name            TYPE rs38l_fnam,
          ls_fp_docparams       TYPE sfpdocparams,
          ls_fp_outputparams    TYPE sfpoutputparams,
          ls_control_parameters TYPE  ssfctrlop,
          lv_lines              TYPE i
          .

    DATA: lo_pdf_merger TYPE REF TO cl_rspo_pdf_merge.

    ls_fp_outputparams-dest     = 'LP01'.
    ls_fp_outputparams-device   = 'PRINTER'.
    ls_fp_outputparams-nodialog = abap_true.
    ls_fp_outputparams-getpdf   = 'M'.
    ls_fp_outputparams-bumode   = 'M'.
    ls_fp_outputparams-assemble = 'S'.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_fp_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    TRY.
*&---- Get the name of the generated function module
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME' ##FM_SUBRC_OK
          EXPORTING
            i_name     = 'ZAFFI_RELAT_CHEQUES'
          IMPORTING
            e_funcname = lv_fm_name.
      CATCH cx_fp_api_internal .
        RETURN.
      CATCH cx_fp_api_repository  .
        RETURN.
      CATCH cx_fp_api_usage .
        RETURN.
    ENDTRY.

    ls_control_parameters-no_open  = abap_true.
    ls_control_parameters-no_close = abap_true.


    CALL FUNCTION lv_fm_name
      EXPORTING
        /1bcdwb/docparams  = ls_fp_docparams
        control_parameters = ls_control_parameters
        gt_relat_cheques   = it_cheque
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

*&---- Close the spool job
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    CALL FUNCTION 'FP_GET_PDF_TABLE'
      IMPORTING
        e_pdf_table = rt_formoutput.

  ENDMETHOD.

ENDCLASS.
