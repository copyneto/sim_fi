CLASS zclfi_rest_credit_seg_get DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zifca_rest .

    METHODS constructor
      IMPORTING
        !io_request  TYPE REF TO if_http_request
        !io_response TYPE REF TO if_http_response .
    METHODS get_data
      IMPORTING
        VALUE(io_request) TYPE REF TO if_http_request
      RETURNING
        VALUE(rs_data)    TYPE zsfi_credit_segment .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS validate_data
      IMPORTING
        iv_bp         TYPE bu_partner
        iv_seg_credit TYPE ukm_credit_sgmnt
      CHANGING
        cs_data       TYPE zsfi_credit_segment.
    METHODS create_json
      IMPORTING
        is_data          TYPE zsfi_credit_segment
      RETURNING
        VALUE(rv_result) TYPE string.
ENDCLASS.



CLASS zclfi_rest_credit_seg_get IMPLEMENTATION.


  METHOD constructor.

    me->zifca_rest~go_response = io_response.
    me->zifca_rest~go_request = io_request.

  ENDMETHOD.


  METHOD get_data.

***************************************************************************
    " VARIABLES
***************************************************************************
    DATA:
      lv_bp         TYPE bu_partner,
      lv_seg_credit TYPE ukm_credit_sgmnt,
      lt_fields     TYPE tihttpnvp,
      lv_taskname   TYPE char10,
      lv_code       TYPE i,
      lv_reason     TYPE string.

***************************************************************************
    " GET HEADER PARAMETERS VALUE FROM URL
***************************************************************************

    lv_taskname = sy-index.

    CALL FUNCTION 'ZFMBP_LOG_API_BP'
      STARTING NEW TASK lv_taskname
      EXPORTING
        iv_processo = 'ZFI_SEGMENT_CREDIT'
        iv_metodo   = CONV char6( zifca_rest~go_request->get_header_field( if_http_header_fields_sap=>request_method ) )
        iv_json     = zifca_rest~go_request->get_cdata( )
        iv_action   = abap_true.

    lv_bp = me->zifca_rest~go_request->get_form_field('PARTNER').
    lv_seg_credit = me->zifca_rest~go_request->get_form_field('CREDIT_SGM').

    IF lv_bp IS NOT INITIAL
    AND lv_seg_credit IS NOT INITIAL.

      UNPACK lv_bp TO lv_bp.

      validate_data(
    EXPORTING
        iv_bp = lv_bp
        iv_seg_credit = lv_seg_credit
      CHANGING
          cs_data = rs_data ).

    ELSE.
      MESSAGE e001(zfi_segment_cred) INTO rs_data-mensagem.
    ENDIF.

    rs_data-partner = lv_bp.
    rs_data-credit_sgm = lv_seg_credit.

    me->zifca_rest~go_response->set_header_field( name = CONV #( TEXT-002 ) value = 'application/json'  ).

    zifca_rest~go_response->get_status(
        IMPORTING
            code   = lv_code
            reason = lv_reason ).

    CALL FUNCTION 'ZFMBP_LOG_API_BP'
      STARTING NEW TASK lv_taskname
      EXPORTING
        iv_processo = 'ZFI_SEGMENT_CREDIT'
        iv_metodo   = CONV char6( zifca_rest~go_request->get_header_field( if_http_header_fields_sap=>request_method ) )
        iv_json_ret = create_json( rs_data )
        iv_code     = lv_code
        iv_reason   = lv_reason.

  ENDMETHOD.


  METHOD zifca_rest~handle_request.

    DATA: lv_json_sub.

***************************************************************************
    " VARIABLES
***************************************************************************
    DATA:
      lv_string_writer TYPE REF TO cl_sxml_string_writer,
      lv_xstring       TYPE xstring,
      ls_data          TYPE zsfi_credit_segment.

***************************************************************************
    " EXECUTE GET_EQUIPMENTS METHOD
***************************************************************************
    TRY.

        ls_data = get_data( me->zifca_rest~go_request ).

***************************************************************************
        " CONVERT EQUIPMENTS TO JSON
***************************************************************************
        lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

        CALL TRANSFORMATION id SOURCE return = ls_data RESULT XML lv_string_writer.

        lv_xstring = lv_string_writer->get_output( ).

***************************************************************************
        " ADD THE JSON EQUIPMENTS TO THE RESPONSE
***************************************************************************
        me->zifca_rest~go_response->set_data( data = lv_xstring ).

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD zifca_rest~set_response.

    CALL METHOD me->zifca_rest~go_response->set_data
      EXPORTING
        data = is_data.

  ENDMETHOD.


  METHOD validate_data.

    DATA: ls_check_result TYPE ukm_s_query_result.

    SELECT  creditsegment, customercreditlimitamount, CreditLimitValidityEndDate
    FROM i_creditmanagementaccount
    WHERE businesspartner = @iv_bp
     INTO TABLE @DATA(lt_credit).

    IF sy-subrc NE 0.
      MESSAGE e005(zfi_segment_cred) WITH iv_bp INTO cs_data-mensagem.
    ELSE.

      SELECT  SINGLE kunnr FROM fndei_kna1_filter
      WHERE kunnr  = @iv_bp
      INTO @DATA(lv_kunnr).

      IF sy-subrc NE 0.
        MESSAGE e005(zfi_segment_cred) WITH iv_bp INTO cs_data-mensagem.
        RETURN.
      ENDIF.

      IF NOT line_exists( lt_credit[ CreditSegment = iv_seg_credit ] ).

        MESSAGE e006(zfi_segment_cred) WITH iv_seg_credit INTO cs_data-mensagem.
        RETURN.

      ELSE.

        SELECT  client INTO @sy-mandt FROM ukmcred_sgm0c UP TO 1 ROWS
        WHERE credit_sgmnt  = @iv_seg_credit.
        ENDSELECT.

        IF  sy-subrc NE 0.
          MESSAGE e006(zfi_segment_cred) WITH iv_seg_credit INTO cs_data-mensagem.
          RETURN.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'UKM_CREDIT_CHECK_SIMU'
        EXPORTING
          i_partner      = iv_bp
          i_segment      = iv_seg_credit
          i_currency     = 'BRL'
          i_no_dialog    = abap_true
          i_date         = sy-datum
        IMPORTING
          e_check_result = ls_check_result.

      IF ls_check_result IS NOT INITIAL.

        IF ls_check_result-limit_valid_date LE sy-datum.

          MESSAGE e004(zfi_segment_cred) WITH ls_check_result-limit_valid_date INTO cs_data-mensagem.

        ELSE.

          cs_data-creditlimitamount = ls_check_result-credit_limit.
          cs_data-zlimitedisponivel = COND #( WHEN ( ls_check_result-credit_limit - ls_check_result-comm_total ) LT 0 THEN 0 ELSE ( ls_check_result-credit_limit - ls_check_result-comm_total ) ).
          MESSAGE e002(zfi_segment_cred) INTO cs_data-mensagem.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD create_json.

    /ui2/cl_json=>serialize(
    EXPORTING
    data = is_data
    pretty_name = /ui2/cl_json=>pretty_mode-low_case
    RECEIVING
    r_json = rv_result ).

  ENDMETHOD.
ENDCLASS.
