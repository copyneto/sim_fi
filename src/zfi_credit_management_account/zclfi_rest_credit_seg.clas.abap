class ZCLFI_REST_CREDIT_SEG definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.

  methods GET_REST
    importing
      !IO_SERVER type ref to IF_HTTP_SERVER
    returning
      value(RO_REST) type ref to ZIFCA_REST .
ENDCLASS.



CLASS ZCLFI_REST_CREDIT_SEG IMPLEMENTATION.


  METHOD get_rest.

***************************************************************************
    " VARIABLES
***************************************************************************
    DATA: lv_class_name           TYPE seoclsname.
    DATA: lv_request_method       TYPE string.

***************************************************************************
    " APPEND REQUEST METHOD TO BASE CLASS
***************************************************************************
    lv_request_method = io_server->request->get_header_field( '~request_method' ).

    CONCATENATE 'ZCLFI_REST_CREDIT_SEG_' lv_request_method INTO lv_class_name.

***************************************************************************
    " RETURN CLASS OBJECT
***************************************************************************
    TRY.
        CREATE OBJECT ro_rest
        TYPE (lv_class_name)
        EXPORTING
        io_request   = io_server->request
        io_response  = io_server->response.

***************************************************************************
        " ERRORS
***************************************************************************
      CATCH cx_sy_create_object_error.
    ENDTRY.


  ENDMETHOD.


  METHOD if_http_extension~handle_request.

***************************************************************************
" VARIABLES
***************************************************************************
DATA: lo_rest_class     TYPE REF TO zifca_rest.
    DATA: lo_error          TYPE REF TO cx_root.
    DATA: lv_reason         TYPE string.

***************************************************************************
    " GET THE CLASS OBJECT
***************************************************************************
    TRY.

        lo_rest_class ?= get_rest( io_server = server ).

***************************************************************************
        " EXECUTE THE RETRIEVED CLASS
***************************************************************************
        lo_rest_class->handle_request( ).

***************************************************************************
        " ERROR
***************************************************************************
      CATCH cx_root INTO lo_error.

        lv_reason = lo_error->get_text( ).
        server->response->set_status( code = 500
        reason = lv_reason ).

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
