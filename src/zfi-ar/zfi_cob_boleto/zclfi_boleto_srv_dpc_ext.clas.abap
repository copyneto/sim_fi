CLASS zclfi_boleto_srv_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zclfi_boleto_srv_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /iwbep/if_mgw_appl_srv_runtime~get_stream
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclfi_boleto_srv_dpc_ext IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.


    DATA: lo_fp          TYPE REF TO if_fp,
          lo_pdfobj      TYPE REF TO if_fp_pdf_object,
          lt_return      TYPE bapiret2_t,
          ls_stream      TYPE ty_s_media_resource,
          ls_lheader     TYPE ihttpnvp,
          ls_meta        TYPE sfpmetadata,
          lv_pdf_file    TYPE xstring,
          lv_boleto_name TYPE char50.

    TRY.
        DATA(lt_keys) = io_tech_request_context->get_keys( ).
        DATA(ls_key) = VALUE zsfi_boleto_ban_key(        "#EC CI_STDSEQ
                           bukrs = CONV bukrs( lt_keys[ name = 'EMPRESA' ]-value  )
                           belnr = CONV belnr_d( lt_keys[ name = 'DOCUMENTO' ]-value )
                           gjahr = CONV gjahr( lt_keys[ name = 'EXERCICIO' ]-value )
                           buzei = CONV buzei( lt_keys[ name = 'PARCELA' ]-value )  ).
      CATCH cx_root.
    ENDTRY.


    NEW zclfi_boleto_util( )->visualizar_boleto_app(
      EXPORTING
        is_boletos  = ls_key
      IMPORTING
        ev_pdf_file = lv_pdf_file
        ev_boleto_name = lv_boleto_name
    ).

* ----------------------------------------------------------------------
* Atualizar Metadata ao arquivo
* ----------------------------------------------------------------------
    DATA lv_text TYPE string.
    TRY.
        "Create PDF Object.
        lo_fp = cl_fp=>get_reference( ).
        lo_pdfobj = lo_fp->create_pdf_object( connection = 'ADS' ).
        lo_pdfobj->set_document( pdfdata = lv_pdf_file ).

        MOVE TEXT-001 TO lv_text.
        "Set PDF title name.
        ls_meta-title = lv_text. "'Boleto Bancário'.
        lo_pdfobj->set_metadata( metadata = ls_meta ).
        lo_pdfobj->execute( ).

        "Get the PDF content back with title
        lo_pdfobj->get_document( IMPORTING pdfdata = lv_pdf_file ).

      CATCH cx_root.
    ENDTRY.

* ----------------------------------------------------------------------
* Muda nome do arquivo
* ----------------------------------------------------------------------
* Tipo comportamento:
* - inline: Não fará download automático
* - outline: Download automático
* ----------------------------------------------------------------------
    MOVE TEXT-002 TO lv_text.
    ls_lheader-name  = lv_text. " 'Content-Disposition'. ##NO_TEXT
    ls_lheader-value = |inline; filename="{ lv_boleto_name }";|.

    set_header( is_header = ls_lheader ).

* ----------------------------------------------------------------------
* Retorna binário do PDF
* ----------------------------------------------------------------------
    ls_stream-mime_type = 'application/pdf'.
    ls_stream-value     = lv_pdf_file.

    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).

  ENDMETHOD.
ENDCLASS.
