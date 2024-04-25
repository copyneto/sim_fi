"! Classe de implementação serviço POST VTIGER
CLASS zclfi_vtiger_int_rest_post DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zifca_rest .

    METHODS constructor
      IMPORTING
        !io_request  TYPE REF TO if_http_request
        !io_response TYPE REF TO if_http_response .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Metodo para selecionar as faturas
    "! @parameter is_request | Dados da interface
    "! @parameter es_response | Retorno para interface
    METHODS get_faturas
      IMPORTING
        is_request  TYPE zsfi_vtiger_in
      EXPORTING
        es_response TYPE zsfi_vtiger_out .
ENDCLASS.



CLASS ZCLFI_VTIGER_INT_REST_POST IMPLEMENTATION.


  METHOD constructor.

    me->zifca_rest~go_response = io_response.
    me->zifca_rest~go_request = io_request.

  ENDMETHOD.


  METHOD get_faturas.


    DATA: lv_dataini TYPE datum,
          lv_datafim TYPE datum,
          lv_tz      TYPE ttzz-tzone.

    DATA lv_cliente TYPE kna1-kunnr.


    SELECT COUNT(*)
      FROM t001
      WHERE bukrs = is_request-header-empresa.

    IF sy-subrc <> 0.
      es_response-mensagem = TEXT-e02.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = is_request-header-cliente
      IMPORTING
        output = lv_cliente.

    SELECT COUNT(*)
      FROM fndei_knb1_filter
      WHERE bukrs = @is_request-header-empresa
        AND kunnr = @lv_cliente.

    IF sy-subrc <> 0.
      CONCATENATE TEXT-e03 is_request-header-cliente TEXT-e04 is_request-header-empresa
        INTO es_response-mensagem SEPARATED BY space.
      RETURN.
    ENDIF.

    lv_tz = sy-tzone.

    CONVERT TIME STAMP is_request-header-datainicial
      TIME ZONE 'BRAZIL'
      INTO DATE lv_dataini.

    CONVERT TIME STAMP is_request-header-datafinal
      TIME ZONE 'BRAZIL'
      INTO DATE lv_datafim.

    SELECT * FROM
    zi_busca_part_vtiger
    INTO TABLE @DATA(lt_faturas)
        WHERE empresa = @is_request-header-empresa
         AND cliente = @lv_cliente
         AND datacriacao >= @lv_dataini
         AND datacriacao <= @lv_datafim .

    IF sy-subrc <> 0.

      SELECT SINGLE stkzn, stcd1, stcd2
        FROM kna1
        INTO @DATA(ls_cliente)
        WHERE kunnr = @lv_cliente.

      IF ls_cliente-stkzn = abap_true.
        DATA: lv_cfp TYPE char14.
        WRITE ls_cliente-stcd2 USING EDIT MASK '___.___.___-__' TO lv_cfp.
        "Não foi encontrado títulos em aberto Cliente & CNPJ/CPF &.
        CONCATENATE TEXT-e01 is_request-header-cliente TEXT-e05 lv_cfp INTO DATA(lv_msg_e) SEPARATED BY space.
        es_response-mensagem = lv_msg_e.
        RETURN.

      ELSE.
        DATA: lv_cnpj TYPE char18.
        WRITE ls_cliente-stcd1 USING EDIT MASK '__.___.___/____-__' TO lv_cnpj.
        "Não foi encontrado títulos em aberto Cliente & CNPJ/CPF &.
        CONCATENATE TEXT-e01 is_request-header-cliente TEXT-e06 lv_cnpj INTO DATA(lv_msg_e_2) SEPARATED BY space.
        es_response-mensagem = lv_msg_e_2.
        RETURN.

      ENDIF.
    ELSE.

      LOOP AT lt_faturas ASSIGNING FIELD-SYMBOL(<fs_fatura>).

        APPEND INITIAL LINE TO es_response-faturas ASSIGNING FIELD-SYMBOL(<fs_fat>).

        CONCATENATE <fs_fatura>-empresa
                    <fs_fatura>-docsap
                    <fs_fatura>-exercicio
                    <fs_fatura>-item
                    INTO <fs_fat>-id.

        SPLIT <fs_fatura>-referencia AT '-' INTO <fs_fat>-numnotafiscal
                                                 <fs_fat>-serienotafiscal.

        <fs_fat>-numerotitulo  = <fs_fatura>-docsap.
        <fs_fat>-valororiginal = <fs_fatura>-valor + <fs_fatura>-valortax.
        <fs_fat>-valoraberto  = <fs_fatura>-valortax.
        <fs_fat>-vencimentooriginal = <fs_fatura>-vencimento.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD zifca_rest~handle_request.

***************************************************************************
    " VARIABLES AND OBJECTS
***************************************************************************
    DATA:
      ls_response_cpi  TYPE zsfi_vtiger_out,
      lr_deserializer  TYPE REF TO cl_trex_json_deserializer,
      lv_string_writer TYPE REF TO cl_sxml_string_writer,
      lv_json_body     TYPE string,
      ls_struc_cpi     TYPE zsfi_vtiger_in,
      lv_xstring       TYPE xstring.

    CREATE OBJECT lr_deserializer.

***************************************************************************
    " JSON TO ABAP DATA
***************************************************************************
    lv_json_body = me->zifca_rest~go_request->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
      json = lv_json_body
      CHANGING
      data = ls_struc_cpi
      ).

    CHECK  ls_struc_cpi IS NOT INITIAL.

***************************************************************************
    " CREATE OBJECT
***************************************************************************
    get_faturas(
      EXPORTING
        is_request  = ls_struc_cpi
      IMPORTING
        es_response = ls_response_cpi
    ).

***************************************************************************
    " CONVERT INPUT TO JSON STRING
***************************************************************************
    lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE array = ls_response_cpi RESULT XML lv_string_writer.
    lv_xstring = lv_string_writer->get_output( ).

***************************************************************************
    " RETURN CREATED OBJECT AS RESPONSE (CONVENTION)
***************************************************************************
    me->zifca_rest~go_response->set_data( data = lv_xstring ).
  ENDMETHOD.


  METHOD zifca_rest~set_response.

    CALL METHOD me->zifca_rest~go_response->set_data
      EXPORTING
        data = is_data.
  ENDMETHOD.
ENDCLASS.
