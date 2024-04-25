*&---------------------------------------------------------------------*
*& Report zfir_bloq_clientes
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_bloq_clientes.
TABLES bseg.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS:
  s_bukrs FOR bseg-bukrs,
  s_kunnr FOR bseg-kunnr,
  s_netdt FOR bseg-netdt.

  SELECTION-SCREEN SKIP 1.

  PARAMETERS:
  p_dislog TYPE c AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_xo_const_message.
    CONSTANTS gc_message_id TYPE t100-arbgb VALUE 'ZFI_BLOQ_CLIENT'.

    CLASS-METHODS get_auth_company
      RETURNING
        VALUE(rt_bukrs) TYPE rsis_t_range
      RAISING
        cx_authorization_missing.

    CLASS-METHODS lock_customer
      IMPORTING
        iv_customer      TYPE kna1-kunnr
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.

    CLASS-METHODS execute.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD get_auth_company.
    CONSTANTS lc_obj_company TYPE ust12-objct VALUE 'F_BKPF_BUK'.
    DATA lt_values TYPE TABLE OF usvalues.

    CALL FUNCTION 'EFG_USER_AUTH_FOR_OBJ_GET'
      EXPORTING
        x_client       = sy-mandt
        x_uname        = sy-uname
        x_object       = lc_obj_company
      TABLES
        yt_usvalues    = lt_values
      EXCEPTIONS
        user_not_found = 1
        not_authorized = 2
        internal_error = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RAISE EXCEPTION TYPE cx_authorization_missing.
    ENDIF.

    SORT lt_values BY objct field.
    DELETE lt_values WHERE field NE 'BUKRS'. "#EC CI_STDSEQ

    LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<fs_value>).
      IF <fs_value>-von = '*'.
        DATA(lv_access_all) = abap_true.
      ENDIF.

      APPEND VALUE #(
        sign   = 'I'
        option = 'EQ'
        low    = <fs_value>-von
      ) TO rt_bukrs.
    ENDLOOP.

    IF lv_access_all = abap_true.
      CLEAR rt_bukrs.
    ENDIF.
  ENDMETHOD.

  METHOD execute.
    DATA lt_return TYPE bapiret2_t.

    SELECT Customer, AccountingDocument
      FROM zi_fi_clientes_inadimplentes
     WHERE CompanyCode IN @s_bukrs[]
       AND Customer    IN @s_kunnr[]
       AND IsBlocked   EQ @abap_false
       AND Block       EQ @abap_true
     GROUP BY Customer, AccountingDocument
      INTO TABLE @DATA(lt_documents).

    IF lt_documents IS INITIAL.
      MESSAGE ID gc_message_id TYPE if_xo_const_message~success NUMBER 002 DISPLAY LIKE if_xo_const_message~warning.
      RETURN.
    ENDIF.

    LOOP AT lt_documents ASSIGNING FIELD-SYMBOL(<fs_document>)
    GROUP BY ( Customer = <fs_document>-Customer ) ASSIGNING FIELD-SYMBOL(<fs_customers>).

      LOOP AT GROUP <fs_customers> ASSIGNING FIELD-SYMBOL(<fs_customer>).
        MESSAGE ID gc_message_id TYPE if_xo_const_message~error NUMBER 001 WITH <fs_customer>-Customer <fs_customer>-AccountingDocument
        INTO DATA(lv_dummy).

        APPEND VALUE #(
          id         = sy-msgid
          number     = sy-msgno
          type       = sy-msgty
          message_v1 = sy-msgv1
          message_v2 = sy-msgv2
          message_v3 = sy-msgv3
          message    = lv_dummy
        ) TO lt_return.

      ENDLOOP.

      APPEND LINES OF lock_customer( iv_customer = <fs_customer>-Customer )
      TO lt_return.

    ENDLOOP.

    IF p_dislog = abap_true.
      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table   = DATA(lo_alv)
            CHANGING
              t_table        = lt_return
          ).

          lo_alv->display( ).
        CATCH cx_salv_msg.
      ENDTRY.
    ELSE.
      MESSAGE ID gc_message_id TYPE if_xo_const_message~success NUMBER 004.
    ENDIF.
  ENDMETHOD.

  METHOD lock_customer.
    DATA ls_centraldata  TYPE bapibus1006_central.
    DATA ls_centraldatax TYPE bapibus1006_central_x.

    ls_centraldata-centralblock  = abap_true.
    ls_centraldatax-centralblock = abap_true.

    CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
      EXPORTING
        businesspartner = iv_customer
        centraldata     = ls_centraldata
        centraldata_x   = ls_centraldatax
      TABLES
        return          = rt_return.

    IF NOT line_exists( rt_return[ type = if_xo_const_message~error ] )."#EC CI_STDSEQ
      CLEAR rt_return.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN OUTPUT.
  TRY.
      s_bukrs[] = CORRESPONDING #( lcl_report=>get_auth_company( ) ).
    CATCH cx_authorization_missing.
      LEAVE LIST-PROCESSING.
  ENDTRY.

  IF s_bukrs[] IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name CS 'S_BUKRS-'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

START-OF-SELECTION.

  lcl_report=>execute( ).
