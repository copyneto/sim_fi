*&---------------------------------------------------------------------*
*& Include zfii_group_reporting_alv
*&---------------------------------------------------------------------*

  IF lt_message IS NOT INITIAL.
*    APPEND VALUE #( msgty  = gc_values-e msgv1 = TEXT-002 ) TO lt_message.

    TRY.

        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = go_table
          CHANGING
            t_table      = lt_message.

      CATCH cx_salv_msg INTO DATA(go_msg).
        DATA(gv_string) = go_msg->get_text( ).
        MESSAGE gv_string TYPE gc_values-i.
    ENDTRY.

  ELSE.

    TRY.

        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = go_table
          CHANGING
            t_table      = gt_acdocu.

      CATCH cx_salv_msg INTO go_msg.
        gv_string = go_msg->get_text( ).
        MESSAGE gv_string TYPE gc_values-i.
    ENDTRY.

  ENDIF.

  PERFORM p_set_toolbar.

  CALL METHOD go_table->display.

  FORM p_set_toolbar.

    DATA lo_functions TYPE REF TO cl_salv_functions_list.

    lo_functions = go_table->get_functions( ).
    lo_functions->set_all( ).

  ENDFORM.
