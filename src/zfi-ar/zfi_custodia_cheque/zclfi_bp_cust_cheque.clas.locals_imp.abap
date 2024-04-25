CLASS lhc_zi_fi_cust_cheque DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    METHODS setup_messages IMPORTING p_task TYPE clike.
    METHODS setup_messages2 IMPORTING p_task TYPE clike.
    METHODS setup_messages3 IMPORTING p_task TYPE clike.

  PRIVATE SECTION.

    CONSTANTS: gc_stgrd TYPE stgrd VALUE '01'.

    DATA gt_messages       TYPE STANDARD TABLE OF bapiret2.
    DATA gv_wait_async     TYPE abap_bool.

    DATA gv_dummy TYPE string.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _cheque RESULT result.

    METHODS setstatus FOR MODIFY
      IMPORTING keys FOR ACTION _cheque~setstatus RESULT result.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR _cheque RESULT result.

    METHODS deletecheque FOR MODIFY
      IMPORTING keys FOR ACTION _cheque~deletecheque RESULT result.

    METHODS contab FOR MODIFY
      IMPORTING keys FOR ACTION _cheque~contab RESULT result.

    METHODS setstatusini FOR DETERMINE ON MODIFY
      IMPORTING keys FOR _cheque~setstatusini.
    METHODS precheck_delete FOR PRECHECK
      IMPORTING keys FOR DELETE _cheque.


ENDCLASS.

CLASS lhc_zi_fi_cust_cheque IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD setstatus.

    DATA ls_key_func TYPE ztfi_cust_cheque.

    CLEAR: gt_messages.

    gv_wait_async = abap_false.


    READ ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
    ENTITY _cheque
     ALL FIELDS
     WITH CORRESPONDING #( keys )
    RESULT DATA(lt_lines).

    READ TABLE lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX 1.
    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_keys>) INDEX 1.

    " status atual devolvido ou compensado
    IF <fs_line>-status = '06' OR <fs_line>-status = '07' .

      "Não é possivel alterar Status atual do Cheque
      MESSAGE e011(zfi_cust_cheque) INTO gv_dummy.
      APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

      "Processamento concluido
      MESSAGE s002(zfi_cust_cheque) INTO gv_dummy.
      APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

      "Devolvido
    ELSEIF <fs_keys>-%param-novostatus = '06'.

      CLEAR: ls_key_func.

      MOVE-CORRESPONDING <fs_line> TO ls_key_func.

      CALL FUNCTION 'ZFMFI_CONTAB_CHEQUE_DEV'
        STARTING NEW TASK 'CONTAB_DEV'
        CALLING setup_messages3 ON END OF TASK
        EXPORTING
          is_key = ls_key_func.

      WAIT UNTIL gv_wait_async = abap_true.


    ELSE.

      MODIFY ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
      ENTITY _cheque
      UPDATE FIELDS ( status )
      WITH VALUE #( FOR ls_key IN keys
                              ( %key-bukrs = ls_key-bukrs
                                %key-kunnr = ls_key-kunnr
                                %key-ncheque = ls_key-ncheque
                                status = ls_key-%param-novostatus ) )
      REPORTED DATA(ls_reported_status)
      FAILED DATA(ls_failed_status).

      "Status atualizado com sucesso
      MESSAGE s012(zfi_cust_cheque) INTO gv_dummy.
      APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    ENDIF.


    reported-_cheque = VALUE #( FOR ls_msg IN gt_messages ( %tky = <fs_line>-%tky
                                                        %msg = new_message(
                                                               id       = ls_msg-id
                                                               number   = ls_msg-number
                                                               severity = CONV #( ls_msg-type )
                                                               v1       = ls_msg-message_v1
                                                               v2       = ls_msg-message_v2
                                                               v3       = ls_msg-message_v3
                                                               v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
       ENTITY _cheque
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                              %param    = ls_all ) ).



  ENDMETHOD.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD deletecheque.

    DATA ls_key TYPE ztfi_cust_cheque.

    CLEAR: gt_messages.

    gv_wait_async = abap_false.

    READ ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
    ENTITY _cheque
     ALL FIELDS
     WITH CORRESPONDING #( keys )
    RESULT DATA(lt_lines).

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).

      CLEAR: ls_key.

      MOVE-CORRESPONDING <fs_lines> TO ls_key.

      CALL FUNCTION 'ZFMFI_ESTORN_CHEQUE'
        STARTING NEW TASK 'ESTORN_CHEQUE'
        CALLING setup_messages ON END OF TASK
        EXPORTING
          is_key = ls_key.

      WAIT UNTIL gv_wait_async = abap_true.

    ENDLOOP.

    "Processamento concluido
    MESSAGE s002(zfi_cust_cheque) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_cheque = VALUE #( FOR ls_msg IN gt_messages ( %tky = <fs_lines>-%tky
                                                            %msg = new_message(
                                                                   id       = ls_msg-id
                                                                   number   = ls_msg-number
                                                                   severity = CONV #( ls_msg-type )
                                                                   v1       = ls_msg-message_v1
                                                                   v2       = ls_msg-message_v2
                                                                   v3       = ls_msg-message_v3
                                                                   v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
       ENTITY _cheque
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                              %param    = ls_all ) ).

  ENDMETHOD.

  METHOD contab.

    DATA ls_key TYPE ztfi_cust_cheque.

    CLEAR: gt_messages.

    gv_wait_async = abap_false.

    READ ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
    ENTITY _cheque
     ALL FIELDS
     WITH CORRESPONDING #( keys )
    RESULT DATA(lt_lines).

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).

      IF <fs_lines>-doc IS INITIAL.

        CLEAR: ls_key.

        MOVE-CORRESPONDING <fs_lines> TO ls_key.

        CALL FUNCTION 'ZFMFI_CONTAB_CHEQUE'
          STARTING NEW TASK 'CONTAB_CHEQUE'
          CALLING setup_messages ON END OF TASK
          EXPORTING
            is_key = ls_key.

        WAIT UNTIL gv_wait_async = abap_true.

      ELSE.

        "Custódia de cheque já contabilizado
        MESSAGE s010(zfi_cust_cheque) WITH <fs_lines>-ncheque INTO gv_dummy.
        APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.
      ENDIF.

    ENDLOOP.

    "Processamento concluido
    MESSAGE s002(zfi_cust_cheque) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_cheque = VALUE #( FOR ls_msg IN gt_messages (  %tky = <fs_lines>-%tky
                                                            %msg = new_message(
                                                                   id       = ls_msg-id
                                                                   number   = ls_msg-number
                                                                   severity = CONV #( ls_msg-type )
                                                                   v1       = ls_msg-message_v1
                                                                   v2       = ls_msg-message_v2
                                                                   v3       = ls_msg-message_v3
                                                                   v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
       ENTITY _cheque
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                              %param    = ls_all ) ).


  ENDMETHOD.

  METHOD setup_messages.

    DATA: lt_msg TYPE bapiret2_tab.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_CONTAB_CHEQUE'
            IMPORTING
              et_return = lt_msg.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.

  ENDMETHOD.

  METHOD setup_messages2.

    DATA: lt_msg TYPE bapiret2_tab.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_ESTORN_CHEQUE'
            IMPORTING
              et_return = lt_msg.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.

  ENDMETHOD.

  METHOD setup_messages3.

    DATA: lt_msg TYPE bapiret2_tab.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_CONTAB_CHEQUE_DEV'
            IMPORTING
              et_return = lt_msg.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.

  ENDMETHOD.

  METHOD setstatusini.

    CONSTANTS lc_status_ini TYPE ztfi_cust_cheque-status VALUE '01'.
    CONSTANTS lc_brl TYPE waers VALUE 'BRL'.

    READ ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
     ENTITY _cheque
      ALL FIELDS
      WITH CORRESPONDING #( keys )
     RESULT DATA(lt_lines).

    MODIFY ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
       ENTITY _cheque
       UPDATE FIELDS ( status moeda )
       WITH VALUE #( FOR ls_key IN keys
                               ( %key-bukrs = ls_key-bukrs
                                 %key-kunnr = ls_key-kunnr
                                 %key-ncheque = ls_key-ncheque
                                 status = lc_status_ini
                                 moeda = lc_brl ) )
       REPORTED DATA(ls_reported_status)
       FAILED DATA(ls_failed_status).

    reported = CORRESPONDING #( DEEP ls_reported_status ).

  ENDMETHOD.

  METHOD precheck_delete.

*    CONSTANTS lc_error_txt TYPE string VALUE 'Documento contabil não estornado'.

    READ ENTITIES OF zi_fi_cust_cheque IN LOCAL MODE
     ENTITY _cheque
     FIELDS ( doc docestorno )
      WITH CORRESPONDING #( keys )
     RESULT DATA(lt_lines).

    READ TABLE lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX 1.
    IF sy-subrc = 0.

      IF <fs_line>-doc IS NOT INITIAL AND
         <fs_line>-docestorno IS INITIAL.
        APPEND VALUE #( %tky = <fs_line>-%tky
                        %delete = if_abap_behv=>mk-on ) TO failed-_cheque.

        APPEND VALUE  #( %tky = keys[ 1 ]-%tky
                         %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = TEXT-e01 )   )
                         TO reported-_cheque.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
