CLASS lcl_header DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    METHODS setup_messages IMPORTING p_task TYPE clike.
    METHODS setup_messages4 IMPORTING p_task TYPE clike.


  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _header RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ _header RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK _header.

    METHODS rba_cheques FOR READ
      IMPORTING keys_rba FOR READ _header\_cheques FULL result_requested RESULT result LINK association_links.


    METHODS getcheques FOR MODIFY
      IMPORTING keys FOR ACTION _header~getcheques RESULT result.
    METHODS rba_faturas FOR READ
      IMPORTING keys_rba FOR READ _header\_faturas FULL result_requested RESULT result LINK association_links.

    METHODS compensar FOR MODIFY
      IMPORTING keys FOR ACTION _header~compensar RESULT result.

    DATA gt_messages       TYPE STANDARD TABLE OF bapiret2.
    DATA gv_wait_async     TYPE abap_bool.

    DATA gv_dummy TYPE string.


ENDCLASS.

CLASS lcl_header IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.

    IF keys IS NOT INITIAL.

      SELECT bukrs,
             kunnr,
             raizcnpj,
             name,
             descemp
      FROM  zi_custcheque_comp_h
       INTO TABLE @DATA(lt_header)
       FOR ALL ENTRIES IN @keys
          WHERE bukrs = @keys-bukrs
            AND kunnr = @keys-kunnr.

      result = CORRESPONDING #( lt_header ).

    ENDIF.

  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD rba_cheques.
    RETURN.
  ENDMETHOD.


  METHOD getcheques.

    CLEAR: gt_messages.

    gv_wait_async = abap_false.

    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
    ENTITY _header
     ALL FIELDS
     WITH CORRESPONDING #( keys )
    RESULT DATA(lt_lines).

    READ TABLE lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX 1.
    IF sy-subrc = 0.

      CALL FUNCTION 'ZFMFI_GET_CHEQUES_COMP'
        STARTING NEW TASK 'GET_CHEQUES'
        CALLING setup_messages ON END OF TASK
        EXPORTING
          is_key = <fs_line>.

      WAIT UNTIL gv_wait_async = abap_true.

    ENDIF.

    "Processamento concluido
    MESSAGE s002(zfi_cust_cheque) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_header = VALUE #( FOR ls_msg IN gt_messages (  %tky = <fs_line>-%tky
                                                          %msg = new_message(
                                                                 id       = ls_msg-id
                                                                 number   = ls_msg-number
                                                                 severity = CONV #( ls_msg-type )
                                                                 v1       = ls_msg-message_v1
                                                                 v2       = ls_msg-message_v2
                                                                 v3       = ls_msg-message_v3
                                                                 v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
       ENTITY _header
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                             %param = ls_all ) ).

  ENDMETHOD.

  METHOD setup_messages.

    DATA: lt_msg TYPE bapiret2_tab.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_GET_CHEQUES_COMP'
            IMPORTING
              et_return = lt_msg.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.

  ENDMETHOD.

  METHOD rba_faturas.
    RETURN.
  ENDMETHOD.

  METHOD compensar.

    DATA: lt_cheques_tb TYPE TABLE OF ztfi_custcheq_cp,
          lt_faturas_tb TYPE TABLE OF ztfi_custcheq_ft.

    CLEAR: gt_messages.

    gv_wait_async = abap_false.

    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
    ENTITY _header
     ALL FIELDS
     WITH CORRESPONDING #( keys )
    RESULT DATA(lt_lines).

    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
    ENTITY _cheques
     ALL FIELDS
     WITH CORRESPONDING #( keys )
    RESULT DATA(lt_cheques).

    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
    ENTITY _fat
     ALL FIELDS
     WITH CORRESPONDING #( keys )
    RESULT DATA(lt_faturas).

    READ TABLE lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX 1.
    IF sy-subrc = 0.

      SORT: lt_cheques BY atribuido.
      LOOP AT lt_cheques ASSIGNING FIELD-SYMBOL(<fs_chq>) WHERE atribuido = 'Marcado'.
        APPEND INITIAL LINE TO lt_cheques_tb ASSIGNING FIELD-SYMBOL(<fs_chq_tb>).
        MOVE-CORRESPONDING <fs_chq> TO <fs_chq_tb>.
        <fs_chq_tb>-buzei = <fs_chq>-item.
      ENDLOOP.

      SORT: lt_faturas BY atribuido.
      LOOP AT lt_faturas ASSIGNING FIELD-SYMBOL(<fs_fat>) WHERE atribuido = 'Marcado'.
        APPEND INITIAL LINE TO lt_faturas_tb ASSIGNING FIELD-SYMBOL(<fs_fat_tb>).
        MOVE-CORRESPONDING <fs_fat> TO <fs_fat_tb>.
      ENDLOOP.


      CALL FUNCTION 'ZFMFI_CHEQUES_COMPENSAR'
        STARTING NEW TASK 'COMP_CHEQUES'
        CALLING setup_messages4 ON END OF TASK
        EXPORTING
          is_key     = <fs_line>
          it_faturas = lt_faturas_tb
          it_cheques = lt_cheques_tb.

      WAIT UNTIL gv_wait_async = abap_true.

    ENDIF.

    "Processamento concluido
    MESSAGE s002(zfi_cust_cheque) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_header = VALUE #( FOR ls_msg IN gt_messages (  %tky = <fs_line>-%tky
                                                          %msg = new_message(
                                                                 id       = ls_msg-id
                                                                 number   = ls_msg-number
                                                                 severity = CONV #( ls_msg-type )
                                                                 v1       = ls_msg-message_v1
                                                                 v2       = ls_msg-message_v2
                                                                 v3       = ls_msg-message_v3
                                                                 v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
       ENTITY _header
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                             %param = ls_all ) ).

  ENDMETHOD.

  METHOD setup_messages4.

    DATA: lt_msg TYPE bapiret2_tab.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_CHEQUES_COMPENSAR'
            IMPORTING
              et_return = lt_msg.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_cheques DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    METHODS setup_messages2 IMPORTING p_task TYPE clike.
    METHODS setup_messages5 IMPORTING p_task TYPE clike.

  PRIVATE SECTION.

    DATA gt_messages       TYPE STANDARD TABLE OF bapiret2.
    DATA gv_wait_async     TYPE abap_bool.

    DATA gv_dummy TYPE string.


    METHODS read FOR READ
      IMPORTING keys FOR READ _cheques RESULT result.

    METHODS rba_header FOR READ
      IMPORTING keys_rba FOR READ _cheques\_header FULL result_requested RESULT result LINK association_links.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _cheques RESULT result.

    METHODS setatribuido FOR MODIFY
      IMPORTING keys FOR ACTION _cheques~setatribuido RESULT result.
    METHODS setdinheiro FOR MODIFY
      IMPORTING keys FOR ACTION _cheques~setdinheiro RESULT result.

ENDCLASS.

CLASS lcl_cheques IMPLEMENTATION.


  METHOD read.

    IF keys IS NOT INITIAL.

      SELECT bukrs, kunnr, ncheque, criticalityatri,
             atribuido, dinheiro, valor, moeda, doc,
             gjahr, item, tpdoc, tipodocdesc, chavelanc,
             codrze, atribuicao, refe, datalanc, vencimento,
             xref1, xref2, name, descemp
      FROM  zi_custcheque_comp_icheques
       INTO TABLE @DATA(lt_items)
       FOR ALL ENTRIES IN @keys
          WHERE bukrs = @keys-bukrs
            AND kunnr = @keys-kunnr
            AND ncheque = @keys-ncheque.


      IF sy-subrc = 0.
        result = CORRESPONDING #( lt_items ).
      ELSE.

        IF keys[ 1 ]-bukrs IS NOT INITIAL AND
           keys[ 1 ]-kunnr IS NOT INITIAL AND
           keys[ 1 ]-ncheque IS INITIAL.

          SELECT bukrs, kunnr, ncheque, criticalityatri,
                  atribuido, dinheiro, valor, moeda, doc,
                  gjahr, item, tpdoc, tipodocdesc, chavelanc,
                  codrze, atribuicao, refe, datalanc, vencimento,
                  xref1, xref2, name, descemp
            FROM  zi_custcheque_comp_icheques
            INTO TABLE @DATA(lt_items1)
            FOR ALL ENTRIES IN @keys
               WHERE bukrs = @keys-bukrs
                 AND kunnr = @keys-kunnr.

          result = CORRESPONDING #( lt_items1 ).

        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD rba_header.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD setatribuido.

    CLEAR: gt_messages.

    gv_wait_async = abap_false.

    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
        ENTITY _cheques
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_lines).

    READ TABLE lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX 1.
    IF sy-subrc = 0.

      CALL FUNCTION 'ZFMFI_MARCAR_CHEQUE'
        STARTING NEW TASK 'MARCA_CHEQUE'
        CALLING setup_messages2 ON END OF TASK
        EXPORTING
          is_key = <fs_line>.

      WAIT UNTIL gv_wait_async = abap_true.

    ENDIF.

    "Processamento concluido
    MESSAGE s002(zfi_cust_cheque) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_header = VALUE #( FOR ls_msg IN gt_messages (  %tky = <fs_line>-%tky
                                                          %msg = new_message(
                                                                 id       = ls_msg-id
                                                                 number   = ls_msg-number
                                                                 severity = CONV #( ls_msg-type )
                                                                 v1       = ls_msg-message_v1
                                                                 v2       = ls_msg-message_v2
                                                                 v3       = ls_msg-message_v3
                                                                 v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
       ENTITY _cheques
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                             %param = ls_all ) ).

  ENDMETHOD.

  METHOD setup_messages2.

    DATA: lt_msg TYPE bapiret2_tab.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_MARCAR_CHEQUE'
            IMPORTING
              et_return = lt_msg.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.

  ENDMETHOD.

  METHOD setdinheiro.

    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
    ENTITY _cheques
    ALL FIELDS
    WITH CORRESPONDING #( keys )
    RESULT DATA(lt_lines).

    READ TABLE lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX 1.
    IF sy-subrc = 0.

      <fs_line>-dinheiro = keys[ 1 ]-%param-dinheiro.

      CALL FUNCTION 'ZFMFI_DINHEIRO_CHEQUE'
        STARTING NEW TASK 'DINHEIRO_CHEQUE'
        CALLING setup_messages5 ON END OF TASK
        EXPORTING
          is_key = <fs_line>.

      WAIT UNTIL gv_wait_async = abap_true.

    ENDIF.

    "Processamento concluido
    MESSAGE s002(zfi_cust_cheque) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_header = VALUE #( FOR ls_msg IN gt_messages (  %tky = <fs_line>-%tky
                                                          %msg = new_message(
                                                                 id       = ls_msg-id
                                                                 number   = ls_msg-number
                                                                 severity = CONV #( ls_msg-type )
                                                                 v1       = ls_msg-message_v1
                                                                 v2       = ls_msg-message_v2
                                                                 v3       = ls_msg-message_v3
                                                                 v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
       ENTITY _cheques
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                             %param = ls_all ) ).


  ENDMETHOD.

  METHOD setup_messages5.

    DATA: lt_msg TYPE bapiret2_tab.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_DINHEIRO_CHEQUE'
            IMPORTING
              et_return = lt_msg.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.


  ENDMETHOD.

ENDCLASS.

CLASS lcl_zi_custcheque_comp_h DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_zi_custcheque_comp_h IMPLEMENTATION.

  METHOD finalize.
    RETURN.
  ENDMETHOD.

  METHOD check_before_save.
    RETURN.
  ENDMETHOD.

  METHOD save.
    RETURN.
  ENDMETHOD.

  METHOD cleanup.
    RETURN.
  ENDMETHOD.

  METHOD cleanup_finalize.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lhc__fat DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    METHODS setup_messages3 IMPORTING p_task TYPE clike.


  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _fat RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ _fat RESULT result.

    METHODS rba_header FOR READ
      IMPORTING keys_rba FOR READ _fat\_header FULL result_requested RESULT result LINK association_links.

    METHODS setatribuidofat FOR MODIFY
      IMPORTING keys FOR ACTION _fat~setatribuidofat RESULT result.

    DATA gt_messages       TYPE STANDARD TABLE OF bapiret2.
    DATA gv_wait_async     TYPE abap_bool.

    DATA gv_dummy TYPE string.

ENDCLASS.

CLASS lhc__fat IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.

    IF keys IS NOT INITIAL.

      SELECT *
      FROM  zi_custcheque_comp_ifat
       INTO TABLE @DATA(lt_items)
       FOR ALL ENTRIES IN @keys
          WHERE bukrs = @keys-bukrs
            AND kunnr = @keys-kunnr
            AND raizcnpj = @keys-raizcnpj
            AND doc = @keys-doc
            AND gjahr = @keys-gjahr
            AND buzei = @keys-buzei.

      IF sy-subrc = 0.
        result = CORRESPONDING #( lt_items ).
      ELSE.

        IF keys[ 1 ]-bukrs IS NOT INITIAL AND
           keys[ 1 ]-kunnr IS NOT INITIAL AND
           keys[ 1 ]-raizcnpj IS INITIAL.

          SELECT *
              FROM  zi_custcheque_comp_ifat
               INTO TABLE @DATA(lt_items1)
               FOR ALL ENTRIES IN @keys
                  WHERE bukrs = @keys-bukrs
                    AND kunnr = @keys-kunnr.

          result = CORRESPONDING #( lt_items1 ).

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD rba_header.
    RETURN.
  ENDMETHOD.

  METHOD setatribuidofat.

    CLEAR: gt_messages.

    gv_wait_async = abap_false.

    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
        ENTITY _fat
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_lines).

    READ TABLE lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX 1.
    IF sy-subrc = 0.

      CALL FUNCTION 'ZFMFI_MARCAR_FATURA'
        STARTING NEW TASK 'MARCA_FATURA'
        CALLING setup_messages3 ON END OF TASK
        EXPORTING
          is_key = <fs_line>.

      WAIT UNTIL gv_wait_async = abap_true.

    ENDIF.

    "Processamento concluido
    MESSAGE s002(zfi_cust_cheque) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_header = VALUE #( FOR ls_msg IN gt_messages (  %tky = <fs_line>-%tky
                                                          %msg = new_message(
                                                                 id       = ls_msg-id
                                                                 number   = ls_msg-number
                                                                 severity = CONV #( ls_msg-type )
                                                                 v1       = ls_msg-message_v1
                                                                 v2       = ls_msg-message_v2
                                                                 v3       = ls_msg-message_v3
                                                                 v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_custcheque_comp_h IN LOCAL MODE
       ENTITY _fat
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                             %param = ls_all ) ).

  ENDMETHOD.

  METHOD setup_messages3.


    DATA: lt_msg TYPE bapiret2_tab.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_MARCAR_FATURA'
            IMPORTING
              et_return = lt_msg.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.


  ENDMETHOD.

ENDCLASS.
