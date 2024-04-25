CLASS lhc__boleto DEFINITION INHERITING FROM cl_abap_behavior_handler.


  PUBLIC SECTION.

    METHODS setup_messages IMPORTING p_task TYPE clike.

  PRIVATE SECTION.

    CONSTANTS: gc_gerado     TYPE string VALUE 'Boleto Gerado' ##NO_TEXT,
               gc_disponivel TYPE string VALUE 'Boleto Disponível' ##NO_TEXT.

    DATA gt_messages       TYPE STANDARD TABLE OF bapiret2.
    DATA gv_wait_async     TYPE abap_bool.

    DATA gv_dummy TYPE string.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR _boleto RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ _boleto RESULT result.

    METHODS enviaemail FOR MODIFY
      IMPORTING keys FOR ACTION _boleto~enviaemail.

    METHODS gerarboleto FOR MODIFY
      IMPORTING keys FOR ACTION _boleto~gerarboleto RESULT result.

    METHODS enviaportalb2b FOR MODIFY
      IMPORTING keys FOR ACTION _boleto~enviaportalb2b.

ENDCLASS.

CLASS lhc__boleto IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zi_fi_boleto IN LOCAL MODE
     ENTITY _boleto
     ALL FIELDS
     WITH CORRESPONDING #( keys )
     RESULT DATA(lt_result).

    result =
        VALUE #(
        FOR ls_result IN lt_result
            ( %tky              = ls_result-%tky
             ) ).

  ENDMETHOD.

  METHOD read.
    CHECK keys IS NOT INITIAL.
    SELECT * FROM zi_fi_boleto
    FOR ALL ENTRIES IN @keys
    WHERE empresa = @keys-empresa
      AND documento = @keys-documento
      AND exercicio = @keys-exercicio
      AND parcela = @keys-parcela
    INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.

  METHOD enviaemail.

    DATA: lt_msg         TYPE bapiret2_tab,
          lt_msg_ret     TYPE bapiret2_tab,
          lt_keys_bol    TYPE zctgfi_boleto_ban_key,
          lt_faturas_bck TYPE TABLE OF zi_fi_boleto,
          lt_faturas     TYPE TABLE OF zi_fi_boleto.


    CLEAR: gt_messages.

    gv_wait_async = abap_false.

    READ ENTITIES OF zi_fi_boleto IN LOCAL MODE
       ENTITY _boleto
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_result).

    DATA(lo_boleto) = NEW zclfi_boleto_util( ).

    lt_faturas_bck[] = lt_result[].

    SORT: lt_result BY documento empresa exercicio.
    DELETE ADJACENT DUPLICATES FROM lt_result COMPARING documento empresa exercicio.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).


      IF <fs_result>-status EQ gc_gerado.

        CLEAR: lt_msg.
        lt_faturas[] = lt_faturas_bck[].

        DELETE lt_faturas WHERE documento <> <fs_result>-documento
                            AND empresa <> <fs_result>-empresa
                            AND exercicio <> <fs_result>-exercicio
                            AND parcela <> <fs_result>-parcela.

        IF  lines( lt_faturas ) > 1.

          " Agrupar boletos
          lt_keys_bol = VALUE #( FOR ls_fat IN lt_faturas
                                ( belnr = ls_fat-documento
                                  bukrs = ls_fat-empresa
                                  gjahr = ls_fat-exercicio
                                  buzei = ls_fat-parcela   ) ).

          lo_boleto->envia_email(
               EXPORTING
                 it_keys  = lt_keys_bol
               IMPORTING
                 et_msg  = lt_msg
             ).

        ELSE.


          DATA(ls_key) = VALUE zsfi_boleto_ban_key(
                 belnr = <fs_result>-documento
                 bukrs = <fs_result>-empresa
                 gjahr = <fs_result>-exercicio
                 buzei = <fs_result>-parcela                      ).

          CALL FUNCTION 'ZFMFI_ENVIA_EMAIL'
            STARTING NEW TASK 'ENVIA_EMAIL'
            CALLING setup_messages ON END OF TASK
            EXPORTING
              is_key = ls_key.

          WAIT UNTIL gv_wait_async = abap_true.

*          lo_boleto->envia_email(
*               EXPORTING
*                 is_key  = ls_key
*               IMPORTING
*                 et_msg  = lt_msg
*             ).

        ENDIF.

        APPEND LINES OF lt_msg TO lt_msg_ret.

      ELSE.

        "Boleto não gerado
        MESSAGE e002(zfi_boleto) WITH <fs_result>-empresa <fs_result>-documento <fs_result>-exercicio INTO gv_dummy.
        APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

      ENDIF.

    ENDLOOP.

    "Processamento concluido
    MESSAGE s003(zfi_boleto) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_boleto = VALUE #( FOR ls_msg IN gt_messages (  %tky = ls_key
                                                        %msg = new_message(
                                                                   id       = ls_msg-id
                                                                   number   = ls_msg-number
                                                                   severity = CONV #( ls_msg-type )
                                                                   v1       = ls_msg-message_v1
                                                                   v2       = ls_msg-message_v2
                                                                   v3       = ls_msg-message_v3
                                                                   v4       = ls_msg-message_v4  ) ) ).



  ENDMETHOD.

  METHOD gerarboleto.

    CLEAR: gt_messages.

    gv_wait_async = abap_false.

    READ ENTITIES OF zi_fi_boleto IN LOCAL MODE
       ENTITY _boleto
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_result).

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).

      IF <fs_result>-status EQ gc_disponivel.

        DATA(ls_key) = VALUE zsfi_boleto_ban_key(
        belnr = <fs_result>-documento
        bukrs = <fs_result>-empresa
        gjahr = <fs_result>-exercicio
        buzei = <fs_result>-parcela
        ).

        CALL FUNCTION 'ZFMFI_GERAR_BOLETO'
          STARTING NEW TASK 'GERBOLETO'
          CALLING setup_messages ON END OF TASK
          EXPORTING
            is_key = ls_key.

        WAIT UNTIL gv_wait_async = abap_true.

      ELSE.

        "Boleto já esta gerado: &1 &2 &3
        MESSAGE e002(zfi_boleto) WITH <fs_result>-empresa <fs_result>-documento <fs_result>-exercicio INTO gv_dummy.
        APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

      ENDIF.

    ENDLOOP.

    "Processamento concluido
    MESSAGE s003(zfi_boleto) INTO gv_dummy.
    APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 ) TO gt_messages.

    reported-_boleto = VALUE #( FOR ls_msg IN gt_messages (  %tky = ls_key
                                                        %msg = new_message(
                                                                   id       = ls_msg-id
                                                                   number   = ls_msg-number
                                                                   severity = CONV #( ls_msg-type )
                                                                   v1       = ls_msg-message_v1
                                                                   v2       = ls_msg-message_v2
                                                                   v3       = ls_msg-message_v3
                                                                   v4       = ls_msg-message_v4  ) ) ).

    "Atualiza as informações
    READ ENTITIES OF zi_fi_boleto IN LOCAL MODE
       ENTITY _boleto
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_all)
       FAILED failed.

    result = VALUE #( FOR ls_all IN lt_all ( %key = ls_all-%key
                                              %param    = ls_all ) ).


  ENDMETHOD.

  METHOD setup_messages.
    DATA: lt_msg TYPE bapiret2_tab.

    CASE p_task.
      WHEN 'GERBOLETO'.
        RECEIVE RESULTS FROM FUNCTION 'ZFMFI_GERAR_BOLETO'
                IMPORTING
                  et_return = lt_msg.

      WHEN 'INTEGRARBOLETO'.
        RECEIVE RESULTS FROM FUNCTION 'ZFMFI_INTEGRAR_BOLETO'
                IMPORTING
                  et_return = lt_msg.
    ENDCASE.

    APPEND LINES OF lt_msg TO gt_messages .
    gv_wait_async = abap_true.
  ENDMETHOD.

  METHOD enviaPortalB2B.
    DATA: lt_msg          TYPE bapiret2_tab,
          lt_msg_ret      TYPE bapiret2_tab,
          lt_keys_bol     TYPE zctgfi_boleto_ban_key,
          lt_faturas_bck  TYPE TABLE OF zi_fi_boleto,
          lt_faturas      TYPE TABLE OF zi_fi_boleto,
          lt_file_content TYPE bcst_attachment.

    READ ENTITIES OF zi_fi_boleto IN LOCAL MODE
       ENTITY _boleto
       ALL FIELDS
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_result).

    DATA(lo_boleto) = NEW zclfi_boleto_util( ).

    SORT lt_result BY documento empresa exercicio.
    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>)
      GROUP BY ( documento = <fs_result>-documento empresa = <fs_result>-empresa exercicio = <fs_result>-exercicio )
      ASSIGNING FIELD-SYMBOL(<fs_group>).

      LOOP AT GROUP <Fs_group> ASSIGNING FIELD-SYMBOL(<fs_group_line>).

        IF <fs_group_line>-status NE gc_gerado.
          APPEND VALUE #(
            id         = 'ZFI_B2B_INTEGRACAO_BOLETO'
            type       = 'E'
            number     = 001
            message_v1 = <fs_group_line>-Documento
            message_v2 = <fs_group_line>-Parcela
          ) TO lt_msg_ret.

          CONTINUE.
        ENDIF.

        CALL METHOD lo_boleto->visualizar_boleto_app
          EXPORTING
            is_boletos     = VALUE #(
              bukrs = <fs_group_line>-Empresa
              belnr = <fs_group_line>-Documento
              gjahr = <Fs_group_line>-Exercicio
              buzei = <fs_group_line>-Parcela
            )
          IMPORTING
            ev_pdf_file    = DATA(lv_file_content)
            ev_boleto_name = DATA(lv_file_name).

        CALL METHOD cl_http_utility=>if_http_utility~encode_x_base64
          EXPORTING
            unencoded = lv_file_content
          RECEIVING
            encoded   = DATA(lv_boleto_base64).

        APPEND VALUE #(
          filename     = lv_file_name
          contents_txt = lv_boleto_base64
        ) TO lt_file_content.
      ENDLOOP.

      CALL FUNCTION 'ZFMFI_INTEGRAR_BOLETO'
        STARTING NEW TASK 'GERBOLETO'
        CALLING setup_messages ON END OF TASK
        EXPORTING
          is_boleto_info = <fs_group_line>
          it_content     = lt_file_content.

      WAIT UNTIL gv_wait_async = abap_true.
*      APPEND LINES OF gt_messages TO lt_msg_ret.

      reported-_boleto = VALUE #( base reported-_boleto
        FOR ls_msg IN gt_messages (
          %key = CORRESPONDING #( <fs_group_line> )
          %msg = new_message(
            id       = ls_msg-id
            number   = ls_msg-number
            severity = CONV #( ls_msg-type )
            v1       = ls_msg-message_v1
            v2       = ls_msg-message_v2
            v3       = ls_msg-message_v3
            v4       = ls_msg-message_v4
          )
        )
      ).

      CLEAR: gv_wait_async, gt_messages, lt_file_content.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zi_boleto DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zi_boleto IMPLEMENTATION.

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
