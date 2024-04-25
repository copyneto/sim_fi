CLASS lcl_Bloq DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Bloq RESULT result.

    METHODS validacliente FOR VALIDATE ON SAVE
      IMPORTING keys FOR bloq~validacliente.

ENDCLASS.

CLASS lcl_Bloq IMPLEMENTATION.

  METHOD get_instance_authorizations.

    RETURN.

  ENDMETHOD.

  METHOD validaCliente.

    TYPES: BEGIN OF ty_cliente,
             cliente TYPE kna1-kunnr,
           END OF ty_cliente.

    DATA lt_cliente TYPE TABLE OF ty_cliente.

    READ ENTITIES OF zi_fi_bloq_client IN LOCAL MODE
         ENTITY Bloq
         FIELDS ( Cliente ) WITH CORRESPONDING #( keys )
         RESULT DATA(lt_client).

    SORT: lt_client BY cliente,
          lt_cliente BY Cliente.

    IF lt_client IS NOT INITIAL.

      SELECT kunnr
       FROM kna1
       INTO TABLE lt_cliente
       FOR ALL ENTRIES IN lt_client
       WHERE kunnr = lt_client-Cliente.

    ENDIF.

    LOOP AT lt_client ASSIGNING FIELD-SYMBOL(<fs_client>).

      READ TABLE lt_cliente ASSIGNING FIELD-SYMBOL(<fs_cliente>) WITH KEY cliente = <fs_client>-Cliente BINARY SEARCH.

      IF sy-subrc IS NOT INITIAL.

        APPEND VALUE #(  %tky = <fs_client>-%tky ) TO failed-Bloq.

        reported-Bloq = VALUE #( ( %msg = new_message(  id       = 'ZFI_BLOQ_CLIENT'
                                                        number   = '000'
                                                        severity = if_abap_behv_message=>severity-error  ) ) ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
