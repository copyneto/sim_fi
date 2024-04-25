CLASS zclfi_arq_cust_txt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tt_stream  TYPE TABLE OF char1000sf,
           tt_cheques TYPE TABLE OF zi_fi_geracao_arq_cust.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zclfi_arq_cust_txt.

    METHODS build
      IMPORTING
                is_filters TYPE zclfi_geracao_arq_cust=>ty_filters
      EXPORTING et_txt     TYPE string_t.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: gc_nr_object TYPE nrobj VALUE 'ZCUSTCHEQ'.

    CLASS-DATA go_instance TYPE REF TO zclfi_arq_cust_txt.

    DATA: gs_header  TYPE zsfi_arquivo_custodia_header,
          gs_detalhe TYPE zsfi_arquivo_custodia_detalhe,
          gs_trailer TYPE zsfi_arquivo_custodia_trailer,
          gs_filters TYPE zclfi_geracao_arq_cust=>ty_filters.

    METHODS get_data
      IMPORTING is_filters TYPE zclfi_geracao_arq_cust=>ty_filters
      EXPORTING et_cheques TYPE tt_cheques.

    METHODS processar_detalhe
      IMPORTING is_cheques  TYPE zi_fi_geracao_arq_cust
                iv_lines    TYPE i
                iv_cont     TYPE i
      CHANGING  !cv_detalhe TYPE string.

    METHODS processar_header
      IMPORTING is_cheques TYPE zi_fi_geracao_arq_cust
                iv_lines   TYPE i
                iv_cont    TYPE i
                iv_remessa TYPE char4
      CHANGING  !cv_header TYPE string.

    METHODS processar_trailer
      IMPORTING is_cheques   TYPE zi_fi_geracao_arq_cust
                iv_lines     TYPE i
                iv_cont      TYPE i
                iv_total_arq TYPE zsfi_arquivo_custodia_trailer-valor_total_arquivo
                iv_remessa   TYPE char4
      CHANGING  !cv_trailer  TYPE string.

    METHODS atualizar_status
      CHANGING !ct_cheques TYPE tt_cheques.

    METHODS get_remessa_nr
      IMPORTING iv_nr          TYPE nrnr
                iv_obj         TYPE nrobj
      CHANGING  !cv_rem_number TYPE char4.

ENDCLASS.



CLASS zclfi_arq_cust_txt IMPLEMENTATION.

  METHOD get_instance.
    IF ( go_instance IS INITIAL ).
      go_instance = NEW zclfi_arq_cust_txt( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD build.
    FIELD-SYMBOLS <fs_struture> TYPE string.
    DATA: lv_count   TYPE i,
          lv_header  TYPE string,
          lv_detalhe TYPE string,
          lv_trailer TYPE string,
          lv_total   TYPE zsfi_arquivo_custodia_trailer-valor_total_arquivo,
          lv_remessa TYPE char4.

    me->get_data(
      EXPORTING
        is_filters = is_filters
      IMPORTING
        et_cheques = DATA(lt_cheques)
    ).

    IF lt_cheques IS NOT INITIAL.

      lv_count = 1.

      READ TABLE lt_cheques ASSIGNING FIELD-SYMBOL(<fs_cheques>) INDEX lv_count.

      me->get_remessa_nr(
            EXPORTING
                iv_nr = '01'
                iv_obj = me->gc_nr_object
            CHANGING
                cv_rem_number = lv_remessa ).

      me->processar_header(
            EXPORTING
              is_cheques = <fs_cheques>
              iv_lines   = lines( lt_cheques )
              iv_cont    = lv_count
              iv_remessa = lv_remessa
            CHANGING
              cv_header  =  lv_header ).

      APPEND lv_header TO et_txt.

      LOOP AT lt_cheques ASSIGNING <fs_cheques>.

        lv_count += 1.

        me->processar_detalhe(
          EXPORTING
            is_cheques = <fs_cheques>
            iv_lines   = lines( lt_cheques )
            iv_cont    = lv_count
          CHANGING
            cv_detalhe =  lv_detalhe
        ).

        lv_total += <fs_cheques>-Valor.

        APPEND lv_detalhe TO et_txt.

      ENDLOOP.

      lv_count += 1.

      me->processar_trailer(
          EXPORTING
            is_cheques   = <fs_cheques>
            iv_lines     = lines( lt_cheques )
            iv_cont      = lv_count
            iv_total_arq = lv_total
            iv_remessa   = lv_remessa
          CHANGING
            cv_trailer   =  lv_trailer ).

      APPEND lv_trailer TO et_txt.

      atualizar_status(
          CHANGING
             ct_cheques = lt_cheques
       ).

    ENDIF.

  ENDMETHOD.

  METHOD get_data.

    IF lines( is_filters-bukrs ) <= 1.

      SELECT *
          FROM zi_fi_geracao_arq_cust
          INTO TABLE @et_cheques
          WHERE Bukrs   IN @is_filters-bukrs
            AND Kunnr   IN @is_filters-kunnr
            AND Ncheque IN @is_filters-ncheque.

    ENDIF.

  ENDMETHOD.

  METHOD processar_detalhe.

    gs_detalhe = VALUE zsfi_arquivo_custodia_detalhe(
                                  compe_destino         = is_cheques-Zcamara
                                  banco_destino         = is_cheques-Zcmc7(3)
                                  agencia_destino       = is_cheques-Zcmc7+3(4)
                                  controle_dv2          = is_cheques-Zcmc7+7(1)
                                  num_conta_destino     = is_cheques-Zcmc7+19(10)
                                  controle_dv1          = is_cheques-Zcmc7+18(1)
                                  nun_documento         = is_cheques-Ncheque
                                  controle_dv3          = is_cheques-Zcmc7+29(1)
                                  valor                 = is_cheques-Valor
                                  tipificacao           = is_cheques-Zcmc7+17(1)
                                  banco_apresentante    = '041'
                                  data_efetivacao       = sy-datum
                                  cpf_cnpj_emitente     = is_cheques-Cnpj
                                  sequencial_registro   = iv_cont
                             ).

    cv_detalhe = |{ gs_detalhe-compe_destino }{ gs_detalhe-banco_destino }{ gs_detalhe-agencia_destino }{ gs_detalhe-controle_dv2 }{ gs_detalhe-num_conta_destino }{ gs_detalhe-controle_dv1 }{ gs_detalhe-nun_documento }| &
                 |{ gs_detalhe-controle_dv3 }{ gs_detalhe-tipo_documento }{ gs_detalhe-valor }{ gs_detalhe-codigo_devolucao }{ gs_detalhe-tipificacao }{ gs_detalhe-banco_apresentante }| &
                 |{ gs_detalhe-codigo_loja }{ gs_detalhe-impressora_fiscal_pos }{ gs_detalhe-sequencia_cheque_lote }{ gs_detalhe-filler ALPHA = OUT }{ gs_detalhe-data_efetivacao }{ gs_detalhe-codigo_garantia ALPHA = OUT }{ gs_detalhe-controle_empresa }| &
                 |{ gs_detalhe-lote_empresa }{ gs_detalhe-filler2 ALPHA = OUT }{ gs_detalhe-cpf_cnpj_emitente }{ gs_detalhe-filler3 ALPHA = OUT }{ gs_detalhe-sequencial_registro }{ gs_detalhe-final_registro }{ cl_abap_char_utilities=>newline }|.

  ENDMETHOD.

  METHOD processar_header.

    gs_header = VALUE zsfi_arquivo_custodia_header(
                                    tipo_registro       = 'H'
                                    agencia_depositante = is_cheques-Hktid
                                    conta_depositante   = is_cheques-Bankn
                                    nome_depositante    = is_cheques-NomeDepos
                                    num_banco_represent = '041'
                                    dv_numero_banco     = '8'
                                    indicador_interface = '9'
                                    data_movimento      = sy-datum
                                    data_arquivo        = sy-datum
                                    hora_arquivo        = sy-timlo
                                    remessa             = iv_remessa
                                    codigo_empresa      = is_cheques-Bukrs
                                    convenio            = '01'
                                    codigo_arquivo      = '000000'
                                    agencia_represent   = is_cheques-Hktid
                                    sequencial_registro = iv_cont
                               ).

    cv_header  = |{ gs_header-tipo_registro }{ gs_header-agencia_depositante }{ gs_header-conta_depositante }{ gs_header-nome_depositante ALPHA = OUT }{ gs_header-num_banco_represent }| &
                 |{ gs_header-dv_numero_banco }{ gs_header-indicador_interface }{ gs_header-data_movimento }{ gs_header-data_arquivo }{ gs_header-hora_arquivo }| &
                 |{ gs_header-remessa }{ gs_header-codigo_devolucao ALPHA = OUT }{ gs_header-filler ALPHA = OUT }{ gs_header-codigo_empresa }{ gs_header-convenio }| &
                 |{ gs_header-codigo_arquivo }{ gs_header-agencia_represent }{ gs_header-filler2 ALPHA = OUT }{ gs_header-sequencial_registro }{ gs_header-final_registro }{ cl_abap_char_utilities=>newline }|.

  ENDMETHOD.

  METHOD processar_trailer.

    gs_trailer = VALUE zsfi_arquivo_custodia_trailer(
                                  tipo_registro        = 'T'
                                  agencia_depositante  = is_cheques-Hktid
                                  conta_depositante    = is_cheques-Bankn
                                  nome_depositante     = is_cheques-NomeDepos
                                  num_banco_represent  = '041'
                                  dv_numero_banco      = '8'
                                  indicador_interface  = '9'
                                  data_movimento       = sy-datum
                                  data_criacao_arquivo = sy-datum
                                  hora_criacao_arquivo = sy-timlo
                                  remessa              = iv_remessa "#----teste-
                                  codigo_empresa       = is_cheques-Bukrs
                                  valor_total_arquivo  = iv_total_arq "#----teste-
                                  sequencial_total     = iv_cont
                             ).

    cv_trailer = |{ gs_trailer-tipo_registro }{ gs_trailer-agencia_depositante }{ gs_trailer-conta_depositante }{ gs_trailer-nome_depositante ALPHA = OUT }{ gs_trailer-num_banco_represent }| &
                 |{ gs_trailer-dv_numero_banco }{ gs_trailer-indicador_interface }{ gs_trailer-data_movimento }{ gs_trailer-data_criacao_arquivo }{ gs_trailer-hora_criacao_arquivo }| &
                 |{ gs_trailer-remessa }{ gs_trailer-codigo_devolucao ALPHA = OUT }{ gs_trailer-filler ALPHA = OUT }{ gs_trailer-codigo_empresa }{ gs_trailer-filler2 ALPHA = OUT }{ gs_trailer-valor_total_arquivo }| &
                 |{ gs_trailer-filler3 }{ gs_trailer-sequencial_total }{ gs_trailer-final_registro }{ cl_abap_char_utilities=>newline }|.

  ENDMETHOD.

  METHOD atualizar_status.

    LOOP AT ct_cheques ASSIGNING FIELD-SYMBOL(<fs_cheque>).

      UPDATE ztfi_cust_cheque SET status = '02' WHERE bukrs   = <fs_cheque>-Bukrs
                                                  AND kunnr   = <fs_cheque>-kunnr
                                                  AND ncheque = <fs_cheque>-ncheque. "#EC CI_IMUD_NESTED

      IF sy-subrc IS INITIAL.

        COMMIT WORK AND WAIT.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_remessa_nr.

    DATA lv_rem_number TYPE char4.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = iv_nr
        object                  = iv_obj
*       QUANTITY                = '1'
*       SUBOBJECT               = ' '
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = lv_rem_number
*       QUANTITY                =
*       RETURNCODE              =
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc IS INITIAL.

      cv_rem_number = lv_rem_number.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
