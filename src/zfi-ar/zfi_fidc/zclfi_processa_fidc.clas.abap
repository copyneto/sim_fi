CLASS zclfi_processa_fidc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_refdata,
        bukrs TYPE REF TO data,
        belnr TYPE REF TO data,
        gjahr TYPE REF TO data,
      END OF ty_refdata .

    DATA gs_refdata TYPE ty_refdata.

    METHODS constructor.
    METHODS check_error
      EXCEPTIONS
        not_parameter .
    METHODS processa_remessa.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_e TYPE char1 VALUE 'E'.
    CONSTANTS gc_s TYPE char1 VALUE 'S'.
    CONSTANTS gc_fidc TYPE hbkid VALUE 'FIDC'.
    CONSTANTS gc_fidc_p TYPE ze_param_chave1 VALUE 'FIDC' ##NO_TEXT.
    CONSTANTS gc_data TYPE ze_param_chave3 VALUE 'DATA' ##NO_TEXT.
    CONSTANTS gc_fi_ar TYPE ze_param_modulo VALUE 'FI-AR' ##NO_TEXT.
    CONSTANTS gc_remessa TYPE ze_param_chave2 VALUE 'REMESSA' ##NO_TEXT.
    CONSTANTS gc_banco_emissor_rej TYPE ze_param_chave2 VALUE 'BANCO_EMISSOR_REJEITADOS' ##NO_TEXT.
    CONSTANTS gc_xref3 TYPE fieldname VALUE 'XREF3' ##NO_TEXT.
    CONSTANTS gc_xref2 TYPE fieldname VALUE 'XREF2' ##NO_TEXT.

    TYPES:
      BEGIN OF ty_selopt,
        bukrs TYPE RANGE OF bseg-bukrs,
        belnr TYPE RANGE OF bseg-belnr,
        gjahr TYPE RANGE OF bseg-gjahr,
      END OF ty_selopt .

    DATA gs_selopt TYPE ty_selopt .

    DATA gt_remessa TYPE TABLE OF zsfi_dados_remessa_job.
    DATA gt_remessa_rej TYPE TABLE OF zsfi_dados_remessa_job.

    DATA gv_data_limite TYPE datum.

    METHODS get_faturas_remessa
      EXCEPTIONS not_found.

    METHODS selection_options.
    METHODS process_arq_rem.
    METHODS gerar_nossonum
      CHANGING
        cs_remessa TYPE zsfi_dados_remessa_job
      EXCEPTIONS
        not_add_number
        not_update
        not_upd_fat.

    METHODS get_parametros.
    METHODS atualiza_fat
      IMPORTING
        is_remessa TYPE zsfi_dados_remessa_job
      EXCEPTIONS
        not_update.
    METHODS save_log
      IMPORTING
        it_remessa TYPE zctgfi_dados_remessa_job.
    METHODS process_arq_rej.
    METHODS atualiza_fat_rej
      IMPORTING
                 is_rej TYPE zsfi_dados_remessa_job
      EXCEPTIONS not_update.


    METHODS get_parameters_single
      IMPORTING
        !iv_modulo TYPE ze_param_modulo
        !iv_chave1 TYPE ze_param_chave1
        !iv_chave2 TYPE ze_param_chave2
        !iv_chave3 TYPE ze_param_chave3
      CHANGING
        !cv_dir    TYPE string .

ENDCLASS.



CLASS zclfi_processa_fidc IMPLEMENTATION.


  METHOD processa_remessa.

    selection_options( ).

    get_faturas_remessa(
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2
    ).
    IF sy-subrc <> 0.
      "erro
      RETURN.
    ENDIF.

    process_arq_rem( ).

    process_arq_rej( ).


  ENDMETHOD.


  METHOD get_faturas_remessa.

    DATA lt_remessa_rej_copy TYPE TABLE OF zsfi_dados_remessa_job.

    CLEAR: gt_remessa.

    SELECT *
    FROM zi_fidc_faturas_envio
    INTO TABLE @DATA(lt_dados_rem)
    WHERE bukrs IN @gs_selopt-bukrs
         AND belnr IN @gs_selopt-belnr
         AND gjahr IN @gs_selopt-gjahr.

    SELECT *
       FROM zi_fi_fidc_faturas_rej
       INTO TABLE @DATA(lt_dados_rej)
       WHERE bukrs IN @gs_selopt-bukrs
            AND belnr IN @gs_selopt-belnr
            AND gjahr IN @gs_selopt-gjahr.

    IF lt_dados_rem IS INITIAL AND
       lt_dados_rej IS INITIAL.
      RAISE not_found.
    ENDIF.

    LOOP AT lt_dados_rem ASSIGNING FIELD-SYMBOL(<fs_dados_rem>).

      APPEND INITIAL LINE TO gt_remessa ASSIGNING FIELD-SYMBOL(<fs_remessa>).

      <fs_remessa>-empresa            = <fs_dados_rem>-bukrs.
      <fs_remessa>-ano                = <fs_dados_rem>-gjahr.
      <fs_remessa>-numero_documento   = <fs_dados_rem>-belnr.
      <fs_remessa>-item               = <fs_dados_rem>-buzei.
      <fs_remessa>-area_credito       = <fs_dados_rem>-kkber.
      <fs_remessa>-divisao            = <fs_dados_rem>-gsber.
      <fs_remessa>-cliente            = <fs_dados_rem>-kunnr.
      <fs_remessa>-nf                 = <fs_dados_rem>-xblnr.
      <fs_remessa>-nosso_numero       = <fs_dados_rem>-xref3.
      <fs_remessa>-valor_total_boleto = <fs_dados_rem>-wrbtr.
      <fs_remessa>-nome_cliente       = <fs_dados_rem>-nomecliente.
      <fs_remessa>-cnpj_cliente       = <fs_dados_rem>-cnpjcliente.
      <fs_remessa>-docnum             = <fs_dados_rem>-docnum.
      <fs_remessa>-data_emissao_nf    = <fs_dados_rem>-budat.
      <fs_remessa>-data_vcto_nf       = <fs_dados_rem>-vencimento.
      <fs_remessa>-valor_nf           = <fs_dados_rem>-valornf.
      <fs_remessa>-cnpj_emissor       = <fs_dados_rem>-cnpjemissor.
      <fs_remessa>-chave_nf           = <fs_dados_rem>-chave.
      <fs_remessa>-endereco_cliente   = <fs_dados_rem>-endcliente.
      <fs_remessa>-complemento = <fs_dados_rem>-compend.
      <fs_remessa>-bairro = <fs_dados_rem>-bairro.
      <fs_remessa>-cidade = <fs_dados_rem>-cidade.
      <fs_remessa>-uf = <fs_dados_rem>-uf.
      <fs_remessa>-cep = <fs_dados_rem>-cep.
      <fs_remessa>-email_1 = <fs_dados_rem>-email1.
      <fs_remessa>-email_2 = <fs_dados_rem>-email2.
      <fs_remessa>-telefone_1 = <fs_dados_rem>-telefone1.
      <fs_remessa>-telefone_2 = <fs_dados_rem>-telefone2.
    ENDLOOP.



    LOOP AT lt_dados_rej ASSIGNING FIELD-SYMBOL(<fs_dados_rej>).

      APPEND INITIAL LINE TO gt_remessa_rej ASSIGNING FIELD-SYMBOL(<fs_remessa_rej>).
      <fs_remessa_rej>-empresa            = <fs_dados_rej>-bukrs.
      <fs_remessa_rej>-ano                = <fs_dados_rej>-gjahr.
      <fs_remessa_rej>-numero_documento   = <fs_dados_rej>-belnr.
      <fs_remessa_rej>-item               = <fs_dados_rej>-buzei.
      <fs_remessa_rej>-area_credito       = <fs_dados_rej>-kkber.
      <fs_remessa_rej>-divisao            = <fs_dados_rej>-gsber.
      <fs_remessa_rej>-cliente            = <fs_dados_rej>-kunnr.
      <fs_remessa_rej>-nf                 = <fs_dados_rej>-xblnr.
      <fs_remessa_rej>-nosso_numero       = <fs_dados_rej>-xref3.
      <fs_remessa_rej>-valor_total_boleto = <fs_dados_rej>-wrbtr.
      <fs_remessa_rej>-nome_cliente       = <fs_dados_rej>-nomecliente.
      <fs_remessa_rej>-cnpj_cliente       = <fs_dados_rej>-cnpjcliente.
      <fs_remessa_rej>-docnum             = <fs_dados_rej>-docnum.
      <fs_remessa_rej>-data_emissao_nf    = <fs_dados_rej>-budat.
      <fs_remessa_rej>-data_vcto_nf       = <fs_dados_rej>-vencimento.
      <fs_remessa_rej>-valor_nf           = <fs_dados_rej>-valornf.
      <fs_remessa_rej>-cnpj_emissor       = <fs_dados_rej>-cnpjemissor.
      <fs_remessa_rej>-chave_nf           = <fs_dados_rej>-chave.
      <fs_remessa_rej>-endereco_cliente   = <fs_dados_rej>-endcliente.
      <fs_remessa_rej>-complemento = <fs_dados_rej>-compend.
      <fs_remessa_rej>-bairro = <fs_dados_rej>-bairro.
      <fs_remessa_rej>-cidade = <fs_dados_rej>-cidade.
      <fs_remessa_rej>-uf = <fs_dados_rej>-uf.
      <fs_remessa_rej>-cep = <fs_dados_rej>-cep.
      <fs_remessa_rej>-email_1 = <fs_dados_rej>-email1.
      <fs_remessa_rej>-email_2 = <fs_dados_rej>-email2.
      <fs_remessa_rej>-telefone_1 = <fs_dados_rej>-telefone1.
      <fs_remessa_rej>-telefone_2 = <fs_dados_rej>-telefone2.

    ENDLOOP.

    lt_remessa_rej_copy[] = gt_remessa[].

    DELETE gt_remessa WHERE data_vcto_nf < gv_data_limite.
    DELETE lt_remessa_rej_copy WHERE data_vcto_nf > gv_data_limite.

    LOOP AT lt_remessa_rej_copy ASSIGNING FIELD-SYMBOL(<fs_rej_rem>).
      APPEND INITIAL LINE TO gt_remessa_rej ASSIGNING FIELD-SYMBOL(<fs_rej>).
      MOVE-CORRESPONDING <fs_rej_rem> TO <fs_rej>.
    ENDLOOP.

    IF gt_remessa IS INITIAL AND
       gt_remessa_rej IS INITIAL .
      RAISE not_found.
    ENDIF.

  ENDMETHOD.

  METHOD selection_options.

    DATA: lo_ref_descr   TYPE REF TO cl_abap_structdescr.
    DATA: lt_detail      TYPE abap_compdescr_tab.

    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                   <fs_ref>   TYPE REF TO data.

    lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( me->gs_refdata ).
    lt_detail[] = lo_ref_descr->components.

    LOOP AT lt_detail ASSIGNING FIELD-SYMBOL(<fs_det>).

      ASSIGN COMPONENT <fs_det>-name OF STRUCTURE me->gs_refdata TO <fs_ref>.
      ASSIGN COMPONENT <fs_det>-name OF STRUCTURE me->gs_selopt  TO FIELD-SYMBOL(<fs_selopt>).

      ASSIGN <fs_ref>->* TO <fs_table>.
      IF <fs_table> IS ASSIGNED.
        <fs_selopt> = <fs_table>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD process_arq_rem.

    DATA: lt_remessa_ok  TYPE TABLE OF zsfi_dados_remessa_job,
          lt_remessa_nok TYPE TABLE OF zsfi_dados_remessa_job.


    LOOP AT gt_remessa ASSIGNING FIELD-SYMBOL(<fs_remessa>).

      gerar_nossonum(
        CHANGING
          cs_remessa     = <fs_remessa>
        EXCEPTIONS
          not_add_number = 1
          not_update     = 2
          not_upd_fat    = 3
          OTHERS         = 4
      ).
      CASE sy-subrc.
        WHEN 1.
          <fs_remessa>-tipo_status = gc_e.
          CONCATENATE TEXT-e01 <fs_remessa>-numero_documento INTO <fs_remessa>-mensagem.
        WHEN 2.
          <fs_remessa>-tipo_status = gc_e.
          CONCATENATE TEXT-e02 <fs_remessa>-numero_documento INTO <fs_remessa>-mensagem.
        WHEN 3.
          <fs_remessa>-tipo_status = gc_e.
          CONCATENATE TEXT-e03 <fs_remessa>-numero_documento INTO <fs_remessa>-mensagem.
      ENDCASE.

    ENDLOOP.

    lt_remessa_ok[] = gt_remessa[].
    DELETE lt_remessa_ok WHERE tipo_status = gc_e.
    DELETE lt_remessa_nok WHERE tipo_status <> gc_e.

    IF lt_remessa_ok IS NOT INITIAL.
      DATA(lo_arq) = NEW zclfi_fidc_remessa( it_report_rem = lt_remessa_ok ).
      lt_remessa_ok = lo_arq->main( ).
      save_log( lt_remessa_ok ).
    ENDIF.

    save_log( lt_remessa_nok ).

  ENDMETHOD.


  METHOD gerar_nossonum.

    CONSTANTS lc_e TYPE schzw_bseg VALUE 'E'.

    SELECT SINGLE *
    FROM pcec
    INTO @DATA(ls_pcec)
    WHERE zbukr = @cs_remessa-empresa
       AND hbkid = @gc_fidc.

    IF sy-subrc = 0.

      CALL FUNCTION 'ADD_N_TO_CHECK_NUMBER'
        EXPORTING
          i_pcec      = ls_pcec
        IMPORTING
          e_pcec      = ls_pcec
        EXCEPTIONS
          not_filled  = 1
          not_found   = 2
          not_numeric = 3
          not_valid   = 4
          OTHERS      = 5.

      IF sy-subrc <> 0.
        RAISE not_add_number.
      ENDIF.

      cs_remessa-nosso_numero = ls_pcec-checl.

      SELECT SINGLE dtaid
        FROM t045t
        INTO @DATA(lv_convenio)
        WHERE bukrs = @cs_remessa-empresa
          AND hbkid = @ls_pcec-hbkid
          AND hktid = @ls_pcec-hktid
          AND zlsch = @lc_e.

      IF sy-subrc = 0.
        cs_remessa-nosso_numero = lv_convenio && cs_remessa-nosso_numero.
      ENDIF.


      UPDATE pcec SET
       checl = ls_pcec-checl
      WHERE zbukr = ls_pcec-zbukr
        AND hbkid = ls_pcec-hbkid
        AND hktid = ls_pcec-hktid
        AND stapl = ls_pcec-stapl.

      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

      atualiza_fat(
        EXPORTING
          is_remessa = cs_remessa
        EXCEPTIONS
          not_update = 1
          OTHERS     = 2
      ).
      IF sy-subrc <> 0.
        RAISE not_upd_fat.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    get_parametros(  ).

  ENDMETHOD.


  METHOD get_parametros.

    DATA: lv_dias      TYPE i,
          lv_months    TYPE t5a4a-dlymo,
          lv_years     TYPE t5a4a-dlyyr,
          lv_dias_func TYPE t5a4a-dlydy.

    DATA lo_parametros TYPE REF TO zclca_tabela_parametros .

    lo_parametros = zclca_tabela_parametros=>get_instance( ).

    TRY.

        lo_parametros->m_get_single( EXPORTING iv_modulo = gc_fi_ar
                                               iv_chave1 = gc_fidc_p
                                               iv_chave2 = gc_remessa
                                               iv_chave3 = gc_data
                                     IMPORTING ev_param  = lv_dias ).

        IF lv_dias IS NOT INITIAL.

          lv_dias_func = lv_dias.
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              date      = sy-datum
              months    = lv_months
              days      = lv_dias_func
              signum    = '+'
              years     = lv_years
            IMPORTING
              calc_date = gv_data_limite.

        ENDIF.

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros

    ENDTRY.

  ENDMETHOD.


  METHOD atualiza_fat.

    DATA: lt_campos TYPE TABLE OF accchg.
    DATA: lv_xref2 TYPE bseg-xref2.

    SELECT SINGLE awtyp, awref, aworg
        FROM acdoca
        INTO @DATA(ls_acdoca)
        WHERE rldnr = '0L'
         AND rbukrs = @is_remessa-empresa
         AND belnr = @is_remessa-numero_documento
         AND ryear = @is_remessa-ano
         AND buzei = @is_remessa-item.

    APPEND VALUE accchg( fdname = gc_xref3
                         newval = is_remessa-nosso_numero ) TO lt_campos.

    CONCATENATE gc_fidc sy-datum+6(2) sy-datum+4(2) sy-datum+2(2) INTO lv_xref2.
    APPEND VALUE accchg( fdname = gc_xref2
                         newval = lv_xref2 ) TO lt_campos.

    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_awtyp              = ls_acdoca-awtyp
        i_awref              = ls_acdoca-awref
        i_aworg              = ls_acdoca-aworg
        i_buzei              = is_remessa-item
        x_lock               = abap_true
        i_upd_fqm            = abap_true
      TABLES
        t_accchg             = lt_campos
      EXCEPTIONS
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      RAISE not_update.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD check_error.

    IF gv_data_limite IS INITIAL.
      RAISE not_parameter.
    ENDIF.

  ENDMETHOD.


  METHOD save_log.

    DATA: lt_fidc_contr TYPE TABLE OF ztfi_fidc_contr.

    lt_fidc_contr = VALUE #(
                             FOR ls_remessa IN it_remessa
                              ( bukrs = ls_remessa-empresa
                                belnr = ls_remessa-numero_documento
                                gjahr = ls_remessa-ano
                                buzei = ls_remessa-item
                                nossonumero = ls_remessa-nosso_numero
                                status = ls_remessa-mensagem
                                tipo_status = ls_remessa-tipo_status
                                data_remessa = sy-datum
                                hora_remessa = sy-uzeit
                                nomearqremessa = ls_remessa-nome_arq_remessa ) ).

    MODIFY ztfi_fidc_contr FROM TABLE lt_fidc_contr.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD process_arq_rej.

    DATA: lt_fidc_contr TYPE TABLE OF ztfi_fidc_contr.
    DATA: ls_key  TYPE zsfi_boleto_ban_key,
          ls_comp TYPE zsfi_dados_retorno.
    DATA: lt_msg TYPE bapiret2_tab.

    LOOP AT gt_remessa_rej ASSIGNING FIELD-SYMBOL(<fs_rej>).

      atualiza_fat_rej(
        EXPORTING
          is_rej     = <fs_rej>
        EXCEPTIONS
          not_update = 1
          OTHERS     = 2
      ).
      IF sy-subrc = 0.

        ls_key-belnr = <fs_rej>-numero_documento.
        ls_key-bukrs = <fs_rej>-empresa.
        ls_key-gjahr = <fs_rej>-ano.
        ls_key-buzei = <fs_rej>-item.

        NEW zclfi_boleto_util(  )->gerar_boleto(
            EXPORTING
              is_key = ls_key
            IMPORTING
              et_msg = lt_msg
          ).

        IF NOT line_exists( lt_msg[ type = gc_e ] ).

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD atualiza_fat_rej.

    CONSTANTS: lc_xref3 TYPE fieldname VALUE 'XREF3' ##NO_TEXT,
               lc_xref2 TYPE fieldname VALUE 'XREF2' ##NO_TEXT,
               lc_hbkid TYPE fieldname VALUE 'HBKID' ##NO_TEXT,
               lc_hktid TYPE fieldname VALUE 'HKTID' ##NO_TEXT.

    DATA: lt_campos TYPE TABLE OF accchg.
    DATA: lv_xref2 TYPE bseg-xref2.
    DATA: lv_newval TYPE string,
          lv_bukrs  TYPE ze_param_chave3.

    lv_bukrs = is_rej-empresa.

    get_parameters_single( EXPORTING iv_modulo = gc_fi_ar
                              iv_chave1 = gc_fidc_p
                              iv_chave2 = gc_banco_emissor_rej
                              iv_chave3 = lv_bukrs
                     CHANGING cv_dir    = lv_newval ).

    SELECT SINGLE awtyp, awref, aworg
        FROM acdoca
        INTO @DATA(ls_acdoca)
        WHERE rldnr = '0L'
         AND rbukrs = @is_rej-empresa
         AND belnr = @is_rej-numero_documento
         AND ryear = @is_rej-ano
         AND buzei = @is_rej-item.

    APPEND VALUE accchg( fdname = lc_xref3
                         newval = '' ) TO lt_campos.

    CONCATENATE gc_fidc sy-datum+6(2) sy-datum+4(2) sy-datum+2(2) INTO lv_xref2 SEPARATED BY space.
    APPEND VALUE accchg( fdname = lc_xref2
                         newval = '' ) TO lt_campos.
*****
    APPEND VALUE accchg( fdname = lc_hbkid
                         newval = lv_newval ) TO lt_campos.

    SELECT SINGLE hktid
      FROM t012k
      INTO @DATA(lv_hktid)
      WHERE bukrs = @is_rej-empresa
      AND hbkid = @lv_newval.

    APPEND VALUE accchg( fdname = lc_hktid
                         newval = lv_hktid ) TO lt_campos.
*****
    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_awtyp              = ls_acdoca-awtyp
        i_awref              = ls_acdoca-awref
        i_aworg              = ls_acdoca-aworg
        i_buzei              = is_rej-item
        x_lock               = abap_true
        i_upd_fqm            = abap_true
      TABLES
        t_accchg             = lt_campos
      EXCEPTIONS
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      RAISE not_update.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD get_parameters_single.

    TRY.

        DATA(lo_parametros) = zclca_tabela_parametros=>get_instance( ).

        lo_parametros->m_get_single( EXPORTING iv_modulo = iv_modulo
                                               iv_chave1 = iv_chave1
                                               iv_chave2 = iv_chave2
                                               iv_chave3 = iv_chave3
                                     IMPORTING ev_param  = cv_dir ).

        IF cv_dir IS INITIAL.

          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = iv_modulo
                                                       iv_chave1 = iv_chave1
                                                       iv_chave2 = iv_chave2
                                                       iv_chave3 = iv_chave3 ).

        ENDIF.

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
*        MODIFY ct_retorno FROM ls_remessa TRANSPORTING tipo_status mensagem WHERE tipo_status <> gc_erro.
        RETURN.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
































