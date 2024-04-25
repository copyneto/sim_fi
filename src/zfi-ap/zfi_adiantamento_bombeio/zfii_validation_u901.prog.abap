*&---------------------------------------------------------------------*
*& Include          ZFII_VALIDATION_U901
*&---------------------------------------------------------------------*

CONSTANTS:
  "! Constante para dados do boleto
  BEGIN OF gc_bombeio,
    modulo TYPE ze_param_modulo VALUE 'FI-AP',
    chave1 TYPE ze_param_chave1 VALUE 'BOMBEIO',
    chave2 TYPE ze_param_chave1 VALUE 'RAIZCNPJ',
    chave3 TYPE ze_param_chave1 VALUE 'FORNECEDORMATRIZ',
    umskz  TYPE bseg-umskz VALUE 'F',
    bschl  TYPE bseg-bschl VALUE '39',
  END OF gc_bombeio.

DATA: lv_raizcnpj       TYPE string,
      lv_raizcnpj_busca TYPE string.

IF sy-tcode NE zclfi_cockpit_bombeio_event=>gc_transaction-bombeio.

  DATA(lo_param) = NEW zclca_tabela_parametros( ).

  IF bseg-umskz = gc_bombeio-umskz AND
     bseg-bschl = gc_bombeio-bschl.

    SELECT SINGLE zlspr
      FROM bseg
      INTO @DATA(lv_zlspr_old)
      WHERE bukrs = @bseg-bukrs
         AND belnr = @bseg-belnr
         AND gjahr = @bseg-gjahr
         AND buzei = @bseg-buzei.

    IF lv_zlspr_old <> bseg-zlspr.

      TRY.

          lo_param->m_get_single(
            EXPORTING
              iv_modulo = gc_bombeio-modulo
              iv_chave1 = gc_bombeio-chave1
              iv_chave2 = gc_bombeio-chave2
              iv_chave3 = gc_bombeio-chave3
            IMPORTING
              ev_param  = lv_raizcnpj     ).

          lv_raizcnpj_busca = lv_raizcnpj && '%'.
          SELECT lifnr, stcd1
              FROM fndei_lfa1_filter
              INTO TABLE @DATA(lt_fornecedores)
              WHERE stcd1 LIKE @lv_raizcnpj_busca.

          SORT lt_fornecedores BY lifnr.

          READ TABLE lt_fornecedores TRANSPORTING NO FIELDS WITH KEY lifnr = bseg-lifnr BINARY SEARCH.

          IF sy-subrc = 0.
            b_result = b_false.
          ENDIF.

        CATCH zcxca_tabela_parametros.

      ENDTRY.

    ENDIF.

  ENDIF.

ENDIF.
