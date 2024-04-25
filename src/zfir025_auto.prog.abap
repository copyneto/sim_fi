*&---------------------------------------------------------------------*
*& Include          ZFIR025_AUTO
*&---------------------------------------------------------------------*
*  Solução para atualização automática da cotação
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Declaração de TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_out,
         rate_type   TYPE kurst_curr,
         from_curr   TYPE fcurr_curr,
         to_currncy  TYPE tcurr_curr,
         currency    TYPE c LENGTH 13,
         exch_rate   TYPE ukursp,
         exch_rate_v TYPE ukursm,
         rate        TYPE ukursp,
         valid_from  TYPE gdatu_cur,
         usua_autor  TYPE syuname,
         index       TYPE int4,
       END OF ty_out.

*&---------------------------------------------------------------------*
*&  Declaração de Tabelas
*&---------------------------------------------------------------------*
DATA: t_004 TYPE TABLE OF zfit004,
      t_out TYPE TABLE OF ty_out.

*&---------------------------------------------------------------------*
*& Form zf_auto_ini
*&---------------------------------------------------------------------*
*  Inicialização do processo para salvar automaticamente os dados
*  de cotação obtidos previamente
*&---------------------------------------------------------------------*
FORM zf_auto_ini .

  PERFORM zf_get_data.            "// Busca dados da zfit004
  IF t_004[] IS NOT INITIAL.
    PERFORM zf_set_data.          "// Realiza verificações e preenche tabela t_out
    PERFORM zf_autor_atualizacao. "// Atualiza os dados
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form zf_get_data
*&---------------------------------------------------------------------*
FORM zf_get_data.

  SELECT *
    FROM zfit004
    INTO TABLE t_004
   WHERE valid_from EQ p_data.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_SET_DATA
*&---------------------------------------------------------------------*
FORM zf_set_data .

  DATA: ls_out      TYPE ty_out,
        lv_currency TYPE c LENGTH 13.

  LOOP AT t_004 ASSIGNING FIELD-SYMBOL(<fs_fit004>).

    CLEAR lv_currency.
    CONCATENATE <fs_fit004>-from_curr 'X' <fs_fit004>-to_currncy INTO lv_currency SEPARATED BY space.

    MOVE-CORRESPONDING <fs_fit004> TO ls_out.
    IF ls_out-exch_rate IS NOT INITIAL.
      ls_out-rate = ls_out-exch_rate.
    ELSEIF ls_out-exch_rate_v IS NOT INITIAL.
      ls_out-rate = ls_out-exch_rate_v.
    ENDIF.
    ls_out-currency = lv_currency.
    ls_out-index    = sy-tabix.
    APPEND ls_out TO t_out.

  ENDLOOP.

  LOOP AT t_out into ls_out.
    IF ls_out-rate_type  = 'M'.
       ls_out-rate_type  = 'P'.
      APPEND ls_out TO t_out.

    ENDIF.
  ENDLOOP.

  SORT t_out BY valid_from rate_type currency.

ENDFORM.                    " F_SET_DATA


*&---------------------------------------------------------------------*
*& Form zf_autor_atualizacao
*&---------------------------------------------------------------------*
FORM zf_autor_atualizacao .

  DATA: wa_return TYPE bapiret2,
        lv_index  TYPE i,
        lv_error  TYPE c,
        lv_cambi  TYPE string,
        lv_curor  TYPE string,
        lv_curdt  TYPE string,
        lv_fator  TYPE string,
        lv_fatdt  TYPE string.

  LOOP AT t_out TRANSPORTING NO FIELDS WHERE usua_autor IS INITIAL.
    EXIT.
  ENDLOOP.

  "// Lança os dados na TCURR
  LOOP AT t_004 ASSIGNING FIELD-SYMBOL(<fs_fit004>). "WHERE usua_autor IS INITIAL.

    CLEAR lv_index.
    READ TABLE t_out ASSIGNING FIELD-SYMBOL(<fs_out>) WITH KEY rate_type   = <fs_fit004>-rate_type
                                                                from_curr  = <fs_fit004>-from_curr
                                                                to_currncy = <fs_fit004>-to_currncy
                                                                valid_from = <fs_fit004>-valid_from.
    lv_index = sy-tabix.
    IF sy-subrc EQ 0.

      IF <fs_out>-exch_rate IS NOT INITIAL.
        <fs_fit004>-exch_rate   = <fs_out>-rate.
      ELSEIF <fs_out>-exch_rate_v IS NOT INITIAL.
        <fs_fit004>-exch_rate_v = <fs_out>-rate.
      ENDIF.

      <fs_fit004>-dt_autor   = sy-datum.
      <fs_fit004>-hora_autor = sy-uzeit.
      <fs_fit004>-usua_autor = sy-uname.

    ELSE.

      DELETE t_004.
      CONTINUE.

    ENDIF.

    CLEAR: wa_return, lv_cambi, lv_curor, lv_curdt, lv_fator, lv_fatdt.
    IF <fs_fit004>-exch_rate IS NOT INITIAL.

      MOVE: <fs_fit004>-exch_rate   TO lv_cambi,
            <fs_fit004>-from_curr   TO lv_curor,
            <fs_fit004>-to_currncy  TO lv_curdt,
            <fs_fit004>-from_factor TO lv_fator,
            <fs_fit004>-to_factor   TO lv_fatdt.

    ELSEIF <fs_fit004>-exch_rate_v IS NOT INITIAL.

      MOVE: <fs_fit004>-exch_rate_v   TO lv_cambi,
            <fs_fit004>-from_curr     TO lv_curor,
            <fs_fit004>-to_currncy    TO lv_curdt,
            "<fs_fit004>-from_factor_v TO lv_fator,
            "<fs_fit004>-to_factor_v   TO lv_fatdt.
            <fs_fit004>-from_factor TO lv_fator,
            <fs_fit004>-to_factor   TO lv_fatdt.

    ENDIF.

    IF <fs_fit004>-exch_rate IS NOT INITIAL.

      CALL FUNCTION 'ZFI_COTACAO_MOEDAS'
        EXPORTING
          taxa_cambio   = lv_cambi
          moeda_destino = lv_curdt
          moeda_origem  = lv_curor
          categoria     = <fs_fit004>-rate_type
          data          = <fs_fit004>-valid_from
          fator_origem  = lv_fator
          fator_destino = lv_fatdt
        IMPORTING
          return        = wa_return.

    ELSEIF <fs_fit004>-exch_rate_v IS NOT INITIAL.

      CALL FUNCTION 'ZFI_COTACAO_MOEDAS'
        EXPORTING
          taxa_cambio_indireta = lv_cambi
          moeda_destino        = lv_curdt
          moeda_origem         = lv_curor
          categoria            = <fs_fit004>-rate_type
          data                 = <fs_fit004>-valid_from
          fator_origem         = lv_fator
          fator_destino        = lv_fatdt
        IMPORTING
          return               = wa_return.

    ENDIF.

    IF ( wa_return-type NE 'E' ) "// Sucesso
    OR ( wa_return-type EQ 'E' AND wa_return-number EQ '020' ). "// Registro já existe na TCURR

      MODIFY zfit004 FROM <fs_fit004>.
      IF sy-subrc EQ 0.

        COMMIT WORK.
        <fs_out>-usua_autor = <fs_fit004>-usua_autor.
        MODIFY t_out FROM <fs_out> INDEX lv_index.
*        MODIFY t_004 FROM <fs_fit004>.

      ENDIF.

    ENDIF.

    DATA lv_string TYPE string.

    IF <fs_fit004>-exch_rate IS NOT INITIAL.
      WRITE:/ sy-uline.
      lv_string = |Tipo de mensagen: { wa_return-type } Mensagen: { wa_return-message } |.
      WRITE:/ lv_string.
      lv_string = |Categoria: { <fs_fit004>-rate_type } - ({ <fs_fit004>-from_curr }x{ <fs_fit004>-to_currncy }) Taxa: { <fs_fit004>-exch_rate } |.
      WRITE:/ lv_string.
    ELSEIF <fs_fit004>-exch_rate_v IS NOT INITIAL.
      WRITE:/ sy-uline.
      lv_string = |Tipo de mensagen: { wa_return-type } Mensagen: { wa_return-message } |.
      WRITE:/ lv_string.
      lv_string = |Categoria: { <fs_fit004>-rate_type } - ({ <fs_fit004>-from_curr }x{ <fs_fit004>-to_currncy }) Taxa: { <fs_fit004>-exch_rate_v } |.
      WRITE:/ lv_string.
    ENDIF.


  ENDLOOP.

ENDFORM.
