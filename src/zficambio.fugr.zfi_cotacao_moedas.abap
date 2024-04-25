FUNCTION zfi_cotacao_moedas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(TAXA_CAMBIO) TYPE  STRING OPTIONAL
*"     REFERENCE(MOEDA_DESTINO) TYPE  STRING
*"     REFERENCE(MOEDA_ORIGEM) TYPE  STRING
*"     REFERENCE(CATEGORIA) TYPE  CHAR04
*"     REFERENCE(DATA) TYPE  D
*"     REFERENCE(FATOR_ORIGEM) TYPE  STRING
*"     REFERENCE(FATOR_DESTINO) TYPE  STRING
*"     REFERENCE(TAXA_CAMBIO_INDIRETA) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(RETURN) LIKE  BAPIRET2 STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------


  DATA: exch_rate  LIKE bapi1093_0   OCCURS 1 WITH HEADER LINE,
        rettab       LIKE bapiret2   OCCURS 1 WITH HEADER LINE,
        ret2         LIKE bapiret2   OCCURS 1 WITH HEADER LINE,
        ret3         LIKE bapiret2   OCCURS 1 WITH HEADER LINE,
        v_data(10)       TYPE c,
        v_gdatu      TYPE tcurr-gdatu.

  DATA:
       i_from_curr_range TYPE TABLE OF bapi1093_3 WITH HEADER LINE,
       i_to_currncy_range TYPE TABLE OF bapi1093_3 WITH HEADER LINE.

    write data DD/MM/YYYY to v_data .

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = v_data
      IMPORTING
        output = v_gdatu.

  SELECT SINGLE gdatu FROM tcurr INTO v_gdatu
    WHERE kurst = categoria and
    fcurr = moeda_origem  and
    tcurr = moeda_destino and
    gdatu = v_gdatu.

    IF sy-subrc EQ 0. "Caso a cottação já tenha sido cadastrada

      rettab-type	      = 'E'.
      rettab-id         = 'E!'.
      rettab-number     = '020'.
      rettab-message    = 'Linha                                                 1: Já existe entrada, nenhum Insert possível'.
      rettab-log_msg_no = '000000'.

    ELSE.

      exch_rate-rate_type   = categoria.
      exch_rate-from_curr   = moeda_origem.
      exch_rate-to_currncy  = moeda_destino.
      exch_rate-valid_from  = data.

      IF taxa_cambio IS NOT INITIAL.
        exch_rate-exch_rate   = taxa_cambio.
        exch_rate-from_factor = fator_origem.
        exch_rate-to_factor   = fator_destino.
      ELSEIF taxa_cambio_indireta IS NOT INITIAL.
        exch_rate-exch_rate_v   = taxa_cambio_indireta.
        exch_rate-from_factor_v = fator_origem.
        exch_rate-to_factor_v   = fator_destino.
      ENDIF.

      APPEND exch_rate.

      CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
        EXPORTING
          exch_rate = exch_rate
          upd_allow = 'X'
        IMPORTING
          return    = rettab.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        IMPORTING
          return = ret2.
    ENDIF.
    if rettab-TYPE   = 'E'   and
       rettab-ID     = 'E!'  and
       rettab-NUMBER = '015'.
       rettab-message = 'Linha                                                 1: Faltam configurações dos fatores de conversão'.
    ENDIF.
    return = rettab.



  ENDFUNCTION.
