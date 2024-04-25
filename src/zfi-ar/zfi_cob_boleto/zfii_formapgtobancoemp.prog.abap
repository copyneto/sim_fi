*&---------------------------------------------------------------------*
*& Include          ZFII_FORMAPGTOBANCOEMP
*&---------------------------------------------------------------------*

  CONSTANTS lc_e TYPE char1 VALUE 'E'.

  IF ( bseg-zlsch IS INITIAL OR bseg-hbkid IS INITIAL ) AND bseg-koart = 'D'. "Se banco empresa ou forma de pagamento estiver em branco


    SELECT SINGLE zwels, hbkid  "Seleciona banco e forma de pagamento do cadastro
      INTO @DATA(ls_dados)
      FROM esh_n_kna1_knb1_knb1_customer
      WHERE kunnr = @bseg-kunnr
         AND bukrs = @bseg-bukrs.

    IF bseg-zlsch IS INITIAL. "Se a forma de pagamento já não estiver preenchida no lançamento
      IF ls_dados-zwels IS NOT INITIAL. "Se cadastro do cliente estiver preenchido
        bseg-zlsch  = ls_dados-zwels.      "Forma de pagamento passa ser a do cadastro.
       ELSE.
        bseg-zlsch  = lc_e.                 "Se não, assume E (Boleto cobrança)
      ENDIF.
    ENDIF.

    IF bseg-hbkid IS INITIAL.        "Se banco empresa estiver vazio
      IF ls_dados-hbkid IS NOT INITIAL. "Se o cadastro do cliente estiver preenchido o banco empresa
           bseg-hbkid = ls_dados-hbkid.   "Banco empresa passa a ser o do cadastro do cliente
           bseg-hktid = '00001'.
      ENDIF.
    ENDIF.

  ENDIF.
