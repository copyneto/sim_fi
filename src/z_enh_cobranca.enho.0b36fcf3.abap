"Name: \PR:J_1BBR20\FO:VORGANG_ERMITTELN\SE:BEGIN\EI
ENHANCEMENT 0 Z_ENH_COBRANCA.

  DATA: l_belnr  TYPE belnr_d,
        l_XREF3  TYPE bseg-XREF3.
  DATA: ls_nosso TYPE bseg.


    IF febko-BUKRS EQ '9999'.

       l_XREF3 = ITEMS_P_T-T13.
    ELSE.

      l_XREF3 = ITEMS_P_T-T13(10).
      UNPACK l_XREF3 TO l_XREF3(10).

    ENDIF.

      IF l_XREF3 IS NOT INITIAL.

        SELECT SINGLE *
          INTO ls_nosso
          FROM bseg
         WHERE XREF3 = l_XREF3.                " Procura alguma partida em aberto com esse número.

        IF sy-subrc EQ 0  AND ls_nosso-augbl IS INITIAL.

          febep-belnr = ls_nosso-belnr.        " Documento
          febep-pnota = ls_nosso-belnr.
          febep-KKREF = ls_nosso-belnr.
          febep-gjahr = ls_nosso-gjahr.        " Ano
          febep-avkon = ls_nosso-kunnr.        " Cliente
          febep-xblnr = |00000| && ls_nosso-buzei+1(2).   " Item
         ELSEIF febep-belnr CS '-'.
          CLEAR: febep-belnr, febep-gjahr.
        ENDIF.

      ENDIF.


ENDENHANCEMENT.
