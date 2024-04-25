"Name: \PR:SAPF110S\FO:BUCHUNGSZEILE_MERKEN\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_FI_SDHB_XREF1.


DATA: lv_xref1 TYPE regup-xref1.

IF regup-zlsch = 'E' AND regup-koart = 'D'.

  IF regup-laufd IS NOT INITIAL AND regup-laufi IS NOT INITIAL.
    CONCATENATE regup-laufd+02(06) regup-laufi(6) INTO lv_xref1.

    DATA: lt_campos TYPE TABLE OF accchg.

    APPEND VALUE accchg( fdname = 'XREF1'
                         newval = lv_xref1 ) TO lt_campos.


    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_bukrs              = regup-bukrs
        i_belnr              = regup-belnr
        i_gjahr              = regup-gjahr
        i_buzei              = regup-buzei
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
  ENDIF.
ENDIF.

ENDENHANCEMENT.
