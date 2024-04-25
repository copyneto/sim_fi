FUNCTION zfmfi_lanc_f47_rwbapi01.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_ACCIT STRUCTURE  ACCIT
*"      IT_ACCCR STRUCTURE  ACCCR
*"      RETURN STRUCTURE  BAPIRET2
*"      EXTENSION STRUCTURE  BAPIACEXTC
*"      IT_ACCWT STRUCTURE  ACCIT_WT
*"      IT_ACCTX STRUCTURE  ACCBSET
*"  CHANGING
*"     VALUE(DOCUMENT_HEADER) LIKE  ACCHD STRUCTURE  ACCHD
*"----------------------------------------------------------------------
  LOOP AT extension ASSIGNING FIELD-SYMBOL(<fs_ext>).

    READ TABLE it_accit ASSIGNING FIELD-SYMBOL(<fs_accit>) WITH KEY posnr = <fs_ext>-field1+4(10).
    IF sy-subrc = 0.

      CASE <fs_ext>-field1(4).

        WHEN '0001'.  "For down payment request

          MOVE 'RFST' TO document_header-glvor.

          ASSIGN COMPONENT <fs_ext>-field2 OF STRUCTURE <fs_accit> TO FIELD-SYMBOL(<fs_campo>).
          IF <fs_campo> IS ASSIGNED.
            <fs_campo> =  <fs_ext>-field3.
          ENDIF.

          UNASSIGN <fs_campo>.

        WHEN OTHERS.

      ENDCASE.

    ENDIF.

  ENDLOOP.




ENDFUNCTION.
