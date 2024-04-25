CLASS zcldms_invoice_update DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_invoice_update .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS valida_dms IMPORTING is_rbkp          TYPE rbkp
                       RETURNING VALUE(rs_return) TYPE bapiret2.
    METHODS update_dms IMPORTING is_rbkp TYPE rbkp.

ENDCLASS.



CLASS zcldms_invoice_update IMPLEMENTATION.


  METHOD if_ex_invoice_update~change_at_save.
    DATA(ls_return) = valida_dms( s_rbkp_new ).

    IF ls_return-type EQ 'E'.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
        RAISING error_with_message.
    ENDIF.

  ENDMETHOD.


  METHOD if_ex_invoice_update~change_before_update.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_invoice_update~change_in_update.
    update_dms( s_rbkp_new ).
  ENDMETHOD.


  METHOD valida_dms.

    CONSTANTS: lc_modulo TYPE ze_param_modulo VALUE 'DMS',
               lc_chave1 TYPE ze_param_chave1 VALUE 'WORKFLOW',
               lc_chave2 TYPE ze_param_chave2 VALUE 'DOCUMENTOS_NAO_FISCAIS'.
    DATA: lr_dokar TYPE RANGE OF dokar.

    IF is_rbkp-zz1_docdms_mih IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_doknr_out) = CONV doknr( |{ is_rbkp-zz1_docdms_mih ALPHA = OUT }| ).

    SELECT dokar, doknr, dokvr, doktl, dokst
      FROM draw
      INTO TABLE @DATA(lt_draw)
      WHERE doknr = @is_rbkp-zz1_docdms_mih.
    IF sy-subrc NE 0.
      MESSAGE e001(zdms) WITH lv_doknr_out
        INTO DATA(lv_message).

      rs_return = VALUE #( type       = sy-msgty
                           id         = sy-msgid
                           number     = sy-msgno
                           message    = lv_message
                           message_v1 = sy-msgv1 ).
      RETURN.
    ENDIF.

    READ TABLE lt_draw INTO DATA(ls_draw) INDEX 1.

    TRY.
        NEW zclca_tabela_parametros( )->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo
            iv_chave1 = lc_chave1
            iv_chave2 = lc_chave2
          IMPORTING
            et_range  = lr_dokar ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
        RETURN.
    ENDTRY.

    IF ls_draw-dokar NOT IN lr_dokar.
      RETURN.
    ENDIF.


    IF is_rbkp-stblg IS INITIAL.

      SELECT SINGLE belnr
        FROM rbkp
        INTO @DATA(lv_belnr)
       WHERE zz1_docdms_mih = @is_rbkp-zz1_docdms_mih
         AND stblg = @space.
      IF sy-subrc = 0.
        "Documento já utilizado na fatura &!
        MESSAGE e002(zfi_doc_nao_fiscais) WITH lv_belnr
          INTO lv_message.

        rs_return = VALUE #( type       = sy-msgty
                             id         = sy-msgid
                             number     = sy-msgno
                             message    = lv_message
                             message_v1 = sy-msgv1 ).
        RETURN.
      ENDIF.

      IF ls_draw-dokst NE 'H3'.
        MESSAGE e002(zdms) WITH lv_doknr_out
          INTO lv_message.

        rs_return = VALUE #( type       = sy-msgty
                             id         = sy-msgid
                             number     = sy-msgno
                             message    = lv_message
                             message_v1 = sy-msgv1 ).
        RETURN.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'CV115_DOC_ENQUEUE'
      EXPORTING
        pf_dokar = ls_draw-dokar
        pf_doknr = ls_draw-doknr
        pf_dokvr = ls_draw-dokvr
        pf_doktl = ls_draw-doktl
      EXCEPTIONS
        error    = 1
        locked   = 2
        OTHERS   = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        INTO lv_message.

      rs_return = VALUE #( type       = sy-msgty
                           id         = sy-msgid
                           number     = sy-msgno
                           message    = lv_message
                           message_v1 = sy-msgv1
                           message_v2 = sy-msgv2
                           message_v3 = sy-msgv3
                           message_v4 = sy-msgv4 ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'CV115_DOC_DEQUEUE'
      EXPORTING
        pf_dokar = ls_draw-dokar
        pf_doknr = ls_draw-doknr
        pf_dokvr = ls_draw-dokvr
        pf_doktl = ls_draw-doktl
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD update_dms.

    CONSTANTS: lc_blart_re  TYPE string          VALUE 'RE',
               lc_h3        TYPE string          VALUE 'H3',
               lc_pg        TYPE string          VALUE 'PG',
               lc_modulo    TYPE ze_param_modulo VALUE 'DMS',
               lc_chave1    TYPE ze_param_chave1 VALUE 'WORKFLOW',
               lc_chave2    TYPE ze_param_chave2 VALUE 'DOCUMENTOS_NAO_FISCAIS',
               lc_classtype TYPE klassenart      VALUE '017',
               lc_obtab     TYPE tabelle         VALUE 'DRAW',
               lc_classnum  TYPE klasse_d        VALUE 'Z_DMS_FI',
               lc_charname  TYPE atnam           VALUE 'Z_DMS_FI_03'.

    DATA: lv_statusintern   TYPE bapi_doc_draw-statusintern.


    DATA: lr_dokar TYPE RANGE OF dokar.

    IF is_rbkp-blart NE lc_blart_re.
      RETURN.
    ENDIF.

    IF is_rbkp-zz1_docdms_mih IS INITIAL.
      RETURN.
    ENDIF.

    SELECT dokar, doknr, dokvr, doktl, dokst
      FROM draw
      INTO TABLE @DATA(lt_draw)
      WHERE doknr = @is_rbkp-zz1_docdms_mih.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE lt_draw INTO DATA(ls_draw) INDEX 1.

    TRY.
        NEW zclca_tabela_parametros( )->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo
            iv_chave1 = lc_chave1
            iv_chave2 = lc_chave2
          IMPORTING
            et_range  = lr_dokar ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
        RETURN.
    ENDTRY.

    IF ls_draw-dokar NOT IN lr_dokar.
      RETURN.
    ENDIF.

    IF is_rbkp-stblg IS INITIAL.
      lv_statusintern = lc_pg.
      DATA(lv_belnr)  = is_rbkp-belnr.
      DATA(lv_delete) = abap_false.
    ELSE.
      lv_statusintern = lc_h3.
      lv_belnr        = is_rbkp-stblg.
      lv_delete       = abap_true.
    ENDIF.

    DATA(ls_documentdata)  = VALUE bapi_doc_draw2( statusintern  = lv_statusintern ).
    DATA(ls_documentdatax) = VALUE bapi_doc_drawx2( statusintern = abap_true ).

    DATA(lt_charvalues) = VALUE tt_dms_bapi_char_values( (
      classtype   = lc_classtype
      classname   = lc_classnum
      charname    = lc_charname
      charvalue   = lv_belnr
      deletevalue = lv_delete ) ).

    DATA(lt_classalloc) = VALUE tt_dms_bapi_class_allocation( (
      classtype        = lc_classtype
      classname        = lc_classnum ) ).

    DATA(ls_return) = VALUE bapiret2( ).
    CALL FUNCTION 'BAPI_DOCUMENT_CHANGE2'
      EXPORTING
        documenttype         = ls_draw-dokar
        documentnumber       = ls_draw-doknr
        documentpart         = ls_draw-doktl
        documentversion      = ls_draw-dokvr
        documentdata         = ls_documentdata
        documentdatax        = ls_documentdatax
      IMPORTING
        return               = ls_return
      TABLES
        characteristicvalues = lt_charvalues
        classallocations     = lt_classalloc.

  ENDMETHOD.

ENDCLASS.
