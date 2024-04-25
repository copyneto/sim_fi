CLASS zcldms_document_main01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_document_main01 .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS start_wf IMPORTING iv_tcode TYPE syst_tcode OPTIONAL
                               is_draw  TYPE draw.

ENDCLASS.



CLASS ZCLDMS_DOCUMENT_MAIN01 IMPLEMENTATION.


  METHOD if_ex_document_main01~add_additional_classes.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~after_change_number_check.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~after_determine_valid_version.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~after_read_data.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~after_save.
    start_wf( iv_tcode = tcode is_draw = draw ).
  ENDMETHOD.


  METHOD if_ex_document_main01~after_search_data.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~assign_number.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~before_delete.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~before_read_data.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~before_save.

    TYPES: BEGIN OF ty_aux,
             objek TYPE rmclausp-objek,
             atinn TYPE atnam,
             atzhl TYPE rmclausp-atzhl,
             mafid TYPE rmclausp-mafid,
             klart TYPE rmclausp-klart,
             adzhl TYPE rmclausp-adzhl,
             atwrt TYPE rmclausp-atwrt,
           END OF ty_aux.

    CONSTANTS: lc_classtype  TYPE klassenart       VALUE '017',
               lc_obtab      TYPE tabelle          VALUE 'DRAW',
               lc_classnum   TYPE klasse_d         VALUE 'Z_DMS_FI',
               lc_modulo     TYPE ze_param_modulo  VALUE 'DMS',
               lc_chave1     TYPE ze_param_chave1  VALUE 'WORKFLOW',
               lc_chave2     TYPE ze_param_chave2  VALUE 'DOCUMENTOS_NAO_FISCAIS',
               lc_dokob_rvkp TYPE drad-dokob       VALUE 'RBKP',
               lc_dokst_pg   TYPE draw-dokst       VALUE 'PG',
               lc_dokst_fl   TYPE draw-dokst       VALUE 'FL'.

    DATA: lt_missing_fields TYPE TABLE OF bapi1003_charact_r,
          lt_all_chars      TYPE STANDARD TABLE OF bapi1003_charact_r,
          lt_kssk_tab       TYPE TABLE OF rmclkssk,
          lt_ausp_tab       TYPE TABLE OF rmclausp,
          lt_ausp_aux       TYPE TABLE OF ty_aux.

    DATA: lr_dokar          TYPE RANGE OF dokar.

    DATA: ls_tdwa      TYPE tdwa,
          ls_return    TYPE bapiret2,
          ls_all_chars TYPE bapi1003_charact_r,
          ls_ausp_aux  LIKE LINE OF lt_ausp_aux.

    DATA: lv_output        TYPE string,
          lv_error_message TYPE string,
          lv_atinn         TYPE atinn.

    DATA(lv_object) = CONV cuobn( |{ draw-dokar }{ draw-doknr }{ draw-dokvr }{ draw-doktl }| ).

    CALL FUNCTION 'CLAP_DDB_GET_BUFFER_PARAMS'
      EXPORTING
        object     = lv_object
        classtype  = lc_classtype
        obtab      = lc_obtab
      TABLES
        e_kssk_tab = lt_kssk_tab
        e_ausp_tab = lt_ausp_tab.

    SORT: lt_kssk_tab BY klart class.

    READ TABLE lt_kssk_tab ASSIGNING FIELD-SYMBOL(<fs_kssk_tab>) WITH KEY klart = lc_classtype
                                                                          class = lc_classnum BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      DELETE lt_ausp_tab WHERE statu = 'L'.              "#EC CI_STDSEQ

      CALL FUNCTION 'BAPI_CLASS_GETDETAIL'
        EXPORTING
          classtype            = lc_classtype
          classnum             = lc_classnum
        IMPORTING
          return               = ls_return
        TABLES
          classcharacteristics = lt_all_chars.

      SORT lt_ausp_tab BY atinn.

      LOOP AT lt_all_chars ASSIGNING FIELD-SYMBOL(<fs_char>).

        CHECK <fs_char>-entry_obligatory = abap_true.

        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = <fs_char>-name_char
          IMPORTING
            output = lv_atinn.

        READ TABLE lt_ausp_tab INTO DATA(ls_ausp_aux1) WITH KEY atinn = lv_atinn BINARY SEARCH.

        IF sy-subrc IS NOT INITIAL OR ( ls_ausp_aux1-atwrt IS INITIAL
                                    AND ls_ausp_aux1-atflv IS INITIAL ).

          IF lv_error_message IS NOT INITIAL.
            CONCATENATE lv_error_message ',' INTO lv_error_message.
          ENDIF.
          IF lv_error_message IS INITIAL.
            CONCATENATE lv_error_message TEXT-t01 INTO lv_error_message.
          ENDIF.
          CONCATENATE lv_error_message <fs_char>-descr '  ' INTO lv_error_message.
        ENDIF.
      ENDLOOP.

      IF lv_error_message IS NOT INITIAL.
        MESSAGE lv_error_message TYPE 'E' DISPLAY LIKE 'E' RAISING cancel.
      ENDIF.

    ENDIF.

    TRY.

        NEW zclca_tabela_parametros( )->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo
            iv_chave1 = lc_chave1
            iv_chave2 = lc_chave2
          IMPORTING
            et_range  = lr_dokar ).

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
    ENDTRY.

    IF lr_dokar[] IS NOT INITIAL AND
       draw-dokar IN lr_dokar.
      READ TABLE drad
        WITH KEY dokob = lc_dokob_rvkp
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        draw-dokst = lc_dokst_fl.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_ex_document_main01~before_search_data.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~document_delete.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~set_kpro_update_mode.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_main01~set_whitelist_name.
    RETURN.
  ENDMETHOD.


  METHOD start_wf.

    CONSTANTS: lc_object_type TYPE swo_objtyp      VALUE 'ZDRAW',
               lc_approval    TYPE swo_event       VALUE 'APPROVAL',
               lc_cancel      TYPE swo_event       VALUE 'CANCEL',
               lc_modulo      TYPE ze_param_modulo VALUE 'DMS',
               lc_chave1      TYPE ze_param_chave1 VALUE 'WORKFLOW',
               lc_chave2      TYPE ze_param_chave2 VALUE 'DOCUMENTOS_NAO_FISCAIS'.
    DATA: lr_dokar TYPE RANGE OF dokar.

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

    IF is_draw-dokar NOT IN lr_dokar.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM i_documentinforecord
      FIELDS internaldocumentstatus
      WHERE documentinforecorddoctype    = @is_draw-dokar
        AND documentinforecorddocversion = @is_draw-dokvr
        AND documentinforecorddocnumber  = @is_draw-doknr
        AND documentinforecorddocpart    = @is_draw-doktl
        INTO @DATA(lv_status_db).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    IF  lv_status_db  EQ 'IA'  " Em Processamento
    AND is_draw-dokst EQ 'H2'. " Em liberação

      DATA(lv_event) = lc_approval.

    ELSEIF ( is_draw-dokst EQ 'H5'   " Cancelado
          OR is_draw-dokst EQ 'ZG'   " Rejeitado
          OR is_draw-dokst EQ 'PG'   " Pago
          OR is_draw-dokst EQ 'H3' ) " Liberado
       AND is_draw-dokst NE lv_status_db.

      lv_event = lc_cancel.

    ELSE.
      RETURN.
    ENDIF.

    DATA(lv_object_key) = CONV swo_typeid( |{ is_draw-dokar }{ is_draw-doknr }{ is_draw-dokvr }{ is_draw-doktl }| ).
    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type = lc_object_type
        object_key  = lv_object_key
        event       = lv_event
        commit_work = abap_false.

  ENDMETHOD.
ENDCLASS.
