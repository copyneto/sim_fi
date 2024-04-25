CLASS zclfi_sugst_adiat_bombeio DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_refdata,
        bukrs TYPE REF TO data,
        werks TYPE REF TO data,
        aedat TYPE REF TO data,
        ebeln TYPE REF TO data,
        ebelp TYPE REF TO data,
        eindt TYPE REF TO data,
      END OF ty_refdata .

    DATA gs_refdata TYPE ty_refdata .

    METHODS constructor .
    METHODS main
      EXCEPTIONS
        error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_pedidos,
        supplier          TYPE a_purchaseorder-supplier,
        companycode       TYPE a_purchaseorder-companycode,
        purchaseorder     TYPE a_purchaseorder-purchaseorder,
        creationdate      TYPE a_purchaseorder-creationdate,
        purchaseorderitem TYPE a_purchaseorderitem-purchaseorderitem,
        orderquantity     TYPE a_purchaseorderitem-orderquantity,
      END OF ty_pedidos .
    TYPES:
      BEGIN OF ty_element,
        purchaseorder      TYPE a_purordpricingelement-purchaseorder,
        purchaseorderitem  TYPE a_purordpricingelement-purchaseorderitem,
        conditiontype      TYPE a_purordpricingelement-conditiontype,
        conditionamount    TYPE a_purordpricingelement-conditionamount,
        conditionquantity  TYPE a_purordpricingelement-conditionquantity,
        conditionbasevalue TYPE a_purordpricingelement-conditionbasevalue,
      END OF ty_element .
    TYPES:
      BEGIN OF ty_schedule,
        purchasingdocument        TYPE a_purchaseorderscheduleline-purchasingdocument,
        purchasingdocumentitem    TYPE a_purchaseorderscheduleline-purchasingdocumentitem,
        scheduleline              TYPE a_purchaseorderscheduleline-scheduleline,
        schedulelineorderquantity TYPE a_purchaseorderscheduleline-schedulelineorderquantity,
      END OF ty_schedule .
    TYPES:
      BEGIN OF ty_selopt,
        bukrs TYPE RANGE OF ekpo-bukrs,
        werks TYPE RANGE OF ekpo-werks,
        aedat TYPE RANGE OF ekko-aedat,
        ebeln TYPE RANGE OF ekpo-ebeln,
        ebelp TYPE RANGE OF ekpo-ebelp,
        eindt TYPE RANGE OF eket-eindt,
      END OF ty_selopt .
    TYPES:
      BEGIN OF ty_fornec,
        stcd1 TYPE stcd1,
        lifnr TYPE lifnr,
      END OF ty_fornec .
    TYPES:
      BEGIN OF ty_total,
        bukrs TYPE bukrs,
        dmbtr TYPE dmbtr,
      END OF ty_total .

    DATA:
      gt_pedidos TYPE STANDARD TABLE OF ty_pedidos .
    DATA:
      gt_element TYPE STANDARD TABLE OF ty_element .
    DATA:
      gt_schedule TYPE STANDARD TABLE OF ty_schedule .
    DATA:
      gt_total TYPE STANDARD TABLE OF ty_total .
    DATA gs_selopt TYPE ty_selopt .
    DATA:
      gr_condtyp TYPE RANGE OF kscha .
    DATA gv_cpnpj_matriz TYPE lfa1-stcd1 .
    DATA:
      gt_fornec TYPE STANDARD TABLE OF ty_fornec .
    DATA gv_log_handle TYPE balloghndl .
    CONSTANTS gc_param_mod TYPE ze_param_modulo VALUE 'FI-AP' ##NO_TEXT.
    CONSTANTS gc_param_c1_bomb TYPE ze_param_chave1 VALUE 'BOMBEIO' ##NO_TEXT.
    CONSTANTS gc_param_c2_ctcnpj TYPE ze_param_chave2 VALUE 'CENTRO' ##NO_TEXT.
    CONSTANTS gc_param_c3_forn TYPE ze_param_chave3 VALUE 'CNPJFORNECEDORES' ##NO_TEXT.
    CONSTANTS gc_param_c2_tpcond TYPE ze_param_chave2 VALUE 'TIPODECONDICAO' ##NO_TEXT.
    CONSTANTS gc_param_c3_pedid TYPE ze_param_chave3 VALUE 'PEDIDOS' ##NO_TEXT.
    CONSTANTS gc_log_id TYPE balobj_d VALUE 'ZFI_SOLADTBOMB' ##NO_TEXT.
    CONSTANTS gc_log_subid TYPE balsubobj VALUE 'SUJ_SOLADT' ##NO_TEXT.
    CONSTANTS gc_msg_id TYPE syst-msgid VALUE 'ZFI_BOMBEIO' ##NO_TEXT.
    CONSTANTS gc_msg_no1 TYPE syst-msgno VALUE '001' ##NO_TEXT.
    CONSTANTS gc_sucess TYPE syst-msgty VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_warning TYPE syst-msgty VALUE 'W' ##NO_TEXT.
    CONSTANTS gc_error TYPE syst-msgty VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_msg_no2 TYPE syst-msgno VALUE '002' ##NO_TEXT.
    CONSTANTS gc_msg_no3 TYPE syst-msgno VALUE '003' ##NO_TEXT.
    CONSTANTS gc_msg_no4 TYPE syst-msgno VALUE '004' ##NO_TEXT.
    CONSTANTS gc_msg_no5 TYPE syst-msgno VALUE '005' ##NO_TEXT.
    CONSTANTS gc_param_c2_raizcnpj TYPE ze_param_chave2 VALUE 'RAIZCNPJ' ##NO_TEXT.
    CONSTANTS gc_param_c3_matriz TYPE ze_param_chave3 VALUE 'FORNECEDORMATRIZ' ##NO_TEXT.
    CONSTANTS gc_msg_no6 TYPE syst-msgno VALUE '006' ##NO_TEXT.
    CONSTANTS gc_awtyp TYPE awtyp VALUE 'BKPFF' ##NO_TEXT.
    CONSTANTS gc_bktxt TYPE bktxt VALUE 'SUGESTAO-BOMBEIO' ##NO_TEXT.
    CONSTANTS gc_blart TYPE blart VALUE 'KA' ##NO_TEXT.
    CONSTANTS gc_xblnr TYPE xblnr VALUE 'SOL.BOMBEIO' ##NO_TEXT.
    CONSTANTS gc_posnr TYPE posnr_acc VALUE '1' ##NO_TEXT.
    CONSTANTS gc_final_busarea TYPE char2 VALUE '01' ##NO_TEXT.
    CONSTANTS gc_pay_meth TYPE acpi_zlsch VALUE 'A' ##NO_TEXT.
    CONSTANTS gc_pmt_block TYPE acpi_zlspr VALUE 'A' ##NO_TEXT.
    CONSTANTS gc_atrib TYPE char7 VALUE 'BOMBEIO' ##NO_TEXT.
    CONSTANTS gc_bvtyp TYPE hbkid VALUE 'SAN01' ##NO_TEXT.
    CONSTANTS gc_bstat TYPE accit-bstat VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_bschl TYPE accit-bschl VALUE '39' ##NO_TEXT.
    CONSTANTS gc_umskz TYPE accit-umskz VALUE 'F' ##NO_TEXT.
    CONSTANTS gc_zumsk TYPE accit-zumsk VALUE 'A' ##NO_TEXT.
    CONSTANTS gc_shkzg TYPE accit-shkzg VALUE 'H' ##NO_TEXT.
    CONSTANTS gc_ext1 TYPE strng250 VALUE 'ACCIT-XREF1_HD' ##NO_TEXT.
    CONSTANTS gc_ext2 TYPE strng250 VALUE 'GSBER' ##NO_TEXT.
    CONSTANTS gc_ext3 TYPE strng250 VALUE 'BSTAT' ##NO_TEXT.
    CONSTANTS gc_ext4 TYPE strng250 VALUE 'BSCHL' ##NO_TEXT.
    CONSTANTS gc_ext5 TYPE strng250 VALUE 'UMSKZ' ##NO_TEXT.
    CONSTANTS gc_ext6 TYPE strng250 VALUE 'ZUMSK' ##NO_TEXT.
    CONSTANTS gc_ext7 TYPE strng250 VALUE 'SHKZG' ##NO_TEXT.
    CONSTANTS gc_msg_no13 TYPE syst-msgno VALUE '013' ##NO_TEXT.
    CONSTANTS gc_msg_no14 TYPE syst-msgno VALUE '014' ##NO_TEXT.
    CONSTANTS gc_msg_no44 TYPE syst-msgno VALUE '044' ##NO_TEXT.
    CONSTANTS gc_msg_no45 TYPE syst-msgno VALUE '045' ##NO_TEXT.
    CONSTANTS gc_brl TYPE waers VALUE 'BRL' ##NO_TEXT.
    CONSTANTS gc_ext8 TYPE strng250 VALUE 'BUPLA' ##NO_TEXT.
    CONSTANTS gc_raz_espec TYPE acpi_umskz VALUE 'D' ##NO_TEXT.
    CONSTANTS gc_sign TYPE char1 VALUE 'I' ##NO_TEXT.
    CONSTANTS gc_opt_ge TYPE char2 VALUE 'GE' ##NO_TEXT.
    CONSTANTS gc_opt_eq TYPE char2 VALUE 'EQ' ##NO_TEXT.

    METHODS check_obrigatorio
      EXCEPTIONS
        werks_not_found .
    METHODS get_pedidos
      EXCEPTIONS
        pedidos_notf .
    METHODS get_parametros
      EXCEPTIONS
        param_not_found
        cnpj_werks_not_found .
    METHODS get_fornec
      EXCEPTIONS
        cnpj_fornec_not_found .
    METHODS selection_options .
    METHODS calcula_pedidos .
    METHODS log_create
      EXCEPTIONS
        error .
    METHODS log_add_msg
      IMPORTING
        !iv_msg_id TYPE syst-msgid
        !iv_msg_no TYPE syst-msgno
        !iv_msg_ty TYPE syst-msgty
        !iv_msg_v1 TYPE any OPTIONAL
        !iv_msg_v2 TYPE any OPTIONAL
        !iv_msg_v3 TYPE any OPTIONAL
        !iv_msg_v4 TYPE any OPTIONAL .
    METHODS log_save .
    METHODS lanca_documentos .
    METHODS fill_extension
      IMPORTING
        !iv_itemno           TYPE posnr_acc
        !iv_bukrs            TYPE bukrs
      RETURNING
        VALUE(rt_extension1) TYPE bapiacextc_tab .
ENDCLASS.



CLASS zclfi_sugst_adiat_bombeio IMPLEMENTATION.


  METHOD calcula_pedidos.

    DATA: ls_total TYPE ty_total.

    DATA: lv_calc TYPE p DECIMALS 6,
          lv_mult TYPE a_purchaseorderscheduleline-schedulelineorderquantity.

    CHECK gt_pedidos[] IS NOT INITIAL.

    LOOP AT gt_pedidos ASSIGNING FIELD-SYMBOL(<fs_pedidos>).

      READ TABLE gt_element TRANSPORTING NO FIELDS WITH KEY purchaseorder     = <fs_pedidos>-purchaseorder
                                                            purchaseorderitem = <fs_pedidos>-purchaseorderitem
                                                            BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT gt_element ASSIGNING FIELD-SYMBOL(<fs_element>) FROM sy-tabix.

          IF <fs_element>-purchaseorder     NE <fs_pedidos>-purchaseorder
          OR <fs_element>-purchaseorderitem NE <fs_pedidos>-purchaseorderitem.
            EXIT.
          ENDIF.

          IF <fs_element>-conditionamount IS NOT INITIAL.
            lv_calc = lv_calc + ( <fs_element>-conditionamount / <fs_pedidos>-orderquantity ).
          ENDIF.

*          IF <fs_element>-conditionbasevalue IS NOT INITIAL.
*            lv_calc = lv_calc + ( <fs_element>-conditionamount / <fs_element>-conditionbasevalue ).
*          ENDIF.

        ENDLOOP.
      ENDIF.

      READ TABLE gt_schedule TRANSPORTING NO FIELDS WITH KEY purchasingdocument     = <fs_pedidos>-purchaseorder
                                                             purchasingdocumentitem = <fs_pedidos>-purchaseorderitem
                                                             BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT gt_schedule ASSIGNING FIELD-SYMBOL(<fs_schedule>) FROM sy-tabix.

          IF <fs_schedule>-purchasingdocument     NE <fs_pedidos>-purchaseorder
          OR <fs_schedule>-purchasingdocumentitem NE <fs_pedidos>-purchaseorderitem.
            EXIT.
          ENDIF.

          lv_mult = lv_mult + <fs_schedule>-schedulelineorderquantity.

        ENDLOOP.
      ENDIF.

      IF lv_calc IS NOT INITIAL.
        ls_total-bukrs = <fs_pedidos>-companycode.
        ls_total-dmbtr = lv_calc * lv_mult.
        COLLECT ls_total INTO gt_total.
      ENDIF.

      CLEAR: ls_total,
             lv_calc,
             lv_mult.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_obrigatorio.

    IF gs_selopt-bukrs[] IS INITIAL.
      log_add_msg( iv_msg_id = gc_msg_id
                   iv_msg_no = gc_msg_no1
                   iv_msg_ty = gc_error ).

      RAISE werks_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD get_fornec.

    CHECK gt_fornec[] IS NOT INITIAL.

    SELECT lifnr,
           stcd1
      FROM fndei_lfa1_filter
       FOR ALL ENTRIES IN @gt_fornec
     WHERE stcd1 = @gt_fornec-stcd1
      INTO TABLE @DATA(lt_lfa1).

    IF sy-subrc IS INITIAL.

      SORT lt_lfa1 BY stcd1.

      DATA(lt_fornec) = gt_fornec[].
      FREE gt_fornec[].

      LOOP AT lt_fornec ASSIGNING FIELD-SYMBOL(<fs_fornec>).

        READ TABLE lt_lfa1 TRANSPORTING NO FIELDS
                                         WITH KEY stcd1 = <fs_fornec>-stcd1
                                         BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>) FROM sy-tabix.
            IF <fs_lfa1>-stcd1 NE <fs_fornec>-stcd1.
              EXIT.
            ENDIF.

            gt_fornec = VALUE #( BASE gt_fornec ( lifnr = <fs_lfa1>-lifnr
                                                  stcd1 = <fs_lfa1>-stcd1 ) ).

          ENDLOOP.
        ENDIF.

      ENDLOOP.

    ELSE.
      FREE gt_fornec[].
      log_add_msg( iv_msg_id = gc_msg_id
                   iv_msg_no = gc_msg_no4
                   iv_msg_ty = gc_error ).
      RAISE cnpj_fornec_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD get_parametros.

    DATA: lr_fornec  TYPE RANGE OF string.

    DATA lo_parametros TYPE REF TO zclca_tabela_parametros .

    lo_parametros = zclca_tabela_parametros=>get_instance( ).

    TRY.
        " Parâmetro Fornecedores PETROBRAS
        lo_parametros->m_get_range( EXPORTING iv_modulo = gc_param_mod
                                              iv_chave1 = gc_param_c1_bomb
                                              iv_chave2 = gc_param_c2_ctcnpj
                                              iv_chave3 = gc_param_c3_forn
                                    IMPORTING et_range  = lr_fornec ).

        IF lr_fornec[] IS NOT INITIAL.

          LOOP AT lr_fornec ASSIGNING FIELD-SYMBOL(<fs_fornec>).

            IF <fs_fornec>-low IN gs_selopt-werks.
              gt_fornec = VALUE #( BASE gt_fornec ( stcd1 = <fs_fornec>-high ) ).
            ENDIF.

          ENDLOOP.

          IF gt_fornec[] IS INITIAL.
            log_add_msg( iv_msg_id = gc_msg_id
                         iv_msg_no = gc_msg_no3
                         iv_msg_ty = gc_error ).
            RAISE cnpj_werks_not_found.
          ENDIF.

        ELSE.
          log_add_msg( iv_msg_id = gc_msg_id
                       iv_msg_no = gc_msg_no2
                       iv_msg_ty = gc_error
                       iv_msg_v1 = gc_param_mod
                       iv_msg_v2 = gc_param_c1_bomb
                       iv_msg_v3 = gc_param_c2_ctcnpj
                       iv_msg_v4 = gc_param_c3_forn ).
          RAISE param_not_found.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no2
                     iv_msg_ty = gc_error
                     iv_msg_v1 = gc_param_mod
                     iv_msg_v2 = gc_param_c1_bomb
                     iv_msg_v3 = gc_param_c2_ctcnpj
                     iv_msg_v4 = gc_param_c3_forn ).
        RAISE param_not_found.
    ENDTRY.

    TRY.
        " Parâmetro Tipo de Condição Pedido Bombeio
        lo_parametros->m_get_range( EXPORTING iv_modulo = gc_param_mod
                                              iv_chave1 = gc_param_c1_bomb
                                              iv_chave2 = gc_param_c2_tpcond
                                              iv_chave3 = gc_param_c3_pedid
                                    IMPORTING et_range  = gr_condtyp ).

        IF gr_condtyp[] IS INITIAL.
          log_add_msg( iv_msg_id = gc_msg_id
                       iv_msg_no = gc_msg_no2
                       iv_msg_ty = gc_error
                       iv_msg_v1 = gc_param_mod
                       iv_msg_v2 = gc_param_c1_bomb
                       iv_msg_v3 = gc_param_c2_tpcond
                       iv_msg_v4 = gc_param_c3_pedid ).
          RAISE param_not_found.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no2
                     iv_msg_ty = gc_error
                     iv_msg_v1 = gc_param_mod
                     iv_msg_v2 = gc_param_c1_bomb
                     iv_msg_v3 = gc_param_c2_tpcond
                     iv_msg_v4 = gc_param_c3_pedid ).
        RAISE param_not_found.
    ENDTRY.

    TRY.
        " Parâmetro Busca CNPJ da Matriz
        lo_parametros->m_get_single( EXPORTING iv_modulo = gc_param_mod
                                               iv_chave1 = gc_param_c1_bomb
                                               iv_chave2 = gc_param_c2_raizcnpj
                                               iv_chave3 = gc_param_c3_matriz
                                     IMPORTING ev_param  = gv_cpnpj_matriz ).

        IF gv_cpnpj_matriz IS INITIAL.
          log_add_msg( iv_msg_id = gc_msg_id
                       iv_msg_no = gc_msg_no5
                       iv_msg_ty = gc_error ).
          RAISE param_not_found.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no5
                     iv_msg_ty = gc_error ).
        RAISE param_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD get_pedidos.

    DATA: lr_date TYPE RANGE OF dats.

    CHECK gt_fornec[] IS NOT INITIAL.

    IF  gs_selopt-eindt[] IS INITIAL.

      " Somente considerar a data atual
      lr_date = VALUE #( BASE lr_date ( sign   = gc_sign
                                        option = gc_opt_eq
                                        low    = sy-datum ) ).

    ELSE.

      lr_date[] = gs_selopt-eindt[].

    ENDIF.

    SELECT supplier,
           companycode,
           purchaseorder,
           creationdate,
           purchaseorderitem,
           orderquantity
      FROM zi_fi_cockpit_bombeio_ped
       FOR ALL ENTRIES IN @gt_fornec
     WHERE companycode       IN @gs_selopt-bukrs
       AND supplier          EQ @gt_fornec-lifnr
       AND purchaseorder     IN @gs_selopt-ebeln
       AND purchaseorderitem IN @gs_selopt-ebelp
       AND creationdate      IN @gs_selopt-aedat
      INTO TABLE @gt_pedidos.

    IF sy-subrc IS INITIAL.

      SORT gt_pedidos BY supplier
                         companycode
                         purchaseorder
                         purchaseorderitem.

      DATA(lt_pedidos_fae) = gt_pedidos[].
      SORT lt_pedidos_fae BY purchaseorder
                             purchaseorderitem.
      DELETE ADJACENT DUPLICATES FROM lt_pedidos_fae COMPARING purchaseorder
                                                               purchaseorderitem.

      SELECT purchaseorder,
             purchaseorderitem,
             conditiontype,
             conditionamount,
             conditionquantity,
             conditionbasevalue
        FROM a_purordpricingelement
         FOR ALL ENTRIES IN @lt_pedidos_fae
       WHERE purchaseorder     EQ @lt_pedidos_fae-purchaseorder
         AND purchaseorderitem EQ @lt_pedidos_fae-purchaseorderitem
         AND conditiontype     IN @gr_condtyp
        INTO TABLE @gt_element.

      IF sy-subrc IS INITIAL.
        SORT gt_element BY purchaseorder
                           purchaseorderitem.
      ENDIF.

      SELECT purchasingdocument,
             purchasingdocumentitem,
             scheduleline,
             schedulelineorderquantity
        FROM a_purchaseorderscheduleline
         FOR ALL ENTRIES IN @lt_pedidos_fae
       WHERE purchasingdocument       = @lt_pedidos_fae-purchaseorder
         AND purchasingdocumentitem   = @lt_pedidos_fae-purchaseorderitem
         AND schedulelinedeliverydate IN @lr_date
        INTO TABLE @gt_schedule.

      IF sy-subrc IS INITIAL.
        SORT gt_schedule BY purchasingdocument
                            purchasingdocumentitem.
      ENDIF.

    ELSE.
      log_add_msg( iv_msg_id = gc_msg_id
                   iv_msg_no = gc_msg_no5
                   iv_msg_ty = gc_error ).
      RAISE pedidos_notf.
    ENDIF.

  ENDMETHOD.


  METHOD main.

    selection_options( ).

    " Validação de obrigatoriedade
    check_obrigatorio( EXCEPTIONS werks_not_found = 1
                                  OTHERS          = 2 ).
    IF sy-subrc IS INITIAL.

      " Busca parâmetros
      get_parametros( EXCEPTIONS param_not_found      = 1
                                 cnpj_werks_not_found = 2
                                 OTHERS               = 3 ).
      IF sy-subrc IS INITIAL.

        " Busca Fornecedores
        get_fornec( EXCEPTIONS cnpj_fornec_not_found = 1
                               OTHERS                = 2 ).
        IF sy-subrc IS INITIAL.

          " Seleção dos pedidos
          get_pedidos( EXCEPTIONS pedidos_notf = 1
                                  OTHERS       = 2 ).

          " Execução dos calculos
          calcula_pedidos( ).

          " Lançamento
          lanca_documentos( ).

        ENDIF.
      ENDIF.
    ENDIF.

    " Salva LOG SLG1
    log_save( ).

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


  METHOD log_add_msg.

    DATA(ls_msg) = VALUE bal_s_msg( msgty = iv_msg_ty
                                    msgid = iv_msg_id
                                    msgno = iv_msg_no
                                    msgv1 = CONV syst_msgv( iv_msg_v1 )
                                    msgv2 = CONV syst_msgv( iv_msg_v2 )
                                    msgv3 = CONV syst_msgv( iv_msg_v3 )
                                    msgv4 = CONV syst_msgv( iv_msg_v4 ) ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = gv_log_handle
        i_s_msg          = ls_msg
*     IMPORTING
*       E_S_MSG_HANDLE   =
*       E_MSG_WAS_LOGGED =
*       E_MSG_WAS_DISPLAYED       =
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      CLEAR ls_msg.
    ENDIF.

  ENDMETHOD.


  METHOD log_create.

    DATA(ls_log) = VALUE bal_s_log( object    = gc_log_id
                                    subobject = gc_log_subid
                                    aluser = sy-uname
                                    alprog = sy-repid ).

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = gv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    log_create( EXCEPTIONS error  = 1
                           OTHERS = 2 ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD log_save.

    DATA: lt_log_handle TYPE bal_t_logh.

    APPEND gv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_save_all       = abap_true
        i_t_log_handle   = lt_log_handle
*       I_2TH_CONNECTION = ' '
*       I_2TH_CONNECT_COMMIT       = ' '
*       I_LINK2JOB       = 'X'
*   IMPORTING
*       E_NEW_LOGNUMBERS =
*       E_SECOND_CONNECTION        =
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      FREE: lt_log_handle[].
    ENDIF.

  ENDMETHOD.


  METHOD fill_extension.

    CONSTANTS lc_0001 TYPE char4 VALUE '0001'.

    DATA: ls_extension1 TYPE bapiacextc.

    DATA: lv_locn TYPE bupla,
          lv_key  TYPE strng250.


    lv_key  = |{ lc_0001 }{ iv_itemno }|.
*    lv_key  = |{ '0001' }{ iv_itemno }|.
    lv_locn = |{ iv_bukrs(2) }{ gc_final_busarea }|.

    rt_extension1 = VALUE #( BASE rt_extension1 ( field1 = lv_key
                                                  field2 = gc_ext2
                                                  field3 = lv_locn ) ).

    rt_extension1 = VALUE #( BASE rt_extension1 ( field1 = lv_key
                                                  field2 = gc_ext3
                                                  field3 = gc_bstat ) ).

    rt_extension1 = VALUE #( BASE rt_extension1 ( field1 = lv_key
                                                  field2 = gc_ext4
                                                  field3 = gc_bschl ) ).

    rt_extension1 = VALUE #( BASE rt_extension1 ( field1 = lv_key
                                                  field2 = gc_ext5
                                                  field3 = gc_umskz ) ).

    rt_extension1 = VALUE #( BASE rt_extension1 ( field1 = lv_key
                                                  field2 = gc_ext6
                                                  field3 = gc_zumsk ) ).

    rt_extension1 = VALUE #( BASE rt_extension1 ( field1 = lv_key
                                                  field2 = gc_ext7
                                                  field3 = gc_shkzg ) ).

    rt_extension1 = VALUE #( BASE rt_extension1 ( field1 = lv_key
                                                  field2 = gc_ext8
                                                  field3 = lv_locn ) ).

  ENDMETHOD.


  METHOD lanca_documentos.

    DATA: lt_payable        TYPE STANDARD TABLE OF bapiacap09,
          lt_currencyamount TYPE STANDARD TABLE OF bapiaccr09,
          lt_extension      TYPE STANDARD TABLE OF bapiacextc,
          lt_return         TYPE STANDARD TABLE OF bapiret2.

    DATA: lv_doc TYPE bapiache09-obj_key.

    CONSTANTS: lc_rw  TYPE syst-msgid VALUE 'RW',
               lc_num TYPE syst-msgno VALUE '605'.

    CHECK gt_total[] IS NOT INITIAL.

    SELECT lifnr,
           stcd1
      FROM fndei_lfa1_filter
     WHERE stcd1 = @gv_cpnpj_matriz
      INTO @DATA(ls_fornc_matriz)
      UP TO 1 ROWS.
    ENDSELECT.

    LOOP AT gt_total ASSIGNING FIELD-SYMBOL(<fs_total>).

      IF <fs_total>-dmbtr IS INITIAL AND gs_selopt-eindt IS NOT INITIAL.

        " Empresa &1 sem remessa prevista para a data informada.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no44
                     iv_msg_ty = gc_warning
                     iv_msg_v1 = <fs_total>-bukrs ).
        CONTINUE.

      ELSEIF <fs_total>-dmbtr IS INITIAL AND gs_selopt-eindt IS INITIAL.

        " Empresa &1 sem remessa prevista para a data atual.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no45
                     iv_msg_ty = gc_warning
                     iv_msg_v1 = <fs_total>-bukrs ).
        CONTINUE.

      ENDIF.


      DATA(ls_header) = VALUE bapiache09( obj_type   = gc_awtyp
                                          username   = sy-uname
                                          header_txt = gc_bktxt
                                          comp_code  = <fs_total>-bukrs
                                          doc_date   = sy-datum
                                          pstng_date = sy-datum
                                          fisc_year  = sy-datum(4)
                                          fis_period = sy-datum+4(2)
                                          doc_type   = gc_blart
                                          ref_doc_no = gc_xblnr ).

      lt_payable = VALUE #( BASE lt_payable ( itemno_acc = gc_posnr
                                              vendor_no  = ls_fornc_matriz-lifnr
                                              comp_code  = <fs_total>-bukrs
                                              bus_area   = |{ <fs_total>-bukrs(2) }{ gc_final_busarea }|
                                              bline_date = sy-datum
                                              pymt_meth  = gc_pay_meth
                                              pmnt_block = gc_pmt_block
                                              sp_gl_ind  = gc_raz_espec
                                              alloc_nmbr = |{ gc_atrib } { sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|
                                              bank_id    = gc_bvtyp
                                              item_text  = gc_bktxt ) ).

      lt_currencyamount = VALUE #( BASE lt_currencyamount ( itemno_acc = gc_posnr
                                                            currency   = gc_brl
                                                            amt_doccur = ( <fs_total>-dmbtr * -1 ) ) ).

      lt_extension = fill_extension( EXPORTING iv_itemno = gc_posnr
                                               iv_bukrs  = <fs_total>-bukrs ).

      " Preenchemos este campo para pular a validação da EXIT.
      sy-tcode = zclfi_cockpit_bombeio_event=>gc_transaction-bombeio.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = ls_header
        IMPORTING
          obj_key        = lv_doc
        TABLES
          accountpayable = lt_payable
          currencyamount = lt_currencyamount
          extension1     = lt_extension
          return         = lt_return.

      IF NOT line_exists( lt_return[ type = gc_error ] ). "#EC CI_STDSEQ
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        DATA(ls_key) = VALUE bkpf_key( bukrs = lv_doc+10(4)
                                       belnr = lv_doc+0(10)
                                       gjahr = lv_doc+14(4) ).

        " Documento &1 Ano &2 criado com sucesso para empresa &3.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no13
                     iv_msg_ty = gc_sucess
                     iv_msg_v1 = ls_key-belnr
                     iv_msg_v2 = ls_key-gjahr
                     iv_msg_v3 = ls_key-bukrs  ).

      ELSE.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no14
                     iv_msg_ty = gc_error
                     iv_msg_v1 = <fs_total>-bukrs ).

        LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).

          log_add_msg( iv_msg_id = <fs_return>-id
                       iv_msg_no = <fs_return>-number
                       iv_msg_ty = <fs_return>-type
                       iv_msg_v1 = <fs_return>-message_v1
                       iv_msg_v2 = <fs_return>-message_v2
                       iv_msg_v3 = <fs_return>-message_v3
                       iv_msg_v4 = <fs_return>-message_v4 ).

        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
