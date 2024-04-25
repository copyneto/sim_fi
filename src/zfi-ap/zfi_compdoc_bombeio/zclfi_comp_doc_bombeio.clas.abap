CLASS zclfi_comp_doc_bombeio DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_refdata,
        bukrs TYPE REF TO data,
        werks TYPE REF TO data,
        belnr TYPE REF TO data,
        gjahr TYPE REF TO data,
        buzei TYPE REF TO data,
        lifnr TYPE REF TO data,
      END OF ty_refdata .
    DATA
      gs_refdata TYPE ty_refdata .
    TYPES:
      BEGIN OF ty_param,
        bomb  TYPE REF TO boolean,
        todos TYPE REF TO boolean,
      END OF ty_param .
    DATA
      gs_param TYPE ty_param.

    METHODS constructor .
    METHODS main
      EXCEPTIONS
        error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_selopt,
        bukrs TYPE RANGE OF bsik_view-bukrs,
        werks TYPE RANGE OF bseg-werks,
        belnr TYPE RANGE OF bsik_view-belnr,
        gjahr TYPE RANGE OF bsik_view-gjahr,
        buzei TYPE RANGE OF bsik_view-buzei,
        lifnr TYPE RANGE OF bsik_view-lifnr,
      END OF ty_selopt .
    TYPES:
      BEGIN OF ty_fornec,
        stcd1_raiz TYPE char8,
        stcd1      TYPE stcd1,
        lifnr      TYPE lifnr,
      END OF ty_fornec .
    TYPES:
      BEGIN OF ty_raiz,
        stcd1      TYPE stcd1,
        stcd1_low  TYPE stcd1,
        stcd1_high TYPE stcd1,
        lifnr      TYPE lifnr,
      END OF ty_raiz .
    TYPES:
      BEGIN OF ty_bco_empresa,
        bukrs TYPE bukrs,
        hbkid TYPE hbkid,
      END OF ty_bco_empresa .

    DATA:
      gt_bco_empresa TYPE STANDARD TABLE OF ty_bco_empresa .
    DATA gs_selopt TYPE ty_selopt .
    DATA:
      gt_fornec TYPE STANDARD TABLE OF ty_fornec .
    DATA gs_raiz TYPE ty_raiz .
    DATA gv_log_handle TYPE balloghndl .
    DATA:
      gr_zterm TYPE RANGE OF dzterm .
    DATA gv_tpdoc TYPE blart .
    DATA gv_fpag TYPE bseg-zlsch .
    CONSTANTS gc_param_mod TYPE ze_param_modulo VALUE 'FI-AP' ##NO_TEXT.
    CONSTANTS gc_param_c1_bomb TYPE ze_param_chave1 VALUE 'BOMBEIO' ##NO_TEXT.
    CONSTANTS gc_param_c2_ctcnpj TYPE ze_param_chave2 VALUE 'RAIZCNPJ' ##NO_TEXT.
    CONSTANTS gc_param_c3_forn TYPE ze_param_chave3 VALUE 'FORNECEDORMATRIZ' ##NO_TEXT.
    CONSTANTS gc_param_c2_tpcond TYPE ze_param_chave2 VALUE 'CONDICAOPGTO' ##NO_TEXT.
    CONSTANTS gc_param_c3_excld TYPE ze_param_chave3 VALUE 'EXCLUIDAS' ##NO_TEXT.
    CONSTANTS gc_log_id TYPE balobj_d VALUE 'ZFI_COMPBOMB' ##NO_TEXT.
    CONSTANTS gc_log_subid TYPE balsubobj VALUE 'COMP_DEBCRED' ##NO_TEXT.
    CONSTANTS gc_msg_id TYPE syst-msgid VALUE 'ZFI_BOMBEIO' ##NO_TEXT.
    CONSTANTS gc_msg_no1 TYPE syst-msgno VALUE '001' ##NO_TEXT.
    CONSTANTS gc_sucess TYPE syst-msgty VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_error TYPE syst-msgty VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_msg_no2 TYPE syst-msgno VALUE '002' ##NO_TEXT.
    CONSTANTS gc_msg_no3 TYPE syst-msgno VALUE '003' ##NO_TEXT.
    CONSTANTS gc_msg_no4 TYPE syst-msgno VALUE '004' ##NO_TEXT.
    CONSTANTS gc_msg_no5 TYPE syst-msgno VALUE '005' ##NO_TEXT.
    CONSTANTS gc_msg_no36 TYPE syst-msgno VALUE '036' ##NO_TEXT.
    CONSTANTS gc_msg_no37 TYPE syst-msgno VALUE '037' ##NO_TEXT.
    CONSTANTS gc_msg_no38 TYPE syst-msgno VALUE '038' ##NO_TEXT.
    CONSTANTS gc_msg_no39 TYPE syst-msgno VALUE '039' ##NO_TEXT.
    CONSTANTS gc_msg_no40 TYPE syst-msgno VALUE '040' ##NO_TEXT.
    CONSTANTS gc_msg_no41 TYPE syst-msgno VALUE '041' ##NO_TEXT.
    CONSTANTS gc_msg_no42 TYPE syst-msgno VALUE '042' ##NO_TEXT.
    CONSTANTS gc_msg_no43 TYPE syst-msgno VALUE '043' ##NO_TEXT.
    CONSTANTS gc_msg_no0 TYPE syst-msgno VALUE '000' ##NO_TEXT.
    CONSTANTS gc_ini_cnpj TYPE char6 VALUE '000000' ##NO_TEXT.
    CONSTANTS gc_fin_cnpj TYPE char6 VALUE '999999' ##NO_TEXT.
    CONSTANTS gc_param_mod_ap TYPE ze_param_modulo VALUE 'FI-AP' ##NO_TEXT.
    CONSTANTS gc_koart_k TYPE koart VALUE 'K' ##NO_TEXT.
    CONSTANTS gc_koart_p TYPE koart VALUE 'P' ##NO_TEXT.
    CONSTANTS gc_debt_s TYPE koart VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_cred_h TYPE koart VALUE 'H' ##NO_TEXT.
    CONSTANTS gc_item_1 TYPE count_pi VALUE '001' ##NO_TEXT.
    CONSTANTS gc_item_2 TYPE count_pi VALUE '002' ##NO_TEXT.
    CONSTANTS gc_bukrs_adcnl TYPE char2 VALUE '01' ##NO_TEXT.
    CONSTANTS gc_field_doc TYPE fld30_f05a VALUE 'BELNR' ##NO_TEXT.
    CONSTANTS gc_field_bldat TYPE bdc_fnam VALUE 'BKPF-BLDAT' ##NO_TEXT.
    CONSTANTS gc_field_budat TYPE bdc_fnam VALUE 'BKPF-BUDAT' ##NO_TEXT.
    CONSTANTS gc_field_blart TYPE bdc_fnam VALUE 'BKPF-BLART' ##NO_TEXT.
    CONSTANTS gc_field_bukrs TYPE bdc_fnam VALUE 'BKPF-BUKRS' ##NO_TEXT.
    CONSTANTS gc_field_waers TYPE bdc_fnam VALUE 'BKPF-WAERS' ##NO_TEXT.
    CONSTANTS gc_field_bktxt TYPE bdc_fnam VALUE 'BKPF-BKTXT' ##NO_TEXT.
    CONSTANTS gc_field_bupla TYPE bdc_fnam VALUE 'BSEG-BUPLA' ##NO_TEXT.
    CONSTANTS gc_field_newbs TYPE bdc_fnam VALUE 'RF05A-NEWBS' ##NO_TEXT.
    CONSTANTS gc_field_newum TYPE bdc_fnam VALUE 'RF05A-NEWUM' ##NO_TEXT.
    CONSTANTS gc_field_newko TYPE bdc_fnam VALUE 'RF05A-NEWKO' ##NO_TEXT.
    CONSTANTS gc_field_wrbtr TYPE bdc_fnam VALUE 'BSEG-WRBTR' ##NO_TEXT.
    CONSTANTS gc_field_sgtxt TYPE bdc_fnam VALUE 'BSEG-SGTXT' ##NO_TEXT.
    CONSTANTS gc_field_zfbdt TYPE bdc_fnam VALUE 'BSEG-ZFBDT' ##NO_TEXT.
    CONSTANTS gc_field_hbkid TYPE bdc_fnam VALUE 'BSEG-HBKID' ##NO_TEXT.
    CONSTANTS gc_field_hktid TYPE bdc_fnam VALUE 'BSEG-HKTID' ##NO_TEXT.
    CONSTANTS gc_field_zlsch TYPE bdc_fnam VALUE 'BSEG-ZLSCH' ##NO_TEXT.
    CONSTANTS gc_brl TYPE bdc_fval VALUE 'BRL' ##NO_TEXT.
    CONSTANTS gc_newbs_d TYPE newbs VALUE '29' ##NO_TEXT.
    CONSTANTS gc_newbs_c TYPE newbs VALUE '31' ##NO_TEXT.
    CONSTANTS gc_newum_a TYPE newum VALUE 'A' ##NO_TEXT.
    CONSTANTS gc_hktid TYPE bseg-hktid VALUE '00001' ##NO_TEXT.
    CONSTANTS gc_param_c2_concil TYPE ze_param_chave2 VALUE 'CONCILIACAO' ##NO_TEXT.
    CONSTANTS gc_param_c3_tpdoc TYPE ze_param_chave3 VALUE 'TIPODOC' ##NO_TEXT.
    CONSTANTS gc_param_c3_bcoemp TYPE ze_param_chave3 VALUE 'BANCOEMPRESA' ##NO_TEXT.
    CONSTANTS gc_param_c3_formpg TYPE ze_param_chave3 VALUE 'FORMAPGTO' ##NO_TEXT.
    CONSTANTS gc_ponto TYPE char1 VALUE '.' ##NO_TEXT.
    CONSTANTS gc_id_post_s TYPE syst-msgid VALUE 'F5' ##NO_TEXT.
    CONSTANTS gc_no_post_s TYPE syst-msgno VALUE '312' ##NO_TEXT.
    CONSTANTS gc_function_c TYPE funct_pi  VALUE 'C' ##NO_TEXT.
    CONSTANTS gc_sfunction_c TYPE sgfunct_pi  VALUE 'C' ##NO_TEXT.
    CONSTANTS gc_mode_batch TYPE rfpdo-allgazmd VALUE 'N' ##NO_TEXT.
    CONSTANTS gc_updt TYPE rfpdo-allgvbmd VALUE 'U' ##NO_TEXT.
    CONSTANTS gc_updt_s TYPE rfpdo-allgvbmd VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_post_id TYPE t041a-auglv VALUE 'UMBUCHNG' ##NO_TEXT.
    CONSTANTS gc_tcode TYPE sy-tcode VALUE 'FB05' ##NO_TEXT.


    METHODS selection_options .
    METHODS get_parameters
      EXCEPTIONS
        cnpj_werks_not_found
        param_not_found .
    METHODS get_fornec
      EXCEPTIONS
        cnpj_fornec_not_found .
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
    METHODS processa_bombeio
      EXCEPTIONS
        fat_notf .
    METHODS processa_todos
      EXCEPTIONS
        fat_notf .
ENDCLASS.



CLASS ZCLFI_COMP_DOC_BOMBEIO IMPLEMENTATION.


  METHOD constructor.

    log_create( EXCEPTIONS error  = 1
                           OTHERS = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD selection_options.

    DATA: lo_ref_descr   TYPE REF TO cl_abap_structdescr.
    DATA: lt_detail      TYPE abap_compdescr_tab.

    FIELD-SYMBOLS: <fs_table>     TYPE STANDARD TABLE,
                   <fs_ref>       TYPE REF TO data,
                   <fs_ref_param> TYPE REF TO boolean.

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


  METHOD main.

    selection_options( ).

    " Busca parâmetros
    get_parameters( EXCEPTIONS param_not_found      = 1
                               cnpj_werks_not_found = 2
                               OTHERS               = 3 ).
    IF sy-subrc IS  INITIAL.

      " Busca Fornecedores
      get_fornec( EXCEPTIONS cnpj_fornec_not_found = 1
                             OTHERS                = 2 ).

      IF sy-subrc IS INITIAL.

        " Processamento de Fornecedores Bombeio
        IF gs_param-bomb->* IS NOT INITIAL.
          processa_bombeio( EXCEPTIONS fat_notf = 1
                                       OTHERS   = 2 ).
        ELSE.
          processa_todos( EXCEPTIONS fat_notf = 1
                                     OTHERS   = 2 ).
        ENDIF.

      ENDIF.
    ENDIF.

    log_save( ).

  ENDMETHOD.


  METHOD get_parameters.

    DATA lo_parametros TYPE REF TO zclca_tabela_parametros .

    DATA: lr_string TYPE RANGE OF string.

    lo_parametros = zclca_tabela_parametros=>get_instance( ).

    TRY.
        " Parâmetro Raiz CNPJ Matriz Petrobras
        lo_parametros->m_get_single( EXPORTING iv_modulo = gc_param_mod
                                               iv_chave1 = gc_param_c1_bomb
                                               iv_chave2 = gc_param_c2_ctcnpj
                                               iv_chave3 = gc_param_c3_forn
                                     IMPORTING ev_param  = gs_raiz-stcd1 ).

        IF gs_raiz-stcd1 IS NOT INITIAL.

          " Considerar somente a Raiz do CNPJ para buscar os fornecedores
          gs_raiz-stcd1_low  = |{ gs_raiz-stcd1(8) }{ gc_ini_cnpj }|.
          gs_raiz-stcd1_high = |{ gs_raiz-stcd1(8) }{ gc_fin_cnpj }|.

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
        " Parâmetro Cond.pgto.(ZTERM) a serem excluídos do processamento
        lo_parametros->m_get_range( EXPORTING iv_modulo = gc_param_mod_ap
                                              iv_chave1 = gc_param_c1_bomb
                                              iv_chave2 = gc_param_c2_tpcond
                                              iv_chave3 = gc_param_c3_excld
                                    IMPORTING et_range  = gr_zterm ).

        IF gr_zterm[] IS INITIAL.

          log_add_msg( iv_msg_id = gc_msg_id
                       iv_msg_no = gc_msg_no2
                       iv_msg_ty = gc_error
                       iv_msg_v1 = gc_param_mod
                       iv_msg_v2 = gc_param_c1_bomb
                       iv_msg_v3 = gc_param_c2_tpcond
                       iv_msg_v4 = gc_param_c3_excld ).
          RAISE param_not_found.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no2
                     iv_msg_ty = gc_error
                     iv_msg_v1 = gc_param_mod
                     iv_msg_v2 = gc_param_c1_bomb
                     iv_msg_v3 = gc_param_c2_tpcond
                     iv_msg_v4 = gc_param_c3_excld ).
        RAISE param_not_found.
    ENDTRY.

    TRY.
        " Parâmetro TIPO DE DOCUMENTO CONCILIACAO BOMBEIO
        lo_parametros->m_get_single( EXPORTING iv_modulo = gc_param_mod_ap
                                               iv_chave1 = gc_param_c1_bomb
                                               iv_chave2 = gc_param_c2_concil
                                               iv_chave3 = gc_param_c3_tpdoc
                                     IMPORTING ev_param  = gv_tpdoc ).

        IF gv_tpdoc IS INITIAL.

          log_add_msg( iv_msg_id = gc_msg_id
                       iv_msg_no = gc_msg_no2
                       iv_msg_ty = gc_error
                       iv_msg_v1 = gc_param_mod
                       iv_msg_v2 = gc_param_c1_bomb
                       iv_msg_v3 = gc_param_c2_concil
                       iv_msg_v4 = gc_param_c3_tpdoc ).
          RAISE param_not_found.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no2
                     iv_msg_ty = gc_error
                     iv_msg_v1 = gc_param_mod
                     iv_msg_v2 = gc_param_c1_bomb
                     iv_msg_v3 = gc_param_c2_concil
                     iv_msg_v4 = gc_param_c3_tpdoc ).
        RAISE param_not_found.
    ENDTRY.

    TRY.
        " Parâmetro BANCO EMPRESA CONCILIACAO BOMBEIO
        lo_parametros->m_get_range( EXPORTING iv_modulo = gc_param_mod_ap
                                              iv_chave1 = gc_param_c1_bomb
                                              iv_chave2 = gc_param_c2_concil
                                              iv_chave3 = gc_param_c3_bcoemp
                                    IMPORTING et_range  = lr_string ).

        IF lr_string[] IS NOT INITIAL.

          LOOP AT lr_string ASSIGNING FIELD-SYMBOL(<fs_string>).
            gt_bco_empresa = VALUE #( BASE gt_bco_empresa ( bukrs = <fs_string>-low
                                                            hbkid = <fs_string>-high ) ).
          ENDLOOP.

          SORT gt_bco_empresa BY bukrs.

        ELSE.

          log_add_msg( iv_msg_id = gc_msg_id
                       iv_msg_no = gc_msg_no2
                       iv_msg_ty = gc_error
                       iv_msg_v1 = gc_param_mod
                       iv_msg_v2 = gc_param_c1_bomb
                       iv_msg_v3 = gc_param_c2_concil
                       iv_msg_v4 = gc_param_c3_bcoemp ).
          RAISE param_not_found.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no2
                     iv_msg_ty = gc_error
                     iv_msg_v1 = gc_param_mod
                     iv_msg_v2 = gc_param_c1_bomb
                     iv_msg_v3 = gc_param_c2_concil
                     iv_msg_v4 = gc_param_c3_bcoemp ).
        RAISE param_not_found.
    ENDTRY.

    TRY.
        " Parâmetro BANCO EMPRESA CONCILIACAO BOMBEIO
        lo_parametros->m_get_single( EXPORTING iv_modulo = gc_param_mod_ap
                                               iv_chave1 = gc_param_c1_bomb
                                               iv_chave2 = gc_param_c2_concil
                                               iv_chave3 = gc_param_c3_formpg
                                     IMPORTING ev_param  = gv_fpag ).

        IF gv_fpag IS INITIAL.

          log_add_msg( iv_msg_id = gc_msg_id
                       iv_msg_no = gc_msg_no2
                       iv_msg_ty = gc_error
                       iv_msg_v1 = gc_param_mod
                       iv_msg_v2 = gc_param_c1_bomb
                       iv_msg_v3 = gc_param_c2_concil
                       iv_msg_v4 = gc_param_c3_formpg ).
          RAISE param_not_found.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        log_add_msg( iv_msg_id = gc_msg_id
                     iv_msg_no = gc_msg_no2
                     iv_msg_ty = gc_error
                     iv_msg_v1 = gc_param_mod
                     iv_msg_v2 = gc_param_c1_bomb
                     iv_msg_v3 = gc_param_c2_concil
                     iv_msg_v4 = gc_param_c3_formpg ).
        RAISE param_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD get_fornec.

    CHECK gs_raiz IS NOT INITIAL.

    " Fornecedores que pertecem ao CNPJ Matriz
    SELECT lifnr,
           stcd1
      FROM fndei_lfa1_filter
      INTO TABLE @DATA(lt_lfa1)
     WHERE stcd1 GE @gs_raiz-stcd1_low
       AND stcd1 LE @gs_raiz-stcd1_high.

    SELECT *
     FROM zi_fi_param_bombeio_cnpj_cent
       INTO TABLE @DATA(lt_forne_bombeio).
    IF sy-subrc = 0.

      SELECT lifnr,
            stcd1
       FROM fndei_lfa1_filter
       FOR ALL ENTRIES IN @lt_forne_bombeio
      WHERE lifnr = @lt_forne_bombeio-supplier
       APPENDING TABLE @lt_lfa1.

    ENDIF.

    SORT: lt_lfa1 BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_lfa1 COMPARING lifnr.

    IF lt_lfa1 IS NOT INITIAL.

      SORT lt_lfa1 BY stcd1.

      LOOP AT lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>).

        gt_fornec = VALUE #( BASE gt_fornec ( stcd1_raiz = <fs_lfa1>-stcd1(8)
                                              stcd1      = <fs_lfa1>-stcd1
                                              lifnr      = <fs_lfa1>-lifnr ) ).
      ENDLOOP.

    ELSE.

      log_add_msg( iv_msg_id = gc_msg_id
                   iv_msg_no = gc_msg_no4
                   iv_msg_ty = gc_error ).
      RAISE cnpj_fornec_not_found.
    ENDIF.

    " Fornecedor do CNPJ Matriz
    SELECT lifnr,
           stcd1
      FROM fndei_lfa1_filter
     WHERE stcd1 EQ @gs_raiz-stcd1
      INTO @DATA(lv_lfa1_matriz)
        UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc IS INITIAL.

      gs_raiz-lifnr = lv_lfa1_matriz-lifnr.

    ELSE.

      log_add_msg( iv_msg_id = gc_msg_id
                   iv_msg_no = gc_msg_no4
                   iv_msg_ty = gc_error ).
      RAISE cnpj_fornec_not_found.
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
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      CLEAR ls_msg.
    ENDIF.

  ENDMETHOD.


  METHOD log_save.

    DATA: lt_log_handle TYPE bal_t_logh.

    APPEND gv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_save_all       = abap_true
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      FREE: lt_log_handle[].
    ENDIF.

  ENDMETHOD.


  METHOD processa_bombeio.

    TYPES: BEGIN OF ty_agroup,
             stcd1_raiz TYPE char6,
             bukrs      TYPE bukrs,
             lifnr      TYPE lifnr,
           END OF ty_agroup.

    DATA: lt_agroup  TYPE STANDARD TABLE OF ty_agroup,
          lt_ftclear TYPE STANDARD TABLE OF ftclear,
          lt_ftpost  TYPE STANDARD TABLE OF ftpost,
          lt_blntab  TYPE STANDARD TABLE OF blntab,
          lt_fttax   TYPE STANDARD TABLE OF fttax.

    DATA: lv_tot_cred TYPE dmbtr,
          lv_tot_deb  TYPE dmbtr,
          lv_dmbtr    TYPE string,
          lv_msgid    TYPE sy-msgid,
          lv_msgno    TYPE sy-msgno,
          lv_msgty    TYPE sy-msgty,
          lv_msgv1    TYPE sy-msgv1,
          lv_msgv2    TYPE sy-msgv2,
          lv_msgv3    TYPE sy-msgv3,
          lv_msgv4    TYPE sy-msgv4,
          lv_subrc    TYPE sy-subrc,
          lv_cmp_z    TYPE char1,
          lv_cmp_p    TYPE char1,
          lv_cmp_n    TYPE char1.

    CHECK gt_fornec[] IS NOT INITIAL.

    SELECT bukrs,
           gjahr,
           belnr,
           buzei,
           lifnr,
           dmbtr,
           umskz,
           shkzg
      FROM bsik_view
       FOR ALL ENTRIES IN @gt_fornec
     WHERE bukrs IN @gs_selopt-bukrs
       AND belnr IN @gs_selopt-belnr
       AND gjahr IN @gs_selopt-gjahr
       AND buzei IN @gs_selopt-buzei
       AND ( lifnr IN @gs_selopt-lifnr AND lifnr EQ @gt_fornec-lifnr ) " O fornecedor deve pertencer ao CNPJ Raiz
       AND bstat IS INITIAL
       AND zterm NOT IN @gr_zterm
      INTO TABLE @DATA(lt_bsik).


    IF sy-subrc IS INITIAL.

*      SORT lt_bsik BY bukrs
*                      lifnr.
*      DATA(lt_bsik_aux) = lt_bsik[].
*      DELETE ADJACENT DUPLICATES FROM lt_bsik_aux COMPARING bukrs
*                                                            lifnr.
*      SORT gt_fornec BY lifnr.
*
*      LOOP AT lt_bsik_aux ASSIGNING FIELD-SYMBOL(<fs_bsik_aux>).
*
*        READ TABLE gt_fornec ASSIGNING FIELD-SYMBOL(<fs_fornec>)
*                                           WITH KEY lifnr = <fs_bsik_aux>-lifnr
*                                           BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*
*          lt_agroup = VALUE #( BASE lt_agroup ( stcd1_raiz = <fs_fornec>-stcd1_raiz
*                                                bukrs      = <fs_bsik_aux>-bukrs
*                                                lifnr      = <fs_bsik_aux>-lifnr ) ).
*
*        ENDIF.
*      ENDLOOP.
*
*      SORT lt_agroup BY stcd1_raiz
*                        bukrs
*                        lifnr.
*      DELETE ADJACENT DUPLICATES FROM lt_agroup COMPARING stcd1_raiz
*                                                          bukrs
*                                                          lifnr.
*      DATA(lt_agroup_head) = lt_agroup[].
*      DELETE ADJACENT DUPLICATES FROM lt_agroup_head COMPARING stcd1_raiz
*                                                               bukrs.

*      LOOP AT lt_agroup_head ASSIGNING FIELD-SYMBOL(<fs_head>).
*      LOOP AT lt_bsik ASSIGNING FIELD-SYMBOL(<fs_head>).
*
*        FREE: lt_ftclear[],
*              lt_ftpost[].
*
*        CLEAR: lv_tot_cred,
*               lv_tot_deb,
*               lv_dmbtr,
*               lv_cmp_p,
*               lv_cmp_n,
*               lv_cmp_z,
*               lv_msgid,
*               lv_msgno,
*               lv_msgty,
*               lv_msgv1,
*               lv_msgv2,
*               lv_msgv3,
*               lv_msgv4,
*               lv_subrc.
*
*        READ TABLE lt_agroup TRANSPORTING NO FIELDS
*                                           WITH KEY stcd1_raiz = <fs_head>-stcd1_raiz
*                                                    bukrs      = <fs_head>-bukrs
*                                                    BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          LOOP AT lt_agroup ASSIGNING FIELD-SYMBOL(<fs_agroup>) FROM sy-tabix.
*            IF <fs_agroup>-stcd1_raiz NE <fs_head>-stcd1_raiz
*            OR <fs_agroup>-bukrs      NE <fs_head>-bukrs.
*              EXIT.
*            ENDIF.


      DATA(lt_bsik_aux) = lt_bsik[].

      SORT lt_bsik BY bukrs.
      SORT lt_bsik_aux BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_bsik_aux COMPARING bukrs.

      LOOP AT lt_bsik_aux ASSIGNING FIELD-SYMBOL(<fs_group>).

        READ TABLE lt_bsik TRANSPORTING NO FIELDS
                                         WITH KEY bukrs = <fs_group>-bukrs
                                                  BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_bsik ASSIGNING FIELD-SYMBOL(<fs_bsik>) FROM sy-tabix.
            IF <fs_bsik>-bukrs NE <fs_group>-bukrs.
              EXIT.
            ENDIF.

            IF <fs_bsik>-shkzg EQ gc_cred_h.
              lv_tot_cred = lv_tot_cred + <fs_bsik>-dmbtr. " H - Crédito - Negativo
            ELSE.
              lv_tot_deb = lv_tot_deb + <fs_bsik>-dmbtr. " S - Débito - Positivo
            ENDIF.

            lt_ftclear = VALUE #( BASE lt_ftclear ( agkoa  = gc_koart_k
                                                    agkon  = <fs_bsik>-lifnr
                                                    agbuk  = <fs_bsik>-bukrs
                                                    xnops  = COND #( WHEN <fs_bsik>-umskz IS INITIAL
                                                                       THEN abap_true
                                                                     ELSE abap_false )
                                                    agums  = <fs_bsik>-umskz
                                                    selfd  = gc_field_doc
                                                    selvon = |{ <fs_bsik>-belnr }{ <fs_bsik>-gjahr }{ <fs_bsik>-buzei }|
                                                    selbis = |{ <fs_bsik>-belnr }{ <fs_bsik>-gjahr }{ <fs_bsik>-buzei }| ) ).
          ENDLOOP.
        ENDIF.


        lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                              count = gc_item_1
                                              fnam  = gc_field_bldat
                                              fval  = |{ sy-datum+6(2) }{ gc_ponto }{ sy-datum+4(2) }{ gc_ponto }{ sy-datum(4) }| ) ).

        lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                              count = gc_item_1
                                              fnam  = gc_field_budat
                                              fval  = |{ sy-datum+6(2) }{ gc_ponto }{ sy-datum+4(2) }{ gc_ponto }{ sy-datum(4) }| ) ).

        lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                              count = gc_item_1
                                              fnam  = gc_field_blart
                                              fval  = gv_tpdoc ) ).

        lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                              count = gc_item_1
                                              fnam  = gc_field_bukrs
                                              fval  = <fs_group>-bukrs ) ).

        lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                              count = gc_item_1
                                              fnam  = gc_field_waers
                                              fval  = gc_brl ) ).

        lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                              count = gc_item_1
                                              fnam  = gc_field_bktxt
                                              fval  = TEXT-001 ) ).

        IF lv_tot_deb NE lv_tot_cred.

          IF lv_tot_deb GT lv_tot_cred. " Positiva - Débito
            lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                  count = gc_item_2
                                                  fnam  = gc_field_newbs
                                                  fval  = gc_newbs_d ) ).

            lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                  count = gc_item_2
                                                  fnam  = gc_field_bupla
                                                  fval  = <fs_group>-bukrs(2) && gc_bukrs_adcnl ) ).

            lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                  count = gc_item_2
                                                  fnam  = gc_field_newum
                                                  fval  = gc_newum_a ) ).
            lv_dmbtr = lv_tot_deb - lv_tot_cred.
            lv_cmp_p = abap_true.

          ELSE. " Negativo - Crédito
            lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                  count = gc_item_2
                                                  fnam  = gc_field_newbs
                                                  fval  = gc_newbs_c ) ).

            lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                  count = gc_item_2
                                                  fnam  = gc_field_bupla
                                                  fval  = <fs_group>-bukrs(2) && gc_bukrs_adcnl ) ).
            lv_dmbtr = lv_tot_cred - lv_tot_deb.
            lv_cmp_n = abap_true.
          ENDIF.

          lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                count = gc_item_2
                                                fnam  = gc_field_newko
                                                fval  = gs_raiz-lifnr ) ). " Fornecedor CPNJ Matriz
          TRANSLATE lv_dmbtr USING '.,'.
          lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                count = gc_item_2
                                                fnam  = gc_field_wrbtr
                                                fval  = lv_dmbtr ) ).

          lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                count = gc_item_2
                                                fnam  = gc_field_sgtxt
                                                fval  = TEXT-002 ) ).

          lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                count = gc_item_2
                                                fnam  = gc_field_zfbdt
                                                fval  = |{ sy-datum+6(2) }{ gc_ponto }{ sy-datum+4(2) }{ gc_ponto }{ sy-datum(4) }| ) ).

          READ TABLE gt_bco_empresa ASSIGNING FIELD-SYMBOL(<fs_bco_empresa>)
                                                  WITH KEY bukrs = <fs_group>-bukrs
                                                  BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                  count = gc_item_2
                                                  fnam  = gc_field_hbkid
                                                  fval  = <fs_bco_empresa>-hbkid ) ).
          ENDIF.

          lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                count = gc_item_2
                                                fnam  = gc_field_hktid
                                                fval  = gc_hktid ) ).

          lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                count = gc_item_2
                                                fnam  = gc_field_zlsch
                                                fval  = gv_fpag ) ).
        ELSE.
          lv_cmp_z = abap_true.
        ENDIF.

        " Chamada das BAPIs
        CALL FUNCTION 'POSTING_INTERFACE_START'
          EXPORTING
            i_function         = gc_function_c
            i_mode             = gc_mode_batch
            i_update           = gc_updt_s
            i_user             = sy-uname
          EXCEPTIONS
            client_incorrect   = 1
            function_invalid   = 2
            group_name_missing = 3
            mode_invalid       = 4
            update_invalid     = 5
            user_invalid       = 6
            OTHERS             = 7.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
            EXPORTING
              i_auglv                    = gc_post_id
              i_tcode                    = gc_tcode
              i_sgfunct                  = gc_sfunction_c
            IMPORTING
              e_msgid                    = lv_msgid
              e_msgno                    = lv_msgno
              e_msgty                    = lv_msgty
              e_msgv1                    = lv_msgv1
              e_msgv2                    = lv_msgv2
              e_msgv3                    = lv_msgv3
              e_msgv4                    = lv_msgv4
              e_subrc                    = lv_subrc
            TABLES
              t_blntab                   = lt_blntab
              t_ftclear                  = lt_ftclear
              t_ftpost                   = lt_ftpost
              t_fttax                    = lt_fttax
            EXCEPTIONS
              clearing_procedure_invalid = 1
              clearing_procedure_missing = 2
              table_t041a_empty          = 3
              transaction_code_invalid   = 4
              amount_format_error        = 5
              too_many_line_items        = 6
              company_code_invalid       = 7
              screen_not_found           = 8
              no_authorization           = 9
              OTHERS                     = 10.

          IF sy-subrc IS INITIAL.

            GET TIME.

            CALL FUNCTION 'POSTING_INTERFACE_END'
              EXPORTING
                i_bdcimmed              = abap_true
                i_bdcstrtdt             = sy-datum
                i_bdcstrttm             = sy-uzeit
              EXCEPTIONS
                session_not_processable = 1
                OTHERS                  = 2.

            IF sy-subrc IS NOT INITIAL.
              log_add_msg( iv_msg_id = sy-msgid
                           iv_msg_no = sy-msgno
                           iv_msg_ty = sy-msgty
                           iv_msg_v1 = sy-msgv1
                           iv_msg_v2 = sy-msgv2
                           iv_msg_v3 = sy-msgv3
                           iv_msg_v4 = sy-msgv4 ).
            ENDIF.

            IF lv_msgid EQ gc_id_post_s
           AND lv_msgno EQ gc_no_post_s
           AND lv_msgty EQ gc_sucess.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.

              log_add_msg( iv_msg_id = gc_msg_id
                           iv_msg_no = COND #( WHEN lv_cmp_z IS NOT INITIAL
                                                 THEN gc_msg_no36
                                               WHEN lv_cmp_p IS NOT INITIAL
                                                 THEN gc_msg_no37
                                               ELSE gc_msg_no38 )
                           iv_msg_ty = gc_sucess
                           iv_msg_v1 = lv_msgv2
                           iv_msg_v2 = lv_msgv1
                           iv_msg_v3 = sy-datum(4) ).

            ELSE.

              log_add_msg( iv_msg_id = gc_msg_id
                           iv_msg_no = COND #( WHEN lv_cmp_z IS NOT INITIAL
                                                 THEN gc_msg_no39
                                               WHEN lv_cmp_p IS NOT INITIAL
                                                 THEN gc_msg_no40
                                               ELSE gc_msg_no41 )
                           iv_msg_ty = gc_error
                           iv_msg_v1 = <fs_group>-bukrs ).

              log_add_msg( iv_msg_id = lv_msgid
                           iv_msg_no = lv_msgno
                           iv_msg_ty = lv_msgty
                           iv_msg_v1 = lv_msgv1
                           iv_msg_v2 = lv_msgv2
                           iv_msg_v3 = lv_msgv3
                           iv_msg_v4 = lv_msgv4 ).
            ENDIF.

          ELSE.

            log_add_msg( iv_msg_id = gc_msg_id
                         iv_msg_no = COND #( WHEN lv_cmp_z IS NOT INITIAL
                                               THEN gc_msg_no39
                                             WHEN lv_cmp_p IS NOT INITIAL
                                               THEN gc_msg_no40
                                             ELSE gc_msg_no41 )
                         iv_msg_ty = gc_error
                         iv_msg_v1 = <fs_group>-bukrs ).

            log_add_msg( iv_msg_id = sy-msgid
                         iv_msg_no = sy-msgno
                         iv_msg_ty = sy-msgty
                         iv_msg_v1 = sy-msgv1
                         iv_msg_v2 = sy-msgv2
                         iv_msg_v3 = sy-msgv3
                         iv_msg_v4 = sy-msgv4 ).
          ENDIF.
        ENDIF.
      ENDLOOP.

    ELSE.
      log_add_msg( iv_msg_id = gc_msg_id
                   iv_msg_no = gc_msg_no42
                   iv_msg_ty = gc_error ).
      RAISE fat_notf.
    ENDIF.

  ENDMETHOD.


  METHOD processa_todos.

    DATA: lt_ftclear TYPE STANDARD TABLE OF ftclear,
          lt_ftpost  TYPE STANDARD TABLE OF ftpost,
          lt_fttax   TYPE STANDARD TABLE OF fttax,
          lt_blntab  TYPE STANDARD TABLE OF blntab.

    DATA: lv_calc_belnr TYPE dmbtr,
          lv_calc_rebzg TYPE dmbtr,
          lv_dmbtr      TYPE string,
          lv_msgid      TYPE sy-msgid,
          lv_msgno      TYPE sy-msgno,
          lv_msgty      TYPE sy-msgty,
          lv_msgv1      TYPE sy-msgv1,
          lv_msgv2      TYPE sy-msgv2,
          lv_msgv3      TYPE sy-msgv3,
          lv_msgv4      TYPE sy-msgv4,
          lv_subrc      TYPE sy-subrc,
          lv_cmp_z      TYPE char1,
          lv_cmp_p      TYPE char1,
          lv_cmp_n      TYPE char1.

    CHECK gt_fornec[] IS NOT INITIAL.

    SELECT a~bukrs,
     a~gjahr,
     a~belnr,
     a~buzei,
     a~lifnr,
     a~dmbtr,
     a~umskz,
     a~shkzg,
     a~rebzg,
     a~rebzj,
     a~rebzz,
     b~bupla,
     b~lifnr AS lifnr_forn
    FROM bsik_view AS a
    INNER JOIN bseg AS b ON b~bukrs = a~bukrs
                   AND b~belnr = a~belnr
                   AND b~gjahr = a~gjahr
                   AND b~buzei = a~buzei
    FOR ALL ENTRIES IN @gt_fornec
    WHERE a~bukrs IN @gs_selopt-bukrs
    AND a~belnr IN @gs_selopt-belnr
    AND a~gjahr IN @gs_selopt-gjahr
    AND a~buzei IN @gs_selopt-buzei
    AND ( a~lifnr IN @gs_selopt-lifnr AND a~lifnr EQ @gt_fornec-lifnr ) " O fornecedor deve conter o CNPJ Raiz
    AND a~bstat IS INITIAL
    AND a~rebzg IS NOT INITIAL
    INTO TABLE @DATA(lt_bsik_ref).

    IF sy-subrc IS INITIAL.

      SORT lt_bsik_ref BY bukrs
                          belnr
                          gjahr
                          buzei.

      DATA(lt_bsik_fae) = lt_bsik_ref[].
      SORT lt_bsik_fae BY bukrs
                          rebzg
                          rebzj
                          rebzz.
      DELETE ADJACENT DUPLICATES FROM lt_bsik_fae COMPARING bukrs
                                                            rebzg
                                                            rebzj
                                                            rebzz.
      IF lt_bsik_fae[] IS NOT INITIAL.
        SELECT bukrs,
               gjahr,
               belnr,
               buzei,
               lifnr,
               dmbtr,
               umskz,
               shkzg,
               rebzg,
               rebzz
          FROM bsik_view
           FOR ALL ENTRIES IN @lt_bsik_ref
         WHERE bukrs EQ @lt_bsik_ref-bukrs
           AND belnr EQ @lt_bsik_ref-rebzg
           AND gjahr EQ @lt_bsik_ref-rebzj
           AND buzei EQ @lt_bsik_ref-rebzz
           AND lifnr EQ @lt_bsik_ref-lifnr
          INTO TABLE @DATA(lt_bsik_docs).

        IF sy-subrc IS INITIAL.
          SORT lt_bsik_docs BY bukrs
                               belnr
                               gjahr
                               buzei.

          DATA(lt_ref) = lt_bsik_ref[].
          SORT lt_ref BY bukrs
                         belnr
                         gjahr.
          DELETE ADJACENT DUPLICATES FROM lt_ref COMPARING bukrs
                                                           belnr
                                                           gjahr.

          LOOP AT lt_ref ASSIGNING FIELD-SYMBOL(<fs_ref>).

            FREE: lt_blntab[],
                  lt_ftclear[],
                  lt_ftpost[],
                  lt_fttax[].

            CLEAR: lv_calc_belnr,
                   lv_calc_rebzg,
                   lv_dmbtr,
                   lv_cmp_p,
                   lv_cmp_n,
                   lv_cmp_z,
                   lv_msgid,
                   lv_msgno,
                   lv_msgty,
                   lv_msgv1,
                   lv_msgv2,
                   lv_msgv3,
                   lv_msgv4,
                   lv_subrc.

            READ TABLE lt_bsik_ref TRANSPORTING NO FIELDS
                                                 WITH KEY bukrs = <fs_ref>-bukrs
                                                          belnr = <fs_ref>-belnr
                                                          gjahr = <fs_ref>-gjahr
                                                          BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              LOOP AT lt_bsik_ref ASSIGNING FIELD-SYMBOL(<fs_bsik_ref>) FROM sy-tabix.
                IF <fs_bsik_ref>-bukrs NE <fs_ref>-bukrs
                OR <fs_bsik_ref>-belnr NE <fs_ref>-belnr
                OR <fs_bsik_ref>-gjahr NE <fs_ref>-gjahr.
                  EXIT.
                ENDIF.

                " Calculo do BELNR - Fatura
                IF <fs_bsik_ref>-shkzg EQ gc_debt_s. " S - Débito
                  lv_calc_belnr = lv_calc_belnr - <fs_bsik_ref>-dmbtr.
                ELSE. " H - Crédito
                  lv_calc_belnr = lv_calc_belnr + <fs_bsik_ref>-dmbtr.
                ENDIF.

                lt_ftclear = VALUE #( BASE lt_ftclear ( agkoa  = gc_koart_k
                                                        agkon  = <fs_bsik_ref>-lifnr
                                                        agbuk  = <fs_bsik_ref>-bukrs
                                                        xnops  = COND #( WHEN <fs_bsik_ref>-umskz IS INITIAL
                                                                           THEN abap_true
                                                                         ELSE abap_false )
                                                        agums  = <fs_bsik_ref>-umskz
                                                        selfd  = gc_field_doc
                                                        selvon = |{ <fs_bsik_ref>-belnr }{ <fs_bsik_ref>-gjahr }{ <fs_bsik_ref>-buzei }|
                                                        selbis = |{ <fs_bsik_ref>-belnr }{ <fs_bsik_ref>-gjahr }{ <fs_bsik_ref>-buzei }| ) ).

                READ TABLE lt_bsik_docs ASSIGNING FIELD-SYMBOL(<fs_docs>)
                                                      WITH KEY bukrs = <fs_bsik_ref>-bukrs
                                                               belnr = <fs_bsik_ref>-rebzg
                                                               gjahr = <fs_bsik_ref>-rebzj
                                                               buzei = <fs_bsik_ref>-rebzz
                                                               BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  " Calculo do REBZG - Fatura Relacionada
                  IF <fs_bsik_ref>-shkzg EQ gc_debt_s. " S - Débito
                    lv_calc_rebzg = lv_calc_rebzg - <fs_bsik_ref>-dmbtr.
                  ELSE. " H - Crédito
                    lv_calc_rebzg = lv_calc_rebzg + <fs_bsik_ref>-dmbtr.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              IF lv_calc_rebzg LT lv_calc_belnr.

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                                      count = gc_item_1
                                                      fnam  = gc_field_bldat
                                                      fval  = |{ sy-datum+6(2) }{ gc_ponto }{ sy-datum+4(2) }{ gc_ponto }{ sy-datum(4) }| ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                                      count = gc_item_1
                                                      fnam  = gc_field_budat
                                                      fval  = |{ sy-datum+6(2) }{ gc_ponto }{ sy-datum+4(2) }{ gc_ponto }{ sy-datum(4) }| ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                                      count = gc_item_1
                                                      fnam  = gc_field_blart
                                                      fval  = gv_tpdoc ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                                      count = gc_item_1
                                                      fnam  = gc_field_bukrs
                                                      fval  = <fs_ref>-bukrs ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                                      count = gc_item_1
                                                      fnam  = gc_field_waers
                                                      fval  = gc_brl ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_k
                                                      count = gc_item_1
                                                      fnam  = gc_field_bktxt
                                                      fval  = TEXT-003 ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                      count = gc_item_2
                                                      fnam  = gc_field_newbs
                                                      fval  = gc_newbs_c ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                      count = gc_item_2
                                                      fnam  = gc_field_bupla
                                                      fval  = <fs_ref>-bupla ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                      count = gc_item_2
                                                      fnam  = gc_field_newko
                                                      fval  = <fs_ref>-lifnr ) ).

                lv_dmbtr = lv_calc_belnr - lv_calc_rebzg.
                TRANSLATE lv_dmbtr USING '.,'.
                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                      count = gc_item_2
                                                      fnam  = gc_field_wrbtr
                                                      fval  = lv_dmbtr ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                      count = gc_item_2
                                                      fnam  = gc_field_sgtxt
                                                      fval  = TEXT-004 ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                      count = gc_item_2
                                                      fnam  = gc_field_zfbdt
                                                      fval  = |{ sy-datum+6(2) }{ gc_ponto }{ sy-datum+4(2) }{ gc_ponto }{ sy-datum(4) }| ) ).

                READ TABLE gt_bco_empresa ASSIGNING FIELD-SYMBOL(<fs_bco_empresa>)
                                                        WITH KEY bukrs = <fs_ref>-bukrs
                                                        BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                        count = gc_item_2
                                                        fnam  = gc_field_hbkid
                                                        fval  = <fs_bco_empresa>-hbkid ) ).
                ENDIF.

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                      count = gc_item_2
                                                      fnam  = gc_field_hktid
                                                      fval  = gc_hktid ) ).

                lt_ftpost = VALUE #( BASE lt_ftpost ( stype = gc_koart_p
                                                      count = gc_item_2
                                                      fnam  = gc_field_zlsch
                                                      fval  = gv_fpag ) ).

                CALL FUNCTION 'POSTING_INTERFACE_START'
                  EXPORTING
                    i_function         = gc_function_c
                    i_mode             = gc_mode_batch
                    i_update           = gc_updt_s
                    i_user             = sy-uname
                  EXCEPTIONS
                    client_incorrect   = 1
                    function_invalid   = 2
                    group_name_missing = 3
                    mode_invalid       = 4
                    update_invalid     = 5
                    user_invalid       = 6
                    OTHERS             = 7.
                IF sy-subrc IS INITIAL.

                  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
                    EXPORTING
                      i_auglv                    = gc_post_id
                      i_tcode                    = gc_tcode
                      i_sgfunct                  = gc_sfunction_c
                    IMPORTING
                      e_msgid                    = lv_msgid
                      e_msgno                    = lv_msgno
                      e_msgty                    = lv_msgty
                      e_msgv1                    = lv_msgv1
                      e_msgv2                    = lv_msgv2
                      e_msgv3                    = lv_msgv3
                      e_msgv4                    = lv_msgv4
                      e_subrc                    = lv_subrc
                    TABLES
                      t_blntab                   = lt_blntab
                      t_ftclear                  = lt_ftclear
                      t_ftpost                   = lt_ftpost
                      t_fttax                    = lt_fttax
                    EXCEPTIONS
                      clearing_procedure_invalid = 1
                      clearing_procedure_missing = 2
                      table_t041a_empty          = 3
                      transaction_code_invalid   = 4
                      amount_format_error        = 5
                      too_many_line_items        = 6
                      company_code_invalid       = 7
                      screen_not_found           = 8
                      no_authorization           = 9
                      OTHERS                     = 10.

                  IF sy-subrc IS INITIAL.

                    GET TIME.

                    CALL FUNCTION 'POSTING_INTERFACE_END'
                      EXPORTING
                        i_bdcimmed              = abap_true
                        i_bdcstrtdt             = sy-datum
                        i_bdcstrttm             = sy-uzeit
                      EXCEPTIONS
                        session_not_processable = 1
                        OTHERS                  = 2.

                    IF sy-subrc IS NOT INITIAL.
                      log_add_msg( iv_msg_id = sy-msgid
                                   iv_msg_no = sy-msgno
                                   iv_msg_ty = sy-msgty
                                   iv_msg_v1 = sy-msgv1
                                   iv_msg_v2 = sy-msgv2
                                   iv_msg_v3 = sy-msgv3
                                   iv_msg_v4 = sy-msgv4 ).
                    ENDIF.

                    IF lv_msgid EQ gc_id_post_s
                   AND lv_msgno EQ gc_no_post_s
                   AND lv_msgty EQ gc_sucess.

                      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                        EXPORTING
                          wait = abap_true.

                      log_add_msg( iv_msg_id = gc_msg_id
                                   iv_msg_no = gc_msg_no0
                                   iv_msg_ty = gc_sucess
                                   iv_msg_v1 = CONV syst_msgv( |{ TEXT-m01 } { lv_msgv2 } { TEXT-m02 } { <fs_ref>-belnr }| )
                                   iv_msg_v2 = CONV syst_msgv( |{ TEXT-m03 } { <fs_ref>-rebzj }| )
                                   iv_msg_v3 = CONV syst_msgv( |{ TEXT-m04 } { lv_msgv1 }| )
                                   iv_msg_v4 = CONV syst_msgv( |{ TEXT-m05 } { sy-datum(4) }| ) ).

                    ELSE.

                      log_add_msg( iv_msg_id = gc_msg_id
                                   iv_msg_no = gc_msg_no40
                                   iv_msg_ty = gc_error
                                   iv_msg_v1 = <fs_ref>-belnr
                                   iv_msg_v2 = <fs_ref>-rebzj
                                   iv_msg_v3 = <fs_ref>-bukrs ).

                      log_add_msg( iv_msg_id = lv_msgid
                                   iv_msg_no = lv_msgno
                                   iv_msg_ty = lv_msgty
                                   iv_msg_v1 = lv_msgv1
                                   iv_msg_v2 = lv_msgv2
                                   iv_msg_v3 = lv_msgv3
                                   iv_msg_v4 = lv_msgv4 ).
                    ENDIF.

                  ENDIF.
                ENDIF.

              ELSE.

                log_add_msg( iv_msg_id = gc_msg_id
                             iv_msg_no = gc_msg_no43
                             iv_msg_ty = gc_sucess
                             iv_msg_v1 = <fs_ref>-bukrs
                             iv_msg_v2 = <fs_ref>-gjahr
                             iv_msg_v3 = <fs_ref>-belnr
                             iv_msg_v4 = <fs_ref>-rebzj ).
              ENDIF.

            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ELSE.
      log_add_msg( iv_msg_id = gc_msg_id
                   iv_msg_no = gc_msg_no42
                   iv_msg_ty = gc_error ).
      RAISE fat_notf.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
