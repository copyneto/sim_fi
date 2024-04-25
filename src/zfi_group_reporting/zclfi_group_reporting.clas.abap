CLASS zclfi_group_reporting DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_group TYPE STANDARD TABLE OF zi_fi_group_reporting.

    CLASS-DATA:
      go_instance    TYPE REF TO zclfi_group_reporting,
      gv_rldnr       TYPE acdocu-rldnr,
      gv_rdimen      TYPE acdocu-rdimen,
      gv_gjahr       TYPE acdocu-ryear,
      gv_rvers       TYPE acdocu-rvers,
      gv_poper       TYPE acdocu-poper,
      gv_ritclg      TYPE acdocu-ritclg,
      gv_rbunit      TYPE acdocu-rbunit,
      gv_destination TYPE char25.

    DATA:
        gt_acdocu TYPE zctfi_acdocu.

    CLASS-METHODS get_instance
      IMPORTING
        VALUE(iv_rldnr)    TYPE acdocu-rldnr OPTIONAL
        VALUE(iv_rdimen)   TYPE  acdocu-rdimen OPTIONAL
        VALUE(iv_gjahr)    TYPE acdocu-ryear OPTIONAL
        VALUE(iv_rvers)    TYPE acdocu-rvers OPTIONAL
        VALUE(iv_poper)    TYPE acdocu-poper OPTIONAL
        VALUE(iv_ritclg)   TYPE acdocu-ritclg OPTIONAL
        VALUE(iv_rbunit)   TYPE acdocu-rbunit OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zclfi_group_reporting.

    METHODS
      constructor
        IMPORTING
          VALUE(iv_rldnr)  TYPE acdocu-rldnr OPTIONAL
          VALUE(iv_rdimen) TYPE  acdocu-rdimen OPTIONAL
          VALUE(iv_gjahr)  TYPE acdocu-ryear OPTIONAL
          VALUE(iv_rvers)  TYPE acdocu-rvers OPTIONAL
          VALUE(iv_poper)  TYPE acdocu-poper OPTIONAL
          VALUE(iv_ritclg) TYPE acdocu-ritclg OPTIONAL
          VALUE(iv_rbunit) TYPE acdocu-rbunit OPTIONAL.

    METHODS
      execute
        EXPORTING
                  et_acdocu        TYPE zctfi_acdocu
        RETURNING VALUE(rt_return) TYPE fc05_t_message .

    METHODS
      execute_fiori
        IMPORTING
          it_acdocu TYPE ty_group
        EXPORTING
          et_return TYPE bapiret2_tab .

    METHODS
      task_finish
        IMPORTING
          p_task TYPE clike.

    METHODS call_fm_analyze
      IMPORTING
                it_acdocu         TYPE zctfi_acdocu
      RETURNING VALUE(rt_message) TYPE fc05_t_message.
    METHODS call_fm_post
      IMPORTING
                iv_estorno        TYPE char1 OPTIONAL
                it_message        TYPE fc05_t_message OPTIONAL
                it_acdocu         TYPE zctfi_acdocu OPTIONAL
      RETURNING VALUE(rt_message) TYPE fc05_t_message.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      gv_result TYPE char1,
      gt_return TYPE bapiret2_tab.

    CONSTANTS: BEGIN OF gc_values,
                 sim             TYPE char3 VALUE 'SIM',
                 nao             TYPE char3 VALUE 'NÃO',
                 n               TYPE char1 VALUE 'N',
                 w               TYPE char1 VALUE 'W',
                 u               TYPE char1 VALUE 'U',
                 e               TYPE char1 VALUE 'E',
                 v01             TYPE fc_cactt VALUE '01',
                 group_reporting TYPE ze_param_chave1 VALUE 'GROUP_REPORTING',
                 destination     TYPE ze_param_chave2 VALUE 'DESTINATION',
                 fi              TYPE          ze_param_modulo VALUE 'FI',
                 v00             TYPE fc_docty VALUE  '00',
                 zfi_group_repot TYPE syst_msgid VALUE 'ZFI_GROUP_REPOT',
               END OF gc_values.

    DATA:
      gt_message                  TYPE fc05_t_message.

    METHODS get_data_oil.
    METHODS validate_data
      RETURNING VALUE(rt_return) TYPE fc05_t_message .
    METHODS get_destiation
      RETURNING
        VALUE(rv_destination) TYPE char25.
    METHODS fill_posting
      RETURNING
        VALUE(rt_result) TYPE fc05_t_message.

ENDCLASS.



CLASS zclfi_group_reporting IMPLEMENTATION.


  METHOD constructor.

    gv_rldnr  = iv_rldnr .
    gv_rdimen = iv_rdimen.
    gv_gjahr = iv_gjahr.
    gv_rvers  = iv_rvers .
    gv_poper = iv_poper.
    gv_ritclg   = iv_ritclg .
    gv_rbunit  = |{ iv_rbunit ALPHA = IN }| .
    gv_destination = get_destiation(  ).

  ENDMETHOD.


  METHOD execute.


    get_data_oil(  ).
    rt_return = validate_data( ).

    et_acdocu = gt_acdocu.

  ENDMETHOD.


  METHOD get_data_oil.

    CALL FUNCTION 'ZFMFI_GROUP_REPORTING'
      DESTINATION gv_destination
      EXPORTING
        iv_rldnr  = gv_rldnr
        iv_rdimen = gv_rdimen
        iv_gjahr  = gv_gjahr
        iv_rvers  = gv_rvers
        iv_poper  = gv_poper
        iv_ritclg = gv_ritclg
        iv_rbunit = gv_rbunit
      IMPORTING
        et_acdocu = gt_acdocu.

  ENDMETHOD.


  METHOD validate_data.

    DATA:
      lv_answer  TYPE string,
      lv_estorno TYPE char1.

    CHECK gt_acdocu IS NOT INITIAL.

    SELECT * FROM acdocu
    FOR ALL ENTRIES IN @gt_acdocu
    WHERE rldnr = @gv_rldnr
     AND       rdimen = @gv_rdimen
     AND      ryear = @gv_gjahr
      AND     rvers = @gv_rvers
      AND     poper = @gv_poper
      AND      ritclg  = @gv_ritclg
      AND     rbunit = @gt_acdocu-rbunit
       AND rvsdocnr = ''
       AND orndocnr = ''
      INTO  TABLE @DATA(lt_acdocu).

    IF lt_acdocu IS NOT INITIAL.

      IF sy-batch IS INITIAL.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = TEXT-001
            text_button_1  = gc_values-sim
            text_button_2  = gc_values-nao
            titlebar       = TEXT-004
          IMPORTING
            answer         = lv_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

      ENDIF.

      IF sy-subrc EQ 0 AND lv_answer EQ 1.

        rt_return = call_fm_post( EXPORTING it_message = call_fm_analyze( CORRESPONDING #( lt_acdocu )  ) iv_estorno =  abap_true  it_acdocu = CORRESPONDING #( lt_acdocu )  ).
        CHECK rt_return IS INITIAL.
        rt_return =  call_fm_post( EXPORTING it_message = call_fm_analyze( CORRESPONDING #( gt_acdocu ) )  it_acdocu = CORRESPONDING #( gt_acdocu )  ).

      ELSE.
        MESSAGE w001(zfi_group_repot) INTO DATA(lv_message).
        APPEND VALUE #( msgty = gc_values-w msgno = 001 msgid = gc_values-zfi_group_repot msgv1 = lv_message ) TO rt_return.
      ENDIF.

    ELSE.

      rt_return  = call_fm_post( EXPORTING it_message = call_fm_analyze( CORRESPONDING #( gt_acdocu ) )  it_acdocu = CORRESPONDING #( gt_acdocu )  ).

    ENDIF.

  ENDMETHOD.


  METHOD call_fm_analyze.

    DATA:
      ls_container                TYPE fc05_s_pdoc_container,
      lt_document_lines_4_posting TYPE fc05_t_pdoc_s4h_qfle,
      ls_control                  TYPE fc05_s_da_control.

    ls_control-mnpos = abap_true.
    ls_control-cactt =  gc_values-v01.
    ls_control-afle = abap_true.
    ls_control-rvs_flag = abap_true.


    LOOP AT it_acdocu ASSIGNING FIELD-SYMBOL(<fs_acdocu>)
        GROUP BY <fs_acdocu>-docnr
        REFERENCE INTO DATA(lt_group_ref) .

      CLEAR: lt_document_lines_4_posting[].

      LOOP AT GROUP lt_group_ref INTO DATA(ls_acdocu).
        APPEND CORRESPONDING #( ls_acdocu ) TO lt_document_lines_4_posting.
      ENDLOOP.

      CALL FUNCTION 'FC_DOCUMENT_ANALYZE3'
        EXPORTING
          e_rldnr      = gv_rldnr
          e_itclg      = gv_ritclg
          e_dimen      = gv_rdimen
          e_docty      = gc_values-v00
          e_rvers      = gv_rvers
          e_ryear      = gv_gjahr
          e_perid      = gv_poper
          es_control   = ls_control
        IMPORTING
          it_message   = gt_message
          is_container = ls_container
        CHANGING
          ct_doc       = lt_document_lines_4_posting.

      IF line_exists( gt_message[ msgty = gc_values-e ] ).
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD call_fm_post.

    DATA:
      ls_control                  TYPE fc05_s_da_control,
      lt_document_lines_4_posting TYPE fc05_t_pdoc_s4h_qfle,
      lt_journalentry_header      TYPE fc05_t_pdoc_head.

    IF it_message IS NOT  INITIAL.
      rt_message = it_message.
      RETURN.
    ENDIF.

    LOOP AT it_acdocu ASSIGNING FIELD-SYMBOL(<fs_acdocu>)
        GROUP BY <fs_acdocu>-docnr
        REFERENCE INTO DATA(lt_group_ref) .

      CLEAR: lt_document_lines_4_posting[], lt_journalentry_header[].

      LOOP AT GROUP lt_group_ref INTO DATA(ls_acdocu).

        IF iv_estorno EQ abap_true.
          ls_acdocu-tsl  =  ls_acdocu-tsl  * -1.
          ls_acdocu-hsl =  ls_acdocu-hsl  * -1.
          ls_acdocu-ksl =  ls_acdocu-ksl  * -1.
        ENDIF.

        ls_acdocu-orig_ref = ls_acdocu-ryear && ls_acdocu-docnr && ls_acdocu-docln.

        APPEND CORRESPONDING #( ls_acdocu ) TO lt_document_lines_4_posting.

      ENDLOOP.

      DESCRIBE TABLE lt_document_lines_4_posting LINES DATA(lv_lines).

      APPEND VALUE fc05_s_pdoc_head(
                docty = COND #( WHEN iv_estorno EQ abap_true THEN gc_values-v01 ELSE gc_values-v00  )
                sgtxt = CONV #( TEXT-003 )
                from = 1
                to = lv_lines
                orndocnr = COND #( WHEN iv_estorno EQ abap_true THEN ls_acdocu-docnr )
                revyear  = COND #( WHEN iv_estorno EQ abap_true THEN ls_acdocu-ryear ) ) TO  lt_journalentry_header.

*      ls_control-mnpos = abap_true.
      ls_control-cactt =  gc_values-v01.
      ls_control-afle = abap_true.

      IF iv_estorno EQ abap_true.
        ls_control-rvs_flag = abap_true.
        ls_control-rvs_mult = abap_true.
*        ls_control-rvs_migr = abap_true.
      ENDIF.

      CALL FUNCTION 'FC_DOCUMENT_POST3'
        EXPORTING
          e_rldnr    = gv_rldnr
          e_itclg    = gv_ritclg
          e_dimen    = gv_rdimen
          e_rvers    = gv_rvers
          e_ryear    = gv_gjahr
          e_perid    = gv_poper
          es_control = ls_control
          e_rrcty    = gc_values-u
          et_doc     = lt_document_lines_4_posting
        IMPORTING
          it_message = rt_message
        CHANGING
          ct_head    = lt_journalentry_header.

      IF NOT line_exists( rt_message[ msgty = gc_values-e ] ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

      ELSE.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_destiation.

    TRY.

        NEW zclca_tabela_parametros(  )->m_get_single(
            EXPORTING
                iv_modulo = gc_values-fi
                iv_chave1 = gc_values-group_reporting
                iv_chave2 =  gc_values-destination
                IMPORTING
                ev_param = rv_destination ).

      CATCH  zcxca_tabela_parametros .
    ENDTRY.


  ENDMETHOD.


  METHOD fill_posting.

    LOOP AT gt_acdocu ASSIGNING FIELD-SYMBOL(<fs_acdocu>).
      APPEND CORRESPONDING #( <fs_acdocu> ) TO rt_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD execute_fiori.

    DATA: lt_group TYPE STANDARD TABLE OF acdocu.

    LOOP AT it_acdocu ASSIGNING FIELD-SYMBOL(<fs_acdocu>).
      APPEND CORRESPONDING #( <fs_acdocu> ) TO lt_group.
    ENDLOOP.

    CALL FUNCTION 'ZFMFI_GROUP_REPORTING_FIORI'
      STARTING NEW TASK 'ZGROUP'
      CALLING task_finish ON END OF TASK
      TABLES
        it_group = lt_group[] ##ENH_OK.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_result IS NOT INITIAL.

    et_return  = gt_return.

  ENDMETHOD.


  METHOD task_finish .

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_GROUP_REPORTING_FIORI'"'ZGROUP'
      IMPORTING
        et_return =  gt_return.

    gv_result = abap_true .

  ENDMETHOD.


  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance TYPE zclfi_group_reporting.
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.
ENDCLASS.
