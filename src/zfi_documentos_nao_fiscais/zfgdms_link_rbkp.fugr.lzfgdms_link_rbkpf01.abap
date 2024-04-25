*----------------------------------------------------------------------*
***INCLUDE LZFGDMS_LINK_RBKPF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  DATA: lo_rbkp TYPE REF TO if_ex_document_obj.

  DATA: lt_drad_work TYPE dms_tbl_drad_badi_work,
        lt_drad      TYPE TABLE OF drad.

  DATA: ls_rbkp_dms  LIKE LINE OF gt_rbkp_dms,
        ls_drad_work TYPE dms_drad_badi_work.

  DATA: lv_activity TYPE i,
        lv_act,
        lv_times    TYPE i,
        lv_screen   TYPE i VALUE 40.

***get instance
  CALL METHOD cl_exithandler=>get_instance_for_subscreens
    CHANGING
      instance = lo_rbkp.

  CALL METHOD lo_rbkp->get_data
    EXPORTING
      flt_val         = gv_val
    IMPORTING
      table_drad_work = lt_drad_work
      table_drad_db   = lt_drad
      activity        = gv_activity_1500.

***transport dynpr data
  REFRESH gt_rbkp_dms.
  LOOP AT lt_drad_work ASSIGNING FIELD-SYMBOL(<fs_drad_work>).
****move control structure
    MOVE-CORRESPONDING <fs_drad_work> TO ls_rbkp_dms.
****move lo_rbkp info
    ls_rbkp_dms-belnr = ls_rbkp_dms-objky(10).
    ls_rbkp_dms-gjahr = ls_rbkp_dms-objky+10(4).

    APPEND ls_rbkp_dms TO gt_rbkp_dms.
  ENDLOOP.

***fill dynpro
  lv_times = lv_screen - lines( gt_rbkp_dms ).
  DO lv_times TIMES.
    CLEAR ls_rbkp_dms.
    ls_rbkp_dms-dokob = gv_val.
    APPEND ls_rbkp_dms TO gt_rbkp_dms.
  ENDDO.

ENDFORM.              " get_data
*&---------------------------------------------------------------------*
*& Form put_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM put_data .
  DATA: lo_rbkp TYPE REF TO if_ex_document_obj.

  DATA: lt_drad_work TYPE dms_tbl_drad_badi_work,
        lt_drad      TYPE TABLE OF drad.

  DATA: ls_drad_work TYPE dms_drad_badi_work,
        ls_rbkp_dms  LIKE LINE OF gt_rbkp_dms.

***get instance
  CALL METHOD cl_exithandler=>get_instance_for_subscreens
    CHANGING
      instance = lo_rbkp.

  LOOP AT gt_rbkp_dms ASSIGNING FIELD-SYMBOL(<fs_rbkp_dms>) WHERE belnr IS NOT INITIAL.
    DATA(lv_tabix) = sy-tabix.
    IF ( gv_ucomm EQ 'CV130_DELZ' OR
         gv_ucomm EQ 'DELZ' ) AND
       <fs_rbkp_dms>-tab_mark IS NOT INITIAL.
      DELETE gt_rbkp_dms INDEX lv_tabix.
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING <fs_rbkp_dms> TO ls_drad_work.
    ls_drad_work-objky = |{ <fs_rbkp_dms>-belnr }{ <fs_rbkp_dms>-gjahr }|.
    APPEND ls_drad_work TO lt_drad_work.
  ENDLOOP.

  CALL METHOD lo_rbkp->put_data
    EXPORTING
      flt_val         = gv_val
      table_drad_work = lt_drad_work
      table_drad_db   = lt_drad
      activity        = gv_activity_1500.

  CALL METHOD lo_rbkp->put_function_code
    EXPORTING
      function_code = sy-ucomm
      flt_val       = gv_val.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_col
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_col .
  IF sy-tcode = 'CV03N'.
    LOOP AT SCREEN.
      IF screen-name = 'GS_LINK_RBKP-BELNR' OR
         screen-name = 'GS_LINK_RBKP-GJAHR'.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_trata_ucomm_delz
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_trata_ucomm_delz .
  FIELD-SYMBOLS: <fs_field> TYPE any.

  ASSIGN ('(SAPLCV130)OK_CODE') TO <fs_field>.
  IF <fs_field> IS ASSIGNED.
    <fs_field> = sy-ucomm.
  ENDIF.
  UNASSIGN <fs_field>.

  ASSIGN ('(SAPLCV130)SY-UCOMM') TO <fs_field>.
  IF <fs_field> IS ASSIGNED.
    <fs_field> = sy-ucomm.
  ENDIF.
  UNASSIGN <fs_field>.

  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = sy-ucomm.

ENDFORM.
