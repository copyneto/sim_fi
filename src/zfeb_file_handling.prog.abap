*&---------------------------------------------------------------------*
*& Report  FEB_FILE_HANDLING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT   zfeb_file_handling.

INCLUDE zschedman_events.
*INCLUDE schedman_events.
INCLUDE zrkasmawf.
*INCLUDE rkasmawf.

TABLES: feb_imp_source,
        sscrfields.

DATA: o_sel_opt         TYPE feby_selopt,
      lv_execpri        TYPE rfpdo1-febeinles,
      ls_printparam     TYPE febs_printparam,
      l_schedman_err(1) TYPE c VALUE space.

SELECTION-SCREEN  BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS t_sel_op FOR feb_imp_source-path_source OBLIGATORY. "n2313727
SELECTION-SCREEN  END OF BLOCK 1.


SELECTION-SCREEN  BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_koausz LIKE rfpdo1-febpausz,   " Kontoauszug drucken
              p_bupro  LIKE rfpdo2-febbupro,
              p_statik LIKE rfpdo2-febstat,
              pa_lsepa LIKE febpdo-lsepa.
SELECTION-SCREEN  END OF BLOCK 2.


*SELECTION-SCREEN  BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-099.
*  PARAMETERS: p_error  LIKE febpdo-lsepa.
*SELECTION-SCREEN  END OF BLOCK 3.



AT SELECTION-SCREEN ON BLOCK 2.

*---- Program started with EXEC+PRINT online
  IF sy-batch NE 'X'.
    IF p_bupro = 'X' OR p_statik = 'X'.
      IF sscrfields-ucomm = 'PRIN'.
        lv_execpri = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  ls_printparam-koausz = p_koausz.
  ls_printparam-bupro = p_bupro.
  ls_printparam-statik = p_statik.
  ls_printparam-lsepa = pa_lsepa.

START-OF-SELECTION.

  o_sel_opt = t_sel_op[].

* registration for shedule manager
  PERFORM schedman_start_stop USING 'START'.

  CALL METHOD zcl_feb_file_handling=>main
    EXPORTING
      iv_execpri            = lv_execpri
      is_printparam         = ls_printparam
      it_sel_opt_input_path = o_sel_opt
    "  p_error               = p_error
    CHANGING
      c_schedman_err        = l_schedman_err.

* notice of departure from schedule manager
  PERFORM schedman_start_stop USING 'STOP'.



*&---------------------------------------------------------------------*
*&      Form  schedman_start_stop
*&---------------------------------------------------------------------*
*       Integration Schedule Manager
*----------------------------------------------------------------------*
FORM schedman_start_stop  USING    p_command.

* local statics
  STATICS: ls_key_static TYPE schedman_key.
*local data declaration
*  DATA: gs_key      LIKE schedman_key.
*  DATA: gt_spono    LIKE schedman_spool.

*  DATA: ld_worklist_flag(1).
  DATA: ls_detail   LIKE schedman_detail_user.
  DATA: lt_selkrit  LIKE schedman_selkrit OCCURS 0 WITH HEADER LINE.
  DATA: lt_param    LIKE schedman_selkrit OCCURS 0 WITH HEADER LINE.
  DATA: ls_witem    LIKE scma_witem.
  DATA: ls_event    LIKE scma_event.
  DATA: ls_ext      LIKE schedman_ext.
  DATA: ls_message LIKE schedman_message,
        ld_objects LIKE smmain-nr_of_objects,
        ld_aplstat LIKE smmain-aplstat.

  DATA: l_status   TYPE tbtcjob-status.

  DATA: jobname  LIKE tbtco-jobname,
        jobcount LIKE tbtco-jobcount.
  DATA: ls_sel_opt TYPE febs_selopt.


  IF p_command = 'START'.
* muss in scmatasks
    ls_detail-repid       = sy-repid.
    ls_detail-variante    = sy-slset.      "<<die variante
    ls_detail-application = 'FI-BL'.
    ls_detail-testflag    = ''.

    CLEAR lt_selkrit.
    lt_selkrit-structure = 'FEB_IMP_SOURCE'.
    lt_selkrit-field = 'PATH_SOURCE'.
    LOOP AT o_sel_opt INTO ls_sel_opt.
      MOVE-CORRESPONDING ls_sel_opt TO lt_selkrit.
      lt_selkrit-optio = ls_sel_opt-option.
      APPEND lt_selkrit.
    ENDLOOP.

    ls_witem-wf_witem = wf_witem.
    ls_witem-wf_wlist = wf_wlist.
    CALL FUNCTION 'KPEP_MONI_INIT_RECORD'
      EXPORTING
        ls_detail  = ls_detail
*       ls_witem   = ls_witem
      IMPORTING
        ls_key     = ls_key_static
      TABLES
        lt_selkrit = lt_selkrit
        lt_param   = lt_param.

  ELSEIF p_command = 'STOP'.

    ld_aplstat  = '0'.
    ls_event-wf_witem = wf_witem.
    ls_event-wf_okey  = wf_okey.
    IF l_schedman_err = 'X'.
      ls_event-wf_event = cs_wf_events-error.
    ELSE.
      ls_event-wf_event = cs_wf_events-finished.
    ENDIF.
    CALL FUNCTION 'KPEP_MONI_CLOSE_RECORD'
      EXPORTING
        ls_key        = ls_key_static
        ls_scma_event = ls_event
      CHANGING
        ld_aplstat    = ld_aplstat
      EXCEPTIONS
*       NO_ID_GIVEN   = 1
        OTHERS        = 0.

  ENDIF.

  COMMIT WORK.           " <<<<<<<<<<  C O M M I T  W O R K  >>>>>>>

ENDFORM.                    "schedman_start_stop
