FUNCTION zfmfi_exec_retorno_extrato.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_FILE) TYPE  FEBAUSZF
*"     VALUE(IV_VARIANT) TYPE  VARIANT
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  DATA: ls_mstr_print_parms LIKE pri_params,
        lv_valid            TYPE c,
        lv_jobname          TYPE btcjob VALUE 'VAN_BANCARIA',
        lv_number           TYPE btcjobcnt,
        lt_log_job          TYPE TABLE OF tbtcjoblog0.

  WAIT UP TO 1 SECONDS.

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      copies                 = '1'
      expiration             = '1'
      new_list_id            = 'X'
      no_dialog              = 'X'
      user                   = sy-uname
    IMPORTING
      out_parameters         = ls_mstr_print_parms
      valid                  = lv_valid
    EXCEPTIONS
      archive_info_not_found = 1
      invalid_print_params   = 2
      invalid_archive_params = 3
      OTHERS                 = 4.
  IF sy-subrc NE 0.
    APPEND VALUE #( type       = 'E'
                    id         = 'ZFI_VAN_BANCARIA'
                    number     = '019'
                    message_v1 = sy-uname ) TO et_return.
    EXIT.
  ENDIF.

  IF ls_mstr_print_parms-pdest = space.
    ls_mstr_print_parms-pdest = 'LOCL'.
  ENDIF.

*  ls_mstr_print_parms-linsz = 132.
*  ls_mstr_print_parms-paart = 'X_65_132'.
*  ls_mstr_print_parms-paart = 'X_65_255'.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_jobname
    IMPORTING
      jobcount         = lv_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc IS NOT INITIAL.
    APPEND VALUE #( type       = 'E'
                    id         = 'ZFI_VAN_BANCARIA'
                    number     = '020'
                    message_v1 = sy-uname ) TO et_return.
    EXIT.
  ENDIF.

  SUBMIT rfebka00
    WITH auszfile = iv_file USING SELECTION-SET iv_variant
    TO SAP-SPOOL
    SPOOL PARAMETERS ls_mstr_print_parms
    WITHOUT SPOOL DYNPRO
    VIA JOB lv_jobname NUMBER lv_number
    AND RETURN.
  IF sy-subrc IS NOT INITIAL.
    DATA(ls_msg) = cl_abap_submit_handling=>get_error_message( ).
    APPEND VALUE #( type       = ls_msg-msgty
                    id         = ls_msg-msgid
                    number     = ls_msg-msgno
                    message_v1 = ls_msg-msgv1
                    message_v2 = ls_msg-msgv2
                    message_v3 = ls_msg-msgv3
                    message_v4 = ls_msg-msgv4 ) TO et_return.
    EXIT.
  ENDIF.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = lv_number
      jobname              = lv_jobname
      strtimmed            = abap_true
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      OTHERS               = 8.
  IF sy-subrc NE 0.
    APPEND VALUE #( type       = ls_msg-msgty
                    id         = ls_msg-msgid
                    number     = ls_msg-msgno
                    message_v1 = ls_msg-msgv1
                    message_v2 = ls_msg-msgv2
                    message_v3 = ls_msg-msgv3
                    message_v4 = ls_msg-msgv4 ) TO et_return.
  ENDIF.

  DATA lv_class TYPE seoclsname VALUE 'ZCLFI_VAN_BANCARIA'.
  DATA lv_joblog TYPE rstsoname.

  DO 60 TIMES.
    CALL METHOD (lv_class)=>get_joblog
      EXPORTING
        iv_jobname  = lv_jobname
        iv_jobcount = lv_number
      RECEIVING
        rv_joblog   = lv_joblog.

    IF lv_joblog IS NOT INITIAL.
      EXIT.
    ENDIF.
    WAIT UP TO 1 SECONDS.
  ENDDO.

  IF lv_joblog IS INITIAL.
    APPEND VALUE #( type       = 'E'
                    id         = 'ZFI_VAN_BANCARIA'
                    number     = '021' ) TO et_return.
    EXIT.
  ENDIF.

  DATA(lv_tab_job) = CONV tabname16( |TBTCJOBLOG{ lv_joblog+2(1) }| ).

  SELECT linenumber,
         enterdate,
         entertime,
         msgid,
         msgno,
         msgtype,
         msgv1,
         msgv2,
         msgv3,
         msgv4
    FROM (lv_tab_job) INTO CORRESPONDING FIELDS OF TABLE @lt_log_job
    WHERE jobname    = @lv_jobname
      AND jobcount   = @lv_number
      ORDER BY linenumber ASCENDING.
  IF sy-subrc NE 0.
    APPEND VALUE #( type       = 'E'
                    id         = 'ZFI_VAN_BANCARIA'
                    number     = '021' ) TO et_return.
    EXIT.
  ENDIF.

  LOOP AT lt_log_job ASSIGNING FIELD-SYMBOL(<fs_log_job>).
    CHECK <fs_log_job>-msgid   NE '00'.
*       OR <fs_log_job>-msgtype EQ 'E'
*       OR <fs_log_job>-msgtype EQ 'A'.
*
*    CHECK <fs_log_job>-msgid   NE '00' OR <fs_log_job>-msgno NE '564'.

    APPEND VALUE #( type       = <fs_log_job>-msgtype
                    id         = <fs_log_job>-msgid
                    number     = <fs_log_job>-msgno
                    message_v1 = <fs_log_job>-msgv1
                    message_v2 = <fs_log_job>-msgv2
                    message_v3 = <fs_log_job>-msgv3
                    message_v4 = <fs_log_job>-msgv4 ) TO et_return.
  ENDLOOP.

ENDFUNCTION.
