class ZCL_FEB_FILE_HANDLING definition
  public
  final
  create public .

public section.

  class-methods IMPORT_FROM_MEMORY
    importing
      !IV_KUKEY type KUKEY_EB
    exceptions
      NO_KUKEY_FOUND .
  class-methods MAIN
    importing
      !IT_SEL_OPT_INPUT_PATH type FEBY_SELOPT optional
      !IV_BANK_STATEMENT type STRING optional
      !IS_PRINTPARAM type FEBS_PRINTPARAM optional
      !IV_EXECPRI type RFPDO1-FEBEINLES optional
      !IV_FORMAT_PARAM type CHAR255 optional
      !IX_SIMULATION type CHAR1 optional
      !IV_FORMAT type FEB_FORMAT optional
      !IV_FORMAT_LONG type FEBFORMAT_LONG optional
    exporting
      !ET_LOG_HANDLE type BAL_T_LOGH
      !X_STATEMENT_ERROR type CHAR1
      !ET_SAVE_BNKSTMT_RESULT type FEBY_SAVE_BNKSTMT_RESULT
    changing
      !C_SCHEDMAN_ERR type CHAR1 optional
    exceptions
      STATEMENT_MESSAGE_EMPTY .
protected section.
*"* protected components of class ZCL_FEB_FILE_HANDLING
*"* do not include other source files here!!!
private section.

  class-data S_POSTING_PARAMETER type FEBS_IMP_POST .
  class-data T_FEB_KEYS type FEBY_APPL_LOG_KEY .
  class-data R_BADI_BANK_STATEMENT type ref to FEB_BSIMP_BANK_STATEMENT .
  class-data R_BADI_FILE type ref to FEB_BSIMP_FILE .
  class-data T_CONTROL_MAIN_PATHS type FEBY_IMP_SOURCE .
  class-data S_CONTROL_MAIN_PATHS type FEB_IMP_SOURCE .
  class-data T_SELOPT_BELNR type FEBY_SELOPT .
  class-data T_SELOPT_XBLNR type FEBY_SELOPT .
  class-data C_ANWND type FEBKO-ANWND .
  class-data S_PRINTPARAM type FEBS_PRINTPARAM .
  class-data X_EXECPRI type RFPDO1-FEBEINLES .
  class-data X_INTRADAY type FEB_INTRADAY .
  class-data X_VGEXT_OK type CHAR1 .
  class-data T_RANGE_KUKEY type FEBY_RANGE_KUKEY .
  class-data T_MANSP type FEBY_MANSP .
  class-data T_NOTT028G type FEBY_NOTT028G .
  class-data T_RANGE_KUKEY_PRINT type FEBY_RANGE_KUKEY .
  class-data C_DETLEVEL1 type CHAR1 value '1'. "#EC NOTEXT .  .  .  .  .  . " .
  class-data C_DETLEVEL2 type CHAR1 value '2'. "#EC NOTEXT .  .  .  .  .  . " .
  class-data C_DETLEVEL3 type CHAR1 value '3'. "#EC NOTEXT .  .  .  .  .  . " .
  class-data C_DETLEVEL4 type CHAR1 value '4'. "#EC NOTEXT .  .  .  .  .  . " .
  class-data C_DETLEVEL5 type CHAR1 value '5'. "#EC NOTEXT .  .  .  .  .  . " .
  class-data C_DETLEVEL6 type CHAR1 value '6'. "#EC NOTEXT .  .  .  .  .  . " .
  class-data X_SCHEDMAN_ERR type CHAR1 .
  class-data C_FORMAT type FEB_FORMAT .
  class-data X_FILEUPL type CHAR1 .
  class-data X_SIMULATION type CHAR1 .
  class-data T_SAVE_BNKSTMT_RESULT type FEBY_SAVE_BNKSTMT_RESULT .

  class-methods AUTOMATIC_PROCESSING
    importing
      !IS_STATEMENT_KEY type FEBS_STATEMENT_KEY
    exceptions
      AUTOMATIC_PROCESSING_FAILED .
  class-methods BANK_STATEMENT
    importing
      !IS_BANK_STATEMENT type FEBS_LOG_FILE
      !IV_FILENAME type FEB_FILENAME .
  class-methods CHECK_KUKEY_IN_FEBKO
    importing
      !IV_KUKEY type KUKEY_EB
    exporting
      !EX_EXIST type BOOLEAN .
  class-methods EXPORT_TO_MEMORY
    exceptions
      EXPORT_FAILED .
  class-methods FREE_MEMORY .
  class-methods INITIALIZE
    importing
      !IT_SEL_OPT_INPUT_PATH type FEBY_SELOPT .
  class-methods PREPROCESSING
    exceptions
      PREPROCESSING_FAILED .
  class-methods PRINT_BNK_STMNT
    exceptions
      PRINT_BNK_STMNT_FAILED .
  class-methods SHIFT_MSG_VARIABLE
    importing
      !IV_LONG_INPUT type TEXT_512
    exporting
      !EV_MSGV1 type SYMSGV
      !EV_MSGV2 type SYMSGV .
  class-methods SAVE_TRANSFER_DETAILS
    importing
      !IS_BANK_STATEMENT type FEBS_LOG_FILE
      !IV_TRANPATH type FEB_PATH_TRANS
      !IV_FILENAME type FEB_FILENAME
      !IV_BANKS type BANKS
      !IV_BUKRS type BUKRS
      !IV_HBKID type HBKID
      !IV_HKTID type HKTID
    exceptions
      CREATE_GUID_FAILED .
ENDCLASS.



CLASS ZCL_FEB_FILE_HANDLING IMPLEMENTATION.


METHOD AUTOMATIC_PROCESSING.

  DATA: sysubrc TYPE sy-subrc.

  CALL METHOD export_to_memory.

  CALL FUNCTION 'FEB_IMP_AUTOMATIC_PROCESSING'
    EXPORTING
      it_range_kukey   = t_range_kukey
      iv_vgext_ok      = x_vgext_ok
      it_mansp         = t_mansp
      it_nott028g      = t_nott028g
      is_statement_key = is_statement_key
    EXCEPTIONS
      error_message    = 1.
  sysubrc = sy-subrc.

  CALL METHOD free_memory.

  IF sysubrc = 1.
    CALL METHOD cl_feb_appl_log_handler=>add_message
      EXPORTING
        i_msgid       = sy-msgid
        i_msgty       = sy-msgty
        i_msgno       = sy-msgno
        i_msgv1       = sy-msgv1
        i_msgv2       = sy-msgv2
        i_msgv3       = sy-msgv3
        i_msgv4       = sy-msgv4
        i_detlevel    = c_detlevel4
      EXCEPTIONS
        error_occured = 1.

    RAISE automatic_processing_failed.
  ENDIF.

ENDMETHOD.


METHOD bank_statement.

  DATA lv_error_path_per_account TYPE feb_path_err.
  DATA lv_msgv1          TYPE symsgv.
  DATA lv_msgv2          TYPE symsgv.
  DATA lv_msgv3          TYPE symsgv.
  DATA lv_msgv4          TYPE symsgv.
  DATA lv_error_path     TYPE feb_path_err.
  DATA ls_feb_key        TYPE febs_appl_log_key.
  DATA lv_transferred    TYPE boolean.
  DATA lv_transfer_failed TYPE boolean.
  DATA lt_tranpath       TYPE feby_tranpath.
  DATA ls_tranpath       TYPE feb_path_trans.
  DATA: lv_bukrs TYPE bukrs,
        lv_hbkid TYPE hbkid,
        lv_hktid TYPE hktid.
  DATA  lt_pattern_value TYPE feby_pattern_value.
  DATA  ls_pattern_value TYPE febs_pattern_value.
  DATA: ls_range_kukey   TYPE febs_range_kukey.
  DATA: ls_nott028g      LIKE LINE OF t_nott028g.
  DATA: sysubrc             TYPE sy-subrc,
        write_error_file    TYPE boolean,
        lx_house_bank       TYPE boolean,
        lx_exist            TYPE boolean,
        lx_post_customizing TYPE boolean.
  DATA: lv_bankn   TYPE bankn,
        lv_bnkn2   TYPE bnkn2,
        lv_iban    TYPE iban,
        lv_swift   TYPE swift,
        lv_blz     TYPE bankk,
        lv_banks   TYPE banks,
        lv_bankl   TYPE bankk,
        lv_bank_id TYPE bankk,
        ls_account TYPE febs_account.
  DATA: lo_log_handler TYPE REF TO cl_feb_appl_log_handler,
        lt_bapiret2    TYPE TABLE OF bapiret2,
        ls_bapiret     TYPE bapiret2,
        lv_esnum       TYPE esnum_eb,
        lv_esnum_prev  TYPE esnum_eb.
  DATA: lv_filename_trans TYPE feb_filename,
        lv_filename       TYPE feb_filename.
  DATA: ls_statement_key TYPE febs_statement_key.
  DATA: lt_acct_statement TYPE tty_feb_if,
        ls_acct_statement TYPE feb_if.
  DATA: ls_save_bnkstmt_result TYPE febs_save_bnkstmt_result.


  CLEAR sysubrc.
  CLEAR: t_range_kukey, x_vgext_ok, t_mansp, t_nott028g.
  CLEAR lt_pattern_value.
  CLEAR: c_anwnd.


* set default error path
  lv_error_path = s_control_main_paths-path_error.

* for status information (MESSAGE_TYPE='N') we only need to
* call method SAVE_BANK_STATEMENT
  IF is_bank_statement-message_type = ''.  "bank statements

    IF is_bank_statement-bank_id IS NOT INITIAL.
      ls_pattern_value-pattern = 'BANK_NO'.
      ls_pattern_value-value = is_bank_statement-bank_id.
      APPEND ls_pattern_value TO lt_pattern_value.
    ENDIF.
    ls_pattern_value-pattern = 'ACCOUNT_NO'.
    ls_pattern_value-value = is_bank_statement-bank_account.
    APPEND ls_pattern_value TO lt_pattern_value.
    ls_pattern_value-pattern = 'STATEMENT_NO'.
    ls_pattern_value-value = is_bank_statement-bank_statement_id.
    APPEND ls_pattern_value TO lt_pattern_value.
    ls_pattern_value-pattern = 'CURRENCY'.
    ls_pattern_value-value = is_bank_statement-currency.
    APPEND ls_pattern_value TO lt_pattern_value.
    ls_pattern_value-pattern = 'STATEMENT_DATE'.
    ls_pattern_value-value = is_bank_statement-bank_statement_date.
    APPEND ls_pattern_value TO lt_pattern_value.


* override error path specific for bank account
    CALL METHOD cl_feb_file_customizing=>read_path_err_lf
      EXPORTING
        iv_banky          = is_bank_statement-bank_id
        iv_bnkaccount_ext = is_bank_statement-bank_account
      IMPORTING
        ev_path_error     = lv_error_path_per_account.

    IF NOT lv_error_path_per_account IS INITIAL.
      lv_error_path = lv_error_path_per_account.
    ENDIF.



    IF is_bank_statement-x_error IS NOT INITIAL.   "bank statement is not ok
      lv_msgv1 = is_bank_statement-bank_id.
      lv_msgv2 = is_bank_statement-bank_account.
      lv_msgv3 = is_bank_statement-bank_statement_id.
      CALL METHOD cl_feb_appl_log_handler=>add_message
        EXPORTING
          i_msgid       = 'FEB_BSIMP'
          i_msgty       = 'E'
          i_msgno       = '040'
          i_msgv1       = lv_msgv1
          i_msgv2       = lv_msgv2
          i_msgv3       = lv_msgv3
          i_detlevel    = c_detlevel4
        EXCEPTIONS
          error_occured = 1.
      IF 1 = 2.
        MESSAGE e040(feb_bsimp).                        "#EC MG_PAR_CNT
      ENDIF.    "Bank statement incorrect
      write_error_file = 'X'.

    ELSE.    "is_bank_statement-x_error IS INITIAL, bank statement looks ok


* Determine BUKRS, HBKID, HKTID, BANKN, BNKN2, IBAN, SWIFT, BLZ
* and transfer paths.
      CLEAR: lv_bukrs, lv_hbkid, lv_hktid, lv_bankn,
             lv_bnkn2, lv_iban, lv_swift, lv_blz, lv_banks, lv_bankl.
      CALL METHOD cl_feb_file_customizing=>determine_account
        EXPORTING
          iv_bank_id      = is_bank_statement-bank_id
          iv_bank_account = is_bank_statement-bank_account
          iv_currency     = is_bank_statement-currency
        IMPORTING
          ev_bukrs        = lv_bukrs
          ev_hbkid        = lv_hbkid
          ev_hktid        = lv_hktid
          ev_bankn        = lv_bankn
          ev_bnkn2        = lv_bnkn2
          ev_iban         = lv_iban
          ev_swift        = lv_swift
          ev_blz          = lv_blz
          ev_banks        = lv_banks
          ev_bankl        = lv_bankl
          es_account      = ls_account
        EXCEPTIONS
          no_curr_code    = 1
          not_found       = 2.

      IF sy-subrc = 0.
        write_error_file = ''.
        lx_house_bank = 'X'.

        ls_pattern_value-pattern = 'CCODE'.
        ls_pattern_value-value = lv_bukrs.
        APPEND ls_pattern_value TO lt_pattern_value.
        ls_pattern_value-pattern = 'BANK_ID'.
        ls_pattern_value-value = lv_hbkid.
        APPEND ls_pattern_value TO lt_pattern_value.
        ls_pattern_value-pattern = 'ACCOUNT_ID'.
        ls_pattern_value-value = lv_hktid.
        APPEND ls_pattern_value TO lt_pattern_value.

      ELSE.
*      write_error_file = 'X'.
        lx_house_bank = ''.
      ENDIF.

      IF is_bank_statement-bank_id IS INITIAL.
        lv_bank_id = lv_bankl.
        ls_pattern_value-pattern = 'BANK_NO'.
        ls_pattern_value-value = lv_bank_id.
        APPEND ls_pattern_value TO lt_pattern_value.
      ELSE.
        lv_bank_id = is_bank_statement-bank_id.
      ENDIF.

      ls_account-banky = lv_bank_id.
      ls_account-bnkaccount_ext = is_bank_statement-bank_account.

* check if transfer is to be done
      CLEAR lt_tranpath.
      CALL METHOD cl_feb_file_customizing=>read_imp_tranpath
        EXPORTING
          iv_banks          = lv_banks
          iv_banky          = lv_bank_id
          iv_bnkaccount_ext = is_bank_statement-bank_account
          iv_bankn          = lv_bankn
          iv_bnkn2          = lv_bnkn2
          iv_iban           = lv_iban
          iv_swift          = lv_swift
          iv_blz            = lv_blz
        IMPORTING
          et_tranpath       = lt_tranpath.


* Do the transfers
      LOOP AT lt_tranpath INTO ls_tranpath.

        IF x_simulation IS INITIAL.
          CALL METHOD cl_feb_file_communication=>write_file
            EXPORTING
              iv_path          = ls_tranpath
              iv_base_filename = iv_filename
              it_pattern_value = lt_pattern_value
              it_content       = is_bank_statement-content
              iv_detlevel      = c_detlevel4
            IMPORTING
              ev_filename      = lv_filename_trans
            EXCEPTIONS
              file_write_error = 1.

          IF sy-subrc = 1.   " write to transfer path failed
            lv_transfer_failed = 'X'.

            IF x_fileupl = 'X'.
              CALL METHOD zcl_feb_file_handling=>shift_msg_variable
                EXPORTING
                  iv_long_input = iv_filename
                IMPORTING
                  ev_msgv1      = lv_msgv1
                  ev_msgv2      = lv_msgv2.
              lv_msgv3 = ls_tranpath.                       "n2824453
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'E'
                  i_msgno       = '22'
                  i_msgv1       = lv_msgv1
                  i_msgv2       = lv_msgv2
                  i_msgv3       = lv_msgv3
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
              IF 1 = 2.
                MESSAGE e022(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF. "File &1 cannot be written in directory &2

            ELSE.  "x_fileupl = ''
              lv_msgv1 = ls_tranpath.                       "n2824453
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'E'
                  i_msgno       = '115'
                  i_msgv1       = lv_msgv1
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
              IF 1 = 2.
                MESSAGE e115(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF. "statement message cannot be written in directory &2
            ENDIF.  "x_fileupl = 'X'

          ELSE.   " write to transfer path ok
            lv_transferred = 'X'.

            IF lv_bankl IS NOT INITIAL.
              lv_msgv1 = lv_bankl.
            ELSE.
              lv_msgv1 = lv_bank_id.
            ENDIF.

            IF lv_bankn IS NOT INITIAL.
              lv_msgv2 = lv_bankn.
            ELSE.
              lv_msgv2 = is_bank_statement-bank_account.
            ENDIF.
            lv_msgv3 = is_bank_statement-bank_statement_id.
            lv_msgv4 = ls_tranpath.
            CALL METHOD cl_feb_appl_log_handler=>add_message
              EXPORTING
                i_msgid       = 'FEB_BSIMP'
                i_msgty       = 'S'
                i_msgno       = '002'
                i_msgv1       = lv_msgv1
                i_msgv2       = lv_msgv2
                i_msgv3       = lv_msgv3
                i_msgv4       = lv_msgv4
                i_detlevel    = c_detlevel4
              EXCEPTIONS
                error_occured = 1.
            IF 1 = 2.
              MESSAGE s002(feb_bsimp).                  "#EC MG_PAR_CNT
            ENDIF. "bank statement ... stored in directory &4

*     save details of transferred file to table FEBKO_TRANSFER
            CALL METHOD save_transfer_details
              EXPORTING
                is_bank_statement  = is_bank_statement
                iv_tranpath        = ls_tranpath
                iv_filename        = lv_filename_trans
                iv_banks           = lv_banks
                iv_bukrs           = lv_bukrs
                iv_hbkid           = lv_hbkid
                iv_hktid           = lv_hktid
              EXCEPTIONS
                create_guid_failed = 1.
            IF sy-subrc NE 0.
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'W'
                  i_msgno       = '094'
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
              "error when creating a GUID, details of transfer not saved
              IF 1 = 2. MESSAGE e094(feb_bsimp). ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
*       simulation mode
          lv_transferred = 'X'.
        ENDIF.  "x_simulation is initial

      ENDLOOP.  "AT lt_tranpath



      IF lx_house_bank = 'X'.   "bank account is a house bank account
* always save in febko and febep if house bank account

*     if format in bank statement is different from format in customizing
*     take this from bank statement
        IF is_bank_statement-format_different IS NOT INITIAL.
          c_format = is_bank_statement-format_different.
        ELSE.
          c_format = s_control_main_paths-febformat.
        ENDIF.

*     set static attributes X_INTRADAY and C_ANWND
        x_intraday = is_bank_statement-x_intraday.
        IF x_intraday = 'X'.
          c_anwnd = '0004'.
        ELSE.
          c_anwnd = '0001'.
        ENDIF.


* read posting parameters from customizing view cluster FEBVC_IMP_POST
* if no entry is found no upload will be done
        CLEAR: s_posting_parameter, t_selopt_belnr, t_selopt_xblnr.

        CALL METHOD cl_feb_file_customizing=>read_imp_post
          EXPORTING
            iv_bukrs        = lv_bukrs
            iv_hbkid        = lv_hbkid
            iv_hktid        = lv_hktid
            iv_intraday     = x_intraday
          IMPORTING
            es_imp_post     = s_posting_parameter
            et_selopt_belnr = t_selopt_belnr
            et_selopt_xblnr = t_selopt_xblnr
          EXCEPTIONS
            not_found       = 1.

        IF sy-subrc = 0. "Upload will be done
          lx_post_customizing = 'X'.

*   Processing before Upload
          CALL METHOD zcl_feb_file_handling=>preprocessing
            EXCEPTIONS
              preprocessing_failed = 1.

          IF sy-subrc = 0.  "Preprocessing was ok
            TRY.
*           upload bank statement
                CALL BADI r_badi_bank_statement->save_bank_statement
                  EXPORTING
                    iv_filter_value            = c_format
                    it_bank_statement          = is_bank_statement-content
                    is_control_main_paths      = s_control_main_paths
                    is_posting_parameter       = s_posting_parameter
                    iv_anwnd                   = c_anwnd
                    is_account                 = ls_account
                    ix_simulation              = x_simulation
                  IMPORTING
                    et_s_kukey                 = t_range_kukey
                    ev_vgext_ok                = x_vgext_ok
                    et_mansp                   = t_mansp
                    et_nott028g                = t_nott028g
                    et_acct_statement          = lt_acct_statement
                  EXCEPTIONS
                    check_before_upload_failed = 1
                    bank_statement_not_saved   = 2.
                sysubrc = sy-subrc.
              CATCH cx_badi_initial_reference.
                sysubrc = 1.
            ENDTRY.
          ELSE.
            sysubrc = sy-subrc.
          ENDIF.

          IF sysubrc IS NOT INITIAL.
            lv_msgv1 = lv_bankl.
            lv_msgv2 = lv_bankn.
            lv_msgv3 = is_bank_statement-bank_statement_id.
            CALL METHOD cl_feb_appl_log_handler=>add_message
              EXPORTING
                i_msgid       = 'FEB_BSIMP'
                i_msgty       = 'E'
                i_msgno       = '041'
                i_msgv1       = lv_msgv1
                i_msgv2       = lv_msgv2
                i_msgv3       = lv_msgv3
                i_detlevel    = c_detlevel4
              EXCEPTIONS
                error_occured = 1.
            IF 1 = 2.
              MESSAGE e041(feb_bsimp).                  "#EC MG_PAR_CNT
            ENDIF.  "Error occured when loading bank account statement
*       Write file to error path
            write_error_file = 'X'.

          ELSE.
*     Processing in RFEBKA00 after Upload (Post, Print, Intraday etc)
*     Only done if not in simulation mode
            IF x_simulation IS INITIAL.
              MOVE-CORRESPONDING is_bank_statement TO ls_statement_key.
              CALL METHOD zcl_feb_file_handling=>automatic_processing
                EXPORTING
                  is_statement_key            = ls_statement_key
                EXCEPTIONS
                  automatic_processing_failed = 1.
              IF sy-subrc NE 0.
                x_schedman_err = 'X'.
              ENDIF.
            ENDIF. "x_simulation is initial
          ENDIF.



          LOOP AT t_range_kukey INTO ls_range_kukey.

            IF x_simulation IS INITIAL.
*       Check if kukey is stored in FEBKO
              CALL METHOD zcl_feb_file_handling=>check_kukey_in_febko
                EXPORTING
                  iv_kukey = ls_range_kukey-low
                IMPORTING
                  ex_exist = lx_exist.

            ELSE.
              IF x_vgext_ok = '0'.
                lx_exist = 'X'.
              ELSE.  "x_vgext_ok = '8'
                lx_exist = ''.
              ENDIF.
            ENDIF.  "x_simulation is initial


            IF lx_exist = 'X'.  "Kukey exists in FEBKO

*             Append to static attribute T_SAVE_BNKSTMT_RESULT
              CLEAR ls_save_bnkstmt_result.
              READ TABLE lt_acct_statement INTO ls_acct_statement INDEX 1.
              ls_save_bnkstmt_result-s_account = ls_account.
              ls_save_bnkstmt_result-kukey = ls_range_kukey-low.
              ls_save_bnkstmt_result-s_acct_statement = ls_acct_statement.
              APPEND ls_save_bnkstmt_result TO t_save_bnkstmt_result.


              IF x_simulation IS INITIAL.
                lv_msgv1 = lv_bankl.
                lv_msgv2 = lv_bankn.
                lv_msgv3 = is_bank_statement-bank_statement_id.
                lv_msgv4 = ls_range_kukey-low.
                CALL METHOD cl_feb_appl_log_handler=>add_message
                  EXPORTING
                    i_msgid       = 'FEB_BSIMP'
                    i_msgty       = 'I'
                    i_msgno       = '009'
                    i_msgv1       = lv_msgv1
                    i_msgv2       = lv_msgv2
                    i_msgv3       = lv_msgv3
                    i_msgv4       = lv_msgv4
                    i_detlevel    = c_detlevel4
                  EXCEPTIONS
                    error_occured = 1.
                IF 1 = 2.
                  MESSAGE i009(feb_bsimp).              "#EC MG_PAR_CNT
                ENDIF. "bank statement with KUKEY .. stored in FEBKO/FEBEP
              ENDIF.  "x_simulation is initial

*      Read messages from interpretation and posting
*      Write them into joblog for the upload and additionally to
*      another joblog for each kukey and esnum (=> these messages
*      can then be seen in TA FEB_BSPROC)
              CREATE OBJECT lo_log_handler.
              CALL METHOD lo_log_handler->retrieve_messages
                EXPORTING
                  i_kukey    = ls_range_kukey-low
                  i_esnum    = 0
                RECEIVING
                  rt_message = lt_bapiret2.

              IF lt_bapiret2 IS NOT INITIAL.
                CALL METHOD cl_feb_appl_log_handler=>add_message
                  EXPORTING
                    i_msgid       = 'FEB_BSIMP'
                    i_msgty       = 'I'
                    i_msgno       = '082'
                    i_detlevel    = c_detlevel4
                  EXCEPTIONS
                    error_occured = 1.
                IF 1 = 2.
                  MESSAGE i082(feb_bsimp).              "#EC MG_PAR_CNT
                ENDIF."Interpretation/ Posting

                SORT lt_bapiret2 STABLE BY log_msg_no.
                CLEAR lv_esnum_prev.
                LOOP AT lt_bapiret2 INTO ls_bapiret.
                  lv_esnum = ls_bapiret-log_msg_no.

                  IF lv_esnum NE lv_esnum_prev.
*               group all messages for one esnum together
                    lv_msgv1 = lv_esnum.
                    CALL METHOD cl_feb_appl_log_handler=>add_message
                      EXPORTING
                        i_msgid       = 'FEB_BSIMP'
                        i_msgty       = 'I'
                        i_msgno       = '083'
                        i_msgv1       = lv_msgv1
                        i_detlevel    = c_detlevel5
                      EXCEPTIONS
                        error_occured = 1.
                    IF 1 = 2.
                      MESSAGE i083(feb_bsimp).          "#EC MG_PAR_CNT
                    ENDIF.
                    lv_esnum_prev = lv_esnum.
                  ENDIF.

*             collect KUKEYs
                  ls_feb_key-kukey = ls_range_kukey-low.
                  ls_feb_key-esnum = lv_esnum.
                  APPEND ls_feb_key TO t_feb_keys.

*             add message to joblog for kukey and esnum
                  CALL METHOD cl_feb_appl_log_handler=>add_message
                    EXPORTING
                      i_kukey       = ls_range_kukey-low
                      i_esnum       = lv_esnum
                      i_msgid       = ls_bapiret-id
                      i_msgty       = ls_bapiret-type
                      i_msgno       = ls_bapiret-number
                      i_msgv1       = ls_bapiret-message_v1
                      i_msgv2       = ls_bapiret-message_v2
                      i_msgv3       = ls_bapiret-message_v3
                      i_msgv4       = ls_bapiret-message_v4
                    EXCEPTIONS
                      error_occured = 1.

*           add message from posting to upload joblog
                  CALL METHOD cl_feb_appl_log_handler=>add_message
                    EXPORTING
                      i_msgid       = ls_bapiret-id
                      i_msgty       = ls_bapiret-type
                      i_msgno       = ls_bapiret-number
                      i_msgv1       = ls_bapiret-message_v1
                      i_msgv2       = ls_bapiret-message_v2
                      i_msgv3       = ls_bapiret-message_v3
                      i_msgv4       = ls_bapiret-message_v4
                      i_detlevel    = c_detlevel6
                    EXCEPTIONS
                      error_occured = 1.
                ENDLOOP.

                DELETE ADJACENT DUPLICATES FROM t_feb_keys.
                LOOP AT t_feb_keys INTO ls_feb_key.
                  CALL METHOD cl_feb_appl_log_handler=>save
                    EXPORTING
                      i_kukey       = ls_feb_key-kukey
                      i_esnum       = ls_feb_key-esnum
                    EXCEPTIONS
                      error_occured = 1.
                ENDLOOP.
                CLEAR t_feb_keys.
              ENDIF.

*         Collect for printing
              APPEND LINES OF t_range_kukey TO t_range_kukey_print.

            ELSE.   "Kukey doesn't exist in FEBKO

              lv_msgv1 = lv_bankl.
              lv_msgv2 = lv_bankn.
              lv_msgv3 = is_bank_statement-bank_statement_id.
*              lv_msgv4 = ls_range_kukey-low.
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'E'
                  i_msgno       = '006'
                  i_msgv1       = lv_msgv1
                  i_msgv2       = lv_msgv2
                  i_msgv3       = lv_msgv3
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
              IF 1 = 2.
                MESSAGE e006(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF. "bank statement not stored in FEBKO/FEBEP
*         bank statement not stored => we write an error file
              write_error_file = 'X'.
            ENDIF.
          ENDLOOP.

*     If GVC was not valid (not in T028G), we give out a warning message
          SORT t_nott028g BY vgext.
          CLEAR lv_msgv1.
          LOOP AT t_nott028g INTO ls_nott028g.
            IF ls_nott028g-vgext NE lv_msgv1.
              lv_msgv1 = ls_nott028g-vgext.
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'W'
                  i_msgno       = '037'
                  i_msgv1       = lv_msgv1
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
              IF 1 = 2.
                MESSAGE i037(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF. "Externer Vorgang &1 nicht in T028G
              x_schedman_err = 'X'.
            ENDIF.
          ENDLOOP.

        ELSE. "Posting customizing was not found, we give out a info message
          lx_post_customizing = ''.

          lv_msgv1 = lv_bankl.
          lv_msgv2 = lv_bankn.
          lv_msgv3 = sy-mandt.
          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'W'
              i_msgno       = '100'
              i_msgv1       = lv_msgv1
              i_msgv2       = lv_msgv2
              i_msgv3       = lv_msgv3
              i_detlevel    = c_detlevel4
            EXCEPTIONS
              error_occured = 1.
*     for house bank account no posting parameters are maintained
          IF 1 = 2.
            MESSAGE w100(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF.

        ENDIF.   "Posting customizing was found and upload was done

      ELSE.   "bank account is not a house bank account
        lv_msgv1 = lv_bank_id.
        lv_msgv2 = is_bank_statement-bank_account.
        lv_msgv3 = sy-mandt.
        CALL METHOD cl_feb_appl_log_handler=>add_message
          EXPORTING
            i_msgid       = 'FEB_BSIMP'
            i_msgty       = 'W'
            i_msgno       = '005'
            i_msgv1       = lv_msgv1
            i_msgv2       = lv_msgv2
            i_msgv3       = lv_msgv3
            i_detlevel    = c_detlevel4
          EXCEPTIONS
            error_occured = 1.
*     bank account is not a house bank account in client
        IF 1 = 2.
          MESSAGE w005(feb_bsimp).                      "#EC MG_PAR_CNT
        ENDIF.
      ENDIF.   "bank account is a house bank account



*error message if not written to transfer path although customizing exists
      IF NOT lt_tranpath IS INITIAL.
        IF lv_transferred IS INITIAL.  "all transfers failed

          IF lv_bankl IS NOT INITIAL.
            lv_msgv1 = lv_bankl.
          ELSE.
            lv_msgv1 = lv_bank_id.
          ENDIF.


          IF lv_bankn IS INITIAL.
            lv_msgv2 = is_bank_statement-bank_account.
          ELSE.
            lv_msgv2 = lv_bankn.
          ENDIF.
          lv_msgv3 = is_bank_statement-bank_statement_id.
          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'E'
              i_msgno       = '015'
              i_msgv1       = lv_msgv1
              i_msgv2       = lv_msgv2
              i_msgv3       = lv_msgv3
              i_detlevel    = c_detlevel4
            EXCEPTIONS
              error_occured = 1.
*     bank statement not stored in ANY transfer paths
          IF 1 = 2.
            MESSAGE e015(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF.
          write_error_file = 'X'.
        ELSE.   "at least one transfer was ok
          IF lv_transfer_failed IS NOT INITIAL.  "at least one transfer was not ok

            IF lv_bankl IS NOT INITIAL.
              lv_msgv1 = lv_bankl.
            ELSE.
              lv_msgv1 = lv_bank_id.
            ENDIF.

            IF lv_bankn IS INITIAL.
              lv_msgv2 = is_bank_statement-bank_account.
            ELSE.
              lv_msgv2 = lv_bankn.
            ENDIF.
            lv_msgv3 = is_bank_statement-bank_statement_id.
            CALL METHOD cl_feb_appl_log_handler=>add_message
              EXPORTING
                i_msgid       = 'FEB_BSIMP'
                i_msgty       = 'E'
                i_msgno       = '027'
                i_msgv1       = lv_msgv1
                i_msgv2       = lv_msgv2
                i_msgv3       = lv_msgv3
                i_detlevel    = c_detlevel4
              EXCEPTIONS
                error_occured = 1.
*         bank statement not stored in ALL transfer paths
            IF 1 = 2.
              MESSAGE e027(feb_bsimp).                  "#EC MG_PAR_CNT
            ENDIF.
            write_error_file = 'X'.
          ENDIF.
        ENDIF.
      ELSE.  "no transfer paths customized
        IF lx_house_bank IS INITIAL OR lx_post_customizing IS INITIAL.

          IF lv_bankl IS NOT INITIAL.
            lv_msgv1 = lv_bankl.
          ELSE.
            lv_msgv1 = lv_bank_id.
          ENDIF.

          IF lv_bankn IS INITIAL.
            lv_msgv2 = is_bank_statement-bank_account.
          ELSE.
            lv_msgv2 = lv_bankn.
          ENDIF.
          lv_msgv3 = is_bank_statement-bank_statement_id.

          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'E'
              i_msgno       = '073'
              i_msgv1       = lv_msgv1
              i_msgv2       = lv_msgv2
              i_msgv3       = lv_msgv3
              i_detlevel    = c_detlevel4
            EXCEPTIONS
              error_occured = 1.
*       no persistency for bank statement
          IF 1 = 2.
            MESSAGE e073(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF.
          write_error_file = 'X'.
        ENDIF.
      ENDIF.

    ENDIF.

  ELSE. "MESSAGE_TYPE = 'N', status notifications

    IF is_bank_statement-format_different IS NOT INITIAL.
      c_format = is_bank_statement-format_different.
    ELSE.
      c_format = s_control_main_paths-febformat.
    ENDIF.

    TRY.
*       upload status notification
        CALL BADI r_badi_bank_statement->save_bank_statement
          EXPORTING
            iv_filter_value          = c_format
            it_bank_statement        = is_bank_statement-content
            is_control_main_paths    = s_control_main_paths
            iv_message_type          = is_bank_statement-message_type
          EXCEPTIONS
            bank_statement_not_saved = 1.
        sysubrc = sy-subrc.
      CATCH cx_badi_initial_reference.
        sysubrc = 1.
    ENDTRY.

    IF sysubrc IS NOT INITIAL.
      CALL METHOD cl_feb_appl_log_handler=>add_message
        EXPORTING
          i_msgid       = 'FEB_BSIMP'
          i_msgty       = 'E'
          i_msgno       = '120'
          i_detlevel    = c_detlevel4
        EXCEPTIONS
          error_occured = 1.
      IF 1 = 2.
        MESSAGE e120(feb_bsimp).                        "#EC MG_PAR_CNT
      ENDIF.  "Error occured when loading status information
      write_error_file = 'X'.
    ENDIF.

  ENDIF.  "MESSAGE_TYPE='', bank statements



  IF write_error_file = 'X'.   "write to error path
    x_schedman_err = 'X'.

    IF x_simulation IS INITIAL.

      CALL METHOD cl_feb_file_communication=>write_file
        EXPORTING
          iv_path          = lv_error_path
          iv_base_filename = iv_filename
          it_pattern_value = lt_pattern_value
          it_content       = is_bank_statement-content
          iv_detlevel      = c_detlevel4
        IMPORTING
          ev_filename      = lv_filename
        EXCEPTIONS
          file_write_error = 1.

      IF sy-subrc = 1. "Write to error path failed

        IF x_fileupl = 'X'.
          CALL METHOD zcl_feb_file_handling=>shift_msg_variable
            EXPORTING
              iv_long_input = iv_filename
            IMPORTING
              ev_msgv1      = lv_msgv1
              ev_msgv2      = lv_msgv2.
          lv_msgv3 = lv_error_path.
          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'E'
              i_msgno       = '022'
              i_msgv1       = lv_msgv1
              i_msgv2       = lv_msgv2
              i_msgv3       = lv_msgv3
              i_detlevel    = c_detlevel4
            EXCEPTIONS
              error_occured = 1.
*     File &1 cannot be stored in directory &2
          IF 1 = 2.
            MESSAGE e022(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF.

        ELSE. "x_fileupl = ''
          lv_msgv1 = lv_error_path.
          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'E'
              i_msgno       = '115'
              i_msgv1       = lv_msgv1
              i_detlevel    = c_detlevel4
            EXCEPTIONS
              error_occured = 1.
*     statement message cannot be stored in directory &2
          IF 1 = 2.
            MESSAGE e115(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF.
        ENDIF.  "x_fileupl = 'X'

*     we write the file to the source path
        CALL METHOD cl_feb_file_communication=>write_file
          EXPORTING
            iv_path          = s_control_main_paths-path_source
            iv_base_filename = iv_filename
            it_pattern_value = lt_pattern_value
            it_content       = is_bank_statement-content
            iv_detlevel      = c_detlevel4
          EXCEPTIONS
            file_write_error = 1.

      ELSE. "Write to error path ok
        IF lv_bankl IS NOT INITIAL.
          lv_msgv1 = lv_bankl.
        ELSE.
          lv_msgv1 = lv_bank_id.
        ENDIF.

        IF lv_bankn IS INITIAL.
          lv_msgv2 = is_bank_statement-bank_account.
        ELSE.
          lv_msgv2 = lv_bankn.
        ENDIF.
        lv_msgv3 = is_bank_statement-bank_statement_id.
        lv_msgv4 = lv_error_path.

        IF is_bank_statement-message_type = ''.
          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'I'
              i_msgno       = '002'
              i_msgv1       = lv_msgv1
              i_msgv2       = lv_msgv2
              i_msgv3       = lv_msgv3
              i_msgv4       = lv_msgv4
              i_detlevel    = c_detlevel4
            EXCEPTIONS
              error_occured = 1.
          IF 1 = 2.
            MESSAGE i002(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF. "bank statement ... stored in directory ...

        ELSE.  "message_type = 'N'
          lv_msgv1 = iv_filename.
          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'I'
              i_msgno       = '121'
              i_msgv1       = lv_msgv1
              i_msgv4       = lv_msgv4
            EXCEPTIONS
              error_occured = 1.
          IF 1 = 2.
            MESSAGE i121(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF. "status information stored in directory ...
        ENDIF.

      ENDIF.  "write to error path failed
    ENDIF.  "x_simulation is initial
  ENDIF.  "write_error_file = 'X'


ENDMETHOD.


METHOD CHECK_KUKEY_IN_FEBKO.

  DATA: lv_kukey TYPE kukey_eb.

  SELECT kukey FROM febko INTO lv_kukey
    WHERE kukey = iv_kukey.
  ENDSELECT.
  IF sy-subrc = 0.
    ex_exist = 'X'.
  ELSE.
    ex_exist = ''.
  ENDIF.

ENDMETHOD.


METHOD EXPORT_TO_MEMORY.

  DATA: l_id(20).
  DATA: ls_range_kukey TYPE febs_range_kukey.

  LOOP AT t_range_kukey INTO ls_range_kukey.
    MOVE ls_range_kukey-low TO l_id.
    EXPORT kukey = ls_range_kukey-low TO MEMORY ID l_id.
  ENDLOOP.

ENDMETHOD.


METHOD FREE_MEMORY.
  DATA: l_id(20).
  DATA: ls_range_kukey TYPE febs_range_kukey.

  LOOP AT t_range_kukey INTO ls_range_kukey.
    MOVE ls_range_kukey-low TO l_id.
    FREE MEMORY ID l_id.
  ENDLOOP.

ENDMETHOD.


METHOD IMPORT_FROM_MEMORY.
  DATA: l_id(20).
  DATA: l_kukey TYPE kukey_eb.

  MOVE iv_kukey TO l_id.
  IMPORT kukey = l_kukey FROM MEMORY ID l_id.
  IF sy-subrc NE 0.
    RAISE no_kukey_found.
  ENDIF.

ENDMETHOD.


METHOD INITIALIZE.
  DATA ls_file_mapping TYPE febs_file_mapping.
  DATA lt_file_mapping TYPE feby_file_mapping.
  DATA lt_filepath TYPE feby_filepath.
  DATA ls_filepath TYPE feb_filepath.
  DATA ls_control_main_paths TYPE feb_imp_source.

  CALL METHOD cl_feb_file_customizing=>get_imp_source
    EXPORTING
      it_selopt     = it_sel_opt_input_path
    RECEIVING
      rt_imp_source = t_control_main_paths.

  IF t_control_main_paths IS INITIAL.
    CALL METHOD cl_feb_appl_log_handler=>add_message
      EXPORTING
        i_msgid       = 'FEB_BSIMP'
        i_msgty       = 'W'
        i_msgno       = '023'
        i_detlevel    = c_detlevel2
      EXCEPTIONS
        error_occured = 1.
    IF 1 = 2. MESSAGE w023(feb_bsimp). ENDIF.
    RETURN.
  ENDIF.

  CALL METHOD cl_feb_file_customizing=>get_filepath
    RECEIVING
      rt_filepath = lt_filepath.

  LOOP AT lt_filepath INTO ls_filepath.
    READ TABLE t_control_main_paths WITH KEY path_source = ls_filepath-path INTO ls_control_main_paths.
    IF sy-subrc = 0.                                           "n2430609
      ls_file_mapping-codepage = ls_control_main_paths-codepage.
    ENDIF.                                                     "n2430609
    MOVE-CORRESPONDING ls_filepath TO ls_file_mapping.
    APPEND ls_file_mapping TO lt_file_mapping.
  ENDLOOP.


* send necessary data to file communication
  CALL METHOD cl_feb_file_communication=>init_customizing
    EXPORTING
      it_file_customizing = lt_file_mapping.


ENDMETHOD.


METHOD main.
  " rafael

  DATA: lv_file_error TYPE febs_filelist-filename.
  STATICS:  lt_log_handle TYPE bal_t_logh.

  DATA lt_file_list TYPE feby_filelist.
  DATA lt_file_list_erro TYPE feby_filelist.
  DATA lt_file_list_true TYPE feby_filelist.
  DATA lt_bank_statements TYPE feby_logical_files.
  DATA ls_bank_statements TYPE febs_log_file.
  DATA lt_file_contents TYPE feby_content.
  DATA ls_list_item TYPE febs_filelist.
  DATA lv_dir TYPE string.
  DATA lx_file_corrupt TYPE boolean.
  DATA lx_badi_err TYPE boolean.
  DATA lx_get_bank_stmnts_failed TYPE boolean.
  DATA lv_msgv1 TYPE symsgv.
  DATA lv_msgv2 TYPE symsgv.
  DATA lv_msgv3 TYPE symsgv.
  DATA lv_sysubrc TYPE sy-subrc.
  DATA lv_print_koausz TYPE febpausz.
  DATA:lt_selopt TYPE feby_selopt,
       ls_selopt TYPE febs_selopt,
       ls_line   TYPE febs_line.

  DATA: ls_file_contents_binary TYPE xstring,               "n2427571
        lv_lines                TYPE i.                     "n2427571



  IF lt_log_handle IS NOT INITIAL.
    CALL METHOD cl_feb_appl_log_handler=>message_delete_all
      EXPORTING
        it_loghandle = lt_log_handle.
    CLEAR lt_log_handle.
  ENDIF.


  CLEAR t_save_bnkstmt_result.


* simulation mode: save in FEBxx or not?
  x_simulation = ix_simulation.

* where do we come from ...?
  IF it_sel_opt_input_path IS NOT INITIAL.
    x_fileupl = 'X'.
  ELSE.
    IF iv_bank_statement IS NOT INITIAL.
      x_fileupl = ''.
    ELSE.
      RAISE statement_message_empty.
    ENDIF.
  ENDIF.


  IF x_simulation IS INITIAL.
    CALL METHOD cl_feb_appl_log_handler=>add_message
      EXPORTING
        i_msgid       = 'FEB_BSIMP'
        i_msgty       = 'I'
        i_msgno       = '039'
        i_msgv1       = lv_msgv1
        i_detlevel    = c_detlevel1
      EXCEPTIONS
        error_occured = 1.
    IF 1 = 2. MESSAGE i039(feb_bsimp). ENDIF. "Bank statement processing
  ELSE.
    CALL METHOD cl_feb_appl_log_handler=>add_message
      EXPORTING
        i_msgid       = 'FEB_BSIMP'
        i_msgty       = 'I'
        i_msgno       = '124'
        i_detlevel    = c_detlevel1
      EXCEPTIONS
        error_occured = 1.
    IF 1 = 2. MESSAGE i124(feb_bsimp). ENDIF. "Simulation bank statement processing
  ENDIF.

* set static attributes
  s_printparam = is_printparam.
  x_execpri = iv_execpri.

* bank statements will be printed all together after the loops and
* not for each bank statement. So we keep the user input in a local
* variable for later printing and clear it in s_printparam
  lv_print_koausz = s_printparam-koausz.
  s_printparam-koausz = ''.


  IF x_fileupl = ''.
    ls_selopt-sign = 'I'.
    ls_selopt-option = 'EQ'.
    APPEND ls_selopt TO lt_selopt.
  ELSE.
    lt_selopt = it_sel_opt_input_path.
  ENDIF.


  CALL METHOD zcl_feb_file_handling=>initialize
    EXPORTING
      it_sel_opt_input_path = lt_selopt.

* get BADI references
  TRY.
      GET BADI r_badi_file.
    CATCH cx_badi_not_implemented.
  ENDTRY.


* loop over selected paths
  LOOP AT t_control_main_paths INTO s_control_main_paths.

* Enqueue on path_source
    CALL FUNCTION 'ENQUEUE_EFEB_IMP_SOURCE'
      EXPORTING
        mandt          = sy-mandt
        path_source    = s_control_main_paths-path_source
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 2 OR sy-subrc = 3.
      CALL METHOD cl_feb_appl_log_handler=>add_message
        EXPORTING
          i_msgid       = sy-msgid
          i_msgty       = sy-msgty
          i_msgno       = sy-msgno
          i_msgv1       = sy-msgv1
          i_msgv2       = sy-msgv2
          i_msgv3       = sy-msgv3
          i_msgv4       = sy-msgv4
          i_detlevel    = c_detlevel2
        EXCEPTIONS
          error_occured = 1.
    ELSEIF sy-subrc = 1.
      lv_msgv2 = s_control_main_paths-path_source.
      CALL METHOD cl_feb_appl_log_handler=>add_message
        EXPORTING
          i_msgid       = 'FEB_BSIMP'
          i_msgty       = 'E'
          i_msgno       = '095'
          i_msgv1       = sy-msgv1
          i_msgv2       = lv_msgv2
          i_detlevel    = c_detlevel2
        EXCEPTIONS
          error_occured = 1.
      IF 1 = 2.
        MESSAGE i095(feb_bsimp).                        "#EC MG_PAR_CNT
      ENDIF. "source path is locked

* source path is locked or ... => we read the next source path
      CONTINUE.
    ENDIF.


    IF x_fileupl = 'X'.

      lv_msgv1 = s_control_main_paths-path_source.
      CALL METHOD cl_feb_appl_log_handler=>add_message
        EXPORTING
          i_msgid       = 'FEB_BSIMP'
          i_msgty       = 'I'
          i_msgno       = '033'
          i_msgv1       = lv_msgv1
          i_detlevel    = c_detlevel2
        EXCEPTIONS
          error_occured = 1.
      IF 1 = 2.
        MESSAGE i033(feb_bsimp).                        "#EC MG_PAR_CNT
      ENDIF. "Begin read from directory &1


*   get list of relevant files from source directory.
      CALL METHOD cl_feb_file_communication=>get_read_file_list
        EXPORTING
          iv_path        = s_control_main_paths-path_source
          iv_detlevel    = c_detlevel3
        IMPORTING
          et_filelist    = lt_file_list
          ev_dir         = lv_dir
        EXCEPTIONS
          not_customized = 1
          no_dir_access  = 2.
      lv_sysubrc = sy-subrc.

      CASE sy-subrc.
        WHEN 1.
          lv_msgv1 = s_control_main_paths-path_source.
          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'E'
              i_msgno       = '012'
              i_msgv1       = lv_msgv1
              i_detlevel    = c_detlevel3
            EXCEPTIONS
              error_occured = 1.
*       no physical path exists for path ...
          IF 1 = 2.
            MESSAGE e012(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF.
          x_schedman_err = 'X'.

        WHEN 2.
          lv_msgv1 = s_control_main_paths-path_source.
          CALL METHOD cl_feb_appl_log_handler=>add_message
            EXPORTING
              i_msgid       = 'FEB_BSIMP'
              i_msgty       = 'E'
              i_msgno       = '013'
              i_msgv1       = lv_msgv1
              i_detlevel    = c_detlevel3
            EXCEPTIONS
              error_occured = 1.
          IF 1 = 2.
            MESSAGE e013(feb_bsimp).                    "#EC MG_PAR_CNT
          ENDIF. "Read not possible
          x_schedman_err = 'X'.

      ENDCASE.
      "BREAK-POINT.
    ENDIF.
*    SORT: lt_file_list BY mtime DESCENDING.
    DATA: lv_auszugfile TYPE rlgrap-filename.
    DATA: ls_febko      TYPE febko.
    DATA: ld_extension(4)   TYPE c .
    CLEAR: lt_file_list_true[], lt_file_list_erro[].
    LOOP AT lt_file_list INTO ls_list_item.
      IF ls_list_item-filename(4) NE 'ERRO'.
        APPEND ls_list_item TO lt_file_list_true.
      ELSE.
        APPEND ls_list_item TO lt_file_list_erro.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_file_list INTO ls_list_item.

      CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
        EXPORTING
          filename  = ls_list_item-filename
          uppercase = 'X'
        IMPORTING
          extension = ld_extension.
      IF sy-subrc EQ 0.
        "All OK
      ENDIF.

      CONCATENATE 'ERRO_' ls_list_item-filename INTO lv_file_error.

      CALL METHOD cl_feb_file_communication=>read_file
        EXPORTING
          iv_path         = s_control_main_paths-path_source
          iv_filename     = ls_list_item-filename
          iv_detlevel     = c_detlevel4
        IMPORTING
          et_content      = lt_file_contents
        EXCEPTIONS
          file_read_error = 1.

      DESCRIBE TABLE lt_file_contents LINES DATA(lines).
      IF lines NE '4'.
  "      BREAK-POINT.
      ENDIF.
      CLEAR: ls_febko, lv_auszugfile.
      CONCATENATE lv_dir '/' ls_list_item-filename INTO lv_auszugfile.

      DATA: lv_num          TYPE bkpf-belnr,
            lv_extrato(100) TYPE c.
      READ TABLE lt_file_contents ASSIGNING FIELD-SYMBOL(<fs_extrato>) INDEX 1.
      lv_num = <fs_extrato>-line+61(9).
      SHIFT lv_num LEFT DELETING LEADING '0'.
      CONDENSE lv_num.

      EXPORT lv_auszugfile FROM lv_auszugfile TO MEMORY ID 'LV_EXTRATO'.
      TRY.
          SUBMIT rfebka00 USING SELECTION-SET 'IGUA_EXTRATO'
                                      AND RETURN.
        CATCH cx_root.
      ENDTRY.
      " WAIT UP TO 5 SECONDS.
      IMPORT ls_febko  TO ls_febko
               FROM MEMORY ID 'LS_FEBKO'.
      FREE MEMORY ID 'LS_FEBKO'.
      IF NOT ls_febko IS INITIAL.
        SELECT SINGLE * FROM febko
          INTO @DATA(ls_febko_check)
          WHERE
          absnd = @ls_febko-absnd AND
          azidt = @ls_febko-azidt AND
          emkey = @ls_febko-emkey.
        IF sy-subrc = 0.

* if not simulation mode: move file to storage or error path
          IF x_simulation IS INITIAL.
*     Move file to storage or error path.
            IF lx_file_corrupt IS INITIAL AND lx_badi_err IS INITIAL.
*       Read file and Split were ok, we write the file to the storage
              CALL METHOD cl_feb_file_communication=>write_file
                EXPORTING
                  iv_path          = s_control_main_paths-path_storage
                  iv_base_filename = ls_list_item-filename
                  it_content       = lt_file_contents
                  iv_detlevel      = c_detlevel4
                EXCEPTIONS
                  file_write_error = 1.

              IF sy-subrc = 1.  "Write to storage path not ok

                IF x_fileupl = 'X'.
                  CALL METHOD zcl_feb_file_handling=>shift_msg_variable
                    EXPORTING
                      iv_long_input = ls_list_item-filename
                    IMPORTING
                      ev_msgv1      = lv_msgv1
                      ev_msgv2      = lv_msgv2.
                  lv_msgv3 = s_control_main_paths-path_storage.
                  CALL METHOD cl_feb_appl_log_handler=>add_message
                    EXPORTING
                      i_msgid       = 'FEB_BSIMP'
                      i_msgty       = 'E'
                      i_msgno       = '022'
                      i_msgv1       = lv_msgv1
                      i_msgv2       = lv_msgv2
                      i_msgv3       = lv_msgv3
                      i_detlevel    = c_detlevel4
                    EXCEPTIONS
                      error_occured = 1.
*             File can not be written to path ...
                  IF 1 = 2.
                    MESSAGE e022(feb_bsimp).            "#EC MG_PAR_CNT
                  ENDIF.
                ELSE. "x_fileupl = ''.
                  lv_msgv1 = s_control_main_paths-path_storage.
                  CALL METHOD cl_feb_appl_log_handler=>add_message
                    EXPORTING
                      i_msgid       = 'FEB_BSIMP'
                      i_msgty       = 'E'
                      i_msgno       = '115'
                      i_msgv1       = lv_msgv1
                      i_detlevel    = c_detlevel4
                    EXCEPTIONS
                      error_occured = 1.
*   Statement message can not be written to path ...
                  IF 1 = 2.
                    MESSAGE e115(feb_bsimp).            "#EC MG_PAR_CNT
                  ENDIF.
                ENDIF.

                x_schedman_err = 'X'.

              ELSE.  "Write to storage path ok => we can delete it in the source path

                IF x_fileupl = 'X'.
                  CALL METHOD cl_feb_file_communication=>delete_file
                    EXPORTING
                      iv_path           = s_control_main_paths-path_source
                      iv_filename       = ls_list_item-filename
                      iv_detlevel       = c_detlevel4
                    EXCEPTIONS
                      not_customized    = 1
                      file_delete_error = 2.
                  IF sy-subrc IS NOT INITIAL.  "Delete failed
                    CALL METHOD zcl_feb_file_handling=>shift_msg_variable
                      EXPORTING
                        iv_long_input = ls_list_item-filename
                      IMPORTING
                        ev_msgv1      = lv_msgv1
                        ev_msgv2      = lv_msgv2.
                    lv_msgv3 = s_control_main_paths-path_source.
                    CALL METHOD cl_feb_appl_log_handler=>add_message
                      EXPORTING
                        i_msgid       = 'FEB_BSIMP'
                        i_msgty       = 'E'
                        i_msgno       = '042'
                        i_msgv1       = lv_msgv1
                        i_msgv2       = lv_msgv2
                        i_msgv3       = lv_msgv3
                        i_detlevel    = c_detlevel4
                      EXCEPTIONS
                        error_occured = 1.
                    IF 1 = 2.
                      MESSAGE e042(feb_bsimp).          "#EC MG_PAR_CNT
                    ENDIF. "File could not be deleted
                    x_schedman_err = 'X'.
                  ENDIF.

                  CALL METHOD zcl_feb_file_handling=>shift_msg_variable
                    EXPORTING
                      iv_long_input = ls_list_item-filename
                    IMPORTING
                      ev_msgv1      = lv_msgv1
                      ev_msgv2      = lv_msgv2.
                  lv_msgv3 = s_control_main_paths-path_storage.
                  CALL METHOD cl_feb_appl_log_handler=>add_message
                    EXPORTING
                      i_msgid       = 'FEB_BSIMP'
                      i_msgty       = 'S'
                      i_msgno       = '004'
                      i_msgv1       = lv_msgv1
                      i_msgv2       = lv_msgv2
                      i_msgv3       = lv_msgv3
                      i_detlevel    = c_detlevel4
                    EXCEPTIONS
                      error_occured = 1.
                  IF 1 = 2.
                    MESSAGE s004(feb_bsimp).            "#EC MG_PAR_CNT
                  ENDIF. "File was read and stored in storage

                ELSE. "x_fileupl = ''.
                  lv_msgv1 = s_control_main_paths-path_storage.
                  CALL METHOD cl_feb_appl_log_handler=>add_message
                    EXPORTING
                      i_msgid       = 'FEB_BSIMP'
                      i_msgty       = 'S'
                      i_msgno       = '113'
                      i_msgv1       = lv_msgv1
                      i_detlevel    = c_detlevel4
                    EXCEPTIONS
                      error_occured = 1.
                  IF 1 = 2.
                    MESSAGE s113(feb_bsimp).            "#EC MG_PAR_CNT
                  ENDIF. "Statement message was read and stored in storage

                ENDIF. "x_fileupl = 'X'.
              ENDIF.


            ENDIF.
          ENDIF.  "x_simulation is initial


        ELSE.  " caso não ache na FEBKO  "Read file or Split file was not ok or BAdI call was not ok

*        x_schedman_err = 'X'. "already done above

*       Try to move the file to error path
          IF lx_file_corrupt IS INITIAL.                    "n2427571
            CALL METHOD cl_feb_file_communication=>write_file
              EXPORTING
                iv_path          = s_control_main_paths-path_error
                "iv_base_filename = ls_list_item-filename
                iv_base_filename = lv_file_error
                it_content       = lt_file_contents
                iv_detlevel      = c_detlevel4
              EXCEPTIONS
                file_write_error = 1.
            lv_sysubrc = sy-subrc.                          "n2427571
          ELSE.
* file_read_error information will be read with binary again   "n2427571
            REFRESH lt_file_contents.
            CALL METHOD cl_feb_file_communication=>read_file
              EXPORTING
                iv_path           = s_control_main_paths-path_source
                iv_filename       = ls_list_item-filename
                iv_detlevel       = c_detlevel4
                iv_corrupt        = lx_file_corrupt
              IMPORTING
                es_content_binary = ls_file_contents_binary
              EXCEPTIONS
                file_read_error   = 1.
            IF sy-subrc = 0.
              CALL METHOD cl_feb_file_communication=>write_file
                EXPORTING
                  iv_path           = s_control_main_paths-path_error
                  "iv_base_filename  = ls_list_item-filename
                  iv_base_filename  = lv_file_error
                  iv_corrupt        = lx_file_corrupt
                  it_content        = lt_file_contents
                  is_content_binary = ls_file_contents_binary
                  iv_detlevel       = c_detlevel4
                EXCEPTIONS
                  file_write_error  = 1.
              lv_sysubrc = sy-subrc.
            ELSE.
              lv_sysubrc = sy-subrc.
            ENDIF.
          ENDIF.                                            "n2427571
          IF lv_sysubrc = 1.  "File can not be written to error path "n2427571

            IF x_fileupl = 'X'.
              CALL METHOD zcl_feb_file_handling=>shift_msg_variable
                EXPORTING
                  "iv_long_input = ls_list_item-filename
                  iv_long_input = lv_file_error
                IMPORTING
                  ev_msgv1      = lv_msgv1
                  ev_msgv2      = lv_msgv2.
              lv_msgv3 = s_control_main_paths-path_error.
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'E'
                  i_msgno       = '022'
                  i_msgv1       = lv_msgv1
                  i_msgv2       = lv_msgv2
                  i_msgv3       = lv_msgv3
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
*         File can not be written to error path
              IF 1 = 2.
                MESSAGE e022(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF.
            ELSE. "x_fileupl = ''.
              lv_msgv1 = s_control_main_paths-path_error.
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'E'
                  i_msgno       = '115'
                  i_msgv1       = lv_msgv1
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
*         statement message can not be written to error path
              IF 1 = 2.
                MESSAGE e115(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF.
            ENDIF.

          ELSE.  " file was successfully written to error path

            IF x_fileupl = 'X'.
              CALL METHOD zcl_feb_file_handling=>shift_msg_variable
                EXPORTING
                  "iv_long_input = ls_list_item-filename
                  iv_long_input = lv_file_error
                IMPORTING
                  ev_msgv1      = lv_msgv1
                  ev_msgv2      = lv_msgv2.
              lv_msgv3 = s_control_main_paths-path_error.
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'S'
                  i_msgno       = '043'
                  i_msgv1       = lv_msgv1
                  i_msgv2       = lv_msgv2
                  i_msgv3       = lv_msgv3
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
              IF 1 = 2.
                MESSAGE s043(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF.  "File was written to error path
*         Write to error path was ok, so we can delete the file in source path
              CALL METHOD cl_feb_file_communication=>delete_file
                EXPORTING
                  iv_path           = s_control_main_paths-path_source
                  iv_filename       = ls_list_item-filename
                  iv_detlevel       = c_detlevel4
                EXCEPTIONS
                  not_customized    = 1
                  file_delete_error = 2.
              IF sy-subrc IS NOT INITIAL.  "Delete failed
                CALL METHOD zcl_feb_file_handling=>shift_msg_variable
                  EXPORTING
                    iv_long_input = ls_list_item-filename
                  IMPORTING
                    ev_msgv1      = lv_msgv1
                    ev_msgv2      = lv_msgv2.
                lv_msgv3 = s_control_main_paths-path_source.
                CALL METHOD cl_feb_appl_log_handler=>add_message
                  EXPORTING
                    i_msgid       = 'FEB_BSIMP'
                    i_msgty       = 'E'
                    i_msgno       = '042'
                    i_msgv1       = lv_msgv1
                    i_msgv2       = lv_msgv2
                    i_msgv3       = lv_msgv3
                    i_detlevel    = c_detlevel4
                  EXCEPTIONS
                    error_occured = 1.
                IF 1 = 2.
                  MESSAGE e042(feb_bsimp).              "#EC MG_PAR_CNT
                ENDIF. "File could not be deleted
              ENDIF.
            ELSE. "x_fileupl = ''.
              lv_msgv1 = s_control_main_paths-path_error.
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'S'
                  i_msgno       = '116'
                  i_msgv1       = lv_msgv1
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
              IF 1 = 2.
                MESSAGE s116(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF.  "statement message was written to error path
            ENDIF. "x_fileupl = 'X'.
          ENDIF.
        ENDIF.
      ELSE.
*       try to move the file to error path
        IF lx_file_corrupt IS INITIAL.                      "n2427571
          CALL METHOD cl_feb_file_communication=>write_file
            EXPORTING
              iv_path          = s_control_main_paths-path_error
              "iv_base_filename = ls_list_item-filename
              iv_base_filename = lv_file_error
              it_content       = lt_file_contents
              iv_detlevel      = c_detlevel4
            EXCEPTIONS
              file_write_error = 1.
          lv_sysubrc = sy-subrc.                            "n2427571
        ELSE.
* file_read_error information will be read with binary again   "n2427571
          REFRESH lt_file_contents.
          CALL METHOD cl_feb_file_communication=>read_file
            EXPORTING
              iv_path           = s_control_main_paths-path_source
              iv_filename       = ls_list_item-filename
              iv_detlevel       = c_detlevel4
              iv_corrupt        = lx_file_corrupt
            IMPORTING
              es_content_binary = ls_file_contents_binary
            EXCEPTIONS
              file_read_error   = 1.
          IF sy-subrc = 0.
            CALL METHOD cl_feb_file_communication=>write_file
              EXPORTING
                iv_path           = s_control_main_paths-path_error
                "iv_base_filename  = ls_list_item-filename
                iv_base_filename  = lv_file_error
                iv_corrupt        = lx_file_corrupt
                it_content        = lt_file_contents
                is_content_binary = ls_file_contents_binary
                iv_detlevel       = c_detlevel4
              EXCEPTIONS
                file_write_error  = 1.
            lv_sysubrc = sy-subrc.
          ELSE.
            lv_sysubrc = sy-subrc.
          ENDIF.
        ENDIF.                                              "n2427571
        IF lv_sysubrc = 1.  "File can not be written to error path "n2427571

          IF x_fileupl = 'X'.
            CALL METHOD zcl_feb_file_handling=>shift_msg_variable
              EXPORTING
                "iv_long_input = ls_list_item-filename
                iv_long_input = lv_file_error
              IMPORTING
                ev_msgv1      = lv_msgv1
                ev_msgv2      = lv_msgv2.
            lv_msgv3 = s_control_main_paths-path_error.
            CALL METHOD cl_feb_appl_log_handler=>add_message
              EXPORTING
                i_msgid       = 'FEB_BSIMP'
                i_msgty       = 'E'
                i_msgno       = '022'
                i_msgv1       = lv_msgv1
                i_msgv2       = lv_msgv2
                i_msgv3       = lv_msgv3
                i_detlevel    = c_detlevel4
              EXCEPTIONS
                error_occured = 1.
*         File can not be written to error path
            IF 1 = 2.
              MESSAGE e022(feb_bsimp).                  "#EC MG_PAR_CNT
            ENDIF.
          ELSE. "x_fileupl = ''.
            lv_msgv1 = s_control_main_paths-path_error.
            CALL METHOD cl_feb_appl_log_handler=>add_message
              EXPORTING
                i_msgid       = 'FEB_BSIMP'
                i_msgty       = 'E'
                i_msgno       = '115'
                i_msgv1       = lv_msgv1
                i_detlevel    = c_detlevel4
              EXCEPTIONS
                error_occured = 1.
*         statement message can not be written to error path
            IF 1 = 2.
              MESSAGE e115(feb_bsimp).                  "#EC MG_PAR_CNT
            ENDIF.
          ENDIF.

        ELSE.  " file was successfully written to error path

          IF x_fileupl = 'X'.
            CALL METHOD zcl_feb_file_handling=>shift_msg_variable
              EXPORTING
                "iv_long_input = ls_list_item-filename
                iv_long_input = lv_file_error
              IMPORTING
                ev_msgv1      = lv_msgv1
                ev_msgv2      = lv_msgv2.
            lv_msgv3 = s_control_main_paths-path_error.
            CALL METHOD cl_feb_appl_log_handler=>add_message
              EXPORTING
                i_msgid       = 'FEB_BSIMP'
                i_msgty       = 'S'
                i_msgno       = '043'
                i_msgv1       = lv_msgv1
                i_msgv2       = lv_msgv2
                i_msgv3       = lv_msgv3
                i_detlevel    = c_detlevel4
              EXCEPTIONS
                error_occured = 1.
            IF 1 = 2.
              MESSAGE s043(feb_bsimp).                  "#EC MG_PAR_CNT
            ENDIF.  "File was written to error path
*         Write to error path was ok, so we can delete the file in source path
            CALL METHOD cl_feb_file_communication=>delete_file
              EXPORTING
                iv_path           = s_control_main_paths-path_source
                iv_filename       = ls_list_item-filename
                iv_detlevel       = c_detlevel4
              EXCEPTIONS
                not_customized    = 1
                file_delete_error = 2.
            IF sy-subrc IS NOT INITIAL.  "Delete failed
              CALL METHOD zcl_feb_file_handling=>shift_msg_variable
                EXPORTING
                  iv_long_input = ls_list_item-filename
                IMPORTING
                  ev_msgv1      = lv_msgv1
                  ev_msgv2      = lv_msgv2.
              lv_msgv3 = s_control_main_paths-path_source.
              CALL METHOD cl_feb_appl_log_handler=>add_message
                EXPORTING
                  i_msgid       = 'FEB_BSIMP'
                  i_msgty       = 'E'
                  i_msgno       = '042'
                  i_msgv1       = lv_msgv1
                  i_msgv2       = lv_msgv2
                  i_msgv3       = lv_msgv3
                  i_detlevel    = c_detlevel4
                EXCEPTIONS
                  error_occured = 1.
              IF 1 = 2.
                MESSAGE e042(feb_bsimp).                "#EC MG_PAR_CNT
              ENDIF. "File could not be deleted
            ENDIF.
          ELSE. "x_fileupl = ''.
            lv_msgv1 = s_control_main_paths-path_error.
            CALL METHOD cl_feb_appl_log_handler=>add_message
              EXPORTING
                i_msgid       = 'FEB_BSIMP'
                i_msgty       = 'S'
                i_msgno       = '116'
                i_msgv1       = lv_msgv1
                i_detlevel    = c_detlevel4
              EXCEPTIONS
                error_occured = 1.
            IF 1 = 2.
              MESSAGE s116(feb_bsimp).                  "#EC MG_PAR_CNT
            ENDIF.  "statement message was written to error path
          ENDIF. "x_fileupl = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD PREPROCESSING.

  CALL FUNCTION 'FEB_IMP_PREPROCESSING'
    EXPORTING
      iv_format            = c_format
      is_printparam        = s_printparam
      iv_anwnd             = c_anwnd
      ix_execpri           = x_execpri
      is_posting_parameter = s_posting_parameter
      it_selopt_belnr      = t_selopt_belnr
      it_selopt_xblnr      = t_selopt_xblnr
    EXCEPTIONS
      error_message        = 1.

  IF sy-subrc = 1.
    CALL METHOD cl_feb_appl_log_handler=>add_message
      EXPORTING
        i_msgid       = sy-msgid
        i_msgty       = sy-msgty
        i_msgno       = sy-msgno
        i_msgv1       = sy-msgv1
        i_msgv2       = sy-msgv2
        i_msgv3       = sy-msgv3
        i_msgv4       = sy-msgv4
        i_detlevel    = c_detlevel4
      EXCEPTIONS
        error_occured = 1.

    RAISE preprocessing_failed.
  ENDIF.

ENDMETHOD.


METHOD PRINT_BNK_STMNT.

  CALL FUNCTION 'FEB_IMP_PRINT_BNK_STMNT'
    EXPORTING
      it_range_kukey = t_range_kukey_print
    EXCEPTIONS
      error_message  = 1.

  IF sy-subrc = 1.
    CALL METHOD cl_feb_appl_log_handler=>add_message
      EXPORTING
        i_msgid       = sy-msgid
        i_msgty       = sy-msgty
        i_msgno       = sy-msgno
        i_msgv1       = sy-msgv1
        i_msgv2       = sy-msgv2
        i_msgv3       = sy-msgv3
        i_msgv4       = sy-msgv4
      EXCEPTIONS
        error_occured = 1.

    RAISE print_bnk_stmnt_failed.
  ENDIF.

ENDMETHOD.


METHOD SAVE_TRANSFER_DETAILS.

  DATA ls_trans TYPE febko_transfer.

* Create GUID
  CALL FUNCTION 'SYSTEM_UUID_C_CREATE'     ##FM_OLDED
    IMPORTING
      uuid   = ls_trans-guid
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc NE 0.
    RAISE create_guid_failed.
  ELSE.
    ls_trans-path_trans = iv_tranpath.
    ls_trans-filename = iv_filename.
    ls_trans-mandt = sy-mandt.
    ls_trans-banks = iv_banks.
    ls_trans-banky = is_bank_statement-bank_id.
    ls_trans-bnkaccount_ext = is_bank_statement-bank_account.
    ls_trans-bukrs = iv_bukrs.
    ls_trans-hbkid = iv_hbkid.
    ls_trans-hktid = iv_hktid.
    ls_trans-waers = is_bank_statement-currency.
    ls_trans-statement_id = is_bank_statement-bank_statement_id.
    ls_trans-statement_date = is_bank_statement-bank_statement_date.
    ls_trans-ssvoz = is_bank_statement-ssvoz.
    ls_trans-ssbtr = is_bank_statement-ssbtr.
    ls_trans-esvoz = is_bank_statement-esvoz.
    ls_trans-esbtr = is_bank_statement-esbtr.

    ls_trans-date_trans = sy-datum.
    ls_trans-time_trans = sy-uzeit.

*   inser into FEBKO_TRANSFER
    INSERT INTO febko_transfer VALUES ls_trans.
  ENDIF.

ENDMETHOD.


METHOD SHIFT_MSG_VARIABLE.
  DATA lv_long_input TYPE text_512.
  CLEAR: ev_msgv1, ev_msgv2.

  lv_long_input = iv_long_input.
*  SHIFT lv_long_input LEFT DELETING LEADING space.
  MOVE lv_long_input(50) TO ev_msgv1.
  MOVE lv_long_input+50(50) TO ev_msgv2.

ENDMETHOD.
ENDCLASS.
