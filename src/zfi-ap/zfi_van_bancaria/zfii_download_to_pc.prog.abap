*&---------------------------------------------------------------------*
*& Include zfii_download_to_pc
*&---------------------------------------------------------------------*
  CONSTANTS lc_linesize TYPE i VALUE 128.

  TYPES ty_linetype TYPE x LENGTH lc_linesize.

  DATA:
    lv_user_codepage(4)      TYPE c,
    lv_out_codepage          LIKE regut-codepage,
    lt_tab_x                 TYPE STANDARD TABLE OF ty_linetype,
    lv_x                     TYPE xstring,
    ls_xfile                 LIKE LINE OF tab_xfile,
    lv_filename_string       TYPE string,
    lv_linesize              TYPE i,
    lv_lines                 TYPE i,
    lv_length                TYPE i,
    lv_fill_length           TYPE i,
    lv_last_length           TYPE i,
    lv_filesize              TYPE i,
    lv_after_download_length TYPE i,
    lv_format_cp             TYPE cpcodepage.


  DATA: lv_van_bancaria  TYPE xfeld.

  "Export executado no programa ZFIR_REM_PGTO
  IMPORT lv_van_bancaria FROM MEMORY ID 'VAN_BANCARIA'.
  IF  lv_van_bancaria = abap_true
  AND sy-binpt        = abap_true.

* Convert data to codepage defined in format, if not defined than to
* codepage in user parameter dcp, if not there then default with 1100

    CLEAR lv_format_cp.
    IF laufi+5(1) = 'M'.
      DATA lv_function TYPE rs38l_fnam.
      lv_function = 'FKK_DME_FDTA_CODEPAGE_GET'.
      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = lv_function
        EXCEPTIONS
          function_not_exist = 1.
      IF sy-subrc = 0.
        CALL FUNCTION lv_function "#EC CI_SUBRC
          EXPORTING
            i_formi    = dtfor
          IMPORTING
            e_codepage = lv_format_cp
          EXCEPTIONS ##FM_SUBRC_OK
            OTHERS     = 1.
      ENDIF.

* begin 2832354
    ELSEIF dttyp = '90'.  " Multi bank connectivity

      DATA : lv_string             TYPE xstring,
             ls_encoding_string_90 TYPE string.

      READ TABLE tab_xfile INDEX 1 INTO lv_string.
*   search for string encoding = ... in file
      PERFORM check_encoding_of_xml_file USING lv_string in_codepage
                                         CHANGING ls_encoding_string_90.
      IF ls_encoding_string_90 = 'utf-8' OR ls_encoding_string_90 = 'UTF-8'.
        lv_format_cp = '4110'.
      ELSEIF ls_encoding_string_90 = 'utf-16' OR ls_encoding_string_90 = 'UTF-16'.
*     keep in utf-16, no conversion, file stored in Temse
        lv_format_cp = in_codepage.
      ELSE.
        CALL FUNCTION 'FI_PAYM_FORMAT_READ_CODEPAGE' "#EC CI_SUBRC
          EXPORTING
            i_formi    = dtfor
          IMPORTING
            e_codepage = lv_format_cp
          EXCEPTIONS ##FM_SUBRC_OK
            not_found  = 1
            OTHERS     = 2.
      ENDIF.
*  end 2832354

    ELSEIF dttyp = '01' OR dttyp = '04' OR dttyp = '06'.
      CALL FUNCTION 'FI_PAYM_FORMAT_READ_CODEPAGE' "#EC CI_SUBRC
        EXPORTING
          i_formi    = dtfor
        IMPORTING
          e_codepage = lv_format_cp
        EXCEPTIONS ##FM_SUBRC_OK
          not_found  = 1
          OTHERS     = 2.
      IF lv_format_cp IS INITIAL AND cl_abap_char_utilities=>charsize > 1 AND
          dttyp = '04'.  " XML
        lv_format_cp = '4110'.
      ENDIF.
    ENDIF.
    IF NOT lv_format_cp IS INITIAL.
      IF in_codepage = lv_format_cp.
* file aready converted to codepage assigned in the format configuration
        lv_out_codepage = in_codepage.
      ELSE.
* old file that was not converted yet: convert it now
        lv_out_codepage = lv_format_cp.
      ENDIF.
    ELSE.
*   no codepage in customizing specified
      GET PARAMETER ID 'DCP' FIELD lv_user_codepage.

* begin 2199778
*   if lv_user_codepage is initial.
*      lv_out_codepage = '1100'.
*    elseif lv_user_codepage = 'NONE'.
*      lv_out_codepage = in_codepage.
*    else.
*      lv_out_codepage = lv_user_codepage.
*    endif.
      IF cl_abap_char_utilities=>charsize > 1.
*     unicode system
        IF ( in_codepage = '4102' OR in_codepage = '4103' ).
*     unicode / system codepage
          IF lv_user_codepage IS INITIAL.
            lv_out_codepage = '1100'.
          ELSEIF lv_user_codepage = 'NONE'.
            lv_out_codepage = in_codepage.
          ELSE.
            lv_out_codepage = lv_user_codepage.
          ENDIF.
        ELSE.
*       ignore DCP parameter, file is already in specific codepage (regut-codepage)
          lv_out_codepage = in_codepage.
        ENDIF.
      ELSE.
*     non unicode system
        IF lv_user_codepage IS INITIAL.
          lv_out_codepage = '1100'.
        ELSEIF lv_user_codepage = 'NONE'.
          lv_out_codepage = in_codepage.
        ELSE.
          lv_out_codepage = lv_user_codepage.
        ENDIF.
      ENDIF.
    ENDIF.
* end 2199778

    IF in_codepage <> lv_out_codepage.
      PERFORM convert_data TABLES tab_xfile
                           USING  in_codepage
                                  lv_out_codepage.
    ENDIF.

*-Write data into lt_tab_x (standard table of ty_linetype)

    DATA lv_rest TYPE i.
    lv_rest = 0. lv_filesize = 0. lv_length = 0.
    DATA ls_rest LIKE ls_xfile.
    LOOP AT tab_xfile INTO ls_xfile.               "#EC CI_LOOP_INTO_WA
      IF lv_rest <> 0.
        CONCATENATE ls_rest(lv_rest) ls_xfile
               INTO ls_xfile IN BYTE MODE.
      ENDIF.
* concatenate
      lv_rest = xstrlen( ls_xfile ).
      lv_length = lv_length + lv_rest.
      WHILE lv_rest >= lc_linesize.                "#EC CI_NESTED " 128
        lv_x = ls_xfile(lc_linesize).
        APPEND lv_x TO lt_tab_x.
        lv_filesize = lv_filesize + lc_linesize.
        SHIFT ls_xfile BY lc_linesize PLACES IN BYTE MODE.
        lv_rest = lv_rest - lc_linesize.
      ENDWHILE.
      ls_rest = ls_xfile.
*  this condition is now always true:
*    rest_ = 0 or lv_rest < lc_linesize  and
*    contents in ls_rest(lv_rest).
    ENDLOOP.
    IF lv_rest > 0.
      lv_x = ls_xfile(lv_rest).
      lv_filesize = lv_filesize + lv_rest.
      APPEND lv_x TO lt_tab_x.
    ENDIF.

* gui_download of lt_tab_x

    lv_filename_string = filename.

    DATA: lv_text TYPE string.
    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = lv_length
      IMPORTING
        text_buffer  = lv_text
      TABLES
        binary_tab   = lt_tab_x
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc = 0.
      OPEN DATASET lv_filename_string FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc NE 0.
        result = 91.
      ELSE.
        TRANSFER lv_text TO lv_filename_string NO END OF LINE.
        CLOSE DATASET lv_filename_string.
      ENDIF.
    ELSE.
      result = 51.
    ENDIF.

*    CALL METHOD cl_gui_frontend_services=>gui_download
*      EXPORTING
*        bin_filesize         = lv_filesize
*        filename          = lv_filename_string
*        filetype             = 'BIN'
*        no_auth_check        = 'X'
*      IMPORTING
*        filelength           = lv_after_download_length
*      CHANGING
*        data_tab             = lt_tab_x
*      EXCEPTIONS
*        file_not_found       = 91
*        file_write_error     = 92
*        filesize_not_allowed = 93
*        invalid_type         = 95
*        no_batch             = 96
*        OTHERS               = 97.
*    hlp_file_exit = filename.
*    IF sy-subrc NE 0.                              "download error
*      result = sy-subrc.
*    ELSEIF lv_after_download_length EQ 0.             "nothing written
*      result = 97.
*    ENDIF.


    "Retorna sem executar o processo para download local,
    "pois o processo já foi feito para Servidor
    EXIT.
  ENDIF.
