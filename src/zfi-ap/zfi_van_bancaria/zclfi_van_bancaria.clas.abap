CLASS zclfi_van_bancaria DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_return_rem_pgto    TYPE TABLE OF zc_fi_van_ban_remessa WITH DEFAULT KEY .
    TYPES:
      ty_return_ret_extrato TYPE TABLE OF zc_fi_van_ban_retorno WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF gc_dir,
        pagamentos  TYPE ze_param_chave1 VALUE 'PAGAMENTOS',
        extratos    TYPE ze_param_chave1 VALUE 'EXTRATOS',
        extrat_erro TYPE ze_param_chave1 VALUE 'EXTRATOS_ERRO',
        extrat_bkp  TYPE ze_param_chave1 VALUE 'EXTRATOS_BACKUP',
        server      TYPE ze_param_chave3 VALUE 'SERVER',
      END OF gc_dir .

    CONSTANTS gc_fi          TYPE ze_param_modulo  VALUE 'FI' ##NO_TEXT.
    CONSTANTS gc_van_bancaria TYPE ze_param_chave1 VALUE 'VAN_BANCARIA' ##NO_TEXT.
    CONSTANTS gc_entrada      TYPE ze_param_chave1 VALUE 'ENTRADA' ##NO_TEXT.
    CONSTANTS gc_destination  TYPE ze_param_chave1 VALUE 'DESTINATION' ##NO_TEXT.
    CONSTANTS gc_cnpj         TYPE ze_param_chave1 VALUE 'CNPJ' ##NO_TEXT.
    CONSTANTS gc_oil          TYPE ze_param_chave1 VALUE 'OIL' ##NO_TEXT.
    CONSTANTS gc_retail       TYPE ze_param_chave1 VALUE 'RETAIL' ##NO_TEXT.
    CONSTANTS gc_msgid        TYPE t100-arbgb      VALUE 'ZFI_VAN_BANCARIA' ##NO_TEXT.

    CONSTANTS:
      BEGIN OF gc_msg,
        erro TYPE msgty VALUE 'E',
        abor TYPE msgty VALUE 'A',
        succ TYPE msgty VALUE 'S',
        warn TYPE msgty VALUE 'W',
        info TYPE msgty VALUE 'I',
      END OF gc_msg .

    DATA gv_date TYPE datum .
    DATA gv_time TYPE uzeit .
    DATA gv_seqnr TYPE seqnr .
    DATA gv_dir_extratos TYPE salfile-longname .
    DATA gv_dir_extratos_erro TYPE salfile-longname .
    DATA gv_dir_extratos_backup TYPE salfile-longname .
    DATA gv_destination_oil TYPE rfcdest .
    DATA gv_destination_retail TYPE rfcdest .
    DATA gv_server TYPE btcsrvname .
    DATA gr_cnpj_oil TYPE RANGE OF char18 .
    DATA gr_cnpj_retail TYPE RANGE OF char18 .
    DATA gv_file_line TYPE string .
    DATA gv_end_of_rfc TYPE xfeld .
    DATA gt_rfc_return TYPE bapiret2_tab .
    DATA gt_return_rem_pgto TYPE ty_return_rem_pgto .
    DATA gt_retorn_ret_extrato TYPE ty_return_ret_extrato .

    CLASS-METHODS get_joblog
      IMPORTING
        !iv_jobname      TYPE btcjob
        !iv_jobcount     TYPE btcjobcnt
      RETURNING
        VALUE(rv_joblog) TYPE rstsoname .
    METHODS constructor
      IMPORTING
        !it_return_rem_pgto    TYPE ty_return_rem_pgto OPTIONAL
        !it_return_ret_extrato TYPE ty_return_ret_extrato OPTIONAL
        !io_parametros         TYPE REF TO zclca_tabela_parametros OPTIONAL .
    METHODS processa_ret_extrato
      IMPORTING
        !iv_variant      TYPE variant
      RETURNING
        VALUE(rt_return) LIKE gt_retorn_ret_extrato .
    METHODS processa_ret_ext_int
      IMPORTING
        !iv_variant      TYPE variant
      RETURNING
        VALUE(rt_return) LIKE gt_retorn_ret_extrato .
    METHODS processa_remessa_pgto
      IMPORTING
        !ir_laufd        TYPE hrdepbsnvt_range_laufd
      RETURNING
        VALUE(rt_return) LIKE gt_return_rem_pgto .
    METHODS altera_dir_download
      IMPORTING
        VALUE(iv_laufd) TYPE laufd
        VALUE(iv_laufi) TYPE laufi
      RETURNING
        VALUE(rv_dwnam) TYPE donam .
    METHODS rfc_result
      IMPORTING
        !p_task TYPE any .
    METHODS processa_ret_job
      IMPORTING
        !iv_file    TYPE febauszf
        !iv_variant TYPE variant
      EXPORTING
        !et_return  TYPE bapiret2_tab .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: go_parametros TYPE REF TO zclca_tabela_parametros.

    METHODS move_file_to_sent_dir IMPORTING iv_variant       TYPE variant
                                            iv_file          TYPE febauszf
                                            iv_bukrs         TYPE bukrs
                                            iv_name          TYPE salfldir-name
                                  RETURNING VALUE(rt_return) TYPE ty_return_ret_extrato.
    METHODS move_file_to_error_dir IMPORTING iv_variant       TYPE variant
                                             iv_file          TYPE febauszf
                                             iv_bukrs         TYPE bukrs
                                             iv_name          TYPE pfeflname
                                   RETURNING VALUE(rt_return) TYPE ty_return_ret_extrato.
    METHODS formata_cnpj IMPORTING iv_cnpj        TYPE char18
                         RETURNING VALUE(rv_cnpj) TYPE char18.
    METHODS get_parameters_ret_extrato RETURNING VALUE(rv_result) TYPE syst_msgno.
    METHODS format_rem_pag_message IMPORTING iv_message       TYPE string
                                   RETURNING VALUE(rs_return) TYPE zc_fi_van_ban_remessa.
    METHODS format_ret_ext_message IMPORTING iv_variant       TYPE variant
                                             iv_file          TYPE febauszf
                                             iv_bukrs         TYPE bukrs
                                             iv_message       TYPE string
                                   RETURNING VALUE(rs_return) TYPE zc_fi_van_ban_retorno.

    METHODS add_bapiret2_to_return IMPORTING iv_variant       TYPE variant
                                             iv_file          TYPE febauszf
                                             iv_bukrs         TYPE bukrs
                                   EXPORTING ev_error         TYPE xfeld
                                   RETURNING VALUE(rt_return) TYPE ty_return_ret_extrato.
    METHODS get_destination IMPORTING iv_cnpj               TYPE string
                            RETURNING VALUE(rv_destination) TYPE rfcdest.
    METHODS format_dir IMPORTING iv_dir   TYPE any
                       EXPORTING ev_msgv1 TYPE msgv1
                                 ev_msgv2 TYPE msgv2.

ENDCLASS.



CLASS zclfi_van_bancaria IMPLEMENTATION.


  METHOD constructor.
    go_parametros = zclca_tabela_parametros=>get_instance( ).
  ENDMETHOD.


  METHOD processa_remessa_pgto.

    DATA lv_dir_van_outbox TYPE string.
    TRY.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi
                                               iv_chave1 = gc_van_bancaria
                                               iv_chave2 = gc_entrada
                                               iv_chave3 = gc_dir-pagamentos
                                     IMPORTING ev_param  = lv_dir_van_outbox ).
      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
    ENDTRY.
    IF lv_dir_van_outbox IS INITIAL.
      MESSAGE e006 INTO DATA(lv_message).
      APPEND format_rem_pag_message( iv_message = lv_message ) TO rt_return.
      RETURN.
    ENDIF.

    SELECT COUNT( * ) FROM regut
      WHERE laufd IN @ir_laufd
        AND xvorl EQ @space
        AND dwdat IS INITIAL
        AND kadat IS INITIAL.
    IF sy-subrc NE 0.
      MESSAGE s208(fz) INTO lv_message.
      APPEND format_rem_pag_message( iv_message = lv_message ) TO rt_return.
      RETURN.
    ENDIF.

    READ TABLE ir_laufd INTO DATA(ls_laufd) INDEX 1.
    IF ls_laufd-low IS INITIAL.
      SELECT MIN( laufd ) FROM regut
        WHERE xvorl EQ @space
          AND dwdat IS INITIAL
          AND kadat IS INITIAL
        INTO @ls_laufd-low.
    ENDIF.

    IF ls_laufd-high IS INITIAL.
      SELECT MAX( laufd ) FROM regut
        WHERE xvorl EQ @space
          AND dwdat IS INITIAL
          AND kadat IS INITIAL
        INTO @ls_laufd-high.
    ENDIF.

    DATA(lv_dtlow)  = CONV char10( |{ ls_laufd-low+6(2) }.{ ls_laufd-low+4(2) }.{ ls_laufd-low(4) }| ).
    DATA(lv_dthigh) = CONV char10( |{ ls_laufd-high+6(2) }.{ ls_laufd-high+4(2) }.{ ls_laufd-high(4) }| ).

    "Import utilizado executado na include ZFII_DOWNLOAD_FILE, ZFII_DOWNLOAD_TO_PC e ZFII_GET_DOWNLOAD_NAME
    DATA(lv_van_bancaria) = abap_true.
    EXPORT lv_van_bancaria = lv_van_bancaria TO MEMORY ID 'VAN_BANCARIA'.

    DATA(lt_bdcdata) = VALUE bdcdata_tab( ( program = 'SAPMFDTA'        dynpro = '0100' dynbegin = abap_true )
                                          ( fnam    = 'BDC_OKCODE'      fval   = '=EXEC' )
                                          ( fnam    = 'TAB_LAUFD-LOW'   fval   = lv_dtlow )
                                          ( fnam    = 'TAB_LAUFD-HIGH'  fval   = lv_dthigh )
                                          ( fnam    = 'REGUT-ZBUKR'     fval   = '*' )
                                          ( fnam    = 'REGUT-BANKS'     fval   = '*' )
                                          ( fnam    = 'FDTA-XECHT'      fval   = abap_true )
                                          ( fnam    = 'FDTA-DOWNL_OHNE' fval   = abap_true )
                                          ( fnam    = 'FDTA-KOAUS_OHNE' fval   = abap_true )

                                          ( program = 'SAPMFDTA'        dynpro = '0200' dynbegin = abap_true )
                                          ( fnam    = 'BDC_OKCODE'      fval   = '=MARK' )

                                          ( program = 'SAPMFDTA'        dynpro = '0200' dynbegin = abap_true )
                                          ( fnam    = 'BDC_OKCODE'      fval   = '=DOWN' )

                                          ( program = 'SAPMFDTA'        dynpro = '0200' dynbegin = abap_true )
                                          ( fnam    = 'BDC_OKCODE'      fval   = '=BACK' )

                                          ( program = 'SAPMFDTA'        dynpro = '0100' dynbegin = abap_true )
                                          ( fnam    = 'BDC_OKCODE'      fval   = '=BACK' ) ).

    DATA(ls_options) = VALUE ctu_params( updmode = 'S' dismode = 'N' racommit = 'X' ).
    DATA(lt_bdc_msg) = VALUE tab_bdcmsgcoll( ).

    CALL TRANSACTION 'FDTA' USING lt_bdcdata
                            OPTIONS FROM ls_options
                            MESSAGES INTO lt_bdc_msg.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
      APPEND format_rem_pag_message( iv_message = lv_message ) TO rt_return.
      RETURN.
    ENDIF.

    LOOP AT lt_bdc_msg ASSIGNING FIELD-SYMBOL(<fs_bdc_msg>).
      IF ( <fs_bdc_msg>-msgid EQ 'FZ' AND <fs_bdc_msg>-msgnr EQ '234' )
      OR ( <fs_bdc_msg>-msgid EQ 'FZ' AND <fs_bdc_msg>-msgnr EQ '236' ).
        <fs_bdc_msg>-msgtyp = gc_msg-succ.
      ELSE.
        <fs_bdc_msg>-msgtyp = gc_msg-erro.
      ENDIF.

      MESSAGE ID <fs_bdc_msg>-msgid TYPE <fs_bdc_msg>-msgtyp NUMBER <fs_bdc_msg>-msgnr
        WITH <fs_bdc_msg>-msgv1 <fs_bdc_msg>-msgv2 <fs_bdc_msg>-msgv3 <fs_bdc_msg>-msgv4
        INTO lv_message.

      APPEND format_rem_pag_message( iv_message = lv_message ) TO rt_return.
    ENDLOOP.

*    IF rt_return IS INITIAL.
*      MESSAGE s234(fz) INTO lv_message.
*      APPEND format_rem_pag_message( iv_message = lv_message ) TO rt_return.
*    ENDIF.

  ENDMETHOD.


  METHOD processa_ret_extrato.
    DATA: lt_log_retorno TYPE TABLE OF ztfi_log_retorno.

    rt_return = processa_ret_ext_int( iv_variant ).

    SORT rt_return BY data time files bukrs msgty msgid msgno.
    DELETE ADJACENT DUPLICATES FROM rt_return
      COMPARING data time files bukrs msgty msgid msgno.

    SORT rt_return BY data time files bukrs seqnr.

    LOOP AT rt_return ASSIGNING FIELD-SYMBOL(<fs_key>)
      GROUP BY ( data  = <fs_key>-data
                 time  = <fs_key>-time
                 files = <fs_key>-files
                 bukrs = <fs_key>-bukrs )
      ASSIGNING FIELD-SYMBOL(<fs_group>).

      DATA(lv_seqnr) = CONV seqnr( 0 ).

      LOOP AT GROUP <fs_group> ASSIGNING FIELD-SYMBOL(<fs_return>).
        APPEND VALUE #( data      = <fs_return>-data
                        hora      = <fs_return>-time
                        arquivo   = <fs_return>-files
                        seqnr     = lv_seqnr
                        bukrs     = <fs_return>-bukrs
                        usuario   = sy-uname
                        msgty     = <fs_return>-msgty
                        msgid     = <fs_return>-msgid
                        msgno     = <fs_return>-msgno
                        mensagem  = <fs_return>-message ) TO lt_log_retorno.
        ADD 1 TO lv_seqnr.
      ENDLOOP.
    ENDLOOP.

    IF lt_log_retorno IS NOT INITIAL.
      MODIFY ztfi_log_retorno FROM TABLE lt_log_retorno.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD processa_ret_ext_int.

    DATA:
      lt_file        TYPE TABLE OF eps2fili,
      lv_count       TYPE i,
      lv_error       TYPE xfeld,
      lv_rfc_message TYPE bapi_msg,
      lv_msgv1       TYPE msgv1,
      lv_msgv2       TYPE msgv2.

    gv_date  = sy-datum.
    gv_time  = sy-uzeit.
    gv_seqnr = 0.

    DATA(lv_number) = get_parameters_ret_extrato( ).

    IF lv_number IS NOT INITIAL.
      MESSAGE ID gc_msgid TYPE gc_msg-erro NUMBER lv_number INTO DATA(lv_message).
      APPEND format_ret_ext_message( iv_variant = iv_variant
                                     iv_file    = space
                                     iv_bukrs   = space
                                     iv_message = lv_message ) TO rt_return.
      RETURN.
    ENDIF.

    LOOP AT gr_cnpj_oil ASSIGNING FIELD-SYMBOL(<fs_cnpj_oil>).
      <fs_cnpj_oil>-low    = formata_cnpj( <fs_cnpj_oil>-low ).
    ENDLOOP.

    LOOP AT gr_cnpj_retail ASSIGNING FIELD-SYMBOL(<fs_cnpj_retail>).
      <fs_cnpj_retail>-low = formata_cnpj( <fs_cnpj_retail>-low ).
    ENDLOOP.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = gv_dir_extratos
      TABLES
        dir_list               = lt_file
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.

    IF sy-subrc NE 0.
      format_dir( EXPORTING iv_dir   = gv_dir_extratos
                  IMPORTING ev_msgv1 = lv_msgv1
                            ev_msgv2 = lv_msgv2 ).
      MESSAGE e005 WITH lv_msgv1 lv_msgv2 INTO lv_message.
      APPEND format_ret_ext_message( iv_variant = iv_variant
                                     iv_file    = space
                                     iv_bukrs   = space
                                     iv_message = lv_message ) TO rt_return.
      RETURN.
    ENDIF.

    SELECT bukrs,
           paval
      FROM t001z
      INTO TABLE @DATA(lt_t001z)
      WHERE party = 'J_1BCG'.
    IF sy-subrc EQ 0.
      SORT lt_t001z BY paval.
    ENDIF.

    LOOP AT lt_file ASSIGNING FIELD-SYMBOL(<fs_file>).
      CHECK <fs_file>-name NE '.'
        AND <fs_file>-name NE '..'
        AND <fs_file>-size GT 0.

      gv_date  = sy-datum.
      gv_time  = sy-uzeit.
      gv_seqnr = 0.

      DATA(lv_file) = CONV febauszf( |{ gv_dir_extratos }{ <fs_file>-name }| ).
      MESSAGE s002 WITH lv_file.

      CLEAR gv_file_line.

      OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING NON-UNICODE
      IGNORING CONVERSION ERRORS.
      IF sy-subrc NE 0.
        MESSAGE e014 WITH <fs_file>-name INTO lv_message.
        APPEND format_ret_ext_message( iv_variant = iv_variant
                                       iv_file    = CONV #( <fs_file>-name )
                                       iv_bukrs   = space
                                       iv_message = lv_message ) TO rt_return.
        CONTINUE.
      ENDIF.

      READ DATASET lv_file INTO gv_file_line.
      CLOSE DATASET lv_file.
      IF sy-subrc <> 0.
        DO 5 TIMES.
          WAIT UP TO 1 SECONDS.
          CLOSE DATASET lv_file.
          IF sy-subrc = 0.
            DATA(lv_closed_file) = abap_true.
            EXIT.
          ENDIF.
        ENDDO.
        IF lv_closed_file IS INITIAL.
          MESSAGE e024 WITH <fs_file>-name INTO lv_message.
          APPEND format_ret_ext_message( iv_variant = iv_variant
                                         iv_file    = CONV #( <fs_file>-name )
                                         iv_bukrs   = space
                                         iv_message = lv_message ) TO rt_return.
          CONTINUE.
        ENDIF.
      ENDIF.

      DATA(lv_cnpj) = gv_file_line+18(8).

      READ TABLE lt_t001z INTO DATA(ls_t001z)
        WITH KEY paval = lv_cnpj BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_t001z.
      ENDIF.

      CLEAR: gv_end_of_rfc,
             gt_rfc_return,
             lv_rfc_message.

      processa_ret_job( EXPORTING iv_file    =  lv_file       " File dump
                                  iv_variant =  iv_variant
                        IMPORTING et_return  =  gt_rfc_return ).

      CLEAR lv_error.
      APPEND LINES OF add_bapiret2_to_return( EXPORTING iv_variant = iv_variant
                                                        iv_file    = CONV #( <fs_file>-name )
                                                        iv_bukrs   = ls_t001z-bukrs
                                              IMPORTING ev_error   = lv_error  ) TO rt_return.
      CASE lv_error.
        WHEN gc_msg-abor.
          CONTINUE.
        WHEN abap_true.
          APPEND LINES OF move_file_to_error_dir(
            EXPORTING iv_variant = iv_variant
                      iv_file    = lv_file
                      iv_bukrs   = ls_t001z-bukrs
                      iv_name    = CONV #( <fs_file> ) ) TO rt_return.

        WHEN OTHERS.
          APPEND LINES OF move_file_to_sent_dir(
            EXPORTING iv_variant = iv_variant
                      iv_file    = lv_file
                      iv_bukrs   = ls_t001z-bukrs
                      iv_name    = CONV #( <fs_file> ) ) TO rt_return.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD rfc_result.

    DATA: lv_rfc_message TYPE bapi_msg.

    RECEIVE RESULTS FROM FUNCTION 'ZFMFI_EXEC_RETORNO_EXTRATO'
      IMPORTING
        et_return             = gt_rfc_return
      EXCEPTIONS
        communication_failure = 1 MESSAGE lv_rfc_message
        system_failure        = 2 MESSAGE lv_rfc_message.
    IF sy-subrc NE 0.
      MESSAGE a018 WITH lv_rfc_message INTO DATA(lv_message).
      APPEND VALUE #( type       = sy-msgty
                      id         = sy-msgid
                      number     = sy-msgno
                      message_v1 = sy-msgv1 ) TO gt_rfc_return.
    ENDIF.
    gv_end_of_rfc = abap_true.

  ENDMETHOD.


  METHOD move_file_to_sent_dir.

    DATA: lv_newline TYPE string,
          lv_msgv1   TYPE msgv1,
          lv_msgv2   TYPE msgv2.

    DATA(lv_backup_dir) = CONV febauszf( |{ gv_dir_extratos_backup }{ iv_name }| ).

    OPEN DATASET iv_file FOR INPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      MESSAGE e004 WITH iv_name INTO DATA(lv_message).
      APPEND format_ret_ext_message( iv_variant = iv_variant
                                     iv_file    = CONV #( iv_name )
                                     iv_bukrs   = iv_bukrs
                                     iv_message = lv_message ) TO rt_return.
      RETURN.
    ENDIF.

    OPEN DATASET lv_backup_dir FOR OUTPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      MESSAGE e003 WITH iv_name INTO lv_message.
      APPEND format_ret_ext_message( iv_variant = iv_variant
                                     iv_file    = CONV #( iv_name )
                                     iv_bukrs   = iv_bukrs
                                     iv_message = lv_message ) TO rt_return.
      CLOSE DATASET iv_file.
      RETURN.
    ENDIF.

    DO.
      READ DATASET iv_file INTO lv_newline.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      TRANSFER lv_newline  TO lv_backup_dir.
    ENDDO.

    CLOSE DATASET lv_backup_dir.
    CLOSE DATASET iv_file.
    DELETE DATASET  iv_file.

    format_dir( EXPORTING iv_dir   = gv_dir_extratos_backup
                IMPORTING ev_msgv1 = lv_msgv1
                          ev_msgv2 = lv_msgv2 ).

    MESSAGE s015 WITH lv_msgv1 lv_msgv2 INTO lv_message.
    APPEND format_ret_ext_message( iv_variant = iv_variant
                                   iv_file    = CONV #( iv_name )
                                   iv_bukrs   = iv_bukrs
                                   iv_message = lv_message ) TO rt_return.
  ENDMETHOD.


  METHOD move_file_to_error_dir.
    DATA: lv_newline TYPE string,
          lv_msgv1   TYPE msgv1,
          lv_msgv2   TYPE msgv2.

    DATA(lv_erro_dir) = CONV febauszf( |{ gv_dir_extratos_erro }{ iv_name }| ).

    OPEN DATASET iv_file FOR INPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      MESSAGE e004 WITH iv_name INTO DATA(lv_message).
      APPEND format_ret_ext_message( iv_variant = iv_variant
                                     iv_file    = CONV #( iv_name )
                                     iv_bukrs   = iv_bukrs
                                     iv_message = lv_message ) TO rt_return.
      RETURN.
    ENDIF.

    OPEN DATASET lv_erro_dir FOR OUTPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      MESSAGE e022 WITH iv_name INTO lv_message.
      APPEND format_ret_ext_message( iv_variant = iv_variant
                                     iv_file    = CONV #( iv_name )
                                     iv_bukrs   = iv_bukrs
                                     iv_message = lv_message ) TO rt_return.
      CLOSE DATASET iv_file.
      RETURN.
    ENDIF.

    DO.
      READ DATASET iv_file INTO lv_newline.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      TRANSFER lv_newline  TO lv_erro_dir.
    ENDDO.

    CLOSE DATASET lv_erro_dir.
    CLOSE DATASET iv_file.
    DELETE DATASET  iv_file.

    format_dir( EXPORTING iv_dir   = gv_dir_extratos_erro
                IMPORTING ev_msgv1 = lv_msgv1
                          ev_msgv2 = lv_msgv2 ).

    MESSAGE s016 WITH lv_msgv1 lv_msgv2 INTO lv_message.
    APPEND format_ret_ext_message( iv_variant = iv_variant
                                   iv_file    = CONV #( iv_name )
                                   iv_bukrs   = iv_bukrs
                                   iv_message = lv_message ) TO rt_return.
  ENDMETHOD.


  METHOD altera_dir_download.
    DATA lv_dir_van_outbox TYPE donam.

    TRY.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi
                                               iv_chave1 = gc_van_bancaria
                                               iv_chave2 = gc_entrada
                                               iv_chave3 = gc_dir-pagamentos
                                     IMPORTING ev_param  = lv_dir_van_outbox ).
      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
    ENDTRY.

    rv_dwnam = |{ lv_dir_van_outbox }{ iv_laufd }_{ iv_laufi }.txt|.

  ENDMETHOD.


  METHOD formata_cnpj.
    rv_cnpj = iv_cnpj.
    TRANSLATE rv_cnpj USING '. '.
    TRANSLATE rv_cnpj USING '/ '.
    TRANSLATE rv_cnpj USING '- '.
    CONDENSE rv_cnpj NO-GAPS.
  ENDMETHOD.


  METHOD get_parameters_ret_extrato.

    TRY.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi
                                               iv_chave1 = gc_van_bancaria
                                               iv_chave2 = gc_entrada
                                               iv_chave3 = gc_dir-server
                                     IMPORTING ev_param  = gv_server ).
      CATCH zcxca_tabela_parametros.
    ENDTRY.

    TRY.
        rv_result = 007.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi
                                               iv_chave1 = gc_van_bancaria
                                               iv_chave2 = gc_entrada
                                               iv_chave3 = gc_dir-extratos
                                     IMPORTING ev_param  = gv_dir_extratos ).
        IF gv_dir_extratos IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_fi
                                                       iv_chave1 = gc_van_bancaria
                                                       iv_chave2 = gc_entrada
                                                       iv_chave3 = gc_dir-extratos ).
        ENDIF.

        rv_result = 008.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi
                                               iv_chave1 = gc_van_bancaria
                                               iv_chave2 = gc_entrada
                                               iv_chave3 = gc_dir-extrat_erro
                                     IMPORTING ev_param  = gv_dir_extratos_erro ).
        IF gv_dir_extratos_erro IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_fi
                                                       iv_chave1 = gc_van_bancaria
                                                       iv_chave2 = gc_entrada
                                                       iv_chave3 = gc_dir-extrat_erro ).
        ENDIF.

        rv_result = 009.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi
                                               iv_chave1 = gc_van_bancaria
                                               iv_chave2 = gc_entrada
                                               iv_chave3 = gc_dir-extrat_bkp
                                     IMPORTING ev_param  = gv_dir_extratos_backup ).
        IF gv_dir_extratos_backup IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_fi
                                                       iv_chave1 = gc_van_bancaria
                                                       iv_chave2 = gc_entrada
                                                       iv_chave3 = gc_dir-extrat_bkp ).
        ENDIF.

        rv_result = 010.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi
                                               iv_chave1 = gc_van_bancaria
                                               iv_chave2 = gc_destination
                                               iv_chave3 = gc_oil
                                     IMPORTING ev_param  = gv_destination_oil ).
        IF gv_destination_oil IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_fi
                                                       iv_chave1 = gc_van_bancaria
                                                       iv_chave2 = gc_destination
                                                       iv_chave3 = gc_oil ).
        ENDIF.

        rv_result = 011.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_fi
                                               iv_chave1 = gc_van_bancaria
                                               iv_chave2 = gc_destination
                                               iv_chave3 = gc_retail
                                     IMPORTING ev_param  = gv_destination_retail ).
        IF gv_destination_retail IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_fi
                                                       iv_chave1 = gc_van_bancaria
                                                       iv_chave2 = gc_destination
                                                       iv_chave3 = gc_retail ).
        ENDIF.

        rv_result = 012.
        go_parametros->m_get_range( EXPORTING iv_modulo = gc_fi
                                              iv_chave1 = gc_van_bancaria
                                              iv_chave2 = gc_cnpj
                                              iv_chave3 = gc_oil
                                    IMPORTING et_range  = gr_cnpj_oil ).

        IF gr_cnpj_oil IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_fi
                                                       iv_chave1 = gc_van_bancaria
                                                       iv_chave2 = gc_cnpj
                                                       iv_chave3 = gc_oil ).
        ENDIF.

        rv_result = 013.
        go_parametros->m_get_range( EXPORTING iv_modulo = gc_fi
                                              iv_chave1 = gc_van_bancaria
                                              iv_chave2 = gc_cnpj
                                              iv_chave3 = gc_retail
                                    IMPORTING et_range  = gr_cnpj_retail ).
        IF gr_cnpj_retail IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_fi
                                                       iv_chave1 = gc_van_bancaria
                                                       iv_chave2 = gc_cnpj
                                                       iv_chave3 = gc_retail ).
        ENDIF.

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
        RETURN.

    ENDTRY.

    CLEAR rv_result.

  ENDMETHOD.


  METHOD format_rem_pag_message.
    rs_return = VALUE #( laufd   = sy-datum
                         data    = sy-datum
                         time    = sy-uzeit
                         msgty   = sy-msgty
                         msgid   = sy-msgid
                         msgno   = sy-msgno
                         message = iv_message ).
  ENDMETHOD.


  METHOD format_ret_ext_message.
    rs_return = VALUE #( variant = iv_variant
                         data    = gv_date
                         time    = gv_time
                         files   = iv_file
                         seqnr   = gv_seqnr
                         bukrs   = iv_bukrs
                         msgty   = sy-msgty
                         msgid   = sy-msgid
                         msgno   = sy-msgno
                         message = iv_message ).
    ADD 1 TO gv_seqnr.
  ENDMETHOD.


  METHOD add_bapiret2_to_return.
    CLEAR ev_error.

    LOOP AT gt_rfc_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      IF <fs_return>-type EQ gc_msg-erro.
        ev_error = abap_true.
      ENDIF.

      IF <fs_return>-type EQ gc_msg-abor.
        <fs_return>-type = gc_msg-erro.
        ev_error = gc_msg-abor.
      ENDIF.

      MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number
        WITH <fs_return>-message_v1 <fs_return>-message_v2
             <fs_return>-message_v3 <fs_return>-message_v4 INTO DATA(lv_message).

      APPEND format_ret_ext_message( iv_variant = iv_variant
                                     iv_file    = iv_file
                                     iv_bukrs   = iv_bukrs
                                     iv_message = lv_message ) TO rt_return.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_destination.

    DATA: lv_destination TYPE rfcdest,
          lv_client      TYPE rfcclient.

    rv_destination = COND #( WHEN iv_cnpj IN gr_cnpj_retail THEN gv_destination_retail
                             WHEN iv_cnpj IN gr_cnpj_oil    THEN gv_destination_oil
                             ELSE space ).
    IF rv_destination IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RFC_READ_R3_DESTINATION'
      EXPORTING
        destination             = rv_destination
      IMPORTING
        client                  = lv_client
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lv_client EQ sy-mandt.
      rv_destination = 'NONE'.
    ENDIF.

  ENDMETHOD.


  METHOD get_joblog.
    SELECT SINGLE FROM tbtco
      FIELDS joblog
      WHERE jobname  EQ @iv_jobname
        AND jobcount EQ @iv_jobcount
        AND status   IN ('A','F')
        AND enddate  IS NOT INITIAL
      INTO @rv_joblog.
    IF sy-subrc NE 0.
      CLEAR rv_joblog.
    ENDIF.
  ENDMETHOD.


  METHOD format_dir.
    CLEAR: ev_msgv1,
           ev_msgv2.

    ev_msgv1 = iv_dir.

    IF strlen( iv_dir ) GT 50.
      ev_msgv2 = iv_dir+50.
    ENDIF.

  ENDMETHOD.


  METHOD processa_ret_job.

    DATA: ls_mstr_print_parms TYPE pri_params,
          lv_valid            TYPE c,
          lv_jobname          TYPE btcjob VALUE 'VAN_BANCARIA',
          lv_number           TYPE btcjobcnt,
          lt_log_job          TYPE TABLE OF tbtcjoblog0.

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
                      id         = gc_msgid
                      number     = '019'
                      message_v1 = sy-uname ) TO et_return.
      EXIT.
    ENDIF.

    IF ls_mstr_print_parms-pdest = space.
      ls_mstr_print_parms-pdest = 'LOCL'.
    ENDIF.

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
                      id         = gc_msgid
                      number     = '020'
                      message_v1 = sy-uname ) TO et_return.
      EXIT.
    ENDIF.

    SUBMIT rfebka00
      WITH auszfile = iv_file
      WITH x_format = space
      USING SELECTION-SET iv_variant
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
        targetserver         = gv_server
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
                      id         = gc_msgid
                      number     = '021' ) TO et_return.
      EXIT.
    ENDIF.

    DATA(lv_tab_job) = CONV tabname16( |TBTCJOBLOG{ lv_joblog+2(1) }| ).

    TRY.
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
          RAISE EXCEPTION TYPE cx_sy_dynamic_osql_semantics.
        ENDIF.

      CATCH cx_sy_dynamic_osql_semantics.
        APPEND VALUE #( type       = 'E'
                    id         = gc_msgid
                    number     = '021' ) TO et_return.
        EXIT.
    ENDTRY.

    LOOP AT lt_log_job ASSIGNING FIELD-SYMBOL(<fs_log_job>).
      CHECK <fs_log_job>-msgid   NE '00'.

      APPEND VALUE #( type       = <fs_log_job>-msgtype
                      id         = <fs_log_job>-msgid
                      number     = <fs_log_job>-msgno
                      message_v1 = <fs_log_job>-msgv1
                      message_v2 = <fs_log_job>-msgv2
                      message_v3 = <fs_log_job>-msgv3
                      message_v4 = <fs_log_job>-msgv4 ) TO et_return.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
