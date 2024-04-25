FUNCTION zfmdms_shlp_exit_docnr.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: lt_record TYPE TABLE OF p_swdocver WITH KEY documenttype docnumber,
        ls_record TYPE p_swdocver.

  IF callcontrol-step NE 'DISP'.
    EXIT.
  ENDIF.

  FREE lt_record.
  LOOP AT record_tab ASSIGNING FIELD-SYMBOL(<fs_record_line>).
    ls_record = <fs_record_line>-string.
    APPEND ls_record TO lt_record.
  ENDLOOP.

  SELECT documenttype,
         docnumber,
         documentversion,
         documentpart,
         internaldocumentstatus,
         embeddedswexternalversion,
         documentstatusname,
         documentdescription
    FROM @lt_record AS record
    WHERE documenttype  NE @space
      AND docnumber NOT IN ( SELECT  zz1_docdms_mih FROM rbkp
                               WHERE zz1_docdms_mih NE @space
                                 AND stblg          EQ @space )
    INTO TABLE @DATA(lt_result).

  FREE record_tab.
  LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).
    APPEND VALUE #( string = <fs_result> ) TO record_tab.
  ENDLOOP.

ENDFUNCTION.
