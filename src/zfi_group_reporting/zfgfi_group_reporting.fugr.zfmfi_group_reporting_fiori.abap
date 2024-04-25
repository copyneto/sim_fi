FUNCTION zfmfi_group_reporting_fiori.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"  TABLES
*"      IT_GROUP TYPE  ZCTFI_ACDOCU
*"----------------------------------------------------------------------
  DATA:
    lt_message             TYPE fc05_t_message,
    lt_journalentry_header TYPE fc05_t_pdoc_head,
    lt_acdocu_remoto       TYPE zctfi_acdocu.

  CHECK it_group[] IS NOT INITIAL.

  LOOP AT it_group ASSIGNING FIELD-SYMBOL(<fs_group>).
    APPEND CORRESPONDING #( <fs_group> ) TO lt_acdocu_remoto.
  ENDLOOP.

  SELECT * FROM acdocu
     FOR ALL ENTRIES IN @it_group
       WHERE  rldnr  = @it_group-rldnr
        AND  rdimen = @it_group-rdimen
        AND  ryear = @it_group-Ryear
        AND   rvers  = @it_group-rvers
        AND poper = @it_group-poper
        AND   ritclg   = @it_group-ritclg
        AND   rbunit  = @it_group-rbunit
        AND rvsdocnr = ''
        AND orndocnr = ''
       INTO  TABLE @DATA(lt_acdocu).

  DATA(lo_group) = NEW zclfi_group_reporting(  iv_rldnr  = <fs_group>-rldnr
                                                                               iv_rdimen = <fs_group>-rdimen
                                                                               iv_gjahr = <fs_group>-ryear
                                                                               iv_rvers  = <fs_group>-rvers
                                                                               iv_poper = <fs_group>-poper
                                                                               iv_ritclg   = <fs_group>-ritclg
                                                                               iv_rbunit  = <fs_group>-rbunit )."->get_instance(  ).

  IF lt_acdocu IS NOT INITIAL.

    lt_message =  lo_group->call_fm_post( EXPORTING it_message = lo_group->call_fm_analyze( CORRESPONDING #( lt_acdocu )  ) iv_estorno =  abap_true  it_acdocu = CORRESPONDING #( lt_acdocu )  ).
    IF  lt_message IS INITIAL.
      lt_message =   lo_group->call_fm_post( EXPORTING it_message = lo_group->call_fm_analyze(  lt_acdocu_remoto  )  it_acdocu = CORRESPONDING #( lt_acdocu_remoto  )  ).
    ENDIF.
  ELSE.

    lt_message =   lo_group->call_fm_post( EXPORTING it_message = lo_group->call_fm_analyze( lt_acdocu_remoto  )  it_acdocu = CORRESPONDING #( lt_acdocu_remoto  )  ).

  ENDIF.

  IF lt_message IS NOT INITIAL.

    LOOP AT lt_message ASSIGNING FIELD-SYMBOL(<fs_message>).
      APPEND VALUE #( type = <fs_message>-msgty id = <fs_message>-msgid  number = <fs_message>-msgno ) TO et_return.
    ENDLOOP.

  ENDIF.

ENDFUNCTION.
