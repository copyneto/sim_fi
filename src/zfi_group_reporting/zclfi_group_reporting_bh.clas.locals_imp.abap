CLASS lcl_Group DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Group RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Group RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ Group RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Group.

    METHODS analisar FOR MODIFY
      IMPORTING keys FOR ACTION Group~analisar.

    METHODS equalizar FOR MODIFY
      IMPORTING keys FOR ACTION Group~equalizar RESULT result.

    METHODS estornar FOR MODIFY
      IMPORTING keys FOR ACTION Group~estornar.

ENDCLASS.

CLASS lcl_Group IMPLEMENTATION.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.

    DATA: ls_result LIKE LINE OF result.

    CHECK keys IS NOT INITIAL.

    SELECT rldnr, rdimen, ryear, docnr, docln
    FROM zi_fi_group_reporting
    INTO TABLE @DATA(lt_data)
    FOR ALL ENTRIES IN @keys
    WHERE rldnr = @keys-Rldnr
       AND rdimen = @keys-rdimen
      AND  ryear = @keys-ryear
       AND docnr = @keys-docnr
       AND docln = @keys-docln.

    IF sy-subrc IS INITIAL.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_key_parent>).

        DATA(ls_data) = VALUE #( lt_data[  rldnr = <fs_key_parent>-%key-Rldnr
                                                                  rdimen = <fs_key_parent>-%key-rdimen
                                                                  ryear = <fs_key_parent>-%key-ryear
                                                                  docnr = <fs_key_parent>-%key-docnr
                                                                  docln = <fs_key_parent>-%key-docln ] OPTIONAL ).

        IF ls_data IS NOT INITIAL.
          APPEND VALUE #( rldnr = ls_data-Rldnr
                                        rdimen = ls_data-rdimen
                                        ryear = ls_data-ryear
                                        docnr = ls_data-docnr
                                        docln = ls_data-docln ) TO result.
        ENDIF.

      ENDLOOP.

    ENDIF.


  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD analisar.
    RETURN.
  ENDMETHOD.

  METHOD equalizar.

    CHECK keys IS NOT INITIAL.
    "   local
    SELECT *
  FROM zi_fi_group_reporting
  INTO TABLE @DATA(lt_local)
  FOR ALL ENTRIES IN @keys
  WHERE rldnr = @keys-Rldnr
     AND rdimen = @keys-rdimen
    AND  ryear = @keys-ryear
     AND docnr = @keys-docnr
     AND docln = @keys-docln.

    IF lt_local IS NOT INITIAL.

      NEW zclfi_group_reporting(
         iv_rldnr  = VALUE #( lt_local[ 1 ]-rldnr OPTIONAL )
         iv_rdimen = VALUE #( lt_local[ 1 ]-rdimen OPTIONAL )
         iv_gjahr = VALUE #( lt_local[ 1 ]-Ryear OPTIONAL )
         iv_rvers  = VALUE #( lt_local[ 1 ]-rvers OPTIONAL )
         iv_poper = VALUE #( lt_local[ 1 ]-poper OPTIONAL )
         iv_ritclg   = VALUE #( lt_local[ 1 ]-ritclg OPTIONAL )
         iv_rbunit  = VALUE #( lt_local[ 1 ]-rbunit OPTIONAL ) )->execute_fiori( EXPORTING It_acdocu = lt_local
         IMPORTING
          et_return = DATA(lt_return) ).

      IF lt_return IS NOT INITIAL.

        LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).

          APPEND VALUE #( %tky = keys[ 1 ]-%tky
                          %msg = new_message(
                                   id       = <fs_return>-id
                                   number   = <fs_return>-number
                                   severity = CONV #( <fs_return>-type )
                                   v1       = |{ <fs_return>-message_v1 }|
                                   v2       = |{ <fs_return>-message_v2 }| )  )
                 TO reported-group.


        ENDLOOP.

      ELSE.

        APPEND VALUE #( %tky = keys[ 1 ]-%tky
                        %msg = new_message(
                                 id       = 'ZFI_GROUP_REPOT'
                                 number   = 002
                                 severity = CONV #( 'S' )
*                                 v1       = |{ <fs_return>-message_v1 }|
*                                 v2       = |{ <fs_return>-message_v2 }|
                                  )  )
               TO reported-group.

      ENDIF.

    ENDIF.


  ENDMETHOD.

  METHOD estornar.
    RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_ZI_FI_GROUP_REPORTING DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_ZI_FI_GROUP_REPORTING IMPLEMENTATION.

  METHOD finalize.
    RETURN.
  ENDMETHOD.

  METHOD check_before_save.
    RETURN.
  ENDMETHOD.

  METHOD save.
    RETURN.
  ENDMETHOD.

  METHOD cleanup.
    RETURN.
  ENDMETHOD.

  METHOD cleanup_finalize.
    RETURN.
  ENDMETHOD.

ENDCLASS.
