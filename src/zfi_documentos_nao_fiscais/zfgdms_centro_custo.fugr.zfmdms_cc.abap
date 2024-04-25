FUNCTION zfmdms_cc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(CHARACT_NO) TYPE  CABN-ATINN
*"     REFERENCE(CHARACT) TYPE  CABN-ATNAM
*"     REFERENCE(VALUE) TYPE  CAWN-ATWRT
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
  DATA lv_kostl TYPE kostl.

  lv_kostl = |{ value ALPHA = IN }| .

  SELECT kostl
    FROM csks "#EC CI_GENBUFF
    BYPASSING BUFFER
    INTO TABLE @DATA(lt_value)
    WHERE kostl = @lv_kostl.

  READ TABLE lt_value ASSIGNING FIELD-SYMBOL(<fs_value>) INDEX 1.

  IF lt_value IS INITIAL.

    RAISE not_found.

  ENDIF.

ENDFUNCTION.
