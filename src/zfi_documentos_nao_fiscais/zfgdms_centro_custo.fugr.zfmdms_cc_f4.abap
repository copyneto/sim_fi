FUNCTION zfmdms_cc_f4.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(CHARACT_NO) TYPE  CABN-ATINN OPTIONAL
*"     REFERENCE(CHARACT) TYPE  CABN-ATNAM OPTIONAL
*"     REFERENCE(DISPLAY) DEFAULT SPACE
*"     REFERENCE(ADDITIONAL_VALUES) TYPE  CABN-ATSON DEFAULT SPACE
*"     REFERENCE(MULTIPLE_VALUES) TYPE  RCTMV-ATLIS DEFAULT SPACE
*"     REFERENCE(LANGUAGE) TYPE  SY-LANGU DEFAULT SY-LANGU
*"     REFERENCE(DISPLAY_WITH_LANGUAGE) DEFAULT SPACE
*"  TABLES
*"      VALUES STRUCTURE  RCTVALUES
*"----------------------------------------------------------------------

  TYPES:
    BEGIN OF ty_data,
      kostl TYPE csks-kostl,
      ktext TYPE cskt-ktext,
    END OF ty_data.

  TYPES:
    BEGIN OF ty_values,
      kostl TYPE c LENGTH 30,
      ktext TYPE cskt-ktext,
    END OF ty_values.

  TYPES tt_values TYPE TABLE OF ty_values.

  DATA lt_values TYPE tt_values.
  DATA ls_values TYPE ty_values.

  DATA: gc_kostl(30) TYPE c VALUE 'KOSTL',
        gc_i(30)     TYPE c VALUE 'I',
        gc_d(30)     TYPE c VALUE 'D'.

  DATA: lt_values1 TYPE TABLE OF ty_data,
        lt_return  TYPE TABLE OF ddshretval.

  FIELD-SYMBOLS: <fs_value>    TYPE ty_data,
                 <fs_rctvalue> TYPE rctvalues.

  SELECT s~kostl, t~ktext
    FROM csks AS s
    LEFT OUTER JOIN cskt AS t ON s~kostl = t~kostl
    BYPASSING BUFFER
    INTO TABLE @lt_values1
    WHERE t~spras = @sy-langu
    AND s~kostl <> ''.

  IF sy-tcode IS INITIAL.
    LOOP AT lt_values1 ASSIGNING FIELD-SYMBOL(<fs_values1>).
      MOVE-CORRESPONDING <fs_values1> TO ls_values.
      APPEND ls_values TO lt_values.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = gc_kostl
        window_title    = TEXT-t01
        value_org       = 'S'
      TABLES
        value_tab       = lt_values
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
  ELSE.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = gc_kostl
        window_title    = TEXT-t01
        value_org       = 'S'
      TABLES
        value_tab       = lt_values1
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
  ENDIF.



  IF sy-subrc = 0.

    SORT lt_values BY kostl.

    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1.

    IF sy-subrc = 0.

      APPEND INITIAL LINE TO values ASSIGNING <fs_rctvalue>.
      <fs_rctvalue>-value = <fs_return>-fieldval.
      <fs_rctvalue>-status = gc_i.

    ENDIF.

    LOOP AT values ASSIGNING <fs_rctvalue> WHERE status <> gc_i. "#EC CI_STDSEQ

      READ TABLE lt_values WITH KEY kostl = <fs_rctvalue>-value TRANSPORTING NO FIELDS BINARY SEARCH.

      IF sy-subrc <> 0.

        <fs_rctvalue>-status = gc_d.

      ENDIF.

    ENDLOOP.

  ELSE.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFUNCTION.
