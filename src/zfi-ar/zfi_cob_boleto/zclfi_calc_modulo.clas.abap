CLASS zclfi_calc_modulo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Calcular DAC - módulo 10/11
    "! @parameter iv_modulo  | Módulo
    "! @parameter ev_dac | DAC
    "! @parameter cv_calc | Chave para calcular a DAC
    METHODS execute
      IMPORTING
        !iv_modulo   TYPE c
        !iv_mod10    TYPE xfeld OPTIONAL
        !iv_nn       TYPE xfeld OPTIONAL
        !iv_barinsul TYPE xfeld OPTIONAL
        !iv_dac      TYPE xfeld OPTIONAL
        !iv_dig1     TYPE string OPTIONAL
      EXPORTING
        !ev_dac      TYPE string
      CHANGING
        !cv_calc     TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Constante mod11
    CONSTANTS gc_modulo_11 TYPE string VALUE '98765432' ##NO_TEXT.
    CONSTANTS gc_modulo_11_banri TYPE string VALUE '765432' ##NO_TEXT.
    "! Constante mod10
    CONSTANTS gc_modulo_10 TYPE string VALUE '1212' ##NO_TEXT.
    "! Constante mod10
    CONSTANTS gc_nosso_n TYPE string VALUE '1973' ##NO_TEXT.
    "! DAC
    DATA gv_dac TYPE string .
    "! Total
    DATA gv_total TYPE i .
    "! Soma
    DATA gv_soma TYPE i .
    "! Contador 1
    DATA gv_cont TYPE i .
    "! Contador 2
    DATA gv_cont2 TYPE i .
    DATA:
      "! Divisor
      gv_div    TYPE p DECIMALS 1 .
    "! Módulo
    DATA gv_modulo TYPE string .
    "! Repetir
    DATA gv_repete TYPE i .
    "! Nro
    DATA gv_num TYPE i .
    DATA gv_mod10 TYPE xfeld .
    DATA gv_mod11 TYPE xfeld .
    DATA gv_nn TYPE xfeld .
    DATA gv_banrisul TYPE xfeld .
    DATA gv_dac_in TYPE xfeld .
    DATA gv_dig1 TYPE xfeld .
    DATA gv_recalc_soma TYPE xfeld .

    "! Obter o valor total dos campos
    "! @parameter cv_calc | Chave para calcular a DAC
    METHODS calc_soma
      CHANGING
        !cv_calc TYPE string .
    "! Dividir o valor total de acordo com o módulo
    METHODS calc_div
      CHANGING
        !cv_calc TYPE string .
    "! Dividir o valor total de acordo com o módulo
    METHODS calc_div_banrisul.
    "! Obter o DAC
    "! @parameter iv_res | Resultado
    METHODS dac
      IMPORTING
        !iv_res  TYPE c
      CHANGING
        !cv_calc TYPE string .
    "! Ordenar posições dinâmicamente
    "! @parameter iv_calc | Calculo
    METHODS recalc_modulo
      IMPORTING
        !iv_calc TYPE string.
ENDCLASS.



CLASS zclfi_calc_modulo IMPLEMENTATION.


  METHOD calc_div.

    DATA: lv_string TYPE string,
          lv_res    TYPE char2.

    CLEAR: gv_num.

    gv_num = COND #( WHEN gv_repete EQ 4 AND gv_nn EQ abap_false THEN 10
                     WHEN gv_repete EQ 4 AND gv_nn EQ abap_true  THEN 11 ELSE 11 ).

    gv_div = gv_total / gv_num.

    lv_string = gv_div.

    IF lv_string NA '.'.

      lv_res = '1'.

    ELSE.

*      DATA(lv_last) = ( strlen(  lv_string ) - 2 ).
*      lv_res = lv_string+lv_last(1).

      lv_res = gv_total MOD gv_num.


    ENDIF.

    me->dac(
      EXPORTING
        iv_res  = lv_res
      CHANGING
        cv_calc = cv_calc    ).

  ENDMETHOD.

  METHOD calc_div_banrisul.

    DATA: lv_string TYPE string,
          lv_res    TYPE char2.

    CLEAR: gv_num.

    gv_num = COND #( WHEN gv_repete EQ 4 AND gv_nn EQ abap_false THEN 10
                     WHEN gv_repete EQ 4 AND gv_nn EQ abap_true  THEN 11 ELSE 11 ).

    gv_div = gv_total / gv_num.

    lv_string = gv_div.

    IF lv_string NA '.'.

      lv_res = '1'.

    ELSE.

*      DATA(lv_last) = ( strlen(  lv_string ) - 2 ).
*      lv_res = lv_string+lv_last(1).

      lv_res = gv_total MOD gv_num.
    ENDIF.

*    me->dac( lv_res ).

  ENDMETHOD.


  METHOD calc_soma.

    DATA: lv_soma TYPE string.

    CLEAR: gv_cont, gv_cont2, gv_soma, gv_total.

    me->recalc_modulo( cv_calc ).

    gv_cont = strlen( gv_modulo ) - strlen( cv_calc ).

    DO strlen( cv_calc ) TIMES.

      gv_cont2 = sy-index - 1.

      gv_soma = gv_modulo+gv_cont(1) * cv_calc+gv_cont2(1) .

      IF gv_repete EQ 4 AND gv_nn EQ abap_false
      AND strlen( CONV char2( gv_soma ) ) GT 1.

        lv_soma = gv_soma.

        gv_soma = lv_soma(1) + lv_soma+1(1).

        IF strlen( CONV char2( gv_soma ) ) GT 1.

          lv_soma =  gv_soma.
          gv_soma =  lv_soma(1)  + lv_soma+1(1).

        ENDIF.

      ENDIF.

      gv_total = gv_total + gv_soma.

      gv_cont = gv_cont + 1.

    ENDDO.

    me->calc_div(
      CHANGING
        cv_calc = cv_calc
    ).


  ENDMETHOD.


  METHOD dac.

    DATA: lv_tamanho TYPE i,
          lv_dig     TYPE i,
          lv_codigo  TYPE char50.

    IF me->gv_mod10 IS NOT INITIAL.
      IF iv_res EQ 0 OR
         iv_res EQ 10.
        gv_dac = 0.
      ELSE.
        gv_dac = gv_num - iv_res.
      ENDIF.
    ENDIF.

    IF me->gv_mod11 IS NOT INITIAL.

      gv_dac = gv_num - iv_res.

      IF gv_banrisul = abap_true.

        "DAC Banrisul
        IF gv_dac_in = abap_true.

          IF iv_res = 0 OR
             iv_res = 1 OR
             iv_res > 9.
            gv_dac = 1.
          ENDIF.

        ELSE.

          IF iv_res = 0.
            gv_dac = 0.

          ELSE.

            lv_codigo = cv_calc.
            lv_tamanho = strlen( cv_calc ).
            lv_tamanho = lv_tamanho - 1.

            IF iv_res = 1.

              IF cv_calc+lv_tamanho(1) = 9.

                lv_codigo+lv_tamanho(1) = 0.
                gv_recalc_soma = abap_true.
                cv_calc = lv_codigo.
              ELSE.
                lv_dig = cv_calc+lv_tamanho(1).
                lv_dig = lv_dig + 1.
                lv_codigo+lv_tamanho(1) = lv_dig.
                cv_calc = lv_codigo.
                gv_recalc_soma = abap_true.
              ENDIF.
            ELSE.

            ENDIF.

          ENDIF.

        ENDIF.

      ELSE.

        IF gv_dac = 0 OR
           gv_dac = 1 OR
           gv_dac > 9.
*         gv_dac = 10 OR
*         gv_dac = 11.
          gv_dac = 1.
        ENDIF.

      ENDIF.

    ENDIF.

    IF me->gv_nn IS NOT INITIAL.
      IF iv_res = 0 OR
         iv_res = 1.

        gv_dac = 0.
      ELSE.
        gv_dac = gv_num - iv_res.
      ENDIF.
    ENDIF.

    IF strlen( CONV char2( gv_dac ) ) GT 1.
      gv_dac = gv_dac(1) + gv_dac+1(1).
    ENDIF.

  ENDMETHOD.


  METHOD execute.

    gv_mod10 = iv_mod10.
    gv_mod11 = iv_modulo.
    gv_nn    = iv_nn.
    gv_banrisul = iv_barinsul.
    gv_dac_in = iv_dac.
    gv_dig1 = iv_dig1.

    CLEAR: gv_dac, gv_repete, gv_recalc_soma.


    IF iv_modulo EQ abap_false OR iv_nn EQ abap_true.
      gv_repete = 4.

    ELSEIF gv_banrisul EQ abap_true AND iv_modulo EQ abap_true AND gv_dac_in = abap_true.
      gv_repete = 8.
    ELSEIF gv_banrisul EQ abap_true AND iv_modulo EQ abap_true AND iv_nn EQ abap_false AND gv_dac_in = abap_false.
      gv_repete = 6.
    ELSE.
      gv_repete = 8.
    ENDIF.

    me->calc_soma(
      CHANGING
        cv_calc  = cv_calc  ).

    IF gv_recalc_soma = abap_true.

      me->calc_soma(
      CHANGING
        cv_calc  = cv_calc  ).

      CLEAR:gv_recalc_soma.

    ENDIF.

    ev_dac  = gv_dac.

  ENDMETHOD.


  METHOD recalc_modulo.

    DATA: lv_calc     TYPE string,
          lv_index    TYPE i,
          lv_modulo   TYPE string,
          lv_constant TYPE string.

    lv_constant = COND #( WHEN gv_repete EQ 4 AND gv_nn EQ abap_false THEN gc_modulo_10
                          WHEN gv_repete EQ 4 AND gv_nn EQ abap_true  THEN gc_nosso_n
                          WHEN gv_repete EQ 6 AND gv_nn EQ abap_false AND gv_banrisul EQ abap_true  THEN gc_modulo_11_banri
                            ELSE  gc_modulo_11 ).

    DO  strlen( iv_calc ) TIMES.

      lv_index = lv_index + 1.

      lv_calc = gv_repete - lv_index .

      lv_modulo = lv_constant+lv_calc(1) && lv_modulo.

      IF lv_index EQ gv_repete.
        CLEAR: lv_index.
      ENDIF.

    ENDDO.

    gv_modulo = lv_modulo.

    CLEAR: lv_modulo, lv_calc, lv_index.

  ENDMETHOD.
ENDCLASS.
