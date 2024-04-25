FUNCTION zdmee_exit_barcod_itau.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_TREE_TYPE) TYPE  DMEE_TREETYPE
*"     VALUE(I_TREE_ID) TYPE  DMEE_TREEID
*"     VALUE(I_ITEM)
*"     VALUE(I_PARAM)
*"     VALUE(I_UPARAM)
*"  EXPORTING
*"     REFERENCE(O_VALUE)
*"     REFERENCE(C_VALUE)
*"     REFERENCE(N_VALUE)
*"     REFERENCE(P_VALUE)
*"  TABLES
*"      I_TAB
*"----------------------------------------------------------------------

* Template function module --------------------------------------------*

  BREAK-POINT.
  BREAK lrocha.
  BREAK rfsousa.

  TYPES: BEGIN OF ty_select,
           line(200) TYPE c,
         END OF ty_select.

  DATA: t_where  TYPE TABLE OF ty_select WITH HEADER LINE.
  DATA: lv_where  TYPE string.
  DATA: where_clause TYPE  string,
        lv_campo(40),
        and(4).
  and = ' AND'.
  lv_campo = 'i_item-fpayp-doc2r(4)'.
  CONCATENATE where_clause     ' BUKRS = '  SPACE SPACE LV_CAMPO SPACE
  INTO where_clause  SEPARATED BY SPACE.
  lv_campo = 'i_item-fpayp-doc2r+4(10)'.
  CONCATENATE where_clause and ' BELNR = ' SPACE SPACE LV_CAMPO SPACE
      INTO where_clause SEPARATED BY SPACE.
  lv_campo = 'i_item-fpayp-doc2r+14(4)'.
  CONCATENATE where_clause and ' GJAHR = ' SPACE SPACE  LV_CAMPO SPACE
      INTO where_clause SEPARATED BY SPACE.
  lv_campo = 'i_item-fpayp-doc2r+18(3)'.
  CONCATENATE where_clause and ' BUZEI = ' SPACE SPACE LV_CAMPO SPACE
      INTO where_clause SEPARATED BY SPACE.


  lv_where = 'bukrs EQ i_item-fpayp-doc2r(4) AND belnr EQ i_item-fpayp-doc2r+4(10) AND gjahr EQ i_item-fpayp-doc2r+14(4) AND buzei EQ i_item-fpayp-doc2r+18(3)'.
* Select Totalmente Dinâmico! :)
*---------------------------------
  DATA: lv_glo_ref1 TYPE bseg-glo_ref1.
  DATA: ls_bseg TYPE bseg .
  TRY.
      SELECT SINGLE * FROM bseg INTO ls_bseg
       WHERE (where_clause).

      IF sy-subrc EQ 0 AND ls_bseg IS NOT INITIAL.

        c_value = ls_bseg-glo_ref1.
        o_value = ls_bseg-glo_ref1.
        n_value = ls_bseg-glo_ref1.

      ENDIF.
    CATCH cx_sy_dynamic_osql_error.
      MESSAGE `Wrong WHERE condition!` TYPE 'I'.
  ENDTRY.

ENDFUNCTION.

*  SELECT SINGLE * FROM bseg INTO @DATA(ls_bseg)
*    WHERE bukrs EQ @i_item-fpayp-doc2r(4)
*      AND belnr EQ @i_item-fpayp-doc2r+4(10)
*      AND gjahr EQ @i_item-fpayp-doc2r+14(4)
*      AND buzei EQ @i_item-fpayp-doc2r+18(3).
