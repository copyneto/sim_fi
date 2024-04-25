FUNCTION ZDMEE_DATAJUROS.
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
DATA ls_item TYPE dmee_paym_if_type.
MOVE-CORRESPONDING i_item TO ls_item.
DATA lv_venc TYPE SYDATUM.
DATA lv_juros TYPE SYDATUM.

lv_venc = ls_item-FPAYP-REF01+00(08).

CONCATENATE lv_venc+04(04) lv_venc+02(02) lv_venc+00(02) INTO lv_venc.

CALL FUNCTION 'CALCULATE_DATE'
 EXPORTING
    DAYS              = 1
*   MONTHS            = '0'
   START_DATE        = lv_venc
 IMPORTING
   RESULT_DATE       = lv_juros.

CONCATENATE lv_juros+06(2) lv_juros+04(02) lv_juros+00(4) INTO lv_juros.

  c_value = lv_juros.
  o_value = lv_juros.
  n_value = lv_juros.

ENDFUNCTION.
