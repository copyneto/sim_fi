FUNCTION ZDME_EXIT_MDNT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_TREE_TYPE) TYPE  DMEE_TREETYPE_ABA
*"     VALUE(I_TREE_ID) TYPE  DMEE_TREEID_ABA
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

IF sy-SYSID <> 'PS4'.

  c_value = 'TS'.
  o_value = 'TS'.
  n_value = 'TS'.


ELSE.

  c_value = '00'.
  o_value = '00'.
  n_value = '00'.


ENDIF.


ENDFUNCTION.
