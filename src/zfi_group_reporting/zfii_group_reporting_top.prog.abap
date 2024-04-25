*&---------------------------------------------------------------------*
*& Include zfii_group_reporting_top
*&---------------------------------------------------------------------*

TABLES: acdocu.

SELECTION-SCREEN BEGIN OF BLOCK text WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_rldnr  TYPE acdocu-rldnr OBLIGATORY,
    p_rdimen TYPE acdocu-rdimen OBLIGATORY,
    p_gjahr  TYPE acdocu-ryear OBLIGATORY,
    p_rvers  TYPE acdocu-rvers OBLIGATORY,
    p_poper  TYPE acdocu-poper OBLIGATORY,
    p_ritclg TYPE acdocu-ritclg OBLIGATORY,
    p_rbunit TYPE acdocu-rbunit OBLIGATORY.
SELECTION-SCREEN END OF BLOCK text.

DATA:
  go_table  TYPE REF TO cl_salv_table,
  gt_acdocu TYPE zctfi_acdocu.

CONSTANTS: BEGIN OF gc_values,
             e TYPE char1 VALUE 'E',
             i TYPE char1 VALUE 'i',
           END OF gc_values.
