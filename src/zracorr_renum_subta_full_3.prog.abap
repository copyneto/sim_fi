*&---------------------------------------------------------------------*
*& Report ZRACORR_RENUM_SUBTA_FULL_3
*&---------------------------------------------------------------------*
*& Modified correction report for note 2416765 for releases
*& 103 and higher.
*& Modification was necessary due to re-naming changes with release 103.
*&
*& Corrects duplicate SUBTA on different documents (AWTYP AWREF).
*&
*& Version 2.0
*& due to:
*&  - incorrect updates on ACDOCA due to XNEGP-logic in FAAV_LINEITEMS
*&  - if document with duplicate SUBTA is a reversal document
*&    SUBTA_REV was not corrected thoroughly in the reversed document
*&  - inital SUBTAs (e.g. settlement line items to other reveicevers - VORGN = AUZF)
*&    were filled with a new SUBTA. Initial SUBTAs should not be filled.
*&    This report corrects only duplicate SUBTAs
*&
*&---------------------------------------------------------------------*
REPORT ZRACORR_RENUM_SUBTA_FULL_3.
TABLES ANLA.

PARAMETERS:
p_bukrs TYPE bukrs OBLIGATORY,
p_gjahr TYPE gjahr OBLIGATORY.

SELECT-OPTIONS:
so_anln1  FOR anla-anln1.

PARAMETERS:
p_test  TYPE flag DEFAULT 'X'.

TYPES:
BEGIN OF ls_ty_lineitems.
  TYPES: rldnr          TYPE acdoca-rldnr,
  ldgrp          TYPE faat_doc_it-ldgrp,
  belnr          TYPE acdoca-belnr,
  docln          TYPE acdoca-docln,
  statistic_item TYPE abap_bool.
  INCLUDE TYPE faat_doc_it-item_key.
  TYPES: subta_rev      TYPE acdoca-subta_rev,
  awtyp_rev      TYPE acdoca-awtyp_rev,
  aworg_rev      TYPE acdoca-aworg_rev,
  awref_rev      TYPE acdoca-awref_rev,
  subta_old      TYPE acdoca-subta,
  subta_rev_old  TYPE acdoca-subta_rev,
  upd_success    TYPE C,
  COLOR          TYPE lvc_t_scol,
END OF ls_ty_lineitems,

BEGIN OF ls_ty_anek_key,
  bukrs TYPE bukrs,
  anln1 TYPE anln1,
  anln2 TYPE anln2,
  gjahr TYPE gjahr,
  subta TYPE acdoca-subta,
  awtyp TYPE acdoca-awtyp,
  awref TYPE acdoca-awref,
END OF ls_ty_anek_key.

DATA:
      lt_items_for_repair         TYPE TABLE OF ls_ty_lineitems,
      lt_items_one_asset          TYPE TABLE OF ls_ty_lineitems,
      lt_items_repair_update      TYPE TABLE OF ls_ty_lineitems,
      lt_items_repair_rev_update  TYPE TABLE OF ls_ty_lineitems,
      lt_items_repair_rev_doc     TYPE TABLE OF ls_ty_lineitems,
      ls_item_repair_rev          TYPE ls_ty_lineitems,
      lt_lineitems                TYPE TABLE OF ls_ty_lineitems,
      lt_output                 TYPE TABLE OF ls_ty_lineitems,

      ls_key              TYPE ls_ty_anek_key,
      lt_key              TYPE SORTED TABLE OF ls_ty_anek_key
      WITH UNIQUE KEY bukrs anln1 anln2 gjahr subta, "ANEK key for reporting!

      lv_last_awtyp       TYPE awtyp,
      lv_last_awref       TYPE awref,
      lv_last_subta       TYPE acdoca-subta,
      lv_last_ldgrp       TYPE bkpf-ldgrp,
      lv_next_subta       TYPE acdoca-subta.

CONSTANTS:
lc_initial_subta        TYPE acdoca-subta VALUE IS INITIAL,
lc_initial_afabe        TYPE acdoca-afabe VALUE IS INITIAL.

SELECT rldnr  AS rldnr,
rldnr~ldgrp AS ldgrp,
rbukrs AS bukrs,
belnr  AS belnr,
docln  AS docln,
anln1  AS anln1,
anln2  AS anln2,
gjahr  AS gjahr,
awtyp  AS awtyp,
awref  AS awref,
aworg  AS aworg,
awsys  AS awsys,
subta  AS subta,
afabe  AS afabe,
slalittype AS slalittype,
drcrk  AS drcrk,
subta_rev AS subta_rev,
awtyp_rev AS awtyp_rev,
aworg_rev AS aworg_rev,
awref_rev AS awref_rev,
' '    AS statistic_item
FROM acdoca AS acdoca
INNER JOIN faa_rep_ledger AS rldnr
ON  rldnr~bukrs = acdoca~rbukrs
AND rldnr~rep_ledger = acdoca~rldnr
AND rldnr~lead_afabe = acdoca~afabe
WHERE rbukrs = @p_bukrs
AND gjahr = @p_gjahr
AND anln1 IN @so_anln1
AND subta <> @lc_initial_subta
AND slalittype <> @if_faa_posting_constants=>gc_slalittype-misc-aci
AND awtyp <> @if_faa_posting_constants=>gc_awtyp-amdp
AND NOT (    poper  = @cl_faa_bcf_services=>gc_poper_bcf
OR movcat = '00' "@if_faa_posting_constants=>gc_classification-bcf-category
OR anbwa  = @if_faa_posting_constants=>gc_tty-bcf )
INTO CORRESPONDING FIELDS OF TABLE @lt_lineitems.

SELECT ' '    AS rldnr,
ldgrp  AS ldgrp,
bukrs  AS bukrs,
anln1  AS anln1,
anln2  AS anln2,
gjahr  AS gjahr,
awtyp  AS awtyp,
awref  AS awref,
aworg  AS aworg,
awsys  AS awsys,
subta  AS subta,
afabe  AS afabe,
slalittype AS slalittype,
drcrk  AS drcrk,
subta_rev AS subta_rev,
awtyp_rev AS awtyp_rev,
aworg_rev AS aworg_rev,
awref_rev AS awref_rev,
'X'    AS statistic_item
FROM faat_doc_it
WHERE bukrs = @p_bukrs
AND gjahr = @p_gjahr
AND anln1 IN @so_anln1
AND subta <> @lc_initial_subta
AND slalittype <> @if_faa_posting_constants=>gc_slalittype-misc-aci
AND awtyp <> @if_faa_posting_constants=>gc_awtyp-amdp
AND NOT (    poper  = @cl_faa_bcf_services=>gc_poper_bcf
OR movcat = '00' "@if_faa_posting_constants=>gc_classification-bcf
OR bwasl  = @if_faa_posting_constants=>gc_tty-bcf )
APPENDING CORRESPONDING FIELDS OF TABLE @lt_lineitems.

SORT lt_lineitems ASCENDING BY bukrs anln1 anln2 gjahr subta awtyp awref afabe slalittype drcrk.

IF lt_lineitems IS INITIAL.
  WRITE: 'no items found'.
  EXIT.
ENDIF.

LOOP AT lt_lineitems ASSIGNING FIELD-SYMBOL(<ls_item>).
  MOVE-CORRESPONDING <ls_item> TO ls_key.
  INSERT ls_key INTO TABLE lt_key.                  "check SUBTA unique on ANEK key level
  CHECK sy-subrc <> 0.
  READ TABLE lt_key INTO ls_key
  WITH KEY bukrs = <ls_item>-bukrs
  anln1 = <ls_item>-anln1
  anln2 = <ls_item>-anln2
  gjahr = <ls_item>-gjahr
  subta = <ls_item>-subta.
  "we repair only DUPRECS on different documents
  IF sy-subrc = 0 AND ( <ls_item>-awtyp <> ls_key-awtyp OR <ls_item>-awref <> ls_key-awref ).
    APPEND <ls_item> TO lt_items_for_repair.
  ENDIF.
ENDLOOP.

IF lt_items_for_repair IS INITIAL.
  WRITE: 'no items to be repaired'.
  EXIT.
ENDIF.



SORT lt_items_for_repair BY bukrs anln1 anln2 gjahr subta awtyp awref ldgrp afabe.
LOOP AT lt_items_for_repair ASSIGNING FIELD-SYMBOL(<ls_item_repair>)
GROUP BY ( key1 = <ls_item_repair>-anln1 key2 = <ls_item_repair>-anln2 ) REFERENCE INTO DATA(lr_one_asset).
  CLEAR:  lt_items_one_asset,
  lv_last_subta,
  lv_last_ldgrp,
  lv_last_awtyp,
  lv_last_awref,
  lv_next_subta,
  ls_item_repair_rev,
  lt_items_repair_rev_doc.

  lt_items_one_asset = VALUE #( FOR <line> IN GROUP lr_one_asset ( <line> ) ).
  DATA(mo_asset) = cl_faa_mdo_factory_static=>get_instance_root(
        iv_comp_code    = p_bukrs
        iv_asset_no     = lt_items_one_asset[ 1 ]-anln1
        iv_asset_subno  = lt_items_one_asset[ 1 ]-anln2
        iv_mode         = if_faa_frw_constants=>gc_mode-UPDATE ).

  SORT lt_items_one_asset BY subta awtyp awref ldgrp afabe.
  LOOP AT lt_items_one_asset ASSIGNING <ls_item_repair>.
    IF lv_last_subta <> <ls_item_repair>-subta
    OR lv_last_ldgrp <> <ls_item_repair>-ldgrp
    OR lv_last_awtyp <> <ls_item_repair>-awtyp
    OR lv_last_awref <> <ls_item_repair>-awref.
      lv_last_awtyp = <ls_item_repair>-awtyp.
      lv_last_awref = <ls_item_repair>-awref.
      lv_last_ldgrp = <ls_item_repair>-ldgrp.
      lv_last_subta = <ls_item_repair>-subta.
      lv_next_subta = mo_asset->mo_item_manager->get_next_seqno( p_gjahr ).
      CHECK <ls_item_repair>-subta NE lv_next_subta.

      "repair correspondig SUBTA_REV of reversed document
      IF <ls_item_repair>-subta_rev IS NOT INITIAL.
        CLEAR lt_items_repair_rev_doc.
        LOOP AT lt_lineitems ASSIGNING FIELD-SYMBOL(<ls_item_repair_rev>)
        WHERE bukrs = <ls_item_repair>-bukrs
        AND anln1 = <ls_item_repair>-anln1
        AND anln2 = <ls_item_repair>-anln2
        AND gjahr = <ls_item_repair>-gjahr
        AND subta = <ls_item_repair>-subta_rev
        AND awtyp = <ls_item_repair>-awtyp_rev
        AND awref = <ls_item_repair>-awref_rev
        AND aworg = <ls_item_repair>-aworg_rev.
          CLEAR ls_item_repair_rev.

          IF <ls_item_repair_rev>-subta_rev <> <ls_item_repair>-subta.
            "no correction if subta reference is not correct in the document
            CLEAR lt_items_repair_rev_doc.
            EXIT.
          ENDIF.
          MOVE-CORRESPONDING <ls_item_repair_rev> TO ls_item_repair_rev.
          ls_item_repair_rev-subta_rev_old = ls_item_repair_rev-subta_rev.
          ls_item_repair_rev-subta_rev     = lv_next_subta.
          APPEND ls_item_repair_rev TO lt_items_repair_rev_doc.
        ENDLOOP.
        IF lt_items_repair_rev_doc IS NOT INITIAL.
          APPEND LINES OF lt_items_repair_rev_doc TO lt_items_repair_rev_update.
        ENDIF.
      ENDIF.
    ENDIF.

    <ls_item_repair>-subta_old = <ls_item_repair>-subta.
    <ls_item_repair>-subta     = lv_next_subta.
  ENDLOOP.
  APPEND LINES OF lt_items_one_asset TO lt_items_repair_update.
ENDLOOP.


IF p_test IS INITIAL.
  LOOP AT lt_items_repair_update ASSIGNING <ls_item_repair>.
    IF <ls_item_repair>-statistic_item = abap_false.
      UPDATE acdoca SET subta = <ls_item_repair>-subta
      WHERE rldnr  EQ <ls_item_repair>-rldnr
      AND rbukrs EQ <ls_item_repair>-bukrs
      AND gjahr  EQ <ls_item_repair>-gjahr
      AND belnr  EQ <ls_item_repair>-belnr
      AND docln  EQ <ls_item_repair>-docln.
      IF sy-subrc = 0.
        <ls_item_repair>-upd_success = 'X'.
      ENDIF.
    ELSE.
      UPDATE faat_doc_it SET subta = <ls_item_repair>-subta
      WHERE bukrs  EQ <ls_item_repair>-bukrs
      AND anln1  EQ <ls_item_repair>-anln1
      AND anln2  EQ <ls_item_repair>-anln2
      AND gjahr  EQ <ls_item_repair>-gjahr
      AND awtyp  EQ <ls_item_repair>-awtyp
      AND awref  EQ <ls_item_repair>-awref
      AND aworg  EQ <ls_item_repair>-aworg
      AND awsys  EQ <ls_item_repair>-awsys
      AND subta  EQ <ls_item_repair>-subta_old
      AND afabe  EQ <ls_item_repair>-afabe
      AND slalittype EQ <ls_item_repair>-slalittype
      AND drcrk  EQ <ls_item_repair>-drcrk.
      IF sy-subrc = 0.
        <ls_item_repair>-upd_success = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_items_repair_rev_update IS NOT INITIAL.
    LOOP AT lt_items_repair_rev_update ASSIGNING <ls_item_repair>.
      IF <ls_item_repair>-statistic_item = abap_false.
        UPDATE acdoca SET subta_rev = <ls_item_repair>-subta_rev
        WHERE rldnr  EQ <ls_item_repair>-rldnr
        AND rbukrs EQ <ls_item_repair>-bukrs
        AND gjahr  EQ <ls_item_repair>-gjahr
        AND belnr  EQ <ls_item_repair>-belnr
        AND docln  EQ <ls_item_repair>-docln.
        IF sy-subrc = 0.
          <ls_item_repair>-upd_success = 'X'.
        ENDIF.
      ELSE.
        UPDATE faat_doc_it SET subta_rev = <ls_item_repair>-subta_rev
        WHERE bukrs  EQ <ls_item_repair>-bukrs
        AND anln1  EQ <ls_item_repair>-anln1
        AND anln2  EQ <ls_item_repair>-anln2
        AND gjahr  EQ <ls_item_repair>-gjahr
        AND awtyp  EQ <ls_item_repair>-awtyp
        AND awref  EQ <ls_item_repair>-awref
        AND aworg  EQ <ls_item_repair>-aworg
        AND awsys  EQ <ls_item_repair>-awsys
        AND subta  EQ <ls_item_repair>-subta
        AND afabe  EQ <ls_item_repair>-afabe
        AND slalittype EQ <ls_item_repair>-slalittype
        AND drcrk  EQ <ls_item_repair>-drcrk.
        IF sy-subrc = 0.
          <ls_item_repair>-upd_success = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  COMMIT WORK.
ENDIF.

*******************************************************************
* Output
*******************************************************************
DATA:
      lr_output    TYPE REF TO cl_salv_table,
      lr_columns   TYPE REF TO cl_salv_columns_table,
      lr_column    TYPE REF TO cl_salv_column,
      lr_functions TYPE REF TO cl_salv_functions_list,
      ls_color_new TYPE lvc_s_scol,
      ls_color_old TYPE lvc_s_scol.

ls_color_new-fname     = 'SUBTA'.
ls_color_new-COLOR-col = col_positive.
ls_color_old-fname     = 'SUBTA_OLD'.
ls_color_old-COLOR-col = COL_NEGATIVE.
LOOP AT lt_items_repair_update ASSIGNING <ls_item_repair>.
  APPEND ls_color_new TO <ls_item_repair>-COLOR.
  APPEND ls_color_old TO <ls_item_repair>-COLOR.
ENDLOOP.

ls_color_new-fname     = 'SUBTA_REV'.
ls_color_new-COLOR-col = col_positive.
ls_color_old-fname     = 'SUBTA_REV_OLD'.
ls_color_old-COLOR-col = COL_NEGATIVE.
LOOP AT lt_items_repair_rev_update ASSIGNING <ls_item_repair_rev>.
  APPEND ls_color_new TO <ls_item_repair_rev>-COLOR.
  APPEND ls_color_old TO <ls_item_repair_rev>-COLOR.
ENDLOOP.

APPEND LINES OF lt_items_repair_update TO lt_output.
APPEND LINES OF lt_items_repair_rev_update TO lt_output.

TRY.
  CALL METHOD cl_salv_table=>factory
  IMPORTING
    r_salv_table = lr_output
  CHANGING
    t_table      = lt_output.
CATCH cx_salv_msg.                                    "#EC NO_HANDLER
ENDTRY.

lr_functions = lr_output->get_functions( ).
lr_functions->set_default( abap_true ).
lr_functions->set_export_localfile( abap_true ).

lr_columns = lr_output->get_columns( ).

TRY.
  lr_column = lr_columns->get_column( 'MANDT' ).
  lr_column->set_visible( abap_false ).
CATCH cx_salv_not_found.                              "#EC NO_HANDLER
ENDTRY.
TRY.
  lr_column = lr_columns->get_column( 'UPD_SUCCESS' ).
  lr_column->set_short_text( 'UPDATED' ).
CATCH cx_salv_not_found.                              "#EC NO_HANDLER
ENDTRY.
TRY.
  lr_column = lr_columns->get_column( 'STATISTIC_ITEM' ).
  lr_column->set_medium_text( 'Statistic Item' ).
CATCH cx_salv_not_found.                              "#EC NO_HANDLER
ENDTRY.
TRY.
  lr_column = lr_columns->get_column( 'SUBTA' ).
  lr_column->set_short_text( 'SUBTA' ).
  lr_column->set_medium_text( 'SUBTA' ).
  lr_column->set_long_text( 'SUBTA' ).
CATCH cx_salv_not_found.                              "#EC NO_HANDLER
ENDTRY.
TRY.
  lr_column = lr_columns->get_column( 'SUBTA_REV' ).
  lr_column->set_short_text( 'SUBTA_REV' ).
  lr_column->set_medium_text( 'SUBTA_REV' ).
  lr_column->set_long_text( 'SUBTA_REV' ).
CATCH cx_salv_not_found.                              "#EC NO_HANDLER
ENDTRY.
TRY.
  lr_column = lr_columns->get_column( 'SUBTA_OLD' ).
  lr_column->set_short_text( 'Old SUBTA' ).
  lr_column->set_medium_text( 'Old SUBTA' ).
  lr_column->set_long_text( 'Old SUBTA' ).
CATCH cx_salv_not_found.                              "#EC NO_HANDLER
ENDTRY.
TRY.
  lr_column = lr_columns->get_column( 'SUBTA_REV_OLD' ).
  lr_column->set_short_text( 'Old REV' ).
  lr_column->set_medium_text( 'Old SUBTA_REV' ).
  lr_column->set_long_text( 'Old SUBTA_REV' ).
CATCH cx_salv_not_found.                              "#EC NO_HANDLER
ENDTRY.

lr_columns->set_column_position(  columnname = 'UPD_SUCCESS'
POSITION   = 1 ).
lr_columns->set_color_column('COLOR').
lr_columns->set_optimize( abap_true ).
lr_output->display( ).
