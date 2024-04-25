*&---------------------------------------------------------------------*
*& Include LZFGDMS_LINK_RBKPI01
*&---------------------------------------------------------------------*
MODULE tc_link_rbkp_modify INPUT.
  gs_link_rbkp-tab_mark = gv_tab_mark.
  MODIFY gt_rbkp_dms
    FROM gs_link_rbkp
    INDEX tc_link_rbkp2-current_line.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LINK_RBKP_PAI_PUT_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE link_rbkp_pai_put_data INPUT.
  gv_ucomm = sy-ucomm.
  IF gv_ucomm = 'CV130_DELZ' OR
     gv_ucomm = 'DELZ'.
    sy-ucomm = 'ENTER'.
  ENDIF.

  PERFORM put_data.

  IF gv_ucomm = 'CV130_DELZ' OR
     gv_ucomm = 'DELZ'.
    PERFORM f_trata_ucomm_delz.
  ENDIF.
  CLEAR: gv_ucomm.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_LINK_RBKP_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_link_rbkp_check INPUT.
  IF gs_link_rbkp-belnr IS NOT INITIAL AND
     gs_link_rbkp-gjahr IS NOT INITIAL.
    SELECT SINGLE belnr
      FROM rbkp
      INTO @DATA(lv_belnr)
     WHERE belnr = @gs_link_rbkp-belnr
       AND gjahr = @gs_link_rbkp-gjahr.
    IF sy-subrc <> 0.
      MESSAGE e002(tpm_trl).
    ENDIF.
  ELSE.
    MESSAGE e115(42).
  ENDIF.
ENDMODULE.
