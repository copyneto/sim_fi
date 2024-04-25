*&---------------------------------------------------------------------*
*& Include LZFGDMS_LINK_RBKPO01
*&---------------------------------------------------------------------*

MODULE tc_link_rbkp_init OUTPUT.
  IF gv_link_rbkp_copied IS INITIAL.
    PERFORM get_data.
    gv_link_rbkp_copied = 'X'.
    REFRESH CONTROL 'TC_LINK_RBKP2' FROM SCREEN '9000'.
  ENDIF.
ENDMODULE.

MODULE tc_link_rbkp_move OUTPUT.
  MOVE-CORRESPONDING gs_link_rbkp TO rbkp.
  PERFORM display_col.
ENDMODULE.

MODULE tc_link_rbkp_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_rbkp_dms LINES tc_link_rbkp2-lines.
ENDMODULE.
