*----------------------------------------------------------------------*
***INCLUDE ZFIR026_PAI .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1010  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_1010 INPUT.

  DATA: lv_code TYPE sy-ucomm,
        lv_rfsh TYPE sy-ucomm,
        lt_rows TYPE lvc_t_roid,
        ls_rows TYPE LINE OF lvc_t_roid,
        lr_rows TYPE TABLE OF int4 WITH HEADER LINE.

  lv_code = sy-ucomm.
  lv_rfsh = '&REFRESH'.

  CALL METHOD go_alvgrid->get_selected_rows(
    IMPORTING
      et_row_no = lt_rows
  ).

  REFRESH lr_rows.
  LOOP AT lt_rows INTO ls_rows.
    CLEAR lr_rows.
    lr_rows = ls_rows-row_id.
    APPEND lr_rows.
  ENDLOOP.

  CALL METHOD go_alvgrid->set_function_code
    CHANGING
      c_ucomm = lv_rfsh.

  CASE lv_code.

    WHEN '&AUTOR'.
      PERFORM f_atualizacao.

    WHEN '&DELETE'.
      PERFORM f_eliminacao TABLES lr_rows.

    WHEN '&CALCTAX'.
      PERFORM f_calc_taxa.

    WHEN OTHERS.
      CALL METHOD go_alvgrid->set_function_code
        CHANGING
          c_ucomm = lv_code.

  ENDCASE.

  CLEAR lv_code.
  CLEAR sy-ucomm.

ENDMODULE.                 " USER_COMMAND_1010  INPUT


*&---------------------------------------------------------------------*
*&      Module  EXIT_1010  INPUT
*&---------------------------------------------------------------------*
MODULE exit_1010 INPUT.

  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT_1010  INPUT
