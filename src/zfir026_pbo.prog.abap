*----------------------------------------------------------------------*
***INCLUDE ZFIR026_PBO .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_1010  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_1010 OUTPUT.

  SET PF-STATUS 'ALV'.
  SET TITLEBAR '1010' WITH 'ALV'.

ENDMODULE.                 " STATUS_1010  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE display_alv OUTPUT.

  IF go_alvgrid IS INITIAL .

    CREATE OBJECT go_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

*   Create docking container and dock at left side
    CREATE OBJECT go_docking
      EXPORTING
        parent                      = cl_gui_container=>screen0
        side                        = cl_gui_docking_container=>dock_at_left
        ratio                       = 90  " 90% of screen
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

*   Set very high extension -> not overruled by screen resizing
    CALL METHOD go_docking->set_extension
      EXPORTING
        extension  = 99999
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2.

    CREATE OBJECT go_splitter
      EXPORTING
        parent  = go_docking
        rows    = 2
        columns = 1.

    CALL METHOD go_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_parent_html.

    CALL METHOD go_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = go_parent_grid.

    CALL METHOD go_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 27.

*   Creating ALV Grid instance
    CREATE OBJECT go_alvgrid
      EXPORTING
        i_parent          = go_parent_grid
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM prepare_field_catalog CHANGING gt_fieldcat .
    PERFORM prepare_layout CHANGING gs_layout.

    gs_variant-report = sy-repid.

    CREATE OBJECT go_event_receiver.
    SET HANDLER go_event_receiver->handle_top_of_page  FOR go_alvgrid.
    SET HANDLER go_event_receiver->handle_data_changed FOR go_alvgrid.
    PERFORM event_top_of_page USING go_dyndoc_id.

*   Functions
    CALL METHOD go_alvgrid->set_table_for_first_display
      EXPORTING
        is_variant = gs_variant
        i_save     = 'A'
        is_layout  = gs_layout
      CHANGING
        it_outtab       = gt_out[]
        it_fieldcatalog = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 .

    CALL METHOD go_alvgrid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

*   Initializing document
    CALL METHOD go_dyndoc_id->initialize_document.

*   Processing events
    CALL METHOD go_alvgrid->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = go_dyndoc_id.

  ELSE .

    CALL METHOD go_alvgrid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2 .

    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = go_alvgrid.

  ENDIF .

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
