class ZCL_IM_EIDMS_DOC_OBJ_RBKP definition
  public
  final
  create public .

public section.

  interfaces IF_EX_DOCUMENT_OBJ .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_EIDMS_DOC_OBJ_RBKP IMPLEMENTATION.


  METHOD if_ex_document_obj~before_execute_function.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_obj~delete_check.
    RETURN.
  ENDMETHOD.


  method IF_EX_DOCUMENT_OBJ~GET_DATA.
    MOVE if_ex_document_obj~table_drad_db   TO table_drad_db.
    MOVE if_ex_document_obj~table_drad_work TO table_drad_work.
    MOVE if_ex_document_obj~draw            TO draw.
    MOVE if_ex_document_obj~activity        TO activity.
  endmethod.


  METHOD if_ex_document_obj~get_external_guid.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_obj~get_field_list.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_obj~get_function_code.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_obj~get_object_guid.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_document_obj~jump_to_screen.
    RETURN.
  ENDMETHOD.


  method IF_EX_DOCUMENT_OBJ~OBJECT_CHECK.
    run_no_action = abap_true .
  endmethod.


  method IF_EX_DOCUMENT_OBJ~PUT_DATA.
    MOVE table_drad_db   TO if_ex_document_obj~table_drad_db.
    MOVE table_drad_work TO if_ex_document_obj~table_drad_work.
    MOVE draw            TO if_ex_document_obj~draw.
    MOVE activity        TO if_ex_document_obj~activity.

*   delete all object links except maintenance operation's object links.
    DELETE if_ex_document_obj~table_drad_db WHERE dokob NE flt_val.
  endmethod.


  METHOD if_ex_document_obj~put_function_code.
    RETURN.
  ENDMETHOD.


  method IF_EX_DOCUMENT_OBJ~VALIDATE_DELETE.
    run_no_action = abap_true .
  endmethod.
ENDCLASS.
