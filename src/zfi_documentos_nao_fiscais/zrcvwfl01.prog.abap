*****           Implementation of object type ZDRAW                *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
  " begin of private,
  "   to declare private attributes remove comments and
  "   insert private attributes here ...
  " end of private,
  BEGIN OF key,
    documenttype    LIKE draw-dokar,
    documentnumber  LIKE draw-doknr,
    documentversion LIKE draw-dokvr,
    documentpart    LIKE draw-doktl,
  END OF key.
end_data object. " Do not change.. DATA is generated

begin_method getemail changing container.

DATA: lv_requester TYPE syst-ctype,
      lt_email     TYPE TABLE OF strustcab-email.

swc_get_element container 'Requester' lv_requester.

IF lv_requester EQ abap_true.
  SELECT email~smtp_addr
    FROM i_documentinforecord AS doc
    INNER JOIN i_sadl_gw_cpluser AS email
       ON email~bname = doc~responsiblepersonname
    INTO TABLE @lt_email
    WHERE doc~documentinforecorddoctype    EQ @object-key-documenttype
      AND doc~documentinforecorddocnumber  EQ @object-key-documentnumber
      AND doc~documentinforecorddocpart    EQ @object-key-documentpart
      AND doc~documentinforecorddocversion EQ @object-key-documentversion.
ELSE.
  SELECT email~smtp_addr
    FROM ztdms_aprovador         AS appr
    INNER JOIN i_sadl_gw_cpluser AS email
       ON email~bname EQ appr~aprovador
    WHERE appr~status EQ @abap_true
    INTO TABLE @lt_email.
ENDIF.

swc_set_table container 'Email' lt_email.

end_method.

begin_method getapprovers changing container.

DATA: lt_approvers TYPE TABLE OF wfsyst-agent.

SELECT concat( 'US', aprovador )
  FROM ztdms_aprovador
  WHERE status EQ @abap_true
  INTO TABLE @lt_approvers.

swc_set_table container 'Approvers' lt_approvers.

end_method.

begin_method readcomments changing container.

DATA: lt_comments    TYPE TABLE OF solisti1-line.
DATA: lo_wi_handle          TYPE REF TO if_swf_run_wim_internal,
      lv_workitemid         TYPE swwwihead-wi_id,
      lv_text_string        TYPE string,
      lt_attachment_content TYPE TABLE OF solisti1.

swc_get_element container 'WorkitemID' lv_workitemid.

TRY.
    CALL METHOD cl_swf_run_wim_factory=>find_by_wiid
      EXPORTING
        im_wiid     = lv_workitemid
      RECEIVING
        re_instance = lo_wi_handle.

    DATA(ls_swwwihead) = CORRESPONDING swwwihead( lo_wi_handle->m_sww_wihead ).

    DATA(ls_lpor) = cl_swf_utl_pers_profile=>get_cnt_lpor_from_header( im_header = ls_swwwihead ).
    DATA(lo_container) = cl_swf_cnt_factory=>create( ibf_por = ls_lpor ).
    DATA(lt_comment_objects) = lo_wi_handle->get_attach_objects( im_container        = lo_container
                                                                 im_comment_semantic = 'X' ).
  CATCH cx_swf_ifs_exception INTO DATA(lo_exception).
    RETURN.
ENDTRY.

DATA(lv_last) = lines( lt_comment_objects ).
IF lv_last IS INITIAL.
  RETURN.
ENDIF.

READ TABLE lt_comment_objects INTO DATA(ls_comment_object) INDEX lv_last.
IF sy-subrc NE 0.
  RETURN.
ENDIF.

DATA(lv_document_id) = CONV so_entryid( ls_comment_object-instid ).

CALL FUNCTION 'SO_DOCUMENT_READ_API1'
  EXPORTING
    document_id                = lv_document_id
  TABLES
    object_content             = lt_attachment_content
  EXCEPTIONS
    document_id_not_exist      = 1
    operation_no_authorization = 2
    x_error                    = 3
    OTHERS                     = 4.
IF sy-subrc = 0.
  LOOP AT lt_attachment_content ASSIGNING FIELD-SYMBOL(<fs_attach_content>).
    IF lv_text_string IS INITIAL.
      lv_text_string = <fs_attach_content>-line.
    ELSE.
      CONCATENATE lv_text_string <fs_attach_content>-line INTO lv_text_string
        SEPARATED BY cl_abap_char_utilities=>cr_lf.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = lv_text_string
      i_tabline_length = 255
    TABLES
      et_table         = lt_comments.
ENDIF.

swc_set_table container 'Comments' lt_comments.

end_method.

begin_method linkgenerator changing container.

CONSTANTS: lc_object TYPE char30 VALUE 'DocumentInfoRecord',
           lc_action TYPE char60 VALUE 'manage',
           lc_alias  TYPE string VALUE 'FIORI'.

DATA: lv_url1       TYPE t5asrlink-link,
      lv_url2       TYPE t5asrlink-link,
      lv_url3       TYPE t5asrlink-link,
      ls_return     TYPE bapiret2,
      lt_returnhtml TYPE TABLE OF solisti1-line.

swc_get_element container 'Return' ls_return.

DATA(lt_parameters) = VALUE tihttpnvp(
( name = |DocumentInfoRecordDocNumber|  value = object-key-documentnumber )
( name = |DocumentInfoRecordDocType|    value = object-key-documenttype )
*  ( name = |DocumentInfoRecordDocVersion| value = object-key-documentversion )
*  ( name = |DocumentInfoRecordDocPart|    value = object-key-documentpart )
).

DATA(lv_url) = cl_lsapi_manager=>create_flp_url( object       = lc_object
                                                 action       = lc_action
                                                 system_alias = lc_alias
                                                 parameters   = lt_parameters ).
DATA(lv_len) = strlen( lv_url ).

IF lv_len LE 75.
  lv_url1 = lv_url.
ELSE.
  lv_url1 = lv_url(75).

  IF lv_len LE 150.
    lv_url2 = lv_url+75.
  ELSE.
    lv_url2 = lv_url+75(75).
    lv_url3 = lv_url+150.
  ENDIF.
ENDIF.

FREE lt_returnhtml.
IF ls_return-type IS NOT INITIAL.
  MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
    WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
    INTO DATA(ls_message).

  APPEND |<h4 style="color:red;">A alteração de status retornou o seguinte erro:</h4>| TO lt_returnhtml.
  APPEND ls_message TO lt_returnhtml.
ENDIF.

CLEAR ls_return.

swc_set_element container 'URL1' lv_url1.
swc_set_element container 'URL2' lv_url2.
swc_set_element container 'URL3' lv_url3.
swc_set_element container 'Return' ls_return.
swc_set_table container 'ReturnHTML' lt_returnhtml.

end_method.
