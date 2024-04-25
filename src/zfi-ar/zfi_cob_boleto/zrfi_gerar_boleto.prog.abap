*&---------------------------------------------------------------------*
*& Report ZRFI_GERAR_BOLETO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrfi_gerar_boleto MESSAGE-ID 8b..

*======================================================================*
*  TABLES, INCLUDES, STRUCTURES, DATAS, ...                            *
*======================================================================*

*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
* tables ---------------------------------------------------------------
TABLES: j_1bnfdoc,
        vbrk,                          " billing document header
        bkpf,
        kna1,
        lfa1,                          " financial document header
        j_1bnfe_active,
        j_1b_nfe_access_key.
*----------------------------------------------------------------------*
*  INCLUDES                                                            *
*----------------------------------------------------------------------*
* INCLUDE for General Table Descriptions for Print Programs ------------
INCLUDE rvadtabl.
INCLUDE zsd_nfe_j_1bnfpr_printinc.

*----------------------------------------------------------------------*
*  STRUCTURES                                                          *
*----------------------------------------------------------------------*
* Nota Fiscal header structure -----------------------------------------
DATA: BEGIN OF gt_header.
        INCLUDE STRUCTURE j_1bnfdoc.
DATA: END OF gt_header.

* Nota Fiscal header structure - add. segment --------------------------
DATA: BEGIN OF gt_header_add.
        INCLUDE STRUCTURE j_1bindoc.
DATA: END OF gt_header_add.

* Nota Fiscal partner structure ----------------------------------------
DATA: BEGIN OF gt_partner OCCURS 0.
        INCLUDE STRUCTURE j_1bnfnad.
DATA: END OF gt_partner.

* Nota Fiscal item structure -------------------------------------------
DATA: BEGIN OF gt_item OCCURS 0.
        INCLUDE STRUCTURE j_1bnflin.
DATA: END OF gt_item.

* Nota Fiscal item structure - add. segment ----------------------------
DATA: BEGIN OF gt_item_add OCCURS 0.
        INCLUDE STRUCTURE j_1binlin.
DATA: END OF gt_item_add.

* Nota Fiscal item tax structure ---------------------------------------
DATA: BEGIN OF gt_item_tax OCCURS 0.
        INCLUDE STRUCTURE j_1bnfstx.
DATA: END OF gt_item_tax.

* Nota Fiscal header message structure ---------------------------------
DATA: BEGIN OF gt_header_msg OCCURS 0.
        INCLUDE STRUCTURE j_1bnfftx.
DATA: END OF gt_header_msg.

* Nota Fiscal reference to header message structure -------------------
DATA: BEGIN OF gt_refer_msg OCCURS 0.
        INCLUDE STRUCTURE j_1bnfref.
DATA: END OF gt_refer_msg.

* Carrega a Work area para Contigencia ----------------------
DATA: BEGIN OF gt_danfe OCCURS 0.
        INCLUDE STRUCTURE j_1bnfe_active.
DATA: END OF gt_danfe.


* auxiliar structure for vbrk key (used to update FI) ------------------
DATA: BEGIN OF gt_key_vbrk,
        vbeln LIKE vbrk-vbeln,
      END OF gt_key_vbrk.

DATA: gv_my_destination LIKE j_1binnad,
      gv_my_issuer      LIKE j_1binnad,
      gv_my_carrier     LIKE j_1binnad,
      gv_my_items       LIKE j_1bprnfli OCCURS 0 WITH HEADER LINE.

DATA: gv_fm_name        TYPE rs38l_fnam.

DATA: BEGIN OF gt_inter_total_table OCCURS 0,
        matorg    LIKE j_1bprnfli-matorg,
        taxsit    LIKE j_1bprnfli-taxsit,
        icmsrate  LIKE j_1bprnfli-icmsrate,
        condensed TYPE c,
        nfnett    LIKE j_1bprnfli-nfnett,
      END OF gt_inter_total_table.

*---data for SmartForms---*
DATA: gs_output_options TYPE ssfcompop. " transfer printer to SM
DATA: gs_control_parameters TYPE ssfctrlop.

* Tabela para dados da fatura (SMARTFORMS).
DATA: gs_danfe  TYPE zssd_nfe_header.

* Informações de contigencia
DATA: gv_contingkey(36) TYPE c,
      gv_nftot_char(14) TYPE c,
*      gv_cpf(11)        TYPE c,
*      gv_icmproprio     TYPE c,
      gv_icmsub         TYPE c,
      gv_nfe            TYPE string,
      gv_nfe1           TYPE string. "Não encontrei preenchimento.
DATA: gs_e_znfecontigekey TYPE zssd_nfe_contigekey.

*----------------------------------------------------------------------*
*  DATA AND CONSTANTS                                                 *
*----------------------------------------------------------------------*
DATA: gv_docnum       TYPE j_1bnfdoc-docnum,
      gv_retcode      TYPE sy-subrc,
      gv_xscreen,
      gv_xblnr        TYPE bkpf-xblnr,
      gv_subrc_upd_bi TYPE sy-subrc.
DATA: gv_bi_subrc TYPE sy-subrc,
      gv_fi_subrc TYPE sy-subrc.

CLASS cl_exithandler DEFINITION LOAD.

DATA: gs_nfeactive TYPE        j_1bnfe_active,
      lr_badi      TYPE REF TO zifsd_ex_nfe.

DATA: gv_nova_versao TYPE c.

CONSTANTS: gc_reftyp TYPE j_1bnflin-reftyp VALUE 'BI',
           gc_taxsit TYPE j_1bnflin-taxsit VALUE 'F'.
*======================================================================*
*  PROGRAM                                                             *
*======================================================================*

*&---------------------------------------------------------------------*
*&       FORM ENTRY  (MAIN FORM)                                       *
*&---------------------------------------------------------------------*
*       Form for Message Control                                       *
*----------------------------------------------------------------------*
FORM entry_danfe USING uv_return_code uv_us_screen.

  CLEAR: gv_retcode.
  gv_xscreen = uv_us_screen.

  DATA: lv_sform TYPE tdsfname.

  SELECT SINGLE sform
         INTO lv_sform
         FROM tnapr
         WHERE kschl EQ 'NF01'
           AND kappl EQ 'NF'
           AND nacha EQ '1'.

  IF sy-subrc EQ 0.
    IF NOT lv_sform IS INITIAL.
      tnapr-sform = lv_sform.
      CLEAR tnapr-funcname.
      CLEAR tnapr-fonam.
    ENDIF.
  ENDIF.

  IF lr_badi IS INITIAL.

    CALL METHOD cl_exithandler=>get_instance "#EC CI_BADI_OLD
      EXPORTING
        exit_name                     = 'ZNFE'
      CHANGING
        instance                      = lr_badi
      EXCEPTIONS
        no_reference                  = 1
        no_interface_reference        = 2
        no_exit_interface             = 3
        class_not_implement_interface = 4
        single_exit_multiply_active   = 5
        cast_error                    = 6
        exit_not_existing             = 7
        data_incons_in_exit_managem   = 8
        OTHERS                        = 9. "#EC CI_BADI_GETINST

    IF sy-subrc <> 0.                                       "#EC NEEDED
    ENDIF.

  ENDIF.

  PERFORM smart_sub_printing.

* main -----------------------------------------------------------------

* check gv_retcode (return code) ------------------------------------------
  IF gv_retcode NE 0.
    uv_return_code = 1.
  ELSE.
    uv_return_code = 0.
  ENDIF.
* BEGIN OF INSERT - 08/11/2023 - GAP QM01 - Boletim de Conformidade
  IF uv_return_code = 0.

    DATA(lo_print) = NEW zclqm_qcert_01( ).
    lo_print->entry( is_objky = nast-objky ).

  ENDIF.
* END OF INSERT - 08/11/2023 - GAP QM01 - Boletim de Conformidade

ENDFORM.                               " ENTRY

*&---------------------------------------------------------------------*
*&      Form  NOTA_FISCAL_READ
*&---------------------------------------------------------------------*
*       Read the Nota Fiscal based in the key giving by Message        *
*       Control.                                                       *
*----------------------------------------------------------------------*
FORM nota_fiscal_read.

* get the key ----------------------------------------------------------
  MOVE nast-objky TO gv_docnum.

  DATA: lo_nf_texts_utilities TYPE REF TO if_logbr_nf_texts_utilities,
        lo_nf_texts_data      TYPE REF TO if_logbr_nf_texts_data,
        lt_1bnfftx            TYPE TABLE OF j_1bnfftx,
        lt_1bnfref            TYPE TABLE OF j_1bnfref.

  lo_nf_texts_utilities = NEW logbr_nf_texts_utilities( ).

  CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
    EXPORTING
      doc_number         = gv_docnum
    IMPORTING
      doc_header         = gt_header
    TABLES
      doc_partner        = gt_partner
      doc_item           = gt_item
      doc_item_tax       = gt_item_tax
      doc_header_msg     = gt_header_msg
      doc_refer_msg      = gt_refer_msg
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.

* check the sy-subrc ---------------------------------------------------
  IF sy-subrc IS INITIAL.
    PERFORM check_error.
  ENDIF.

  DELETE gt_item_tax WHERE taxtyp EQ 'ICAP'.             "#EC CI_STDSEQ

  CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
    EXPORTING
      nf_header   = gt_header
    IMPORTING
      ext_header  = gt_header_add
    TABLES
      nf_item     = gt_item
      nf_item_tax = gt_item_tax
      ext_item    = gt_item_add.

ENDFORM.                               " NOTA_FISCAL_READ
*&---------------------------------------------------------------------*
*&      Form  NOTA_FISCAL_NUMBER
*&---------------------------------------------------------------------*
*       Get the next Nota Fiscal number                                *
*----------------------------------------------------------------------*
FORM nota_fiscal_number.

  CALL FUNCTION 'J_1B_NF_NUMBER_GET_NEXT'
    EXPORTING
      bukrs                         = gt_header-bukrs
      branch                        = gt_header-branch
      form                          = gt_header-form
      headerdata                    = gt_header
    IMPORTING
      nf_number                     = gt_header-nfnum
    EXCEPTIONS
      print_number_not_found        = 1
      interval_not_found            = 2
      number_range_not_internal     = 3
      object_not_found              = 4
      other_problems_with_numbering = 5
      OTHERS                        = 6.

  IF sy-subrc IS INITIAL.
    PERFORM check_error.
  ENDIF.

ENDFORM.                               " NOTA_FISCAL_NUMBER

*&---------------------------------------------------------------------*
*&      Form  NOTA_FISCAL_UPDATE
*&---------------------------------------------------------------------*
*       Update NF date and number                                      *
*----------------------------------------------------------------------*
FORM nota_fiscal_update.

  gt_header-printd = 'X'.

  UPDATE j_1bnfdoc SET printd = gt_header-printd
                       follow = gt_header-follow
                 WHERE docnum = gt_header-docnum.

  IF sy-subrc <> 0.
    gv_retcode = sy-subrc.
    syst-msgid = '8B'.
    syst-msgno = '107'.
    syst-msgty = 'E'.
    syst-msgv1 = gt_header-docnum.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                               " NOTA_FISCAL_UPDATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ERROR
*&---------------------------------------------------------------------*
*       Check return code                                              *
*----------------------------------------------------------------------*
FORM check_error.

  IF sy-subrc <> 0.
    gv_retcode = sy-subrc.
    PERFORM protocol_update.
  ENDIF.

ENDFORM.                               " CHECK_ERROR
*&---------------------------------------------------------------------*
*&      Form  PROTOCOL_UPDATE
*&---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.        *
*----------------------------------------------------------------------*
FORM protocol_update.

  CHECK gv_xscreen = space.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4.

ENDFORM.                               " PROTOCOL_UPDATE
*&---------------------------------------------------------------------*
*&      Form  FINANCIAL_DOC_UPDATE
*&---------------------------------------------------------------------*
*       Update the sales document and Financial document with the      *
*       Nota Fiscal number and the Nota Fiscal with the financial      *
*       document                                                       *
*----------------------------------------------------------------------*
FORM financial_doc_update.

  SORT gt_item.
  READ TABLE gt_item INDEX 1.

  CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
    EXPORTING
      nf_number  = gt_header-nfnum
      series     = gt_header-series
      subseries  = gt_header-subser
      nf_number9 = gt_header-nfenum
    IMPORTING
      ref_number = gv_xblnr.

* get the type of the document and update the documents ----------------
  CASE gt_item-reftyp.

    WHEN 'BI'.
      MOVE gt_item-refkey TO gt_key_vbrk.
      PERFORM read_bi_document.
      CLEAR bkpf.
      IF NOT vbrk IS INITIAL.          " if find VBRK (Billing document)
        PERFORM get_fi_number.
      ENDIF.
      IF bkpf-belnr IS INITIAL.        " there is not FI document
        IF NOT vbrk IS INITIAL.        " if find VBRK (Billing document)
          PERFORM update_bi_document.
        ENDIF.
      ELSE.                            " there is FI document
        PERFORM update_bi_document.
        IF  gv_subrc_upd_bi IS INITIAL.   " update in billing ok.
          PERFORM update_fi_nf_document
                    USING bkpf-bukrs bkpf-belnr bkpf-gjahr.
*          perform update_bsid_nf_document
*                  using bkpf-bukrs bkpf-belnr bkpf-gjahr.
        ENDIF.
      ENDIF.

    WHEN OTHERS.  " for MD or <space> that means writer.
      gt_header-follow = 'X'.

  ENDCASE.

ENDFORM.                               " FINANCIAL_DOC_UPDATE

*&---------------------------------------------------------------------*
*&      Form  READ_BI_DOCUMENT
*&---------------------------------------------------------------------*
*       This form read the billing document                            *
*----------------------------------------------------------------------*
FORM read_bi_document.

  SELECT SINGLE * FROM  vbrk
         WHERE  vbeln       = gt_key_vbrk-vbeln.

  IF sy-subrc <> 0.
    CLEAR vbrk.
  ENDIF.

ENDFORM.                               " READ_BI_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  READ_FI_DOCUMENT
*&---------------------------------------------------------------------*
*       read the fi_document                                           *
*----------------------------------------------------------------------*
FORM read_fi_document USING uv_xbukrs uv_xbelnr uv_xgjahr.

*  SELECT SINGLE * FROM  bkpf
*         WHERE  bukrs       = xbukrs
*         AND    belnr       = xbelnr
*         AND    gjahr       = xgjahr.

  "#2023
  SELECT SINGLE belnr bukrs gjahr xblnr awtyp awkey
  INTO CORRESPONDING FIELDS OF bkpf
  FROM  bkpf
  WHERE  bukrs       = uv_xbukrs
  AND    belnr       = uv_xbelnr
  AND    gjahr       = uv_xgjahr.

  IF sy-subrc <> 0.
    CLEAR bkpf.
  ENDIF.

ENDFORM.                               " READ_FI_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BI_DOCUMENT
*&---------------------------------------------------------------------*
*       Update billing document                                        *
*----------------------------------------------------------------------*
FORM update_bi_document.

  IF gv_bi_subrc = 0.                     " billing not lock

    UPDATE vbrk SET xblnr = gv_xblnr
                WHERE  vbeln = gt_key_vbrk-vbeln.

    PERFORM check_error.

    gv_subrc_upd_bi = sy-subrc.

    CALL FUNCTION 'DEQUEUE_EVVBRKE'
      EXPORTING
        mandt = sy-mandt
        vbeln = gt_key_vbrk-vbeln.

  ELSE.                                " billing lock

    gv_subrc_upd_bi = sy-subrc.

  ENDIF.

ENDFORM.                               " UPDATE_BI_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  UPDATE_FI_NF_DOCUMENT
*&---------------------------------------------------------------------*
*       Update financial and nota fiscal document                      *
*----------------------------------------------------------------------*
FORM update_fi_nf_document USING uv_xbukrs uv_xbelnr uv_xgjahr.

  IF gv_fi_subrc = 0.                     " billing not lock

    UPDATE bkpf SET xblnr = gv_xblnr
                WHERE  bukrs       = uv_xbukrs
                AND    belnr       = uv_xbelnr
                AND    gjahr       = uv_xgjahr.

    PERFORM check_error.

    gt_header-follow = 'X'.

  ENDIF.

ENDFORM.                               " UPDATE_FI_NF_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  GET_FI_NUMBER
*&---------------------------------------------------------------------*
*       Read financial document via Billing document number            *
*----------------------------------------------------------------------*
FORM get_fi_number.



*  SELECT SINGLE * FROM  bkpf
*         WHERE  bukrs = vbrk-bukrs
*         AND    awtyp = 'VBRK'
*         AND    awkey = gt_key_vbrk-vbeln.

  "#2023
  SELECT SINGLE belnr bukrs gjahr xblnr awtyp awkey
  INTO CORRESPONDING FIELDS OF bkpf
  FROM  bkpf
  WHERE  bukrs = vbrk-bukrs
  AND    awtyp = 'VBRK'
  AND    awkey = gt_key_vbrk-vbeln.                         "#EC WARNOK

  IF sy-subrc <> 0.
    CLEAR bkpf.
  ENDIF.

ENDFORM.                               " GET_FI_NUMBER

*---------------------------------------------------------------------*
*       FORM ENQUEUE_BI_FI                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM enqueue_bi_fi.

  CLEAR gv_bi_subrc.

* sort the gt_item to get the first item
  SORT gt_item.
  READ TABLE gt_item INDEX 1.

  CHECK gt_item-reftyp = 'BI'.
  MOVE gt_item-refkey TO gt_key_vbrk.

  PERFORM read_bi_document.

  IF NOT vbrk IS INITIAL.              "call via SD
    CALL FUNCTION 'ENQUEUE_EVVBRKE'
      EXPORTING
        mandt          = sy-mandt
        vbeln          = gt_key_vbrk-vbeln
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    gv_bi_subrc = sy-subrc.
    PERFORM check_error.
    IF gv_bi_subrc = 0.                   "BI document not locked
      CLEAR bkpf.
      PERFORM get_fi_number.
      IF NOT bkpf-belnr IS INITIAL.
        CALL FUNCTION 'ENQUEUE_EFBKPF'
          EXPORTING
            bukrs          = bkpf-bukrs
            belnr          = bkpf-belnr
            gjahr          = bkpf-gjahr
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        gv_fi_subrc = sy-subrc.
        IF sy-subrc IS INITIAL.
          PERFORM check_error.
        ENDIF.
        IF gv_fi_subrc <> 0.     "FI lock not successful -> release BI lock
          CALL FUNCTION 'DEQUEUE_EVVBRKE'
            EXPORTING
              mandt  = sy-mandt
              vbeln  = gt_key_vbrk-vbeln
            EXCEPTIONS
              OTHERS = 1.
          IF sy-subrc IS INITIAL.
            PERFORM check_error.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " ENQUEUE_BI_FI
*&---------------------------------------------------------------------*
*&      Form  check_nf_canceled
*&---------------------------------------------------------------------*
*       allow print of NF only when NF is not canceled
*----------------------------------------------------------------------*
FORM check_nf_canceled.
  DATA: lv_dummy  TYPE c.

  IF NOT gt_header-cancel IS INITIAL AND gt_header-nfnum IS INITIAL.
    sy-subrc = 1.
    MESSAGE ID '8B'
            TYPE 'E'
            NUMBER '678'
            WITH gt_header-docnum
            INTO lv_dummy.

    PERFORM check_error.
    IF sy-batch IS INITIAL.                " corr. of note 442570
      MESSAGE e678 WITH gt_header-docnum.
    ENDIF.                                 " corr. of note 442570
  ENDIF.
ENDFORM.                    " check_nf_canceled
*&---------------------------------------------------------------------*
*&      Form  check_nfe_authorized
*&---------------------------------------------------------------------*
FORM check_nfe_authorized.
  DATA: lv_dummy   TYPE c,
        lv_subrc   TYPE sy-subrc,
        lo_obj_ref TYPE REF TO if_ex_cl_nfe_print.

  CLEAR gs_nfeactive.

* only NFes
  CHECK gt_header-nfe = 'X'.

  SELECT SINGLE * FROM j_1bnfe_active INTO gs_nfeactive
  WHERE docnum = gt_header-docnum.

  IF NOT sy-subrc IS INITIAL.
    "MESSAGE e012 WITH gt_header-docnum.
    MESSAGE e012.
  ENDIF.

*  IF gs_nfeactive-code IS INITIAL.
*
*    COMMIT WORK AND WAIT.
*    WAIT UP TO 30 SECONDS.
*
*    SELECT SINGLE * FROM j_1bnfe_active INTO gs_nfeactive
*    WHERE docnum = gt_header-docnum.
*
*    IF NOT sy-subrc IS INITIAL.
*      "MESSAGE e012 WITH gt_header-docnum.
*      MESSAGE e012.
*    ENDIF.
*
*  ENDIF.

  j_1bnfe_active = gs_nfeactive.

* don't print NF-e when ...
* ... rejected docsta = 2
* ... denied   docsta = 3
* ... switches manual to contingency
  IF gs_nfeactive-conting_s = 'X'
  OR gs_nfeactive-docsta    = '2'
  OR gs_nfeactive-docsta    = '3'
  OR gs_nfeactive-docsta IS INITIAL.

    lv_subrc = 1.

  ELSE.

*-- don´t print not authorized NFes

    IF  gt_header-authcod IS INITIAL    "Nfe is not authorized
    AND gt_header-conting IS INITIAL.   "and not in contingency
      lv_subrc = 1.
    ENDIF.
  ENDIF.

*-- BADI for reset subrc
*-- When subrc is 0 NFes can be printed without aauthorization code

  IF lo_obj_ref IS INITIAL.

    CALL METHOD cl_exithandler=>get_instance       " #EC CI_BADI_GETINST "#EC CI_BADI_OLD
      EXPORTING
        exit_name                     = 'CL_NFE_PRINT'
        null_instance_accepted        = seex_false
      CHANGING
        instance                      = lo_obj_ref
      EXCEPTIONS
        no_reference                  = 1
        no_interface_reference        = 2
        no_exit_interface             = 3
        class_not_implement_interface = 4
        single_exit_multiply_active   = 5
        cast_error                    = 6
        exit_not_existing             = 7
        data_incons_in_exit_managem   = 8
        OTHERS                        = 9.

    IF sy-subrc IS INITIAL.
*- nothing to do
    ENDIF.

  ENDIF.

  IF lo_obj_ref IS BOUND.
    CALL METHOD lo_obj_ref->reset_subrc
      EXPORTING
        is_nfdoc = gt_header
      CHANGING
        ch_subrc = lv_subrc.
  ENDIF.

  sy-subrc = lv_subrc.

  IF sy-subrc IS NOT INITIAL.
    RETURN.
*    IF gs_nfeactive-conting_s = 'X'.
*      MESSAGE ID 'J1B_NFE'
*              TYPE 'E'
*              NUMBER '040'
*              WITH gt_header-docnum
*              INTO lv_dummy.
*    ELSE.
*      MESSAGE ID 'J1B_NFE'
*              TYPE 'E'
*              NUMBER '039'
*              WITH gt_header-docnum
*              INTO lv_dummy.
*    ENDIF.
*    IF sy-batch IS INITIAL.
*      PERFORM check_error.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
  ELSE.
    gs_nfeactive-printd = 'X'.
  ENDIF.

ENDFORM.                    " check_nfe_authorized
*&---------------------------------------------------------------------*
*&      Form  active_update
*&---------------------------------------------------------------------*
FORM active_update .


  UPDATE j_1bnfe_active FROM gs_nfeactive.

  IF sy-subrc <> 0.
    MESSAGE a021(j1b_nfe) WITH gs_nfeactive-docnum.
  ENDIF.


ENDFORM.                    " active_update
*&--------------------------------------------------------------------*
*&      Form  smart_sub_printing
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM smart_sub_printing.

  DATA:   lt_tax_types LIKE j_1baj OCCURS 30 WITH HEADER LINE.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname = tnapr-sform
    IMPORTING
      fm_name  = gv_fm_name.

  REFRESH: gv_my_items.

* read the Nota Fiscal
  PERFORM nota_fiscal_read.            " read nota fiscal

* allow print of NF only when NF is not canceled
  PERFORM check_nf_canceled.   " check nota fiscal canceled,442570

* number and update the Nota Fiscal
  CHECK gv_retcode IS INITIAL.

* The NFe to be printed must have an authorization code
  IF gt_header-nfe = 'X'.

*   The NFe to be printed must have an authorization code
    PERFORM check_nfe_authorized.
    CHECK gv_retcode IS INITIAL.

  ENDIF.
* for NFe the DANFE is printed. Numbering has already taken place
* before sending teh XML document to SEFAZ.
* Billing document is updated for NFe with the 9 digit NFe number
  IF gt_header-entrad = 'X' OR  "update only entradas or outgoing NF
     gt_header-direct  = '2'.
    IF gt_header-printd IS INITIAL AND " not printed.
       gt_header-nfnum IS INITIAL  AND " without NF number
       nast-nacha = '1'.               " sent to printer

      PERFORM enqueue_bi_fi.
      CHECK gv_retcode IS INITIAL.
* get NF number only for "normal NFs" NFe has already the number
      IF gt_header-nfe IS INITIAL.
        PERFORM nota_fiscal_number.      " get the next number
      ENDIF.

      IF gv_retcode IS INITIAL.
        PERFORM financial_doc_update.    " update in database
        PERFORM nota_fiscal_update.      " update in database
        IF NOT gs_nfeactive IS INITIAL.
          PERFORM active_update. "ON COMMIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gv_retcode IS INITIAL.
  ELSE.
    MESSAGE a114 WITH '01' 'J_1BNFNUMB'.
  ENDIF.

*----------------------------------------------------------------------*
*    read tax types into internal buffer table                         *
*----------------------------------------------------------------------*
  SELECT * FROM j_1baj INTO TABLE lt_tax_types ORDER BY PRIMARY KEY.

  CLEAR  gs_danfe.
  CLEAR: gs_danfe-issuer,
         gs_danfe-destination,
         gs_danfe-carrier,
         gs_danfe-nota_fiscal,
         gs_danfe-others,
         gs_danfe-nfe,
         gs_danfe-observ1,
         gs_danfe-observ2,
         gs_danfe-item,
         gs_danfe-invoice.

*----------------------------------------------------------------------*
*    fill header data into communication structure                     *
*----------------------------------------------------------------------*
  MOVE-CORRESPONDING gt_header TO gs_danfe-nota_fiscal.

  SELECT SINGLE *
         FROM j_1bnfe_active
         INTO gs_danfe-nfe
         WHERE docnum EQ gt_header-docnum.    "#EC CI_ALL_FIELDS_NEEDED


*---> determine CFOP length, extension and deafulttext from version
*---> table
  PERFORM get_cfop_length_smart  USING gt_header-bukrs
                                 gt_header-branch
                                 gt_header-pstdat
                        CHANGING gv_cfop_version     " BOI note 593218
                                 gv_cfop_length
                                 gv_extension_length
                                 gv_defaulttext
                                 gv_issuer_region.

  MOVE gv_cfop_length TO gs_danfe-nota_fiscal-cfop_len.
*... fill header CFOP .................................................*

  DATA: BEGIN OF lt_cfop OCCURS 0,
          key(6)          TYPE c,
          char6(6)        TYPE c,
          dupl_text_indic TYPE c,
          text(50)        TYPE c.
  DATA: END OF lt_cfop.
  DATA: lv_help_cfop(6)    TYPE c,
        lv_default_cfop(6) TYPE c,
        lv_tabix           TYPE sytabix,
        lv_cfop            TYPE j_1bnflin-cfop.


  LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
*    concatenate gt_item-cfop(3) '0' gt_item-cfop+4(2) into lv_cfop.
*    gt_item-cfop = lv_cfop.
    WRITE <fs_item>-cfop  TO lv_help_cfop.
    lv_help_cfop = lv_help_cfop(gv_cfop_length).
    CASE gv_extension_length.
      WHEN 1.
        IF ( <fs_item>-cfop+1(3) = '991' OR <fs_item>-cfop+1(3) = '999' )
                                             AND gv_issuer_region = 'SP'.
          CONCATENATE lv_help_cfop '.' <fs_item>-cfop+3(1) INTO lv_help_cfop.
        ENDIF.
      WHEN 2.
        IF <fs_item>-cfop+1(2) = '99' AND gv_issuer_region = 'SC'.
          CONCATENATE lv_help_cfop '.' <fs_item>-cfop+3(2) INTO lv_help_cfop.
        ENDIF.
    ENDCASE.

    READ TABLE lt_cfop WITH KEY key = lv_help_cfop. "#EC CI_STDSEQ "#EC CI_SEL_DEL
    lv_tabix = sy-tabix.
    IF sy-subrc <> 0.  " new CFOP on this NF: append this CFOP to list
      lt_cfop-char6  =  <fs_item>-cfop.
      lt_cfop-key    =  lv_help_cfop.

      SELECT SINGLE * FROM j_1bagnt   WHERE spras   = sy-langu
                                        AND version = gv_cfop_version
                                        AND cfop    = <fs_item>-cfop.
      IF sy-subrc = 0.
        lt_cfop-text = j_1bagnt-cfotxt.
        APPEND lt_cfop.
      ELSE.
        gv_encoded_cfop = <fs_item>-cfop.
        IF gv_encoded_cfop(1) CA                    " BOI note 593218-470
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ[-<>=!?]'.
          WRITE <fs_item>-cfop  TO gv_encoded_cfop.
          REPLACE '/' IN gv_encoded_cfop WITH ' '.
          CONDENSE gv_encoded_cfop NO-GAPS.
        ELSE.
          PERFORM encoding_cfop_smart CHANGING gv_encoded_cfop.
        ENDIF.                                   " EOI note 593218-470
*PERFORM ENCODING_CFOP_SMART CHANGING ENCODED_CFOP." note 593218-470
        SELECT SINGLE * FROM j_1bagnt WHERE spras = nast-spras
                                          AND version = gv_cfop_version
                                          AND cfop    = gv_encoded_cfop.
        IF sy-subrc = 0.
          lt_cfop-text = j_1bagnt-cfotxt.
          APPEND lt_cfop.
        ENDIF.
      ENDIF.
    ELSE. " CFOP already on list; however, could be rel. to other text
      IF lt_cfop-char6 <> <fs_item>-cfop AND
                                  lt_cfop-dupl_text_indic IS INITIAL.
        lv_default_cfop      = <fs_item>-cfop.
        lv_default_cfop+4(2) = gv_defaulttext.
        SELECT SINGLE * FROM j_1bagnt WHERE spras   = nast-spras
                                        AND version = gv_cfop_version
                                        AND cfop    = lv_default_cfop.
        IF sy-subrc = 0.
          lt_cfop-text = j_1bagnt-cfotxt.
          lt_cfop-dupl_text_indic = 'X'.
          MODIFY lt_cfop INDEX lv_tabix.
        ELSE.
          gv_encoded_cfop = lv_default_cfop.
          PERFORM encoding_cfop_smart CHANGING gv_encoded_cfop.
          SELECT SINGLE * FROM j_1bagnt WHERE spras = nast-spras
                                          AND version = gv_cfop_version
                                          AND cfop    = gv_encoded_cfop.
          IF sy-subrc = 0.
            lt_cfop-text = j_1bagnt-cfotxt.
            lt_cfop-dupl_text_indic = 'X'.
            MODIFY lt_cfop INDEX lv_tabix.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_cfop LINES gv_cfop_lines.
  IF gv_cfop_lines > 1.
    SORT lt_cfop.
    DELETE ADJACENT DUPLICATES FROM lt_cfop COMPARING key.
    LOOP AT lt_cfop ASSIGNING FIELD-SYMBOL(<fs_cfop>).
      CONCATENATE gs_danfe-nota_fiscal-cfop_text
                  '/'
                  <fs_cfop>-key <fs_cfop>-text
                  INTO gs_danfe-nota_fiscal-cfop_text.
      IF gs_danfe-nota_fiscal-cfop_text(1) EQ '/'.
        SHIFT gs_danfe-nota_fiscal-cfop_text LEFT BY 1 PLACES.
      ENDIF.
    ENDLOOP.
  ELSEIF gv_cfop_lines = 1.      " NF with items that all have one CFOP
    MOVE lt_cfop-key  TO gs_danfe-nota_fiscal-cfop.
    MOVE lt_cfop-text TO gs_danfe-nota_fiscal-cfop_text.
  ENDIF.                                             " BOI note 593218

*----------------------------------------------------------------------*
*    If you are on contingency, print barcode                          *
*----------------------------------------------------------------------*

  CLEAR  gv_contingkey.
  ##UOM_IN_MES
  WRITE: gt_header_add-nftot TO gv_nftot_char(14).


  REPLACE '.' INTO gv_nftot_char WITH ''.
  REPLACE ',' INTO gv_nftot_char WITH ''.
  CONDENSE gv_nftot_char NO-GAPS.
  UNPACK gv_nftot_char TO gv_nftot_char .


*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = gv_nftot_char
*    IMPORTING
*      output = gv_nftot_char.


  IF NOT gs_nfeactive-conting IS INITIAL.
* Adiciona dados de icms proprio e icms de substituicao para contigencia
*gv_nftot_char
    gs_e_znfecontigekey-icmsp = '2'.
    gs_e_znfecontigekey-icmss = '2'.


    LOOP AT gt_item  ASSIGNING <fs_item>.
      IF <fs_item>-taxsit = '0' OR <fs_item>-taxsit = '1' OR <fs_item>-taxsit  = '2'
         OR <fs_item>-taxsit = '6' OR <fs_item>-taxsit = '7'
         OR <fs_item>-taxsit = '10' OR <fs_item>-taxsit = '51' OR <fs_item>-taxsit = 'B'.
        gs_e_znfecontigekey-icmsp = '1'.
      ENDIF.

      IF  <fs_item>-taxsit = '1' OR <fs_item>-taxsit = '3' OR <fs_item>-taxsit
                = '6' OR <fs_item>-taxsit = '7'.
        gs_e_znfecontigekey-icmss = '1'.
      ENDIF.

    ENDLOOP.



* Monta o número de contigencia
* Tipo de impressão (FS - Contingência com uso do Formulário de
* segurança) página 8 do manual de contingência
    gs_e_znfecontigekey-tpemiss = '5'.

* Valor total da nota sem pontos, virgulas e com zeros a esquerda (14
*    posições)
    gs_e_znfecontigekey-vtotal = gv_nftot_char.

* Dia da data de emissão
    gs_e_znfecontigekey-ddemiss = gt_header-docdat+6(2).

    MOVE j_1bnfe_active TO gt_danfe.

* Código da região e CNPJ do destinatário

* Se for cliente
    IF gt_header-partyp = 'C'.

      "#2023
      SELECT SINGLE txjcd stcd1
        INTO CORRESPONDING FIELDS OF kna1
        FROM kna1
        WHERE kunnr = gt_header-parid.

*      SELECT SINGLE *
*        FROM kna1
*        WHERE kunnr = gt_header-parid.


      gs_e_znfecontigekey-regio = kna1-txjcd+3(2).
      gs_e_znfecontigekey-stcd1 = kna1-stcd1(14).
* Não Esquecer de colocar o campo stcd2 na estrutura
*      if GS_e_znfecontigekey-stcd1 = '00000000000000'.
*        clear GS_e_znfecontigekey-stcd1.
*      endif.
*      WRITE: kna1-stcd2 TO gv_cpf.
*      REPLACE '.' INTO gv_cpf WITH ''.
*      REPLACE '.' INTO gv_cpf WITH ''.
*      REPLACE '-' INTO gv_cpf WITH ''.
*
*      CONDENSE gv_cpf NO-GAPS.
*
*      UNPACK gv_cpf TO  GS_e_znfecontigekey-stcd2 .

    ENDIF.

* se for Fornecedor

    IF gt_header-partyp = 'V' OR
       gt_header-partyp = 'B'.

*      SELECT SINGLE *
*        FROM lfa1
*        WHERE lifnr = gt_header-parid.

      "#2023
      SELECT SINGLE txjcd stcd1
        INTO CORRESPONDING FIELDS OF lfa1
        FROM lfa1
        WHERE lifnr = gt_header-parid.

      gs_e_znfecontigekey-regio = lfa1-txjcd+3(2).
      gs_e_znfecontigekey-stcd1 = lfa1-stcd1(14).
*      if GS_e_znfecontigekey-stcd1 = '00000000000000'.
*        clear GS_e_znfecontigekey-stcd1.
*      endif.
*      WRITE: lfa1-stcd2 TO gv_cpf.
*      REPLACE '.' INTO gv_cpf WITH ''.
*      REPLACE '.' INTO gv_cpf WITH ''.
*      REPLACE '-' INTO gv_cpf WITH ''.
*
*      CONDENSE gv_cpf NO-GAPS.
*
*      UNPACK gv_cpf TO  GS_e_znfecontigekey-stcd2 .

    ENDIF.

* Se for cliente do exterior
*    IF j_1bprnfde-land1 <> 'BR'.
*
*      CLEAR:
*        GS_e_znfecontigekey-regio,
*        GS_e_znfecontigekey-stcd1.
*
*      GS_e_znfecontigekey-regio = '99'.
*      GS_e_znfecontigekey-stcd1 = '00000000000000'.
*    ENDIF.

* Gera digito verificador
    CALL FUNCTION 'ZFMSD_NFE_CREATE_CONTI_CHECK_D'
      CHANGING
        cs_contingkey = gs_e_znfecontigekey.

* Cria chave de contingencia
    CONCATENATE gs_e_znfecontigekey-regio
                gs_e_znfecontigekey-tpemiss
                gs_e_znfecontigekey-stcd1
*                GS_e_znfecontigekey-stcd2
                gs_e_znfecontigekey-vtotal
                gs_e_znfecontigekey-icmsp
                gs_e_znfecontigekey-icmss
                gs_e_znfecontigekey-ddemiss
                gs_e_znfecontigekey-cdv
                INTO gv_contingkey.

* Apagar chave de contingência, caso não seja do tipo

    MOVE TEXT-t04 TO gv_nfe."T04  DADOS NF-e

    IF gs_nfeactive-conting = space.

      CLEAR: gv_contingkey,
             gv_nfe.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*    determine issuer and destination (only for test)                  *
*----------------------------------------------------------------------*
  IF gt_header-direct = '1'   AND
     gt_header-entrad = ' '.
    gs_issuer-partner_type      = gt_header-partyp.
    gs_issuer-partner_id        = gt_header-parid.
    gs_issuer-partner_function  = gt_header-parvw.
    gs_destination-partner_type = 'B'.
    gs_destination-partner_id   = gt_header-bukrs.
    gs_destination-partner_id+4 = gt_header-branch.
  ELSE.
    gs_issuer-partner_type          = 'B'.
    gs_issuer-partner_id            = gt_header-bukrs.
    gs_issuer-partner_id+4          = gt_header-branch.
    gs_destination-partner_type     = gt_header-partyp.
    gs_destination-partner_id       = gt_header-parid.
    gs_destination-partner_function = gt_header-parvw.
  ENDIF.

*----------------------------------------------------------------------*
*    read branch data (issuer)                                         *
*----------------------------------------------------------------------*

  CLEAR j_1binnad.

  CALL FUNCTION 'J_1B_NF_PARTNER_READ'
    EXPORTING
      partner_type           = gs_issuer-partner_type
      partner_id             = gs_issuer-partner_id
      partner_function       = gs_issuer-partner_function
      doc_number             = gt_header-docnum
      obj_item               = gt_item
    IMPORTING
      parnad                 = j_1binnad
    EXCEPTIONS
      partner_not_found      = 1
      partner_type_not_found = 2
      OTHERS                 = 3.

  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING j_1binnad TO gs_danfe-issuer.
  ENDIF.


*... check the sy-subrc ...............................................*
  PERFORM check_error.
  CHECK gv_retcode IS INITIAL.

*----------------------------------------------------------------------*
*    read destination data                                             *
*----------------------------------------------------------------------*

  CLEAR j_1binnad.

  CALL FUNCTION 'J_1B_NF_PARTNER_READ'
    EXPORTING
      partner_type           = gs_destination-partner_type
      partner_id             = gs_destination-partner_id
      partner_function       = gs_destination-partner_function
      doc_number             = gt_header-docnum
      obj_item               = gt_item
    IMPORTING
      parnad                 = j_1binnad
    EXCEPTIONS
      partner_not_found      = 1
      partner_type_not_found = 2
      OTHERS                 = 3.

  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING j_1binnad TO gs_danfe-destination.
  ENDIF.

*----------------------------------------------------------------------*
*    read fatura data if the Nota Fiscal is a Nota Fiscal Fatura       *
*----------------------------------------------------------------------*

  DATA: lv_loops TYPE i,
        lv_linha TYPE i,
        lv_index TYPE i.

  CLEAR lv_linha.

  SELECT ndup,
         dvenc,
         vdup
    FROM j_1bnftradenotes
    INTO TABLE @gs_danfe-invoice
    WHERE docnum = @gs_danfe-nota_fiscal-docnum.

  IF sy-subrc IS INITIAL.

  ENDIF.
*----------------------------------------------------------------------*
*    read carrier data                                                 *
*----------------------------------------------------------------------*

  IF gt_header-doctyp NE '2'.          "no carrier for Complementars

    READ TABLE gt_partner WITH KEY docnum = gt_header-docnum
                                   parvw  = 'SP'.        "#EC CI_STDSEQ
    IF sy-subrc = 0.

      CLEAR j_1binnad.
      CALL FUNCTION 'J_1B_NF_PARTNER_READ'
        EXPORTING
          partner_type           = gt_partner-partyp
          partner_id             = gt_partner-parid
          partner_function       = gt_partner-parvw
          doc_number             = gt_header-docnum
        IMPORTING
          parnad                 = j_1binnad
        EXCEPTIONS
          partner_not_found      = 1
          partner_type_not_found = 2
          OTHERS                 = 3.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING j_1binnad TO gs_danfe-carrier.
      ENDIF.
    ENDIF.

  ENDIF.          "no carrier for Complementars



*----------------------------------------------------------------------*
*    read reference NF                                                 *
*----------------------------------------------------------------------*
  IF gs_danfe-nota_fiscal-docref <> space.
    SELECT SINGLE * FROM j_1bnfdoc INTO *j_1bnfdoc
             WHERE docnum = gs_danfe-nota_fiscal-docref. "#EC CI_ALL_FIELDS_NEEDED
    gs_danfe-nota_fiscal-nf_docref = *j_1bnfdoc-nfnum.
    gs_danfe-nota_fiscal-nf_serref = *j_1bnfdoc-series.
    gs_danfe-nota_fiscal-nf_subref = *j_1bnfdoc-subser.
    gs_danfe-nota_fiscal-nf_datref = *j_1bnfdoc-docdat.
  ENDIF.

*---- ------------------------------------------------------------------*
*    get information about form                                        *
*----------------------------------------------------------------------*

  DATA: lt_print_conf TYPE j_1bb2.

  CALL FUNCTION 'J_1BNF_GET_PRINT_CONF'
    EXPORTING
      headerdata = gt_header
    IMPORTING
      print_conf = lt_print_conf
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  IF sy-subrc IS INITIAL.
    PERFORM check_error.
  ENDIF.

  CHECK gv_retcode IS INITIAL.

*----------------------------------------------------------------------*
*    write texts to TEXTS window                                       *
*----------------------------------------------------------------------*
  DATA: ls_line TYPE tline.
  DATA: lt_table TYPE tdtab_c134.
  DATA: lv_text TYPE string.


  gv_istart = lt_print_conf-totlih.                       " note note 743361

*<--- Incío - Fusion - E.B - 01.11.2022 - UPGRADE SAP #DS4K902759
  DATA(lt_header_msg_tp) = gt_header_msg[].
  CLEAR gt_header_msg[].
  IF gv_nova_versao IS NOT INITIAL.

*<--- Incío - Fusion - E.B - 01.11.2022 - UPGRADE SAP #DS4K902759


    CONSTANTS lc_line_size  TYPE i VALUE 72.
    CONSTANTS lc_line_size2 TYPE i VALUE 132.

    TYPES: ty_line  TYPE c LENGTH lc_line_size.
    TYPES: ty_line2 TYPE c LENGTH lc_line_size2.

    DATA: ls_header_msg TYPE j_1bnfftx,
          lv_msg_string TYPE string,
          lt_line       TYPE TABLE OF ty_line,
          lt_line2      TYPE TABLE OF ty_line2.
    DATA: ls_textos TYPE zssd_nfe_text_item,
          lt_lines  TYPE TABLE OF tdline.

    SELECT * FROM logbr_nf_texts INTO TABLE @DATA(lt_txt) WHERE docnum EQ @gv_docnum. "#EC CI_SEL_DEL
    "AND type   EQ 'P'.

    LOOP AT lt_txt ASSIGNING FIELD-SYMBOL(<fs_txt1>) WHERE ( type EQ 'P' ). "#EC CI_STDSEQ
      "OR ( type EQ 'G' AND itmnum IS NOT INITIAL ). INTO DATA(ls_txt1)

      ls_textos-itmnum = <fs_txt1>-itmnum.
      lv_msg_string    = <fs_txt1>-text.

*--- Quebra o texto em 132 caracteres
      CLEAR: lt_line2[].
      CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
        EXPORTING
          text                = lv_msg_string
          flag_no_line_breaks = 'X'
          line_length         = 132
        TABLES
          text_tab            = lt_line2[].

      LOOP AT lt_line2 ASSIGNING FIELD-SYMBOL(<fs_txt_break2>). "#EC CI_NESTED INTO DATA(ls_txt_break2)
        ls_textos-tdline = <fs_txt_break2>.
        APPEND ls_textos TO  gs_danfe-text_item.
      ENDLOOP.

      "Deleta registro duplicado
      DELETE lt_txt WHERE text = <fs_txt1>-text.         "#EC CI_STDSEQ

    ENDLOOP.


    DATA: lv_lenght     TYPE i,
          lv_lim        TYPE i,
          lv_loop       TYPE i,
          lv_limite_min TYPE i,
          lv_limite_max TYPE i.



    LOOP AT lt_txt ASSIGNING FIELD-SYMBOL(<fs_txt2>) WHERE ( type <> 'P' ). "#EC CI_STDSEQ INTO DATA(ls_txt2)
      IF ( <fs_txt2>-type EQ 'G' AND <fs_txt2>-itmnum  <> '000000' ).
        CONTINUE.
      ENDIF.

      CLEAR: lv_msg_string.
      ls_header_msg-docnum  = <fs_txt2>-docnum.
      ADD 1 TO ls_header_msg-linnum.
      FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN <fs_txt2>-text.
      IF sy-subrc IS INITIAL.
        CLEAR: lt_lines[].
        SPLIT <fs_txt2>-text AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.
        LOOP AT lt_lines  ASSIGNING FIELD-SYMBOL(<fs_lines2>). "#EC CI_NESTED "INTO DATA(ls_lines2).
          CONCATENATE lv_msg_string
                      <fs_lines2>
                 INTO lv_msg_string.
        ENDLOOP.
      ELSE.
        lv_msg_string = <fs_txt2>-text.
      ENDIF.

      lv_lenght = strlen( lv_msg_string ).
      CLEAR: lt_line[].
      CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
        EXPORTING
          text                = lv_msg_string
          flag_no_line_breaks = 'X'
          line_length         = 72
*         LANGU               = SY-LANGU
        TABLES
          text_tab            = lt_line[].

      LOOP AT lt_line ASSIGNING FIELD-SYMBOL(<fs_txt_break>). "#EC CI_NESTED INTO DATA(ls_txt_break)
        ls_header_msg-message = <fs_txt_break>.
        APPEND ls_header_msg TO gt_header_msg.
      ENDLOOP.
      "Deleta registro duplicado
      DELETE lt_txt WHERE text = <fs_txt2>-text.         "#EC CI_STDSEQ
    ENDLOOP.

  ELSE. "Verifica versão

    LOOP AT lt_header_msg_tp ASSIGNING FIELD-SYMBOL(<fs_head>).

      DATA(ls_ref_msg) = VALUE #( gt_refer_msg[ seqnum = <fs_head>-seqnum ] OPTIONAL ). "#EC CI_STDSEQ

      IF <fs_head>-manual = abap_true AND
          ls_ref_msg IS NOT INITIAL.

        gs_danfe-text_item = VALUE #( BASE gs_danfe-text_item
                                      ( itmnum = ls_ref_msg-itmnum
                                        seqnum = <fs_head>-seqnum
                                        tdline = <fs_head>-message  ) ).


      ELSE.
        APPEND <fs_head> TO gt_header_msg.
      ENDIF.
      CLEAR ls_ref_msg.

    ENDLOOP.

  ENDIF. "Verifica versão nova


  LOOP AT gt_item_tax ASSIGNING FIELD-SYMBOL(<fs_item_tax>)." INTO DATA(ls_item_tax).

    CHECK <fs_item_tax>-taxgrp = 'ICMS' AND ( <fs_item_tax>-taxtyp = 'ICM1' OR <fs_item_tax>-taxtyp = 'ICM3' ).

    DATA(ls_tax) = VALUE zssd_nfe_item_tax(
     docnum = <fs_item_tax>-docnum
     itmnum = <fs_item_tax>-itmnum
     taxgrp = <fs_item_tax>-taxgrp
     taxval = <fs_item_tax>-taxval
     base   = COND #( WHEN <fs_item_tax>-base   IS NOT INITIAL THEN <fs_item_tax>-base
                      WHEN <fs_item_tax>-othbas IS NOT INITIAL
                       AND <fs_item_tax>-taxval IS NOT INITIAL
                       AND <fs_item_tax>-base   IS INITIAL
                       THEN <fs_item_tax>-othbas ) ).

    WRITE <fs_item_tax>-rate TO ls_tax-rate.

    COLLECT ls_tax INTO gs_danfe-item_tax.

  ENDLOOP.

  LOOP AT gt_header_msg  ASSIGNING FIELD-SYMBOL(<fs_header_msg>).
    ls_line-tdline = <fs_header_msg>-message.
    IF sy-index LT gv_istart.
      IF sy-index EQ 1.
        gs_danfe-observ1 = <fs_header_msg>-message.
      ELSE.
        CONCATENATE gs_danfe-observ1
                    cl_abap_char_utilities=>cr_lf
                    <fs_header_msg>-message
                    INTO gs_danfe-observ1.
      ENDIF.
      APPEND ls_line TO gs_danfe-text1.
    ELSE.
      IF sy-index EQ gv_istart.
        gs_danfe-observ2 = <fs_header_msg>-message.
      ELSE.
        CONCATENATE gs_danfe-observ2
                    cl_abap_char_utilities=>cr_lf
                    <fs_header_msg>-message
                    INTO gs_danfe-observ2.
      ENDIF.
      APPEND ls_line TO gs_danfe-text2.
    ENDIF.
  ENDLOOP.


  LOOP AT lt_txt ASSIGNING FIELD-SYMBOL(<fs_txt3>) WHERE type <> 'P'. "#EC CI_STDSEQ
    IF ( <fs_txt3>-type EQ 'G' AND <fs_txt3>-itmnum  <> '000000' ).
      CONTINUE.
    ENDIF.
    DELETE  lt_txt WHERE text = <fs_txt3>-text.          "#EC CI_STDSEQ
    CLEAR: lv_msg_string.
    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN <fs_txt3>-text.
    IF sy-subrc IS INITIAL.
      CLEAR: lt_lines[].
      SPLIT <fs_txt3>-text AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.
      LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines3>) . "#EC CI_NESTED
        CONCATENATE lv_msg_string
                    <fs_lines3>
               INTO lv_msg_string.
      ENDLOOP.
    ELSE.
      lv_msg_string = <fs_txt3>-text.
    ENDIF.

    CLEAR: lt_table[].
    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = lv_msg_string
        i_tabline_length = 132
      TABLES
        et_table         = lt_table.

    LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<fs_tablex>). "#EC CI_NESTED INTO DATA(ls_tablex)
      CLEAR ls_line.
      ls_line-tdline = <fs_tablex>.
      APPEND ls_line TO gs_danfe-text1.
    ENDLOOP.

  ENDLOOP.


  DATA(lt_item) = gt_item[].
  DELETE lt_item WHERE refkey IS INITIAL.                "#EC CI_STDSEQ
  SORT lt_item BY refkey refitm.

  DELETE ADJACENT DUPLICATES FROM lt_item COMPARING refkey refitm.

  IF NOT lt_item IS INITIAL.
    SELECT *
    INTO TABLE @DATA(lt_vbrp)
    FROM vbrp
    FOR ALL ENTRIES IN @lt_item
    WHERE vbeln EQ @lt_item-refkey(10)
      AND posnr EQ @lt_item-refitm.

    IF sy-subrc EQ 0.
      SORT lt_vbrp BY vbeln posnr.
    ENDIF.

    DATA(lt_fatura) = lt_vbrp.
    DELETE ADJACENT DUPLICATES FROM lt_fatura COMPARING vbeln.

    DATA(lt_remessa) = lt_vbrp.
    SORT lt_remessa BY vgbel.
    DELETE ADJACENT DUPLICATES FROM lt_remessa COMPARING vgbel.

    DATA(lt_ordem) = lt_vbrp.
    SORT lt_ordem BY aubel.
    DELETE ADJACENT DUPLICATES FROM lt_ordem COMPARING aubel.

    IF NOT lt_ordem IS INITIAL.
      "#2023
      "SELECT *
      SELECT vbkd~vbeln, vbkd~bstkd
      INTO TABLE @DATA(lt_vbkd)
      FROM vbkd
      FOR ALL ENTRIES IN @lt_ordem
      WHERE vbeln EQ @lt_ordem-aubel.                   "#EC CI_SEL_DEL
    ENDIF.

    LOOP AT lt_ordem INTO DATA(ls_ordem).
      SHIFT ls_ordem-aubel LEFT DELETING LEADING '0'.

      IF lv_text IS INITIAL.
        lv_text = TEXT-t06 && ` ` && ls_ordem-aubel."T06  Ordem de Venda:
      ELSE.
        lv_text = lv_text && ` ` && TEXT-t06 && ` ` && ls_ordem-aubel."T06  Ordem de Venda:
      ENDIF.
    ENDLOOP.

    LOOP AT lt_remessa INTO DATA(ls_remessa).
      SHIFT ls_remessa-vgbel LEFT DELETING LEADING '0'.

      IF lv_text IS INITIAL.
        lv_text = TEXT-t07 && ` ` && ls_remessa-vgbel."T07  Fornecimento:
      ELSE.
        lv_text = lv_text && ` ` && TEXT-t07 && ` ` && ls_remessa-vgbel."T07  Fornecimento:
      ENDIF.
    ENDLOOP.

    LOOP AT lt_fatura INTO DATA(ls_fatura).
      SHIFT ls_fatura-vbeln LEFT DELETING LEADING '0'.

      IF lv_text IS INITIAL.
        lv_text = TEXT-t01 && ` ` && ls_fatura-vbeln."T01	Fatura:
      ELSE.
        lv_text = lv_text && ` ` && TEXT-t01 && ` ` && ls_fatura-vbeln."T01	Fatura:
      ENDIF.
    ENDLOOP.

    IF lv_text IS INITIAL.
      lv_text = TEXT-t02 && ` ` && gt_header-docnum."T02  Doc.NFe:
    ELSE.
      lv_text = lv_text && ` ` && TEXT-t02 && ` ` && gt_header-docnum."T02  Doc.NFe:
    ENDIF.

    LOOP AT lt_vbkd ASSIGNING FIELD-SYMBOL(<fs_vbkd>) WHERE NOT bstkd IS INITIAL. "#EC CI_STDSEQ
      IF lv_text IS INITIAL.
        lv_text = TEXT-t03 && ` ` && <fs_vbkd>-bstkd."T03	Ordem de compra:
        DELETE lt_vbkd[] WHERE vbeln = <fs_vbkd>-vbeln. "#EC CI_STDSEQ "IL - Ch.16997 - Fusion - 29.06.2022 15:35:21
      ELSE.
        lv_text = lv_text && ` ` && TEXT-t03 && ` ` && <fs_vbkd>-bstkd."T03	Ordem de compra:
        DELETE lt_vbkd[] WHERE vbeln = <fs_vbkd>-vbeln. "#EC CI_STDSEQ "IL - Ch.16997 - Fusion - 29.06.2022 15:35:33
      ENDIF.
    ENDLOOP.
    CLEAR: lt_table[].

    "dm

    READ TABLE gt_item[] ASSIGNING FIELD-SYMBOL(<fs_nfitem>) INDEX 1.
    .
    IF <fs_nfitem> IS ASSIGNED AND <fs_nfitem>-reftyp = gc_reftyp AND <fs_nfitem>-taxsit = gc_taxsit.
      lv_text = |{ lv_text } { TEXT-t08 } { TEXT-t09 }, ({ TEXT-t10 } { <fs_nfitem>-qbcmonoret } { TEXT-t11 } { <fs_nfitem>-vicmsmonoret }) |.
    ENDIF.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = lv_text
        i_tabline_length = 132
      TABLES
        et_table         = lt_table.

    LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<fs_table>). "INTO DATA(ls_table).
      CLEAR ls_line.
      ls_line-tdline = <fs_table>.
      APPEND ls_line TO gs_danfe-text1.
    ENDLOOP.
  ENDIF.

  LOOP AT gt_item ASSIGNING <fs_item>.


    READ TABLE gt_item_add WITH KEY docnum = <fs_item>-docnum "#EC CI_STDSEQ
                                    itmnum = <fs_item>-itmnum. "#EC CI_SEL_DEL #EC CI_STDSEQ
    IF <fs_item>-netdis < 0.
      <fs_item>-netdis = <fs_item>-netdis * -1.
    ENDIF.

    MOVE <fs_item>-netdis TO gt_header_add-nfdis.

    CLEAR j_1bprnfli.
    MOVE-CORRESPONDING <fs_item> TO j_1bprnfli.
    MOVE-CORRESPONDING gt_item_add TO j_1bprnfli.

*... fill text reference ..............................................*

    LOOP AT gt_refer_msg ASSIGNING FIELD-SYMBOL(<fs_refer_msg>)  WHERE itmnum = <fs_item>-itmnum. "#EC CI_STDSEQ "#EC CI_NESTED
      REPLACE '  ' WITH <fs_refer_msg>-seqnum INTO j_1bprnfli-text_ref.
      REPLACE ' '  WITH ','                 INTO j_1bprnfli-text_ref.
    ENDLOOP.
    REPLACE ', ' WITH '  ' INTO j_1bprnfli-text_ref.

    APPEND j_1bprnfli TO gs_danfe-item.

  ENDLOOP.

  CHECK gv_retcode IS INITIAL.

  MOVE-CORRESPONDING gt_header_add TO gs_danfe-nota_fiscal.

  "valor liquido
  gs_danfe-nota_fiscal-nfnett = gs_danfe-nota_fiscal-nfnet.

  IF NOT lr_badi IS INITIAL.
    CALL METHOD lr_badi->filling_danfe
      CHANGING
        cs_danfe = gs_danfe.
  ENDIF.

  PERFORM call_smartform.

  IF  gs_danfe-nfe-code EQ '100'
   AND gs_danfe-nfe-cancel IS INITIAL
   AND gs_danfe-nfe-conting_s IS INITIAL.

    "Autorizar GRC envio XML e DANFE anexo
*    CALL FUNCTION 'ZF_AUTH_EMAIL_B2B'
*      EXPORTING
*        is_active = gs_danfe-nfe.

  ENDIF.

ENDFORM.                        "smart_sub_printing

*&---------------------------------------------------------------------
*&      Form  GET_CFOP_LENGTH_SMART
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
FORM get_cfop_length_smart USING    uv_bukrs
                                    uv_branch
                                    uv_pstdat
                           CHANGING cv_version         " note 593218
                                    cv_clength
                                    cv_elength
                                    cv_text
                                    cv_region.         " note 593218

  DATA: lv_adress   TYPE addr1_val.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      bukrs             = uv_bukrs
      branch            = uv_branch
    IMPORTING
      address1          = lv_adress
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  cv_region = lv_adress-region.                   " note 593218

  CALL FUNCTION 'J_1B_CFOP_GET_VERSION'
    EXPORTING
      region            = lv_adress-region
      date              = uv_pstdat
    IMPORTING
      version           = cv_version        " note 593218
      extension         = cv_elength
      cfoplength        = cv_clength
      txtdef            = cv_text
    EXCEPTIONS
      date_missing      = 1
      version_not_found = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                             " GET_CFOP_LENGTH_SMART

*&---------------------------------------------------------------------
*&      Form  ENCODING_CFOP_SMART
*&---------------------------------------------------------------------
*       encode the CFOP
*      51234   =>  51234
*      5123A   =>  5123A
*      512345  =>  512345
*      51234A  =>  51234A
*      5123B4  =>  5123B4
*      5123BA  =>  5123BA
*----------------------------------------------------------------------
FORM encoding_cfop_smart  CHANGING cv_cfop.

  DATA: lv_len(1)         TYPE n,
        lv_helpstring(60) TYPE c,
        lv_d              TYPE i.

  lv_helpstring = TEXT-t05."T05  abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ[-<>=!?]

  lv_len = strlen( cv_cfop ).
  IF lv_len = 6.
    CASE cv_cfop(1).
      WHEN 1. lv_d = 0.
      WHEN 2. lv_d = 1.
      WHEN 3. lv_d = 2.
      WHEN 5. lv_d = 3.
      WHEN 6. lv_d = 4.
      WHEN 7. lv_d = 5.
    ENDCASE.
    lv_d = lv_d * 10 + cv_cfop+1(1).
    SHIFT lv_helpstring BY lv_d PLACES.
    MOVE lv_helpstring(1) TO cv_cfop(1).
    cv_cfop+1(4) = cv_cfop+2(4).
    CLEAR cv_cfop+5(1).
  ENDIF.

ENDFORM.                    " ENCODING_CFOP_SMART
*&--------------------------------------------------------------------*
*&      Form  call_smartform
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM call_smartform.

  DATA: lv_subject    TYPE tdtitle,
        lt_otfdata    TYPE ssfcrescl-otfdata,
        lv_getotf     TYPE ssfctrlop-getotf,
        lv_job_output TYPE ssfcrescl.

  gs_output_options-tdimmed       = nast-dimme.
  gs_output_options-tddest        = nast-ldest.
  gs_control_parameters-no_dialog = 'X'.
  gs_control_parameters-preview   = 'X'.
  gs_control_parameters-device    = 'PRINTER'.

  IMPORT getotf TO lv_getotf FROM MEMORY ID 'DANFE_PDF'. "CRIAR PDF

  IF lv_getotf IS NOT INITIAL.
    gs_control_parameters-getotf = abap_true.
    gs_control_parameters-no_dialog = 'X'.
    gs_control_parameters-preview   = abap_false.
  ENDIF.

  CALL FUNCTION gv_fm_name
    EXPORTING
      gs_control_parameters = gs_control_parameters
      gs_output_options     = gs_output_options
      user_settings         = ''
      nota_fiscal           = gs_danfe
      v_contingkey          = gv_contingkey
      v_nfe                 = gv_nfe
      v_nfe1                = gv_nfe1
    IMPORTING
      job_output_info       = lv_job_output
    EXCEPTIONS
      formatting_error      = 1
      internal_error        = 2
      send_error            = 3
      user_canceled         = 4
      OTHERS                = 5.

  IF sy-subrc <> 0.
  ELSEIF lv_getotf IS NOT INITIAL.
    lt_otfdata = lv_job_output-otfdata[].
    EXPORT otfdata FROM lt_otfdata[] TO MEMORY ID 'DANFE_OTF'.
  ENDIF.

ENDFORM.                    "call_smartform

*&---------------------------------------------------------------------*
*&       FORM ENTRY  (MAIN FORM)                                       *
*&---------------------------------------------------------------------*
*       Form for Message Control                                       *
*----------------------------------------------------------------------*
FORM entry USING return_code us_screen.

  CLEAR: gv_retcode.
  screen = us_screen.

  PERFORM smart_sub_printing_boleto.

* check retcode (return code) ------------------------------------------
  IF gv_retcode NE 0.
    return_code = 1.
  ELSE.
    return_code = 0.
  ENDIF.

ENDFORM.                               " ENTRY

*&---------------------------------------------------------------------*
*& Form smart_sub_printing_boleto
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM smart_sub_printing_boleto .
  DATA lv_getotf TYPE ssfctrlop-getotf.

  REFRESH: gv_my_items.

* read the Nota Fiscal
  PERFORM nota_fiscal_read.            " read nota fiscal

* allow print of NF only when NF is not canceled
  PERFORM check_nf_canceled.   " check nota fiscal canceled,442570

* number and update the Nota Fiscal
  CHECK gv_retcode IS INITIAL.

* The NFe to be printed must have an authorization code
  IF gt_header-nfe = 'X'.

*   The NFe to be printed must have an authorization code
    PERFORM check_nfe_authorized.
    CHECK gv_retcode IS INITIAL.

  ENDIF.
* for NFe the DANFE is printed. Numbering has already taken place
* before sending teh XML document to SEFAZ.
* Billing document is updated for NFe with the 9 digit NFe number
  IF gt_header-entrad = 'X' OR  "update only entradas or outgoing NF
     gt_header-direct  = '2'.
*    IF wk_header-printd IS INITIAL AND " not printed.
*       wk_header-nfnum IS INITIAL  AND " without NF number
*       nast-nacha = '1'.               " sent to printer

    "??? aqui busc a a fatura
    PERFORM enqueue_bi_fi.

    CHECK gv_retcode IS INITIAL.
* get NF number only for "normal NFs" NFe has already the number
    IF gt_header-nfe IS INITIAL.
      PERFORM nota_fiscal_number.      " get the next number
    ENDIF.

    IMPORT getotf TO lv_getotf FROM MEMORY ID 'DANFE_PDF'. "CRIAR PDF
    IF lv_getotf IS INITIAL.
      PERFORM call_smartform_boleto.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form call_smartform_boleto
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM call_smartform_boleto .

  DATA: lv_printer      TYPE rspopname,
        lv_buf_id_print TYPE indx_srtfd.

  DATA: ls_param TYPE ssfctrlop,
        ls_key   TYPE zsfi_boleto_ban_key.

  DATA lt_msg TYPE bapiret2_tab.

  SELECT a~empresa,
         a~documento,
         a~exercicio,
         a~parcela
    FROM zi_fi_boleto AS a
   WHERE empresa   = @vbrk-bukrs
     AND documento = @vbrk-belnr
     AND exercicio = @vbrk-gjahr
    INTO TABLE @DATA(lt_boleto).

  IF sy-subrc EQ 0.

    DATA(lo_gerar_boleto) = NEW zclfi_boleto_util( nast-ldest ).

    LOOP AT lt_boleto ASSIGNING FIELD-SYMBOL(<fs_boleto>).

      ls_key = VALUE #(
                        bukrs = <fs_boleto>-empresa
                        belnr = <fs_boleto>-documento
                        gjahr = <fs_boleto>-exercicio
                        buzei = <fs_boleto>-parcela
                ).

      lo_gerar_boleto->gerar_boleto_fat(
        EXPORTING
          is_key = ls_key
        IMPORTING
          et_msg = lt_msg
      ).

    ENDLOOP.

***    " Import da impressora escolhida para a DANFE
***    lv_buf_id_print = 'NFE_PRINTER_' && w_danfe-nota_fiscal-docnum.
***    IMPORT lv_printer = lv_printer FROM MEMORY ID lv_buf_id_print.
***    IF sy-subrc IS INITIAL.
***      ls_param-device = lv_printer.
***    ENDIF.
***
***    ls_param-no_dialog = abap_true.
***    ls_param-preview   = abap_false.
***    ls_param-getotf    = abap_true.


  ENDIF.


ENDFORM.
