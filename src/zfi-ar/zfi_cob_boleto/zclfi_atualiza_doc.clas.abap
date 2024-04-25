"! Classe global para atualização de documentos financeiros
CLASS zclfi_atualiza_doc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Método especifico para atualizar campo XREF3 na geração do boleto
    "! @parameter iv_chave | Chave
    "! @parameter iv_bank | Banco
    "! @parameter iv_xref3| Nosso nro
    METHODS atualiza_xref3_bank
      IMPORTING  iv_chave TYPE zsfi_boleto_ban_key
                 iv_bank  TYPE bseg-hbkid
                 iv_xref3 TYPE bseg-xref3
      EXCEPTIONS
                 not_update.

    "! Campo fixo para utilizar na BAPI
    CONSTANTS gc_bkpf TYPE acchd-awtyp VALUE 'BKPF'.
    "! Campo fixo para informar o campo de alteração
    CONSTANTS gc_xref3 TYPE fieldname VALUE 'XREF3'.
    "! Campo fixo para banco
    CONSTANTS gc_hbkid TYPE fieldname VALUE 'HBKID'.


  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      "! Tipo para dados da ACDOCA
      BEGIN OF ty_acdoca ,
        awtyp TYPE acdoca-awtyp,
        awref TYPE acdoca-awref,
        aworg TYPE acdoca-aworg,
      END OF ty_acdoca.

    "! Estrutura para dados da ACDOCA
    DATA gs_acdoca TYPE ty_acdoca.

    "! Método para buscar dados da ACDOCA
    "! @parameter iv_chave | Chave
    METHODS get_dados_acdoca
      IMPORTING
        iv_chave TYPE zsfi_boleto_ban_key.
ENDCLASS.



CLASS zclfi_atualiza_doc IMPLEMENTATION.


  METHOD atualiza_xref3_bank.

    DATA: lt_campos TYPE TABLE OF accchg.

    get_dados_acdoca( iv_chave ).

    APPEND VALUE accchg( fdname = gc_xref3
                         newval = iv_xref3 ) TO lt_campos.
    APPEND VALUE accchg( fdname = gc_hbkid
                         newval = iv_bank ) TO lt_campos.

    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_awtyp              = gs_acdoca-awtyp
        i_awref              = gs_acdoca-awref
        i_aworg              = gs_acdoca-aworg
        i_buzei              = iv_chave-buzei
        x_lock               = abap_true
        i_upd_fqm            = abap_true
      TABLES
        t_accchg             = lt_campos
      EXCEPTIONS
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      RAISE not_update.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD get_dados_acdoca.

    SELECT SINGLE awtyp awref aworg
   FROM acdoca
   INTO gs_acdoca
   WHERE rldnr = '0L'
     AND rbukrs = iv_chave-bukrs
     AND belnr = iv_chave-belnr
     AND ryear = iv_chave-gjahr
     AND buzei = iv_chave-buzei.

  ENDMETHOD.
ENDCLASS.
