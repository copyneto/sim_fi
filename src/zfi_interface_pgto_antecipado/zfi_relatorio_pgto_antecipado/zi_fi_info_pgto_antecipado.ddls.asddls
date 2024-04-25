@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Relat. Solic. Pgto. Antecip'
define root view entity ZI_FI_INFO_PGTO_ANTECIPADO
  as select from ztfi_adto_ov

  association to I_SalesOrder                          as _SalesOrder on  _SalesOrder.SalesOrder = $projection.vbeln
  association to ZI_FI_VH_STATUS_INTF_PGTO             as _VhStatus   on  _VhStatus.DomainValue = $projection.status

  association to ZI_SD_NFE_DOCREF                      as _DocRef     on  _DocRef.PrecedingDocument          = $projection.vbeln
                                                                      and _DocRef.SubsequentDocumentCategory = 'M'

  association to ZI_FI_ORDENS_VALOR_COND_PIX           as _Valor      on  _Valor.SalesOrder = $projection.vbeln

  composition [0..*] of ZC_FI_INFO_PGTO_ANTECIPADO_LOG as _Mensagens

{

  key vbeln                                                                     as vbeln,
      bukrs                                                                     as bukrs,
      kunnr                                                                     as kunnr,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      _Valor.TotalNetAmount                                                     as valor,
      _SalesOrder.TransactionCurrency,
      criado_em                                                                 as criado_em,
      hora_registro                                                             as hora_registro,
      usuario                                                                   as usuario,
      cast( left(doc_adiantamento,10) as belnr_d )                              as Doc_adiantamento,
      cast( right(doc_adiantamento,4) as gjahr )                                as Ano_adiantamento,
      left(doc_compensacao, 10)                                                 as Doc_compensacao,
      left(doc_compensacao_fatura, 10)                                          as DocCompensacaoFatura,
      status                                                                    as status,
      txid                                                                      as Txid,
      flag_cancelamento                                                         as flag_cancelamento,

      _DocRef._BillingDocument.BillingDocument                                  as DocFatura,
      _DocRef._BillingDocument.AccountingDocument                               as DocContabilFatura,
      _DocRef._BillingDocument.FiscalYear                                       as FiscalPeriod,

      _DocRef._BillingDocument._AccountingDocument.DocumentDate                 as DocContabilFatDocDate,
      _DocRef._BillingDocument._AccountingDocument.PostingDate                  as DocContabilFatPstDate,
      _DocRef._BillingDocument._AccountingDocument.AccountingDocumentHeaderText as DocContabilFatTextHead,
      _DocRef._BillingDocument._AccountingDocument.Currency                     as DocContabilFatCurrency,
      _DocRef._BillingDocument._AccountingDocument.AccountingDocumentType       as DocContabilFatType,

      _VhStatus.DomainText                                                      as Status_Txt,
      _SalesOrder._SoldToParty.CustomerName,
      concat_with_space('Ordem de venda:', vbeln, 1)                            as ObjpageTitle,

      case
        when flag_pago = 'X' then 'Pago'
        when status    = '9' then 'Pagamento não realizado'
        else 'Pendente'
      end                                                                       as Status_Pgto,

      case
        when expira_em < tstmp_current_utctimestamp()
         and expira_em is not initial
         and flag_pago is initial
        then 'X'
        else ''
      end                                                                       as IsExpired,

      _Mensagens
}
