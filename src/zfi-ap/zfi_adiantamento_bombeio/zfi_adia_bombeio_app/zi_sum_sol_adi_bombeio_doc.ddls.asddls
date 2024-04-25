@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar Solicitacoes de adiantamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SUM_SOL_ADI_BOMBEIO_DOC

  as select from I_OperationalAcctgDocItem     as _AccDocItem

    inner join   ZI_FI_PARAM_BOMBEIO_CNPJ_RAIZ as _Parameter on _Parameter.Supplier = _AccDocItem.Supplier

  association [0..1] to I_AccountingDocument as _AccDoc on  _AccDoc.CompanyCode        = $projection.CompanyCode
                                                        and _AccDoc.FiscalYear         = $projection.FiscalYear
                                                        and _AccDoc.AccountingDocument = $projection.AccountingDocument
{
  key _AccDocItem.CompanyCode                 as CompanyCode,
  key _AccDocItem.FiscalYear                  as FiscalYear,
  key _AccDocItem.AccountingDocument          as AccountingDocument,
  key _AccDocItem.AccountingDocumentItem      as AccountingDocumentItem,

      @Semantics.amount.currencyCode: 'Currency'
      _AccDocItem.AmountInCompanyCodeCurrency as Amount,
      _AccDocItem.CompanyCodeCurrency         as Currency
}

where
      _AccDocItem.AccountingDocumentCategory = 'S'      -- Partida-memo
  and _AccDocItem.SpecialGLCode              = 'F'      -- Solicitação de adiantamento
  and _AccDocItem.PostingKey                 = '39'     -- Adiantamento crédito
  and _AccDoc.IsReversal                     is initial -- Documento não é de estorno
  and _AccDoc.IsReversed                     is initial -- Documento não está estornado
  and _AccDocItem.ClearingAccountingDocument is initial -- Documento compensado
