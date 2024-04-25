@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit - Solicitações de Adiantamemto'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COCKPIT_BOMBEIO_SOL_ADI

  as select from I_OperationalAcctgDocItem     as _AccDocItem

    inner join   ZI_FI_PARAM_BOMBEIO_CNPJ_RAIZ as _Parameter on _Parameter.Supplier = _AccDocItem.Supplier

  association        to parent ZI_FI_COCKPIT_BOMBEIO as _Bombeio               on  _Bombeio.CompanyCode = $projection.CompanyCode

  association [0..1] to I_AccountingDocument         as _AccDoc                on  _AccDoc.CompanyCode        = $projection.CompanyCode
                                                                               and _AccDoc.FiscalYear         = $projection.FiscalYear
                                                                               and _AccDoc.AccountingDocument = $projection.AccountingDocument

  association [0..1] to I_BusinessPlace              as _BusinessPlace         on  _BusinessPlace.CompanyCode   = $projection.CompanyCode
                                                                               and _BusinessPlace.BusinessPlace = $projection.BusinessPlace

  association [0..1] to I_PaymentBlockingReason      as _PaymentBlockingReason on  _PaymentBlockingReason.PaymentBlockingReason = $projection.PaymentBlockingReason

  association [0..1] to I_PaymentMethod              as _PaymentMethod         on  _PaymentMethod.Country       = 'BR'
                                                                               and _PaymentMethod.PaymentMethod = $projection.PaymentMethod

  association [0..1] to ZI_FI_AUX_BOMBEIO_TOTAL      as _Total                 on  _Total.CompanyCode = $projection.CompanyCode
{
  key _AccDocItem.CompanyCode                                                   as CompanyCode,
  key _AccDocItem.FiscalYear                                                    as FiscalYear,
  key _AccDocItem.AccountingDocument                                            as AccountingDocument,
  key _AccDocItem.AccountingDocumentItem                                        as AccountingDocumentItem,

      concat_with_space( 'Empresa:',
      concat_with_space( _AccDocItem.CompanyCode,
      concat_with_space( 'Exercício:',
      concat_with_space( _AccDocItem.FiscalYear,
      concat_with_space( 'Doc:',
      concat_with_space( _AccDocItem.AccountingDocument,
      concat_with_space( 'Item:', _AccDocItem.AccountingDocumentItem
      ,1) ,1) ,1) ,1) ,1) ,1) ,1 )                                              as TextKey,

      _AccDocItem.AccountingDocumentCategory                                    as AccountingDocumentCategory,
      _AccDocItem._AccountingDocumentCategory.
      _Text[1:Language=$session.system_language].AccountingDocumentCategoryName as AccountingDocumentCategoryName,
      _AccDocItem.SpecialGLCode                                                 as SpecialGLCode,
      _AccDocItem._SpecialGLCode.
      _Text[1:Language=$session.system_language].SpecialGLCodeName              as SpecialGLCodeName,
      _AccDocItem.PostingKey                                                    as PostingKey,
      _AccDocItem._PostingKey.
      _Text[1:Language=$session.system_language
           and SpecialGLCode = $projection.specialglcode ].PostingKeyName       as PostingKeyName,
      _AccDocItem.PaymentBlockingReason                                         as PaymentBlockingReason,
      _PaymentBlockingReason.
      _Text[1:Language=$session.system_language].PaymentBlockingReasonName      as PaymentBlockingReasonName,

      _AccDocItem.Supplier                                                      as Supplier,
      _AccDocItem._Supplier.SupplierFullName                                    as SupplierName,
      _AccDocItem.NetDueDate                                                    as NetDueDate,

      @EndUserText.label: 'Novo Vencimento'
      $session.system_date                                                      as NetDueDateNew,

      _AccDocItem.BusinessPlace                                                 as BusinessPlace,
      _BusinessPlace.BusinessPlaceDescription                                   as BusinessPlaceDescription,
      _AccDocItem.HouseBank                                                     as HouseBank,
      _AccDocItem._HouseBank._Bank.BankName                                     as HouseBankName,
      _AccDocItem.PaymentMethod                                                 as PaymentMethod,
      _PaymentMethod.
      _Text[1:Language=$session.system_language].PaymentMethodDescription       as PaymentMethodDescription,
      _AccDoc.DocumentReferenceID                                               as DocumentReferenceID,

      @Semantics.amount.currencyCode: 'Currency'
      _AccDocItem.AmountInCompanyCodeCurrency                                   as Amount,

      @EndUserText.label: 'Novo Montante'
      @Semantics.amount.currencyCode: 'Currency'
      abs( _Total.PaymentAmount )                                               as AmountNew,

      _AccDocItem.CompanyCodeCurrency                                           as Currency,

      _AccDoc.IsReversal                                                        as IsReversal,
      _AccDoc.IsReversed                                                        as IsReversed,
      _AccDocItem.ClearingAccountingDocument                                    as ClearingAccountingDocument,

      /* Associations */
      _Bombeio

}
where
      _AccDocItem.AccountingDocumentCategory = 'S'      -- Partida-memo
  and _AccDocItem.SpecialGLCode              = 'F'      -- Solicitação de adiantamento
  and _AccDocItem.PostingKey                 = '39'     -- Adiantamento crédito
  and _AccDoc.IsReversal                     is initial -- Documento não é de estorno
  and _AccDoc.IsReversed                     is initial -- Documento não está estornado
  and _AccDocItem.ClearingAccountingDocument is initial -- Documento compensado
