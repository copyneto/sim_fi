@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Adiantamento de Bombeio - Pagos'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COCKPIT_BOMBEIO_ADI_PAG

  as select from bkpf_bsik_ddl                 as _Pago

    inner join   I_OperationalAcctgDocItem     as _AccDocItem on  _AccDocItem.CompanyCode            = _Pago.bukrs
                                                              and _AccDocItem.FiscalYear             = _Pago.gjahr
                                                              and _AccDocItem.AccountingDocument     = _Pago.belnr
                                                              and _AccDocItem.AccountingDocumentItem = _Pago.buzei

    inner join   ZI_FI_PARAM_BOMBEIO_CNPJ_RAIZ as _Parameter  on _Parameter.Supplier = _AccDocItem.Supplier

  association        to parent ZI_FI_COCKPIT_BOMBEIO as _Bombeio               on  _Bombeio.CompanyCode = $projection.CompanyCode

  association [0..1] to I_AccountingDocument         as _AccDoc                on  _AccDoc.CompanyCode        = $projection.CompanyCode
                                                                               and _AccDoc.FiscalYear         = $projection.FiscalYear
                                                                               and _AccDoc.AccountingDocument = $projection.AccountingDocument

  association [0..1] to I_BusinessPlace              as _BusinessPlace         on  _BusinessPlace.CompanyCode   = $projection.CompanyCode
                                                                               and _BusinessPlace.BusinessPlace = $projection.BusinessPlace

  association [0..1] to I_PaymentBlockingReason      as _PaymentBlockingReason on  _PaymentBlockingReason.PaymentBlockingReason = $projection.PaymentBlockingReason

  association [0..1] to I_PaymentMethod              as _PaymentMethod         on  _PaymentMethod.Country       = 'BR'
                                                                               and _PaymentMethod.PaymentMethod = $projection.PaymentMethod
{

  key _Pago.bukrs                                                          as CompanyCode,
  key _Pago.gjahr                                                          as FiscalYear,
  key _Pago.belnr                                                          as AccountingDocument,
  key _Pago.buzei                                                          as AccountingDocumentItem,

      concat_with_space( 'Empresa:',
      concat_with_space( _AccDocItem.CompanyCode,
      concat_with_space( 'Exercício:',
      concat_with_space( _AccDocItem.FiscalYear,
      concat_with_space( 'Doc:',
      concat_with_space( _AccDocItem.AccountingDocument,
      concat_with_space( 'Item:', _AccDocItem.AccountingDocumentItem
      ,1) ,1) ,1) ,1) ,1) ,1) ,1 )                                         as TextKey,

      _AccDocItem.Supplier                                                 as Supplier,
      _AccDocItem._Supplier.SupplierFullName                               as SupplierName,
      _AccDocItem.BusinessPlace                                            as BusinessPlace,
      _BusinessPlace.BusinessPlaceDescription                              as BusinessPlaceDescription,
      _AccDocItem.PaymentBlockingReason                                    as PaymentBlockingReason,
      _PaymentBlockingReason.
      _Text[1:Language=$session.system_language].PaymentBlockingReasonName as PaymentBlockingReasonName,
      _AccDocItem.PaymentMethod                                            as PaymentMethod,
      _PaymentMethod.
      _Text[1:Language=$session.system_language].PaymentMethodDescription  as PaymentMethodDescription,
      _AccDocItem.HouseBank                                                as HouseBank,
      _AccDocItem._HouseBank._Bank.BankName                                as HouseBankName,
      _AccDoc.DocumentReferenceID                                          as DocumentReferenceID,
      _AccDocItem.NetDueDate                                               as NetDueDate,
      @Semantics.amount.currencyCode: 'Currency'
      abs( _AccDocItem.AmountInCompanyCodeCurrency  )                      as Amount,
      _AccDocItem.CompanyCodeCurrency                                      as Currency,

      /* Associations */
      _Bombeio

}
where
      _Pago.umsks        = 'A'
  and _Pago.bschl        = '29'
  and _AccDoc.IsReversal is initial -- Documento não é de estorno
  and _AccDoc.IsReversed is initial -- Documento não está estornado
