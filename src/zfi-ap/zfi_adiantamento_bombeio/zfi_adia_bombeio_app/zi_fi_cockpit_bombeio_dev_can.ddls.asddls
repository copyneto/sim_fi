@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Bombeio - Devoluções e Cancel.'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COCKPIT_BOMBEIO_DEV_CAN

  as select from I_OperationalAcctgDocItem     as _AccDocItem

    inner join   ZI_FI_PARAM_BOMBEIO_CNPJ_CENT as _Parameter on _Parameter.Supplier = _AccDocItem.Supplier

  association        to parent ZI_FI_COCKPIT_BOMBEIO as _Bombeio               on  _Bombeio.CompanyCode = $projection.CompanyCode

  association [0..1] to I_AccountingDocument         as _AccDoc                on  _AccDoc.CompanyCode        = $projection.CompanyCode
                                                                               and _AccDoc.FiscalYear         = $projection.FiscalYear
                                                                               and _AccDoc.AccountingDocument = $projection.AccountingDocument

  association        to ZI_FI_AUX_ACCDOC_TO_PURCHASE as _PurchaseOrder         on  _PurchaseOrder.AccountingDocument = $projection.AccountingDocument
                                                                               and _PurchaseOrder.CompanyCode        = $projection.CompanyCode
                                                                               and _PurchaseOrder.FiscalYear         = $projection.FiscalYear

  association        to ZI_FI_AUX_ACCDOC_TO_SUPINV   as _SupplierInvoice       on  _SupplierInvoice.AccountingDocument     = $projection.AccountingDocument
                                                                               and _SupplierInvoice.CompanyCode            = $projection.CompanyCode
                                                                               and _SupplierInvoice.FiscalYear             = $projection.FiscalYear
                                                                               and _SupplierInvoice.AccountingDocumentItem = $projection.AccountingDocumentItem

  association [0..1] to I_BusinessPlace              as _BusinessPlace         on  _BusinessPlace.CompanyCode   = $projection.CompanyCode
                                                                               and _BusinessPlace.BusinessPlace = $projection.BusinessPlace

  association [0..1] to I_PaymentBlockingReason      as _PaymentBlockingReason on  _PaymentBlockingReason.PaymentBlockingReason = $projection.PaymentBlockingReason

  association [0..1] to I_PaymentMethod              as _PaymentMethod         on  _PaymentMethod.Country       = 'BR'
                                                                               and _PaymentMethod.PaymentMethod = $projection.PaymentMethod

  association [0..1] to I_PaymentTerms               as _PaymentTerms          on  _PaymentTerms.PaymentTerms = $projection.PaymentTerms



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

      _AccDocItem.InvoiceReference                                              as InvoiceReference,
      _AccDocItem.InvoiceReferenceFiscalYear                                    as InvoiceReferenceFiscalYear,
      _AccDocItem.InvoiceItemReference                                          as InvoiceItemReference,

      _PurchaseOrder.PurchaseOrder                                              as PurchaseOrder,
      _PurchaseOrder.PurchaseOrderItem                                          as PurchaseOrderItem,

      _SupplierInvoice.SupplierInvoice                                          as SupplierInvoice,

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
      _AccDocItem.BusinessPlace                                                 as BusinessPlace,
      _BusinessPlace.BusinessPlaceDescription                                   as BusinessPlaceDescription,
      _AccDocItem.HouseBank                                                     as HouseBank,
      _AccDocItem._HouseBank._Bank.BankName                                     as HouseBankName,
      _AccDocItem.PaymentMethod                                                 as PaymentMethod,
      _PaymentMethod.
      _Text[1:Language=$session.system_language].PaymentMethodDescription       as PaymentMethodDescription,
      _AccDoc.DocumentReferenceID                                               as DocumentReferenceID,

      @Semantics.amount.currencyCode: 'Currency'
      abs( _AccDocItem.AmountInCompanyCodeCurrency  )                           as Amount,
      _AccDocItem.CompanyCodeCurrency                                           as Currency,

      _AccDocItem.PaymentTerms                                                  as PaymentTerms,
      _PaymentTerms.
       _Text[1:Language=$session.system_language].PaymentTermsDescription       as PaymentTermsDescription,

      _AccDoc.IsReversal                                                        as IsReversal,
      _AccDoc.IsReversed                                                        as IsReversed,

      /* Associations */
      _Bombeio

}
where
      _AccDocItem.PostingKey                 =  '21'   -- Nota de crédito
  and _AccDocItem.PaymentTerms               <> 'FCAP' -- Compra à prazo
  and _AccDocItem.PaymentTerms               <> 'FCAV' -- Compra à vista
  //  and _AccDoc.IsReversal                     is not initial -- Documento é de estorno
  and _AccDocItem.ClearingAccountingDocument =  ''
