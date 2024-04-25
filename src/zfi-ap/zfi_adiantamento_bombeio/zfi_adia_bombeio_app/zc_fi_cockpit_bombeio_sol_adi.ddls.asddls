@EndUserText.label: 'Cockpit - Solicitações de Adiantam.'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_FI_COCKPIT_BOMBEIO_SOL_ADI
  as projection on ZI_FI_COCKPIT_BOMBEIO_SOL_ADI

  association [0..1] to ZI_FI_VH_BANK_ID   as _HouseBank     on  _HouseBank.bukrs = $projection.CompanyCode
                                                             and _HouseBank.hbkid = $projection.HouseBank

  association [0..1] to ZI_FI_VH_PYMT_METH as _PaymentMethod on  _PaymentMethod.zlsch = $projection.PaymentMethod

{
      @EndUserText.label: 'Empresa'
  key CompanyCode,

      @EndUserText.label: 'Exercício'
  key FiscalYear,

      @EndUserText.label: 'Nº da Solicitação'
  key AccountingDocument,

      @EndUserText.label: 'Item'
  key AccountingDocumentItem as AccountingDocumentItemNo, -- Para não utilizar durante navegação

      @EndUserText.label: 'Chave'
      TextKey,

      @EndUserText.label: 'Status do documento'
      @ObjectModel.text.element: ['AccountingDocumentCategoryName']
      AccountingDocumentCategory,

      @EndUserText.label: 'Texto Status do documento'
      AccountingDocumentCategoryName,

      @EndUserText.label: 'Código do Razão Especial'
      @ObjectModel.text.element: ['SpecialGLCodeName']
      SpecialGLCode,

      @EndUserText.label: 'Texto Código do Razão Especial'
      SpecialGLCodeName,

      @EndUserText.label: 'Chave de lançamento'
      @ObjectModel.text.element: ['PostingKeyName']
      PostingKey,

      @EndUserText.label: 'Texto Chave de lançamento'
      PostingKeyName,

      @EndUserText.label: 'Bloqueio de pagamento'
      @ObjectModel.text.element: ['PaymentBlockingReasonName']
      PaymentBlockingReason,

      @EndUserText.label: 'Texto Bloqueio de pagamento'
      PaymentBlockingReasonName,

      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['SupplierName']
      Supplier,

      @EndUserText.label: 'Nome do Fornecedor'
      SupplierName,

      @EndUserText.label: 'Vencimento'
      NetDueDate,

      @EndUserText.label: 'Data Atual'
      NetDueDateNew,

      @EndUserText.label: 'Local de Negócio'
      @ObjectModel.text.element: ['BusinessPlaceDescription']
      BusinessPlace,

      @EndUserText.label: 'Nome do Local de Negócio'
      BusinessPlaceDescription,

      @Consumption.valueHelpDefinition: [{ entity: {name: 'ZI_FI_VH_BANK_ID', element: 'hbkid' },
                                           additionalBinding: [{ element: 'bukrs', localElement: 'CompanyCode' }] }]

      @EndUserText.label: 'Banco Empresa'
      @ObjectModel.text.element: ['HouseBankName']
      HouseBank,

      @EndUserText.label: 'Nome do Banco Empresa'
      HouseBankName,

      @Consumption.valueHelpDefinition: [{ entity: {name: 'ZI_FI_VH_PYMT_METH', element: 'zlsch' } }]

      @EndUserText.label: 'Forma de pagamento'
      @ObjectModel.text.element: ['PaymentMethodDescription']
      PaymentMethod,

      @EndUserText.label: 'Texto Forma de pagamento'
      PaymentMethodDescription,

      @EndUserText.label: 'Referência'
      DocumentReferenceID,

      @EndUserText.label: 'Montante'
      Amount,

      @EndUserText.label: 'Novo Montante'
      AmountNew,

      @EndUserText.label: 'Moeda'
      Currency,

      /* Associations */
      _Bombeio : redirected to parent ZC_FI_COCKPIT_BOMBEIO,

      _HouseBank,
      _PaymentMethod
}
