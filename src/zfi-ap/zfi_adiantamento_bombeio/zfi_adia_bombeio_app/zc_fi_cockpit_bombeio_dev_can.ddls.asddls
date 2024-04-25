@EndUserText.label: 'Cockpit Bombeio - Devoluções e Cancel.'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_FI_COCKPIT_BOMBEIO_DEV_CAN
  as projection on ZI_FI_COCKPIT_BOMBEIO_DEV_CAN
{
      @EndUserText.label: 'Empresa'
  key CompanyCode,

      @EndUserText.label: 'Exercício'
  key FiscalYear,

      @EndUserText.label: 'Nº Documento'
  key AccountingDocument,

      @EndUserText.label: 'Item'
  key AccountingDocumentItem as AccountingDocumentItemNo, -- Para não utilizar durante navegação

      @EndUserText.label: 'Chave'
      TextKey,

      @EndUserText.label: 'Fatura relacionada'
      InvoiceReference,

      @EndUserText.label: 'Ano Fatura rel.'
      InvoiceReferenceFiscalYear,

      @EndUserText.label: 'Item Fatura rel.'
      InvoiceItemReference,

      @EndUserText.label: 'Nº Pedido'
      PurchaseOrder,

      @EndUserText.label: 'Item Pedido'
      PurchaseOrderItem,

      @EndUserText.label: 'Faturamento'
      SupplierInvoice,

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

      @EndUserText.label: 'Local de Negócio'
      @ObjectModel.text.element: ['BusinessPlaceDescription']
      BusinessPlace,

      @EndUserText.label: 'Nome do Local de Negócio'
      BusinessPlaceDescription,

      @Consumption.valueHelpDefinition: [{ entity: {name: 'ZI_FI_VH_BANK_ID', element: 'hbkid' } }]

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

      @EndUserText.label: 'Moeda'
      Currency,

      @EndUserText.label: 'Condição de pagamento'
      @ObjectModel.text.element: ['PaymentTermsDescription']
      PaymentTerms,

      @EndUserText.label: 'Texto Condição de pagamento'
      PaymentTermsDescription,

      /* Associations */
      _Bombeio : redirected to parent ZC_FI_COCKPIT_BOMBEIO
}
