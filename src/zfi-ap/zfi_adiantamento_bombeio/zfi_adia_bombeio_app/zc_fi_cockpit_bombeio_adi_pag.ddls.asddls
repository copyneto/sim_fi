@EndUserText.label: 'Cockpit Adiantamento de Bombeio - Pagos'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_FI_COCKPIT_BOMBEIO_ADI_PAG
  as projection on ZI_FI_COCKPIT_BOMBEIO_ADI_PAG
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
      
      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['SupplierName']
      Supplier,
      
      @EndUserText.label: 'Nome do Fornecedor'
      SupplierName,
      
      @EndUserText.label: 'Centro'
      @ObjectModel.text.element: ['BusinessPlaceDescription']
      BusinessPlace,
      
      @EndUserText.label: 'Nome do Centro'
      BusinessPlaceDescription,
      
      @EndUserText.label: 'Bloqueio'
      @ObjectModel.text.element: ['PaymentBlockingReasonName']
      PaymentBlockingReason,
      
      @EndUserText.label: 'Texto do Bloqueio'
      PaymentBlockingReasonName,
      
      @EndUserText.label: 'Forma de pagamento'
      @ObjectModel.text.element: ['PaymentMethodDescription']
      PaymentMethod,
      
      @EndUserText.label: 'Texto da Forma de pagamento'
      PaymentMethodDescription,
      
      @EndUserText.label: 'Banco Empresa'
      @ObjectModel.text.element: ['HouseBankName']
      HouseBank,
      
      @EndUserText.label: 'Texto Banco Empresa'
      HouseBankName,
      
      @EndUserText.label: 'Referência'
      DocumentReferenceID,
      
      @EndUserText.label: 'Vencimento'
      NetDueDate,
      
      @EndUserText.label: 'Montante'
      Amount,
      
      @EndUserText.label: 'Moeda'
      Currency,
      
      /* Associations */
      _Bombeio : redirected to parent ZC_FI_COCKPIT_BOMBEIO
}
