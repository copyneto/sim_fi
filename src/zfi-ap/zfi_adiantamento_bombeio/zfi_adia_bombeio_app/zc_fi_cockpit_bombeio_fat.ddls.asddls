@EndUserText.label: 'Cockpit Faturas  - Projeção'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_FI_COCKPIT_BOMBEIO_FAT
  as projection on ZI_FI_COCKPIT_BOMBEIO_FAT
{
      @EndUserText.label: 'Empresa'
  key CompanyCode,

      @EndUserText.label: 'Exercício'
  key FiscalYear,

      @EndUserText.label: 'Nº Documento'
  key AccountingDocument,

      @EndUserText.label: 'Item'
  key AccountingDocumentItem as AccountingDocumentItemNo, -- Para não utilizar durante navegação

      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['NomeFornecedor']
  key Supplier,

      @EndUserText.label: 'Nome do Fornecedor'
      NomeFornecedor,

      @EndUserText.label: 'Centro'
      Centro,

      @EndUserText.label: 'Nome do Centro'
      @ObjectModel.text.element: ['NomeCentro']
      NomeCentro,

      @EndUserText.label: 'Nº Pedido'
      PurchaseOrder,

      @EndUserText.label: 'Item Pedido'
      PurchaseOrderItem,

      @EndUserText.label: 'Chave referência '
      SupplierInvoice,

      @EndUserText.label: 'Bloqueio'
      @ObjectModel.text.element: ['TextoBloqueio']
      Bloqueio,

      @EndUserText.label: 'Texto do Bloqueio'
      TextoBloqueio,

      @EndUserText.label: 'Forma de pagamento'
      @ObjectModel.text.element: ['TextoFormPagto']
      FormPagto,

      @EndUserText.label: 'Texto da Forma de pagamento'
      TextoFormPagto,

      @EndUserText.label: 'Banco Empresa'
      @ObjectModel.text.element: ['TextoBancoEmpresa']
      BancoEmpresa,

      @EndUserText.label: 'Texto Banco Empresa'
      TextoBancoEmpresa,

      @EndUserText.label: 'Referência'
      Referencia,

      @EndUserText.label: 'Vencimento'
      Vencimento,

      @EndUserText.label: 'Moeda'
      Moeda,

      @EndUserText.label: 'Montante'
      Valor,

      /* Associations */
      _Bombeio : redirected to parent ZC_FI_COCKPIT_BOMBEIO
}
