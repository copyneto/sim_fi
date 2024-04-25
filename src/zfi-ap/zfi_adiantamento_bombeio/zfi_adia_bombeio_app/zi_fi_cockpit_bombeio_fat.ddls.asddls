@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Faturas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COCKPIT_BOMBEIO_FAT
  as select from ZI_FI_FATURAS_BOMBEIO

  association to parent ZI_FI_COCKPIT_BOMBEIO as _Bombeio on _Bombeio.CompanyCode = $projection.CompanyCode

{
  key CompanyCode,
  key FiscalYear,
  key AccountingDocument,
  key AccountingDocumentItem,
  key Supplier,
      NomeFornecedor,
      Centro,
      NomeCentro,
      PurchaseOrder,
      PurchaseOrderItem,
      SupplierInvoice,
      Bloqueio,
      TextoBloqueio,
      FormPagto,
      TextoFormPagto,
      BancoEmpresa,
      TextoBancoEmpresa,
      Referencia,
      Vencimento,
      Moeda,
      @Semantics.amount.currencyCode: 'Moeda'
      - Valor as Valor,
      _Bombeio
}
