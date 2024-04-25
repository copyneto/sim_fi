@EndUserText.label: 'Cockpit Bombeio - Pedidos'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_FI_COCKPIT_BOMBEIO_PED
  as projection on ZI_FI_COCKPIT_BOMBEIO_PED
{
      @EndUserText.label: 'Empresa'
  key CompanyCode,

      @EndUserText.label: 'Nº do Pedido'
  key PurchaseOrder,

      @EndUserText.label: 'Item'
  key PurchaseOrderItem,

      @EndUserText.label: 'Centro'
      @ObjectModel.text.element: ['PlantName']
      Plant,

      @EndUserText.label: 'Nome do Centro'
      PlantName,

      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['SupplierName']
      Supplier,

      @EndUserText.label: 'Nome do Fornecedor'
      SupplierName,

      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['MaterialName']
      Material,

      @EndUserText.label: 'Nome do Material'
      MaterialName,

      @EndUserText.label: 'Quantidade'
      OrderQuantity,

      @EndUserText.label: 'Unidade de Medida'
      PurchaseOrderQuantityUnit,

      @EndUserText.label: 'Inserido em'
      CreationDate,

      /* Associations */
      _Bombeio : redirected to parent ZC_FI_COCKPIT_BOMBEIO
}
