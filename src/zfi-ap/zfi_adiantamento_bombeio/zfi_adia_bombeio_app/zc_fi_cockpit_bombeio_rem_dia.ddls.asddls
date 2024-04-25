@EndUserText.label: 'Cockpit Remessas do dia - Projeção'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_FI_COCKPIT_BOMBEIO_REM_DIA
  as projection on ZI_FI_COCKPIT_BOMBEIO_REM_DIA
{

      @EndUserText.label: 'Empresa'
  key CompanyCode,

      @EndUserText.label: 'Nº do Pedido'
  key PurchaseOrder,

      @EndUserText.label: 'Item'
  key PurchaseOrderItem,

      @EndUserText.label: 'Remessa'
  key Remessa,

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

      @EndUserText.label: 'Data Remessa'
      DataRemessa,

      @EndUserText.label: 'Unidade de Medida'
      QtdUnidade,


      @EndUserText.label: 'Quantidade'
      Quantidade,

      Moeda,

      @EndUserText.label: 'Valor Unitário'
      Montante,

      @EndUserText.label: 'Valor Total Remessa'
      ValorTotalRemessa,

      /* Associations */
      _Bombeio : redirected to parent ZC_FI_COCKPIT_BOMBEIO
}
