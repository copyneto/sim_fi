@AbapCatalog.sqlViewName: 'ZVFIREMDIAC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Remessas do Dia'
define view ZI_FI_REMESSA_BOMBEIO_CAST
  as select from ZI_FI_REMESSA_BOMBEIO
{
  key CompanyCode,
  key PurchaseOrder,
  key PurchaseOrderItem,
  key Remessa,
      Plant,
      PlantName,
      Supplier,
      SupplierName,
      DataRemessa,
      Quantidade,
      QtdUnidade,
      Moeda,
      @Semantics.amount.currencyCode: 'Moeda'
      FLTP_TO_DEC( Montante as wrbtr )          as Montante,
      @Semantics.amount.currencyCode: 'Moeda'
      FLTP_TO_DEC( ValorTotalRemessa as wrbtr ) as ValorTotalRemessa
}
