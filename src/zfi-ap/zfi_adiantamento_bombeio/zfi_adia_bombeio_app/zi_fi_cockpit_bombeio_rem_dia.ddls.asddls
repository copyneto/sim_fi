@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Remessas do dia'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COCKPIT_BOMBEIO_REM_DIA
  as select from ZI_FI_REMESSA_BOMBEIO

  association to parent ZI_FI_COCKPIT_BOMBEIO as _Bombeio on _Bombeio.CompanyCode = $projection.CompanyCode

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
      QtdUnidade,
      @Semantics.quantity.unitOfMeasure: 'QtdUnidade'
      Quantidade,
      Moeda,
      //      @Semantics.amount.currencyCode: 'Moeda'
      FLTP_TO_DEC( Montante as abap.dec( 15, 6 ) ) as Montante,

      @Semantics.amount.currencyCode: 'Moeda'
      FLTP_TO_DEC( ValorTotalRemessa as wrbtr )    as ValorTotalRemessa,

      /* Associations */
      _Bombeio
}
//where
//  DataRemessa = $session.system_date
