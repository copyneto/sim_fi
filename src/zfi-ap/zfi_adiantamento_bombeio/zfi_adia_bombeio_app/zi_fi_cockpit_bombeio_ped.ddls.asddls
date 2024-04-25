@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Bombeio - Pedidos'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COCKPIT_BOMBEIO_PED
  as select distinct from ZI_FI_BOMBEIO_PED             as _PurchaseOrder

    inner join            ZI_FI_PARAM_BOMBEIO_CNPJ_CENT as _Parameter on _Parameter.Supplier = _PurchaseOrder.Supplier

  association to parent ZI_FI_COCKPIT_BOMBEIO as _Bombeio on _Bombeio.CompanyCode = $projection.CompanyCode


{
  key _PurchaseOrder.CompanyCode               as CompanyCode,
  key _PurchaseOrder.PurchaseOrder             as PurchaseOrder,
  key _PurchaseOrder.PurchaseOrderItem         as PurchaseOrderItem,
      _PurchaseOrder.Plant                     as Plant,
      _PurchaseOrder.PlantName                 as PlantName,
      _PurchaseOrder.Supplier                  as Supplier,
      _PurchaseOrder.SupplierName              as SupplierName,
      _PurchaseOrder.Material                  as Material,
      _PurchaseOrder.MaterialName              as MaterialName,
      @Semantics.quantity.unitOfMeasure: 'PurchaseOrderQuantityUnit'
      _PurchaseOrder.OrderQuantity             as OrderQuantity,
      _PurchaseOrder.PurchaseOrderQuantityUnit as PurchaseOrderQuantityUnit,
      _PurchaseOrder.CreationDate              as CreationDate,

      /* Associations */
      _Bombeio
}
where
      _PurchaseOrder.PurchasingDocumentDeletionCode is initial
  and _PurchaseOrder.ExcluirCondPagto               = ''
