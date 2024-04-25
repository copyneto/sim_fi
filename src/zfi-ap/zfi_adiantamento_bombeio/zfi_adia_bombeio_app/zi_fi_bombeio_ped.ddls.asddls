@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Bombeio - Pedidos'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_BOMBEIO_PED
  as select from    I_PurchaseOrderItem         as _PurchaseOrderItem

    left outer join ZI_FI_PED_BOMBEIO_CONDPAGTO as _Excluir on  _PurchaseOrderItem.CompanyCode   = _Excluir.CompanyCode
                                                            and _PurchaseOrderItem.PurchaseOrder = _Excluir.PurchaseOrder

  association [1..1] to I_PurchaseOrder as _PurchaseOrder on _PurchaseOrder.PurchaseOrder = $projection.PurchaseOrder

{
  key _PurchaseOrder.CompanyCode                                                           as CompanyCode,
  key _PurchaseOrderItem.PurchaseOrder                                                     as PurchaseOrder,
  key _PurchaseOrderItem.PurchaseOrderItem                                                 as PurchaseOrderItem,
      _PurchaseOrderItem.Plant                                                             as Plant,
      _PurchaseOrderItem._Plant.PlantName                                                  as PlantName,
      _PurchaseOrder.Supplier                                                              as Supplier,
      _PurchaseOrder._Supplier.SupplierFullName                                            as SupplierName,
      _PurchaseOrderItem.Material                                                          as Material,
      _PurchaseOrderItem._Material._Text[1:Language=$session.system_language].MaterialName as MaterialName,
      @Semantics.quantity.unitOfMeasure: 'PurchaseOrderQuantityUnit'
      _PurchaseOrderItem.OrderQuantity                                                     as OrderQuantity,
      _PurchaseOrderItem.PurchaseOrderQuantityUnit                                         as PurchaseOrderQuantityUnit,
      _PurchaseOrder.CreationDate                                                          as CreationDate,
      _PurchaseOrderItem.PurchasingDocumentDeletionCode                                    as PurchasingDocumentDeletionCode,
      _PurchaseOrder.PaymentTerms                                                          as PaymentTerms,
      case when _Excluir.PurchaseOrder is not initial
         then 'X'
         else '' end                                                                       as ExcluirCondPagto

}
