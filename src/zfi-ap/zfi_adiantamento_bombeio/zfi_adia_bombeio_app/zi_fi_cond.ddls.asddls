@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cálculo condition ICMS MONOFÁSICO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COND
  as select from ZI_FI_COCKPIT_BOMBEIO_PED as _Ped
    inner join   A_PurOrdPricingElement    as _Pric on  _Ped.PurchaseOrder     = _Pric.PurchaseOrder
                                                    and _Ped.PurchaseOrderItem = _Pric.PurchaseOrderItem
                                                    and _Pric.ConditionType    = 'ZCM8'

{
  key   _Ped.CompanyCode,
  key   _Ped.PurchaseOrder,
  key   _Pric.PurchaseOrderItem,
        cast( _Pric.ConditionAmount as abap.fltp )
                        / cast( _Ped.OrderQuantity as abap.fltp ) as Montante,
        @Semantics.amount.currencyCode: 'TransactionCurrency'
        _Pric.ConditionAmount,
        _Pric.TransactionCurrency
}
