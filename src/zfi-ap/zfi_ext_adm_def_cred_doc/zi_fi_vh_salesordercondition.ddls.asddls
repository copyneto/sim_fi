@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help - Nº condição do documento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_VH_SALESORDERCONDITION
  as select from I_SalesOrder
{
      @UI.lineItem: [{ position: 10 }]
  key SalesOrderCondition
}
where
  SalesOrderCondition is not initial
group by
  SalesOrderCondition
