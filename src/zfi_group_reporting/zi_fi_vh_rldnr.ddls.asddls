@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View basic - RLDNR'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_VH_RLDNR
  as select from acdocu
{
  key  rldnr  as Rldnr,
  key  rdimen as Rdimen,
  key  ryear  as Ryear,
  key  docnr  as Docnr,
  key  docln  as Docln
}
