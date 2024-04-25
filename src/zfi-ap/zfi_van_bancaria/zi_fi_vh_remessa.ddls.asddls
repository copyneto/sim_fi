@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help - Remessa'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_VH_REMESSA
  as select from reguh
{
  @Search.defaultSearchElement: true
  @EndUserText.label: 'Value Help - Execução em:'
  key laufd,
  @Search.defaultSearchElement: true
  @EndUserText.label: 'Value Help - Identificador:'
  key laufi 
}
