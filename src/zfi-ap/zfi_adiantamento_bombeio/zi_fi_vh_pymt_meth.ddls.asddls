@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Forma de Pagamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_VH_PYMT_METH
  as select from t042z
{
      @UI.lineItem : [{ position: 10, label: 'Forma de pagamento' }]
  key zlsch as zlsch,

      @UI.lineItem : [{ position: 20, label: 'Descrição' }]
      text1 as text1
}

where
  land1 = 'BR'
