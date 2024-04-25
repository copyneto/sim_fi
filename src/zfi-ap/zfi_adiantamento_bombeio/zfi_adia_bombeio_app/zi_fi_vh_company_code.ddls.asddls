@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Empresa'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_VH_COMPANY_CODE
  as select from I_CompanyCode
{
      @UI.lineItem:        [{ position: 10 }]
      @UI.identification:  [{ position: 10 }]
  key CompanyCode,

      @UI.lineItem:        [{ position: 20 }]
      @UI.identification:  [{ position: 20 }]
      CompanyCodeName
}
