@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda de pesquisa - Status Contabilizado'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_VH_CONTABILIZADOS
  as select from dd07t
{
  key domvalue_l as VHContabilizado,
      ddtext     as Descricao
}

where
      ddlanguage = $session.system_language
  and domname    = 'ZD_CHQ_CONTAB'
