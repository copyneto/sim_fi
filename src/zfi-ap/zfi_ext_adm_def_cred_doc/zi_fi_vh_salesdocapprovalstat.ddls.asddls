@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Status de aprovação do documento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS

define view entity ZI_FI_VH_SALESDOCAPPROVALSTAT
  as select from    dd07l as Objeto
    left outer join dd07t as Text on  Text.domname    = Objeto.domname
                                  and Text.as4local   = Objeto.as4local
                                  and Text.valpos     = Objeto.valpos
                                  and Text.as4vers    = Objeto.as4vers
                                  and Text.ddlanguage = $session.system_language
{
      @EndUserText.label: 'Status'
      @ObjectModel.text.element: ['SalesDocApprovalStatusDesc']
      @UI.lineItem: [{ position: 10 }]
  key cast( Objeto.domvalue_l as sd_apm_approval_status   ) as SalesDocApprovalStatus,

      @Semantics.text: true
      @UI.lineItem: [{ position: 20 }]
      Text.ddtext                                           as SalesDocApprovalStatusDesc
}
where
  Objeto.domname = 'SD_APM_APPROVAL_STATUS'
