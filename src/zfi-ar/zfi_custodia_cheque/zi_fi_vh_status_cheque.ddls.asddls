@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search help para campo Status - Custódia de Cheques'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS
@ObjectModel.dataCategory: #VALUE_HELP
define view entity ZI_FI_VH_STATUS_CHEQUE
  as select from    dd07l as Objeto
    left outer join dd07t as Text on  Text.domname    = Objeto.domname
                                  and Text.as4local   = Objeto.as4local
                                  and Text.valpos     = Objeto.valpos
                                  and Text.as4vers    = Objeto.as4vers
                                  and Text.ddlanguage = $session.system_language
{
      @EndUserText.label: 'Status'
      @ObjectModel.text.element: ['ObjetoName']
  key Objeto.domvalue_l as ObjetoId,
      @Semantics.text: true
      Text.ddtext       as ObjetoName
}
where
      Objeto.domname  = 'ZD_STATUS_CHEQUE'
  and Text.ddlanguage = $session.system_language
