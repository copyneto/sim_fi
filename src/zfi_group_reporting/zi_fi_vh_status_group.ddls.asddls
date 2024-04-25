@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View basic - Status group'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZI_FI_VH_STATUS_GROUP
  as select from    dd07l as Objeto
    left outer join dd07t as Text on  Text.domname    = Objeto.domname
                                  and Text.as4local   = Objeto.as4local
                                  and Text.valpos     = Objeto.valpos
                                  and Text.as4vers    = Objeto.as4vers
                                  and Text.ddlanguage = $session.system_language

{

      @EndUserText.label: 'Log'
      @ObjectModel.text.element: ['ObjetoName']
//      @UI: {textArrangement: #TEXT_ONLY}
  key Objeto.domvalue_l as ObjetoId,
      @UI.hidden: true
      Text.ddtext       as ObjetoName
}
where
  Objeto.domname = 'ZD_STATUS_GROUP'
//  and Objeto.as4local = 'A'
