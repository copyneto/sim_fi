@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tipo de Mensagem'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZI_CA_VH_MESSAGE_TYPE
  as select from dd07l as _Domain
    join         dd07t as _DomainText on  _DomainText.domname    = _Domain.domname
                                      and _DomainText.as4local   = _Domain.as4local
                                      and _DomainText.valpos     = _Domain.valpos
                                      and _DomainText.as4vers    = _Domain.as4vers
                                      and _DomainText.ddlanguage = $session.system_language
{
      @ObjectModel.text.element: ['MsgText']
      @UI.textArrangement: #TEXT_LAST
  key _Domain.domvalue_l as MsgType,
      _DomainText.ddtext as MsgText
}

where
      _Domain.domname  = 'MPE_MSG_SEVERITY'
  and _Domain.as4local = 'A'
