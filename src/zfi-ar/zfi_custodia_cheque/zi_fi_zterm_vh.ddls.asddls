@AbapCatalog.sqlViewName: 'ZVZTERMVH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda de pesquisa - ZTERM'
define view zi_fi_ZTERM_VH
  as select from t052u
{
  key zterm as Zterm,
      text1 as Text1
}
where
  spras = $session.system_language
