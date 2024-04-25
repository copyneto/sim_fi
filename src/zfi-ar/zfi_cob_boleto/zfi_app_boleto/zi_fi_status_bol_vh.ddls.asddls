@AbapCatalog.sqlViewName: 'ZVSTBOLETOVH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda de pesquisa - Status Boleto'
define view ZI_FI_STATUS_BOL_VH
  as select from dd07t
{
  key domvalue_l as Domname,
      ddtext     as Ddtext
}

where
      domname    = 'ZD_FI_STATUS_BOL'
  and ddlanguage = $session.system_language
