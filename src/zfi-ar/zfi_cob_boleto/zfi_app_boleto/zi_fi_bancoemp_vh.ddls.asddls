@AbapCatalog.sqlViewName: 'ZVFIBKEMPVH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda de pesquisa - Banco empresa'
define view ZI_FI_BANCOEMP_VH
  as select from V_T012t_Ddl
{
  key spras,
  key bukrs,
  key hbkid,
  key hktid,
      text1
}
where
  spras = $session.system_language
