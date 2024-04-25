@AbapCatalog.sqlViewName: 'ZVFIRAIZCNPJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Raiz CNPJ'
define view ZI_FI_RAIZCNPJ
  as select from kna1
{
  key kunnr                 as Kunnr,
  key substring(stcd1,1,8 ) as raizcnpj

}
