@AbapCatalog.sqlViewName: 'ZVFISUMCHE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar valores cheques'
define view ZI_FI_SOMAR_CHEQUES
  as select from ZI_CUSTCHEQUE_COMP_CHEQUES
{
  key Bukrs        as Bukrs,
  key Kunnr        as Kunnr,
  key marcado      as Atribuido,
      sum( Valor ) as Valor

}
group by
  Bukrs,
  Kunnr,
  marcado
