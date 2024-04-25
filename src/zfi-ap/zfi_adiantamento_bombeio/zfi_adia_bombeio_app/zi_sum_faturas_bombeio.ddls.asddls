@AbapCatalog.sqlViewName: 'ZVFIFATSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar Faturas'
define view ZI_SUM_FATURAS_BOMBEIO
  as select from ZI_FI_COCKPIT_BOMBEIO_FAT
{
  key CompanyCode,
      sum( Valor ) as Amount
}
group by
  CompanyCode
