@AbapCatalog.sqlViewName: 'ZVFIADPGSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar Adiantamentos Pagos'
define view ZI_SUM_ADI_PAG_BOMBEIO
  as select from ZI_FI_COCKPIT_BOMBEIO_ADI_PAG
{
  key CompanyCode,
      sum( Amount ) as Amount
}
group by
  CompanyCode
