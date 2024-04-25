@AbapCatalog.sqlViewName: 'ZVFIDEVSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar Devoluções Cancelamentos'
define view ZI_SUM_DEV_CANC_BOMBEIO
  as select from ZI_FI_COCKPIT_BOMBEIO_DEV_CAN
{
  key CompanyCode,
      sum( Amount ) as Amount
}
group by
  CompanyCode
