@AbapCatalog.sqlViewName: 'ZVFIFATDRCSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar Faturas DRC'
define view ZI_SUM_FATURAS_DRC_BOMBEIO
  as select from ZI_FI_COCKPIT_BOMBEIO_FAT_DRC
{
  key CompanyCode,
      sum( BR_NFNetAmount ) as AMOUNT
}
group by
  CompanyCode
