@AbapCatalog.sqlViewName: 'ZVFIREMSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar Remessas do dia'
define view ZI_SUM_REM_DIA
  as select from ZI_FI_COCKPIT_BOMBEIO_REM_DIA
{
  key CompanyCode,
      - sum(  ValorTotalRemessa ) as ValorTotalRemessa

}

where
  DataRemessa = $session.system_date
group by
  CompanyCode
