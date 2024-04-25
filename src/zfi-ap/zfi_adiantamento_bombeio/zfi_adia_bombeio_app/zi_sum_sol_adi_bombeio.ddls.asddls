@AbapCatalog.sqlViewName: 'ZVFISUMSOLAD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar Solicitacoes de adiantamento'
define view ZI_SUM_SOL_ADI_BOMBEIO

  as select from ZI_SUM_SOL_ADI_BOMBEIO_DOC
{
  key CompanyCode   as CompanyCode,
      sum( Amount ) as Amount
}
group by
  CompanyCode
