@AbapCatalog.sqlViewName: 'ZVFISUMFAT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar valores faturas de cheques'
define view ZI_FI_SOMAR_FATURAS
  as select from ztfi_custcheq_ft
{
  key bukrs           as Bukrs,
  key kunnr           as Kunnr,
  key atribuido       as Atribuido,
      sum(    wrbtr ) as Valor
}
group by
  bukrs,
  kunnr,
  atribuido
