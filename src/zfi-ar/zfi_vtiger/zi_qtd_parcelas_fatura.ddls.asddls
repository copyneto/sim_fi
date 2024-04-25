@AbapCatalog.sqlViewName: 'ZVQTDPARCELAS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Quantidade de parcelas'
define view ZI_QTD_PARCELAS_FATURA
  as select from bsid_view as Fat
{

  key Fat.bukrs        as Empresa,
  key Fat.belnr        as DocSAP,
  key Fat.gjahr        as Exercicio,
      max( Fat.buzei ) as QtdItens


}
where
      Fat.shkzg = 'S'
  and Fat.zumsk = ''
group by
  Fat.bukrs,
  Fat.belnr,
  Fat.gjahr
