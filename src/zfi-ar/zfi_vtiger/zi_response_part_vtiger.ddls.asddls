@AbapCatalog.sqlViewName: 'ZVRESPVTIGER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Partidas VTIGER'
define view ZI_RESPONSE_PART_VTIGER
  as select from zi_busca_part_vtiger
{
  key Empresa,
  key DocSAP,
  key Item,
  key Exercicio,
      Cliente,
      Referencia,
      Valor,
      ValorTax,
      Stcd1,
      Stcd2,
      divNF
//      left( Referencia ,  abs(divNF) ) as NumNotaFiscal
}
