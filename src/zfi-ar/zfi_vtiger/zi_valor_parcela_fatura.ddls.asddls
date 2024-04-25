@AbapCatalog.sqlViewName: 'ZVVALORPARCELA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Calcula valor parcela'
define view ZI_valor_parcela_fatura
  as select from I_BillingDocument as Valor

  association to ZI_QTD_PARCELAS_FATURA as _Qtd on  _Qtd.Empresa   = Valor.CompanyCode
                                                and _Qtd.Exercicio = Valor.FiscalYear
                                                and _Qtd.DocSAP    = Valor.AccountingDocument

{
  key CompanyCode,
  key FiscalYear,
  key AccountingDocument,
      TransactionCurrency,

      division( TotalNetAmount , cast( _Qtd.QtdItens as abap.int4 ), 2 ) as Valor,
      division(  TotalTaxAmount , cast( _Qtd.QtdItens as abap.int4) , 2) as ValorTax
}
