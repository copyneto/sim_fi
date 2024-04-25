@AbapCatalog.sqlViewName: 'ZVPARTVTIGER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca partidas VTIGER'
define view zi_busca_part_vtiger
  as select from bsid_view as Fat

  association to Fclm_Bseg_Basic         as _Venc on  _Venc.bukrs = Fat.bukrs
                                                  and _Venc.belnr = Fat.belnr
                                                  and _Venc.buzei = Fat.buzei
                                                  and _Venc.gjahr = Fat.gjahr

  association to ZI_valor_parcela_fatura as _Vlr  on  _Vlr.CompanyCode        = Fat.bukrs
                                                  and _Vlr.AccountingDocument = Fat.belnr
                                                  and _Vlr.FiscalYear         = Fat.gjahr

  association to P_AUDIT_AT_KNA1         as _Cli  on  _Cli.bukrs = Fat.bukrs
                                                  and _Cli.kunnr = Fat.kunnr

{
  key Fat.bukrs                                as Empresa,
  key Fat.belnr                                as DocSAP,
  key Fat.buzei                                as Item,
  key Fat.gjahr                                as Exercicio,
      Fat.kunnr                                as Cliente,
      Fat.xblnr                                as Referencia,
      Fat.cpudt                                as DataCriacao,
      _Venc.netdt                              as Vencimento,
      _Vlr.Valor                               as Valor,
      _Vlr.ValorTax                            as ValorTax,
      _Cli.stcd1                               as Stcd1,
      _Cli.stcd2                               as Stcd2,
      cast( instr(Fat.xblnr,'-') as abap.int1) as divNF

      //      substring( Fat.xblnr, 1 , $projection.divnf ) as NumNotaFiscal

}
where
      Fat.shkzg = 'S'
  and Fat.zumsk = ''
