@AbapCatalog.sqlViewName: 'ZVFIBOLETOINFO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Boleto'
define view zi_fi_boleto_info
  as select from bsid_view as bsid
  association        to t001                      as emp   on  bsid.bukrs = emp.bukrs
  association        to bseg                      as bseg  on  bsid.bukrs = bseg.bukrs
                                                           and bsid.belnr = bseg.belnr
                                                           and bsid.gjahr = bseg.gjahr
                                                           and bsid.buzei = bseg.buzei
  association [1..*] to I_HouseBankAccountLinkage as Link  on  bsid.bukrs = Link.CompanyCode
                                                           and bsid.hbkid = Link.HouseBank

  association        to bsid_view                 as Valor on  Valor.bukrs = bsid.bukrs
                                                           and Valor.rebzg = bsid.belnr
                                                           and Valor.rebzj = bsid.gjahr
                                                           and Valor.rebzz = bsid.buzei

{
  key bsid.bukrs                                                             as Empresa,
  key bsid.belnr                                                             as Documento,
  key bsid.gjahr                                                             as Ano,
  key bsid.buzei                                                             as Item,
      bsid.hbkid                                                             as Banco,
      Link.HouseBankAccount                                                  as HouseBankAccount,
      bsid.zlsch                                                             as formpgto,
      bsid.kunnr                                                             as Cliente,
      bsid.dmbtr                                                             as Valor,
      bsid.xref3                                                             as xref3,
      bsid.xblnr                                                             as xblnr,
      bseg.netdt                                                             as netdt,
      bsid.budat                                                             as budat,
      emp.butxt                                                              as EmpresaTXT,
      Valor.dmbtr                                                            as ValorAbatimento,
      concat(concat(bsid.bukrs, concat(bsid.belnr, bsid.gjahr)), bsid.buzei) as boletoKey,
      substring( Link.BankInternalID, 1,3)                                   as CodBanco

}

where
  bsid.xstov = ' '
