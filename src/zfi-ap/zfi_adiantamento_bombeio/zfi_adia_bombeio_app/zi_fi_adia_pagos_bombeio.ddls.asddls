@AbapCatalog.sqlViewName: 'ZVFIADIPAGO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Adiantamentos Pagos'
define view zi_fi_adia_pagos_bombeio
  as select from bkpf_bsak_ddl as _Pago

  association to Fclm_Bseg_Prjk as _Venc  on  _Pago.belnr = _Venc.belnr
                                          and _Pago.bukrs = _Venc.bukrs
                                          and _Pago.gjahr = _Venc.gjahr
                                          and _Pago.buzei = _Venc.buzei

  association to I_Supplier     as _Forn  on  _Pago.lifnr = _Forn.Supplier

  association to I_Plant        as _Plant on  _Pago.bupla = _Plant.Plant

{

  key _Pago.bukrs            as Empresa,
  key _Pago.lifnr            as Fornecedor,
  key _Pago.belnr            as Documento,
  key _Pago.gjahr            as Exercicio,
  key _Pago.buzei            as Item,
      _Forn.SupplierFullName as NomeFornecedor,
      _Pago.bupla            as Centro,
      _Plant.PlantName       as NomeCentro,
      _Pago.zlspr            as Bloqueio,
      _Pago.zlsch            as FormaPgto,
      _Pago.hbkid            as BancoEmpresa,
      _Pago.xblnr            as Referencia,
      _Venc.netdt            as Vencimento,
      _Pago.waers            as Moeda,
      _Pago.dmbtr            as Valor

}

where
      _Pago.umsks = 'A'
  and _Pago.bschl = '29'
