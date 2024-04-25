@AbapCatalog.sqlViewName: 'ZVFIFATGRP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Faturas para compensação'
define view ZI_FI_FAT_RAIZCNPJ_GROUP
  as select from    ZI_FI_FAT_RAIZCNPJ_DADOS as _Fat

    left outer join ZI_FI_FAT_CREDITO        as _Cred on  _Fat.belnr = _Cred.belnr
                                                      and _Fat.bukrs = _Cred.bukrs
                                                      and _Fat.gjahr = _Cred.gjahr
                                                      and _Fat.buzei = _Cred.buzei

{
  key _Fat.Kunnr,
  key _Fat.raizcnpj,
  key _Fat.belnr,
  key _Fat.gjahr,
  key _Fat.bukrs,
  key _Fat.buzei,
      _Fat.waers,
      _Fat.wrbtr,
      _Fat.blart,
      _Fat.xblnr,
      _Fat.bschl,
      _Fat.umskz,
      _Fat.zuonr
}
where
  _Cred.belnr is null
