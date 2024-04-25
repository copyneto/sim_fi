@AbapCatalog.sqlViewName: 'ZVFIFATCRED'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Faturas com credito'
define view ZI_FI_FAT_CREDITO
  as select from bsid as _Fat

    inner join   bsid as _Cred on  _Cred.bukrs = _Fat.bukrs
                               and _Cred.rebzg = _Fat.belnr
                               and _Cred.rebzj = _Fat.gjahr
                               and _Cred.rebzz = _Fat.buzei
                               and _Cred.rebzt = 'G'

{
  key _Fat.belnr,
  key _Fat.bukrs,
  key _Fat.gjahr,
  key _Fat.buzei,
      _Cred.belnr as DocCred,
      _Cred.gjahr as ExecCred,
      _Cred.buzei as ItemCred

}
