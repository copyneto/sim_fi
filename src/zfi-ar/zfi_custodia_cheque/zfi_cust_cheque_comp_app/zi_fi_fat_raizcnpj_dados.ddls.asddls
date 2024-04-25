@AbapCatalog.sqlViewName: 'ZVFIFATRCNPJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca faturas com raiz CNPJ'
define view ZI_FI_FAT_RAIZCNPJ_DADOS
  as select from ZI_FI_RAIZCNPJ as _Raiz

    inner join   bsid           as _Fat on _Raiz.Kunnr = _Fat.kunnr

{
  key _Raiz.Kunnr,
  key _Raiz.raizcnpj,
      _Fat.belnr,
      _Fat.gjahr,
      _Fat.bukrs,
      _Fat.buzei,
      _Fat.waers,
      _Fat.wrbtr,
      _Fat.blart,
      _Fat.xblnr,
      _Fat.bschl,
      _Fat.umskz,
      _Fat.zuonr

}
where
       _Fat.augbl = ''
  and(
       _Fat.bschl = '01'
    or _Fat.bschl = '09'
  )
