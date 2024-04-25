@AbapCatalog.sqlViewName: 'ZVFIDEVOBOM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Devoluções/Cancelamentos Bombeio'
define view ZI_FI_DEV_CANC_BOMBEIO
  as select from bkpf_bsak_ddl as _Devo

  association to Fclm_Bseg_Prjk as _Venc on  _Devo.belnr = _Venc.belnr
                                         and _Devo.bukrs = _Venc.bukrs
                                         and _Devo.gjahr = _Venc.gjahr
                                         and _Devo.buzei = _Venc.buzei


{

  key _Devo.bukrs as Empresa,
  key _Devo.lifnr as Fornecedor,
  key _Devo.belnr as Documento,
  key _Devo.gjahr as Exercicio,
  key _Devo.buzei as Item,
      _Devo.bupla as Centro,
      _Devo.zlspr as Bloqueio,
      _Devo.zlspr as FormaPgto,
      _Devo.hbkid as BancoEmpresa,
      _Devo.xblnr as Referencia,
      _Venc.netdt as Vencimento,
      _Devo.waers as Moeda,
      _Devo.dmbtr as Valor

}


where
        _Devo.bschl =  '21'
  and(
        _Devo.zterm <> 'FCAP'
    and _Devo.zterm <> 'FCAV'
  )
  and   _Venc.ktosl =  'WRX'
