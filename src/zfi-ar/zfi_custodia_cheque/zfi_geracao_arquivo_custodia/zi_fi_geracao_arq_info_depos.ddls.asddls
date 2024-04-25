@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Interf. - Busca de Informações de Depósito'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_GERACAO_ARQ_INFO_DEPOS
  as select from bsid_view as _Bsid
  association [1..1] to t012k as _T012K on  _T012K.bukrs = _Bsid.bukrs
                                        and _T012K.hktid = _Bsid.hktid
                                        and _T012K.hbkid = _Bsid.hbkid
{
  key _Bsid.bukrs,
  key _Bsid.belnr,
  key _Bsid.gjahr,
  key _Bsid.kunnr,
  key _Bsid.hbkid,
  key _Bsid.hktid,
      _T012K.bankn
}
