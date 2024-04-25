@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Entity Faturas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_CUSTCHEQUE_COMP_IFAT
  as select from ztfi_custcheq_ft as _Fat

  association to parent ZI_CUSTCHEQUE_COMP_H as _Header on  $projection.Bukrs = _Header.Bukrs
                                                        and $projection.Kunnr = _Header.Kunnr

  association to ZI_FI_FAT_RAIZCNPJ_GROUP    as _FatAdd on  _FatAdd.bukrs    = _Fat.bukrs
                                                        and _FatAdd.Kunnr    = _Fat.kunnr
                                                        and _FatAdd.raizcnpj = _Fat.raizcnpj
                                                        and _FatAdd.belnr    = _Fat.doc
                                                        and _FatAdd.gjahr    = _Fat.gjahr
                                                        and _FatAdd.buzei    = _Fat.buzei

{
  key bukrs       as Bukrs,
  key kunnr       as Kunnr,
  key raizcnpj    as Raizcnpj,
  key doc         as Doc,
  key gjahr       as Gjahr,
  key buzei       as Buzei,

      case _Fat.atribuido
      when 'X'
      then 3
      else 2
      end         as CriticalityAtri,

      case _Fat.atribuido
      when 'X'
       then 'Marcado'
      else 'Não Marcado'
      end         as Atribuido,
      @Semantics.amount.currencyCode: 'moeda'
      wrbtr       as Wrbtr,
      moeda       as Moeda,
      cliente_fat as ClienteFat,
      _FatAdd.blart,
      _FatAdd.xblnr,
      _FatAdd.bschl,
      _FatAdd.umskz,
      _FatAdd.zuonr,
      _Header
}
