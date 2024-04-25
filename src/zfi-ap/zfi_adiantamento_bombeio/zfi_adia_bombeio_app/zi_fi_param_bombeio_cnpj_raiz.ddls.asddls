@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Parâmetro: Forn. por Raiz CNPJ'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_PARAM_BOMBEIO_CNPJ_RAIZ
  as select from ZI_CA_PARAM_VAL as _Param
    inner join   I_Supplier      as _Forn on _Param.Low = _Forn.TaxNumber1

{
  key _Forn.Supplier          as Supplier,
      max( _Forn.TaxNumber1 ) as TaxNumber1
}
where
      _Param.Modulo = 'FI-AP'
  and _Param.Chave1 = 'BOMBEIO'
  and _Param.Chave2 = 'RAIZCNPJ'
  and _Param.Chave3 = 'FORNECEDORMATRIZ'

group by
  _Forn.Supplier
