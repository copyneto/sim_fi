@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Parâmetro: Fornecedores por Centro'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_PARAM_BOMBEIO_CNPJ_CENT
  as select from ZI_CA_PARAM_VAL as _Param
    inner join   I_Supplier      as _Forn on _Param.High = _Forn.TaxNumber1

{
  key _Forn.Supplier          as Supplier,
      max( _Forn.TaxNumber1 ) as TaxNumber1
}
where
      _Param.Modulo = 'FI-AP'
  and _Param.Chave1 = 'BOMBEIO'
  and _Param.Chave2 = 'CENTRO'
  and _Param.Chave3 = 'CNPJFORNECEDORES'

group by
  _Forn.Supplier
