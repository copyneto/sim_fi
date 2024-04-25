@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valor ordem para geração pix'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_ORDENS_VALOR_COND_PIX
  as select from I_SalesOrderItemPricingElement as _Cond

    inner join   ZI_CA_PARAM_VAL                as _TipoCondPreco on  _TipoCondPreco.Modulo = 'FI-AR'
                                                                  and _TipoCondPreco.Chave1 = 'A27'
                                                                  and _TipoCondPreco.Chave2 = 'CONDICAO_PRICING'
                                                                  and _TipoCondPreco.Low    = _Cond.ConditionType

{
  _Cond.SalesOrder,
  sum( cast( _Cond.ConditionAmount as abap.dec(13,3) ) ) as TotalNetAmount
}
group by
  _Cond.SalesOrder
