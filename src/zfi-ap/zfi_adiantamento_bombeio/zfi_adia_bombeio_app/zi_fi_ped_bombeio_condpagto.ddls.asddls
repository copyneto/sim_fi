@AbapCatalog.sqlViewName: 'ZVFIPEDCDPGT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca pedidos Cond Pagto'
define view ZI_FI_PED_BOMBEIO_CONDPAGTO
  as select distinct from I_PurchaseOrder as _Pedido

    inner join            ZI_CA_PARAM_VAL as _CondPagto on  _CondPagto.Modulo = 'FI-AP'
                                                        and _CondPagto.Chave1 = 'BOMBEIO'
                                                        and _CondPagto.Chave2 = 'CONDICAOPGTO'
                                                        and _CondPagto.Low    = _Pedido.PaymentTerms
{
  key _Pedido.CompanyCode   as CompanyCode,
  key _Pedido.PurchaseOrder as PurchaseOrder,
      _Pedido.PaymentTerms  as PaymentTerms
}
