@AbapCatalog.sqlViewName: 'ZVPEDBOMBEIO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados pedidos bombeio'
define view ZI_FI_PEDIDOS_BOMBEIO
  as select from A_PurchaseOrderItem as _Item
  association [0..*] to A_PurchaseOrder        as _Pedido on  _Pedido.PurchaseOrder = _Item.PurchaseOrder
  association [0..*] to A_PurOrdPricingElement as _Preco  on  _Item.PurchaseOrder     = _Preco.PurchaseOrder
                                                          and _Item.PurchaseOrderItem = _Preco.PurchaseOrderItem
                                                          and _Preco.ConditionType    = 'PBXX'
{
  key _Item.PurchaseOrder             as DocCompra,
  key _Item.PurchaseOrderItem         as Item,
      _Pedido.CompanyCode             as Empresa,
      _Pedido.Supplier                as Fornecedor,
      _Item.Plant                     as Centro,
      _Item.Material                  as Material,
      _Item.OrderQuantity             as Quantidade,
      _Item.PurchaseOrderQuantityUnit as UM,
      _Pedido.CreationDate,
      _Preco.ConditionAmount          as Valor,
      _Preco.ConditionCurrency        as Moeda,
      _Pedido,
      _Preco
}
