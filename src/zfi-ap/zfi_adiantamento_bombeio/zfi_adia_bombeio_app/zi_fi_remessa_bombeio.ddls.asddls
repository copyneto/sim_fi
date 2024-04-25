@AbapCatalog.sqlViewName: 'ZVFIREMEBOMB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Remessa bombeio'
define view ZI_FI_REMESSA_BOMBEIO
  as select from    ZI_FI_COCKPIT_BOMBEIO_PED   as _Ped

    inner join      A_PurchaseOrderScheduleLine as _Sche   on  _Sche.PurchasingDocument     = _Ped.PurchaseOrder
                                                           and _Sche.PurchasingDocumentItem = _Ped.PurchaseOrderItem

    inner join      t001                        as _Emp    on _Ped.CompanyCode = _Emp.bukrs

    left outer join A_PurOrdPricingElement      as _Preco  on  _Ped.PurchaseOrder     = _Preco.PurchaseOrder
                                                           and _Ped.PurchaseOrderItem = _Preco.PurchaseOrderItem
                                                           and _Preco.ConditionType   = 'PBXX'

    left outer join A_PurOrdPricingElement      as _Preco2 on  _Ped.PurchaseOrder     = _Preco2.PurchaseOrder
                                                           and _Ped.PurchaseOrderItem = _Preco2.PurchaseOrderItem
                                                           and _Preco2.ConditionType  = 'PB00'

    left outer join ZI_FI_COND                  as _Preco3 on  _Ped.PurchaseOrder     = _Preco3.PurchaseOrder
                                                           and _Ped.PurchaseOrderItem = _Preco3.PurchaseOrderItem
{
  key _Ped.CompanyCode,
  key _Ped.PurchaseOrder,
  key _Ped.PurchaseOrderItem,
  key _Sche.ScheduleLine              as Remessa,
      _Ped.Plant,
      _Ped.PlantName,
      _Ped.Supplier,
      _Ped.SupplierName,
      _Sche.ScheduleLineDeliveryDate  as DataRemessa,

      _Sche.ScheduleLineOrderQuantity as Quantidade,
      _Sche.PurchaseOrderQuantityUnit as QtdUnidade,
      _Emp.waers                      as Moeda,

      case when _Ped.OrderQuantity > 0
                then  _Preco3.Montante
                 + ( cast( _Preco.ConditionAmount as abap.fltp )
               / cast( _Ped.OrderQuantity as abap.fltp ) )
                 end                  as Montante,

      //      case when _Preco.ConditionAmount > 0
      //          then ( cast( _Preco.ConditionAmount as abap.fltp )
      //         / cast( _Preco.ConditionBaseValue as abap.fltp ) )
      //         else
      //              ( cast( _Preco2.ConditionAmount as abap.fltp )
      //         / cast( _Preco2.ConditionBaseValue as abap.fltp ) )
      //         end                          as Montante,

      case when _Preco.ConditionAmount > 0
          then  (cast(_Preco3.ConditionAmount as abap.fltp )
           +  cast( _Preco.ConditionAmount as abap.fltp ) )
         / cast( _Ped.OrderQuantity as abap.fltp )
          * cast( _Sche.ScheduleLineOrderQuantity as abap.fltp ) 

      else
      
      (cast(_Preco3.ConditionAmount as abap.fltp )
           +  cast( _Preco2.ConditionAmount as abap.fltp ) )
         / cast( _Ped.OrderQuantity as abap.fltp )
          * cast( _Sche.ScheduleLineOrderQuantity as abap.fltp )
          end                         as ValorTotalRemessa

      //      case when _Preco.ConditionAmount > 0
      //       then
      //              ( cast( _Preco.ConditionAmount as abap.fltp ) / cast( _Preco.ConditionBaseValue as abap.fltp )
      //              * cast( _Sche.ScheduleLineOrderQuantity as abap.fltp ) )
      //        else
      //                ( cast( _Preco2.ConditionAmount as abap.fltp ) / cast( _Preco2.ConditionBaseValue as abap.fltp )
      //              * cast( _Sche.ScheduleLineOrderQuantity as abap.fltp ) )
      //      end                             as ValorTotalRemessa


      //       ( cast( _Preco.ConditionAmount as abap.fltp )
      //           / cast( _Preco.ConditionBaseValue as abap.fltp )  ) as Montante,
      //      ( cast( _Preco.ConditionAmount as abap.fltp ) / cast( _Preco.ConditionBaseValue as abap.fltp )
      //      * cast( _Sche.ScheduleLineOrderQuantity as abap.fltp ) ) as ValorTotalRemessa
}
