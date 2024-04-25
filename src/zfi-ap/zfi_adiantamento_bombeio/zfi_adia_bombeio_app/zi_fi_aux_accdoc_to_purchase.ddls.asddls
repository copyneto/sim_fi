@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca - Documento contábil para Pedido'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_AUX_ACCDOC_TO_PURCHASE
  as select from Fclm_Bseg_Prjk
{
  bukrs        as CompanyCode,
  gjahr        as FiscalYear,
  belnr        as AccountingDocument,
  max( ebeln ) as PurchaseOrder,
  max( ebelp ) as PurchaseOrderItem
}

where
  ktosl = 'WRX'

group by
  bukrs,
  gjahr,
  belnr
