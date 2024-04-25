@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca - Documento contábil para Faturam.'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_AUX_ACCDOC_TO_SUPINV
  as select from bkpf_bsik_ddl
{
  bukrs                                as CompanyCode,
  gjahr                                as FiscalYear,
  belnr                                as AccountingDocument,
  buzei                                as AccountingDocumentItem,

  awtyp                                as ReferenceType,
  awkey                                as ReferenceKey,

  cast( left( awkey, 10) as vbeln_vf ) as SupplierInvoice

}
