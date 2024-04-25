@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Clientes inadimplentes'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_FI_CLIENTES_INADIMPLENTES
  as select from I_JournalEntryItem
  
  association [0..1] to ztfi_bloq_client as _BloqueioCliente on _BloqueioCliente.cliente = $projection.Customer
  association [0..1] to ztfi_retirar_cli as _RetirarCliente  on _RetirarCliente.cliente = $projection.Customer

{
  key CompanyCode                                                                    as CompanyCode,
  key FiscalYear                                                                     as FiscalYear,
  key AccountingDocument                                                             as AccountingDocument,
  key AccountingDocumentItem                                                         as AccountingDocumentItem,
      Customer                                                                       as Customer,
      NetDueDate                                                                     as NetDueDate,
      dats_days_between($session.system_date, NetDueDate)                            as DiasAtraso,

      case
        when dats_days_between($session.system_date, NetDueDate) <= cast(_BloqueioCliente.dias as abap.int4)
        then 'X'
        else ''
      end                                                                            as Block,

      _Customer._CustomerToBusinessPartner._BusinessPartner.BusinessPartnerIsBlocked as IsBlocked

}
where
        FinancialAccountType                     =  'D'
  and   ClearingJournalEntry                     =  ''
  and(
        _JournalEntry.AccountingDocumentCategory <> 'D'
    and _JournalEntry.AccountingDocumentCategory <> 'M'
  )
  and   Ledger                                   =  '0L'
  and   SourceLedger                             =  '0L'
  and   _RetirarCliente.cliente                  is null
