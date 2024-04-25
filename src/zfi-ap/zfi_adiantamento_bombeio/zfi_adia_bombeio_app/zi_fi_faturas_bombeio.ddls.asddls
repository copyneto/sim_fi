@AbapCatalog.sqlViewName: 'ZVFIFATBOMB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Faturas Bombeio'
define view ZI_FI_FATURAS_BOMBEIO
  as select from bsik_view                     as _Fat

    inner join   ZI_FI_PARAM_BOMBEIO_CNPJ_CENT as _Parameter on _Parameter.Supplier = _Fat.lifnr

  association [0..1] to Fclm_Bseg_Prjk               as _Venc                  on  _Fat.belnr = _Venc.belnr
                                                                               and _Fat.bukrs = _Venc.bukrs
                                                                               and _Fat.gjahr = _Venc.gjahr
                                                                               and _Fat.buzei = _Venc.buzei

  association        to ZI_FI_AUX_ACCDOC_TO_PURCHASE as _Pedido                on  _Fat.belnr = _Pedido.AccountingDocument
                                                                               and _Fat.bukrs = _Pedido.CompanyCode
                                                                               and _Fat.gjahr = _Pedido.FiscalYear

  association        to ZI_FI_AUX_ACCDOC_TO_SUPINV   as _Faturamento           on  _Fat.belnr = _Faturamento.AccountingDocument
                                                                               and _Fat.bukrs = _Faturamento.CompanyCode
                                                                               and _Fat.gjahr = _Faturamento.FiscalYear
                                                                               and _Fat.buzei = _Faturamento.AccountingDocumentItem

  association        to I_Supplier                   as _Forn                  on  _Fat.lifnr = _Forn.Supplier

  association        to I_Plant                      as _Plant                 on  _Fat.bupla = _Plant.Plant

  association [0..1] to I_PaymentBlockingReason      as _PaymentBlockingReason on  _PaymentBlockingReason.PaymentBlockingReason = $projection.Bloqueio

  association [0..1] to I_PaymentMethod              as _PaymentMethod         on  _PaymentMethod.Country       = 'BR'
                                                                               and _PaymentMethod.PaymentMethod = $projection.FormPagto

  association [0..1] to I_Housebank                  as _HouseBank             on  $projection.CompanyCode  = _HouseBank.CompanyCode
                                                                               and $projection.BancoEmpresa = _HouseBank.HouseBank
{
  key _Fat.bukrs                                                           as CompanyCode,
  key _Fat.gjahr                                                           as FiscalYear,
  key _Fat.belnr                                                           as AccountingDocument,
  key _Fat.buzei                                                           as AccountingDocumentItem,
  key _Fat.lifnr                                                           as Supplier,
      _Forn.SupplierFullName                                               as NomeFornecedor,
      _Fat.bupla                                                           as Centro,
      _Plant.PlantName                                                     as NomeCentro,
      _Pedido.PurchaseOrder                                                as PurchaseOrder,
      _Pedido.PurchaseOrderItem                                            as PurchaseOrderItem,
      _Faturamento.SupplierInvoice                                         as SupplierInvoice,
      _Fat.zlspr                                                           as Bloqueio,
      _PaymentBlockingReason.
      _Text[1:Language=$session.system_language].PaymentBlockingReasonName as TextoBloqueio,
      _Fat.zlsch                                                           as FormPagto,
      _PaymentMethod.
      _Text[1:Language=$session.system_language].PaymentMethodDescription  as TextoFormPagto,
      _Fat.hbkid                                                           as BancoEmpresa,
      _HouseBank._Bank.BankName                                            as TextoBancoEmpresa,
      _Fat.xblnr                                                           as Referencia,
      _Venc.netdt                                                          as Vencimento,
      _Fat.waers                                                           as Moeda,
      @Semantics.amount.currencyCode: 'Moeda'
      _Fat.dmbtr                                                           as Valor
}

where
        _Fat.bschl =  '31' -- (Chave de Lançamento)
  and(
        _Fat.zterm <> 'FCAP'
    and _Fat.zterm <> 'FCAV'
  )

union select from bsik_view                     as _Fat

  inner join      ZI_FI_PARAM_BOMBEIO_CNPJ_RAIZ as _Parameter on _Parameter.Supplier = _Fat.lifnr

association [0..1] to Fclm_Bseg_Prjk               as _Venc                  on  _Fat.belnr = _Venc.belnr
                                                                             and _Fat.bukrs = _Venc.bukrs
                                                                             and _Fat.gjahr = _Venc.gjahr
                                                                             and _Fat.buzei = _Venc.buzei

association        to ZI_FI_AUX_ACCDOC_TO_PURCHASE as _Pedido                on  _Fat.belnr = _Pedido.AccountingDocument
                                                                             and _Fat.bukrs = _Pedido.CompanyCode
                                                                             and _Fat.gjahr = _Pedido.FiscalYear

association        to ZI_FI_AUX_ACCDOC_TO_SUPINV   as _Faturamento           on  _Fat.belnr = _Faturamento.AccountingDocument
                                                                             and _Fat.bukrs = _Faturamento.CompanyCode
                                                                             and _Fat.gjahr = _Faturamento.FiscalYear
                                                                             and _Fat.buzei = _Faturamento.AccountingDocumentItem

association        to I_Supplier                   as _Forn                  on  _Fat.lifnr = _Forn.Supplier

association        to I_Plant                      as _Plant                 on  _Fat.bupla = _Plant.Plant

association [0..1] to I_PaymentBlockingReason      as _PaymentBlockingReason on  _PaymentBlockingReason.PaymentBlockingReason = $projection.Bloqueio

association [0..1] to I_PaymentMethod              as _PaymentMethod         on  _PaymentMethod.Country       = 'BR'
                                                                             and _PaymentMethod.PaymentMethod = $projection.FormPagto

association [0..1] to I_Housebank                  as _HouseBank             on  $projection.CompanyCode  = _HouseBank.CompanyCode
                                                                             and $projection.BancoEmpresa = _HouseBank.HouseBank
{
  key _Fat.bukrs                                                           as CompanyCode,
  key _Fat.gjahr                                                           as FiscalYear,
  key _Fat.belnr                                                           as AccountingDocument,
  key _Fat.buzei                                                           as AccountingDocumentItem,
  key _Fat.lifnr                                                           as Supplier,
      _Forn.SupplierFullName                                               as NomeFornecedor,
      _Fat.bupla                                                           as Centro,
      _Plant.PlantName                                                     as NomeCentro,
      _Pedido.PurchaseOrder                                                as PurchaseOrder,
      _Pedido.PurchaseOrderItem                                            as PurchaseOrderItem,
      _Faturamento.SupplierInvoice                                         as SupplierInvoice,
      _Fat.zlspr                                                           as Bloqueio,
      _PaymentBlockingReason.
      _Text[1:Language=$session.system_language].PaymentBlockingReasonName as TextoBloqueio,
      _Fat.zlsch                                                           as FormPagto,
      _PaymentMethod.
      _Text[1:Language=$session.system_language].PaymentMethodDescription  as TextoFormPagto,
      _Fat.hbkid                                                           as BancoEmpresa,
      _HouseBank._Bank.BankName                                            as TextoBancoEmpresa,
      _Fat.xblnr                                                           as Referencia,
      _Venc.netdt                                                          as Vencimento,
      _Fat.waers                                                           as Moeda,
      @Semantics.amount.currencyCode: 'Moeda'
      _Fat.dmbtr                                                           as Valor
}

where
        _Fat.bschl =  '31' -- (Chave de Lançamento)
  and(
        _Fat.zterm <> 'FCAP'
    and _Fat.zterm <> 'FCAV'
  )
