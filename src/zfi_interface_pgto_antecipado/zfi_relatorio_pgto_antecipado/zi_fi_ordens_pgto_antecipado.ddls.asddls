@EndUserText.label: 'CDS de Interface prog pgto Antecipado'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZI_FI_ORDENS_PGTO_ANTECIPADO
  as select from    I_SalesOrder     as _Sales

    inner join      I_SalesOrderItem as _Item           on  _Item.SalesOrder     = _Sales.SalesOrder
                                                        and _Item.SalesOrderItem = '000010'

    inner join      ZI_CA_PARAM_VAL  as _BloqFat        on  _BloqFat.Modulo = 'FI-AR'
                                                        and _BloqFat.Chave1 = 'A27'
                                                        and _BloqFat.Chave2 = 'EMPRESAS_CONTEMPLADAS'
                                                        and _BloqFat.Low    = _Sales.BillingCompanyCode

    inner join      ZI_CA_PARAM_VAL  as _CreditControl  on  _CreditControl.Modulo = 'FI-AR'
                                                        and _CreditControl.Chave1 = 'A27'
                                                        and _CreditControl.Chave2 = 'CONTROLE_CREDITO'
                                                        and _CreditControl.Low    = _Sales.TotalCreditCheckStatus

    inner join      ZI_CA_PARAM_VAL  as _Fpgto          on  _Fpgto.Modulo = 'FI-AR'
                                                        and _Fpgto.Chave1 = 'A27'
                                                        and _Fpgto.Chave2 = 'FORMADEPAGAMENTO'
                                                        and _Fpgto.Low    = _Sales.PaymentMethod

    left outer join ZI_CA_PARAM_VAL  as _StatusComplete on  _StatusComplete.Modulo = 'FI-AR'
                                                        and _StatusComplete.Chave1 = 'A27'
                                                        and _StatusComplete.Chave2 = 'CONTROLE_CREDITO'
                                                        and _StatusComplete.Chave3 = 'REJEITADOS'
                                                        and _StatusComplete.Low    = _Sales.OverallSDProcessStatus

  association to ztfi_adto_ov                as _adto   on  _adto.vbeln = _Sales.SalesOrder

  association to ZI_FI_ORDENS_VALOR_COND_PIX as _Valor  on  _Valor.SalesOrder = _Sales.SalesOrder

  association to ZI_SD_NFE_DOCREF            as _DocRef on  _DocRef.PrecedingDocument          = _Sales.SalesOrder
                                                        and _DocRef.SubsequentDocumentCategory = 'M'

{

  key _Sales.SalesOrder,
  key _Sales.BillingCompanyCode,
      _Sales.SoldToParty,
      _Sales.TransactionCurrency,
      _Valor.TotalNetAmount,
      _Item.BusinessArea,
      _Sales._BillingCompanyCode.CompanyCodeName,

      coalesce(_Sales._SoldToParty.TaxNumber1, _Sales._SoldToParty.TaxNumber2) as CnpjCliente,
      _Sales._SoldToParty.CustomerName,

      cast( left(_adto.doc_adiantamento,10) as belnr_d )                       as DocAdiantamento,
      cast( right(_adto.doc_adiantamento,4) as gjahr )                         as DocAdiantamentoYear,
      _adto.doc_compensacao                                                    as DocCompensacao,
      _adto.doc_compensacao_fatura                                             as DocCompensacaoFatura,

      upper( _adto.txid )                                                      as TransactionIdentification,
      _adto.pix_copia_cola                                                     as PixCopiaCola,

      _DocRef.SubsequentDocument                                               as DocFatura,
      _DocRef._BillingDocument.AccountingDocument                              as DocContabilFatura,

      case
        when _adto.status is null or _adto.status = '1' or _adto.status = '2'
        then 'X'
        else ''
      end                                                                      as OutboundStep,

      case
        when _adto.status = '3' or _adto.status = '4' or _adto.status = '5'
        then 'X'
        else ''
      end                                                                      as InboundStep,

      case
        when _adto.status = '1' or _adto.status = '2' or _adto.status = '4' or _adto.status = '5'
        then 'X'
        else ''
      end                                                                      as HasError,

      case
      when _adto.status = '6'
      then 'X'
      else ''
      end                                                                      as IsProcessed,

      case
        when _adto.flag_cancelamento is not initial
        then 'X'
        else ''
      end                                                                      as IsCanceled,

      case
        when _adto.expira_em < tstmp_current_utctimestamp() and _adto.expira_em is not initial
        then 'X'
        else ''
      end                                                                      as IsExpired,

      _adto.status


}
where
       _StatusComplete.Low           is null
  and(
       _Sales.SalesDocApprovalStatus = 'B'
    or _Sales.SalesDocApprovalStatus is initial
  )
