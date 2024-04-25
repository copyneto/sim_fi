@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Adiantamento de Bombeio'

define root view entity ZI_FI_COCKPIT_BOMBEIO

  as select from I_CompanyCode as _Company

  association [0..1] to ZI_FI_AUX_BOMBEIO_TOTAL       as _Total on _Total.CompanyCode = $projection.CompanyCode

  composition [0..*] of ZI_FI_COCKPIT_BOMBEIO_SOL_ADI as _SolicitacaoAdiantamento
  composition [0..*] of ZI_FI_COCKPIT_BOMBEIO_PED     as _Pedido
  composition [0..*] of ZI_FI_COCKPIT_BOMBEIO_ADI_PAG as _AdiaPagos
  composition [0..*] of ZI_FI_COCKPIT_BOMBEIO_DEV_CAN as _DevolucaoCancelamento
  composition [0..*] of ZI_FI_COCKPIT_BOMBEIO_FAT_DRC as _FaturaDRC
  composition [0..*] of ZI_FI_COCKPIT_BOMBEIO_FAT     as _Faturas
  composition [0..*] of ZI_FI_COCKPIT_BOMBEIO_REM_DIA as _RemessaDia

{
  key _Company.CompanyCode     as CompanyCode,
      _Company.CompanyCodeName as CompanyCodeName,

      _Company.Currency        as Currency,

      @Semantics.amount.currencyCode: 'Currency'
      _Total.SolAdiSum         as SolAdiSum,
      @Semantics.amount.currencyCode: 'Currency'
      _Total.AdiPagSum         as AdiPagSum,
      @Semantics.amount.currencyCode: 'Currency'
      _Total.FaturasSum        as FaturasSum,
      @Semantics.amount.currencyCode: 'Currency'
      _Total.FaturasDRCSum     as FaturasDRCSum,
      @Semantics.amount.currencyCode: 'Currency'
      _Total.DevCancSum        as DevCancSum,
      @Semantics.amount.currencyCode: 'Currency'
      _Total.ValorTotalRemessa as ValorTotalRemessa,
      @Semantics.amount.currencyCode: 'Currency'
      _Total.PaymentAmount     as PaymentAmount,
      _Total.PaymentAmountCrit as PaymentAmountCrit,


      /* Associations */
      _SolicitacaoAdiantamento,
      _Pedido,
      _AdiaPagos,
      _DevolucaoCancelamento,
      _FaturaDRC,
      _Faturas,
      _RemessaDia

}
where
     _Total.SolAdiSum     is not initial
  or _Total.AdiPagSum     is not initial
  or _Total.FaturasSum    is not initial
  or _Total.FaturasDRCSum is not initial
  or _Total.DevCancSum    is not initial
