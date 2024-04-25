@EndUserText.label: 'Cockpit Adiantamento de Bombeio'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define root view entity ZC_FI_COCKPIT_BOMBEIO
  provider contract transactional_query
  as projection on ZI_FI_COCKPIT_BOMBEIO
{
      @Consumption.valueHelpDefinition: [{ entity: {name: 'ZI_FI_VH_COMPANY_CODE', element: 'CompanyCode' } }]

      @EndUserText.label: 'Empresa'
      @ObjectModel.text.element: ['CompanyCodeName']
  key CompanyCode,

      @EndUserText.label: 'Nome da Empresa'
      CompanyCodeName,

      Currency,

      @EndUserText.label: 'Valor Solic. Adiantamento'
      SolAdiSum,

      @EndUserText.label: 'Valor Adia. Pagos'
      AdiPagSum,

      @EndUserText.label: 'Valor Faturas'
      FaturasSum,

      @EndUserText.label: 'Valor Faturas DRC'
      FaturasDRCSum,

      @EndUserText.label: 'Valor Devolução Cancelamento'
      DevCancSum,

      @EndUserText.label: 'Sugestão de pagamento'
      PaymentAmount,

      @EndUserText.label: 'Sugestão de pagamento (Criticalidade)'
      PaymentAmountCrit,

      @EndUserText.label: 'Total Remessas do dia '
      ValorTotalRemessa,

      /* Associations */
      _SolicitacaoAdiantamento : redirected to composition child ZC_FI_COCKPIT_BOMBEIO_SOL_ADI,
      _Pedido                  : redirected to composition child ZC_FI_COCKPIT_BOMBEIO_PED,
      _AdiaPagos               : redirected to composition child ZC_FI_COCKPIT_BOMBEIO_ADI_PAG,
      _DevolucaoCancelamento   : redirected to composition child ZC_FI_COCKPIT_BOMBEIO_DEV_CAN,
      _FaturaDRC               : redirected to composition child ZC_FI_COCKPIT_BOMBEIO_FAT_DRC,
      _Faturas                 : redirected to composition child ZC_FI_COCKPIT_BOMBEIO_FAT,
      _RemessaDia              : redirected to composition child ZC_FI_COCKPIT_BOMBEIO_REM_DIA

}
