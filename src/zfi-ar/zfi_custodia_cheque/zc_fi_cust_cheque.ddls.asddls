@EndUserText.label: 'Custódia de cheque'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_FI_CUST_CHEQUE
  provider contract transactional_query
  as projection on ZI_FI_CUST_CHEQUE as _Cheque

  association [0..1] to ZI_FI_VH_STATUS_CHEQUE as _StatusSH on _Cheque.NovoStatus = _StatusSH.ObjetoId

{

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text: {
          element: ['DescEmp']
      }
      @EndUserText.label: 'Empresa'
  key Bukrs,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_Customer', element: 'Customer' }}]
      @ObjectModel.text: {
          element: ['DescName']
      }
      @EndUserText.label: 'Cliente'
  key Kunnr,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @EndUserText.label: 'N do Cheque'
  key Ncheque,
      @EndUserText.label: 'Descrição Empresa'
      DescEmp,
      @EndUserText.label: 'Descrição Cliente'
      DescName,
      @EndUserText.label: 'Data Lançamento'
      Budat,
      @EndUserText.label: 'N Contrato'
      Ncontrato,
      @EndUserText.label: 'N Contrato Jurídico'
      Ncontratojuridico,
      @Consumption.valueHelpDefinition: [{  entity: {name: 'P_BusinessPlace', element: 'branch'},
                            additionalBinding: [{  element: 'bukrs', localElement: 'Bukrs' }]              }]
      @ObjectModel.text: {
          element: ['DescLocal']
      }
      @EndUserText.label: 'Local de Negócio'
      Bupla,
      @EndUserText.label: 'Descrição Local'
      DescLocal,
      @Consumption.valueHelpDefinition: [{entity: {name: 'zi_fi_ZTERM_VH', element: 'Zterm' }}]
      @EndUserText.label: 'Cond de pagamento'
      Zterm,
      @EndUserText.label: 'Descrição Cond'
      DescCond,
      @EndUserText.label: 'N de chamado'
      Nchamado,
      @Semantics.amount.currencyCode : 'Moeda'
      @EndUserText.label: 'Valor'
      Valor,
      @EndUserText.label: 'Moeda'
      Moeda,
      @Consumption.valueHelpDefinition: [ { entity:{ name : 'ZI_FI_VH_STATUS_CHEQUE', element: 'ObjetoId' } } ]
      @ObjectModel.text: {
          element: ['Desc_status']
      }
      @EndUserText.label: 'Status'
      Status,
      @EndUserText.label: 'Descrição Status'
      Desc_status,
      @EndUserText.label: 'Novo Status'
      NovoStatus,
      _StatusSH,
      @EndUserText.label: 'Nº Documento'
      Doc,
      @EndUserText.label: 'Exercício'
      Gjahr,
      @EndUserText.label: 'Doc Estorno'
      DocEstorno,

      @EndUserText.label: 'Doc Devolução'
      DocDevolucao,

      @EndUserText.label: 'Doc Compensação'
      DocCompensacao,

      @EndUserText.label: 'Número CMC7'
      Zcmc7,
      @EndUserText.label: 'Agência'
      Hktid,
      @EndUserText.label: 'Conta Corrente'
      Bankn,
      @EndUserText.label: 'Dígito Agência e Dígito Conta'
      Bkont,
      @EndUserText.label: 'Câmara de Compensação'
      Zcamara,
      @Consumption.valueHelpDefinition: [ { entity:{ name : 'ZI_FI_VH_CONTABILIZADOS', element: 'VHContabilizado' } } ]
      @ObjectModel.text: {
          element: ['VHContabilizadotxt']
      }
      @EndUserText.label: 'Status Contabilização'
      VHContabilizado,
      VHContabilizadotxt
}
