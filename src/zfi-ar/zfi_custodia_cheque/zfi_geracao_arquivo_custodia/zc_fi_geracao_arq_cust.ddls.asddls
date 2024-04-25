@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Geração arquivo custódia'
@Metadata.allowExtensions: true
define root view entity ZC_FI_GERACAO_ARQ_CUST
  provider contract transactional_query
  as projection on ZI_FI_GERACAO_ARQ_CUST
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text: {
        element: ['NomeDepos']
      }
      @UI.facet: [{label: 'Empresa'}]
  key Bukrs,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_Customer', element: 'Customer' }}]
      @ObjectModel.text: {
        element: ['ClienteName']
      }
      @UI.facet: [{label: 'Cliente'}]
  key Kunnr,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @UI.facet: [{label: 'N do Cheque'}]
  key Ncheque,
      @UI.facet: [{label: 'Data Lançamento'}]
      Budat,
      @UI.facet: [{label: 'N Contrato'}]
      Ncontrato,
      @UI.facet: [{label: 'N Contrato Jurídico'}]
      Ncontratojuridico,
      @Consumption.valueHelpDefinition: [{  entity: {name: 'P_BusinessPlace', element: 'branch'},
                            additionalBinding: [{  element: 'bukrs', localElement: 'Bukrs' }]              }]
      @ObjectModel.text: {
          element: ['DescLocal']
      }
      @UI.facet: [{label: 'Local de Negócio'}]
      Bupla,
      @Consumption.valueHelpDefinition: [{entity: {name: 'zi_fi_ZTERM_VH', element: 'Zterm' }}]
      @ObjectModel.text: {
        element: ['DescCond']
      }
      @UI.facet: [{label: 'Cond de pagamento'}]
      Zterm,
      @UI.facet: [{label: 'N de chamado'}]
      Nchamado,
      @UI.facet: [{label: 'Valor'}]
      Valor,
      @UI.facet: [{label: 'Moeda'}]
      Moeda,
      @Consumption.valueHelpDefinition: [ { entity:{ name : 'ZI_FI_VH_STATUS_CHEQUE', element: 'ObjetoId' } } ]
      @ObjectModel.text: {
          element: ['Desc_status']
      }
      @UI.facet: [{label: 'Status'}]
      Status,
      @UI.facet: [{label: 'Tipo Documento'}]
      TpDoc,
      @UI.facet: [{label: 'Chave Lançamento'}]
      ChaveLanc,
      @UI.facet: [{label: 'Código RZE'}]
      CodRZE,
      @UI.facet: [{label: 'Atribuição'}]
      Atribuicao,
      @UI.facet: [{label: 'Referência'}]
      Refe,
      @UI.facet: [{label: 'Vencimento em'}]
      Vencimento,
      @UI.facet: [{label: 'Câmara de compensação'}]
      Zcamara,
      @UI.facet: [{label: 'Número CMC7'}]
      Zcmc7,
      @UI.facet: [{label: 'Agência'}]
      Hktid,
      @UI.facet: [{label: 'Conta Corrente'}]
      Bankn,
      @UI.facet: [{label: 'Descrição Status'}]
      Desc_status,
      //      @UI.facet: [{label: 'Novo Status'}]
      //      NovoStatus,
      @UI.facet: [{label: 'Nº Documento'}]
      Doc,
      @UI.facet: [{label: 'Exercício'}]
      Gjahr,
      @UI.facet: [{label: 'Doc Estorno'}]
      DocEstorno,
      @UI.facet: [{label: 'Descrição Local'}]
      DescLocal,
      @UI.facet: [{label: 'Descrição Cond'}]
      DescCond,
      @UI.facet: [{label: 'Descrição Empresa'}]
      NomeDepos,
      @UI.facet: [{label: 'Descrição Cliente'}]
      ClienteName
}
