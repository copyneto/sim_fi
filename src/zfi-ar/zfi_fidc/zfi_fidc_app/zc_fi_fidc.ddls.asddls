@EndUserText.label: 'CDS de Consumo - FIDC'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_FI_FIDC
  provider contract transactional_query
  as projection on ZI_FI_FIDC
{
      @ObjectModel.text: {
            element: ['CompanyCodeName']
        }
  key Bukrs,
  key Belnr,
  key Gjahr,
  key Buzei,
      CompanyCodeName,
      @EndUserText.label: 'Nosso Número'
      Nossonumero,
      @ObjectModel.text: {
            element: ['NomeCliente']
        }
      kunnr,
      NomeArqRem,
      @EndUserText.label: 'Texto Status'
      Status,
      @EndUserText.label: 'Data Remessa'
      DataRemessa,
      @EndUserText.label: 'Hora Remessa'
      HoraRemessa,
      @EndUserText.label: 'Data Retorno'
      DataRetorno,
      @EndUserText.label: 'Hora Retorno'
      HoraRetorno,
      @EndUserText.label: 'Título Cobrança'
      TituloCob,
      @EndUserText.label: 'Empresa Título'
      TituloBukrs,
      criticality,
      @EndUserText.label: 'Status'
      TipoStatus,
      @EndUserText.label: 'Status Desc'
      Aceito,
      @EndUserText.label: 'Exercício Cob.'
      GjahrCob,
      @EndUserText.label: 'Documento Comp.'
      DocComp,
      @EndUserText.label: 'Motivo Recusa'
      MotRecusa,
      @EndUserText.label: 'Data Recompra'
      DataRecompra,
      @EndUserText.label: 'Valor Cessão'
      ValorCessao,
      @EndUserText.label: 'Taxa Cessão.'
      TaxaCessao,
      @EndUserText.label: 'Valor Desconto'
      ValorDesco,
      @EndUserText.label: 'Área Crédito'
      Areacredito,
      @EndUserText.label: 'Divisão'
      Divisao,
      @EndUserText.label: 'Nota Fiscal'
      Nf,
      @EndUserText.label: 'Chave NF'
      Chave,
      @EndUserText.label: 'Moeda'
      Moeda,
      @EndUserText.label: 'Valor'
      Valor,
      @EndUserText.label: 'Data Emissão NF'
      DataEmissaoNF,
      @EndUserText.label: 'CNPJ Emissor'
      CNPJEmissor,
      @EndUserText.label: 'Nome Cliente'
      NomeCliente,
      @EndUserText.label: 'CNPJ Cliente'
      CNPJCliente,
      @EndUserText.label: 'Telefone1'
      TELEFONE1,
      @EndUserText.label: 'Telefone2'
      TELEFONE2,
      @EndUserText.label: 'DocNum'
      DocNum,
      @EndUserText.label: 'Endereço'
      EndCliente,
      @EndUserText.label: 'Complemento'
      CompEnd,
      @EndUserText.label: 'Bairro'
      Bairro,
      @EndUserText.label: 'Cidade'
      Cidade,
      @EndUserText.label: 'Uf'
      UF,
      @EndUserText.label: 'CEP'
      CEP,
      @EndUserText.label: 'Valor NF'
      ValorNF,
      @EndUserText.label: 'Vencimento'
      Vencimento,
      @EndUserText.label: 'Email1'
      Email1,
      @EndUserText.label: 'Email2'
      Email2
}
