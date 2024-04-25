@EndUserText.label: 'Projeção - Custodia Cheque Header'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_CUSTCHEQUE_COMP_H
  provider contract transactional_query
  as projection on ZI_CUSTCHEQUE_COMP_H
{
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text: {
          element: ['DescEmp']
      }
  key Bukrs,
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_Customer', element: 'Customer' }}]
      @ObjectModel.text: {
          element: ['Name']
      }
  key Kunnr,
      RaizCnpj,
      Name,
      DescEmp,
      Moeda,

      ValorChequeAtribuido,
      ValorChequeNAtribuido,
      ValorFatAtribuido,
      ValorFatNAtribuido,
      ValorResidual,
      _Cheques : redirected to composition child ZC_CUSTCHEQUE_COMP_ICHEQUES,
      _Faturas : redirected to composition child ZC_CUSTCHEQUE_COMP_IFAT


}
