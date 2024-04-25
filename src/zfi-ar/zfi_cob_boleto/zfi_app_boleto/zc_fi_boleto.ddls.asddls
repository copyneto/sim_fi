@EndUserText.label: 'Buscar Faturas - Boleto'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_FI_BOLETO
  as projection on ZI_FI_BOLETO

  association to ZI_FI_STATUS_BOL_VH as _Status on _Status.Ddtext = $projection.Status

{

      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text: {
          element: ['EmpresaDesc']
      }
  key Empresa,
  key Documento,
  key Parcela,
  key Exercicio,
      EmpresaDesc,
      @ObjectModel.text: {
          element: ['TipoDocDesc']
      }
      TipoDoc,
      TipoDocDesc,
      @ObjectModel.text: {
          element: ['ClienteDesc']
      }
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_Customer', element: 'Customer' }}]
      Cliente,
      ClienteDesc,
      Referencia,
      Moeda,
      Valor,
      @ObjectModel.text: {
          element: ['BancoEmpresaDesc']
      }
      @Consumption.valueHelpDefinition: [{ entity:
          {name: 'ZI_FI_BANCOEMP_VH' , element: 'hbkid' },
          additionalBinding: [{ localElement: 'Empresa', element: 'bukrs' }]
          }]
      BancoEmpresa,
      BancoEmpresaDesc,
      FormaPagto,
      DataDoc,
      DataLanc,
      NossoNumero,
      SolicLC,
      Vencimento,
      @EndUserText: {
          label: 'Status Boleto',
          quickInfo: 'Status Boleto'
      }

      @Consumption.valueHelpDefinition: [{entity: {name: 'ZI_FI_STATUS_BOL_VH', element: 'Ddtext' }}]
      Status
}
