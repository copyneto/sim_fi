@EndUserText.label: 'Cockpit Bombeio - Faturas DRC'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define view entity ZC_FI_COCKPIT_BOMBEIO_FAT_DRC
  as projection on ZI_FI_COCKPIT_BOMBEIO_FAT_DRC
{
      @EndUserText.label: 'Empresa'
  key CompanyCode,
  
      @EndUserText.label: 'Nº Documento'
  key BR_NotaFiscal,
  
      @EndUserText.label: 'Local de negócio'
      @ObjectModel.text.element: ['BusinessPlaceDescription']
      BusinessPlace,
      
      @EndUserText.label: 'Nome Local de negócio'
      BusinessPlaceDescription,
      
      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['BR_NFPartnerName']
      BR_NFPartner,
      
      @EndUserText.label: 'Nome do Fornecedor'
      BR_NFPartnerName,
      
      @EndUserText.label: 'Data da Emissão'
      BR_NFAuthenticationDate,
      
      @EndUserText.label: 'Nota Fiscal'
      BR_NFeNumber,
      
      @EndUserText.label: 'Montante original'
      BR_NFOriginalAmount,
      
      @EndUserText.label: 'Montante líquido'
      BR_NFNetAmount,
      
      @EndUserText.label: 'Moeda'
      Currency,

      /* Associations */
      _Bombeio : redirected to parent ZC_FI_COCKPIT_BOMBEIO
}
