@EndUserText.label: 'CDS de Consumo Bloqueio de Cliente'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_FI_BLOQ_CLIENT
  provider contract transactional_query
  as projection on ZI_FI_BLOQ_CLIENT
{
      @UI.textArrangement: #TEXT_LAST
      @ObjectModel.text.element: [ 'Nome' ]
      @Consumption.valueHelpDefinition: [{entity: {name: 'ZI_FI_VH_CUSTOMER', element: 'Customer' }}]
  key Cliente,
      Dias,
      UsuarioCriacao,
      DataCriacao,
      UsuarioModif,
      DataModif,
      Nome
}
