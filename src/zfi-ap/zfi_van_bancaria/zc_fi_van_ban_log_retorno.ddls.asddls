@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDs de Consumo Programa log retorno'
@Metadata.allowExtensions: true
@UI.presentationVariant: [{ sortOrder: [{ by: 'Data', direction: #DESC },
                                        { by: 'Hora', direction: #DESC }]}]
define root view entity ZC_FI_VAN_BAN_LOG_RETORNO
  provider contract transactional_query
  as projection on ZI_FI_VAN_BAN_LOG_RETORNO
{
  key Data,
  key Hora,
  key Arquivo,
  key Sequencial,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' }}]
      @ObjectModel.text.element: ['CompanyName']
      @UI.textArrangement: #TEXT_LAST
      Empresa,
      _CompanyCode.CompanyCodeName as CompanyName,
      @ObjectModel.text.element: ['UserName']
      @UI.textArrangement: #TEXT_LAST
      Usuario,
      _User.Nome                   as UserName,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_CA_VH_MESSAGE_TYPE', element: 'MsgType' }}]
      MsgTipo,
      @UI.hidden: true
      MsgId,
      @UI.hidden: true
      Msgnumero,
      Mensagem,
      Criticality,
      _CompanyCode,
      _User
}
