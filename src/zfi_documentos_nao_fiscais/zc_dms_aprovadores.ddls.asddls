@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Consumo para Aprovadores'
@Metadata.allowExtensions: true

define root view entity ZC_DMS_APROVADORES
  provider contract transactional_query
  as projection on ZI_DMS_APROVADORES

{
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_DMS_VH_APROVADOR',
                                                              element: 'UserID'} }]
  key Aprovador,
      DescAprovador,

      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_DMS_VH_STATUS_APROVADOR',
                                                              element: 'Opt'} }]
      Status,
      Criado_por,
      Criado_em,
      Modificado_por,
      Modificado_em,
      Hora
}
