@EndUserText.label: 'Log de solicitação pagamento antecipado'
@ObjectModel.query.implementedBy: 'ABAP:ZCLFI_LOG_PGTO_ANTECIPADO'
@UI: {
  headerInfo: {
    typeName: 'Log',
    typeNamePlural: 'Logs'
  }}
define custom entity ZC_FI_INFO_PGTO_ANTECIPADO_LOG
{
      @UI.facet         : [{
          id            : 'msgDDAPetrobras',
          purpose       : #STANDARD,
          type          : #IDENTIFICATION_REFERENCE,
          label         : 'Mensagens de Processamento',
          position      : 10
      }]

      @UI               : {
        lineItem        : [{ position: 10 }],
        identification  : [{ position: 10 }],
        selectionField  : [{ position: 10 }]
      }

      @UI.hidden        : true
      @Consumption.filter.mandatory: true
      @EndUserText.label: 'Referência'
  key Vbeln             : abap.char(10);

      @UI.hidden        : true
  key MessageId         : char30;

      @UI               : {
        lineItem        : [{ position: 20, criticality: 'StatusCriticality' }],
        identification  : [{ position: 20 }]
      }
      @EndUserText.label: 'Mensagem'
      mensagem          : msg_info;

      @UI               : {
        lineItem        : [{ position: 30 }],
        identification  : [{ position: 30 }]
      }
      @EndUserText.label: 'Tipo'
      Tipo              : msgty;

      @UI               : {
        lineItem        : [{ position: 40 }],
        identification  : [{ position: 40 }]
      }
      Criado_por        : uname;

      @UI               : {
        lineItem        : [{ position: 50 }],
        identification  : [{ position: 50 }]
      }
      @EndUserText.label: 'Data Criação'
      Criado_em         : tzntstmpl;

      StatusCriticality : abap.numc( 1 );

      @ObjectModel.sort.enabled: false
      @ObjectModel.filter.enabled: false
      _Header           : association to parent ZI_FI_INFO_PGTO_ANTECIPADO on _Header.vbeln = $projection.Vbeln;
}
