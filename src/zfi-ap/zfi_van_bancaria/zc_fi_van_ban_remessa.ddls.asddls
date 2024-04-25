@EndUserText.label: 'Programa de Remessa de Pagamentos'
@ObjectModel.query.implementedBy: 'ABAP:ZCLFI_VAN_BAN_REMESSA'
define custom entity ZC_FI_VAN_BAN_REMESSA
{
      @UI.selectionField: [{ position: 10 }]
      @EndUserText.label: 'Execução em'
      @Consumption.filter: { selectionType: #INTERVAL,
                             multipleSelections : false }
      //      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_REMESSA', element: 'laufd' } }]
  key laufd   : laufd;
      //      @UI.lineItem  : [{ position: 10 }]
      @UI.hidden: true
  key data    : datum;
      //      @UI.lineItem  : [{ position: 20 }]
      @UI.hidden: true
  key time    : uzeit;
      @EndUserText.label: 'Arquivo'
      //      @UI.lineItem  : [{ position: 30 }]
      @UI.hidden: true
  key files   : febauszf;
      //      @UI.lineItem  : [{ position: 40 }]
      @UI.hidden: true
  key seqnr   : seqnr;
      //      @UI.lineItem  : [{ position: 50 }]
      @UI.hidden: true
      uname   : uname;
      //      @UI.lineItem  : [{ position: 60 }]
      msgty   : msgty;
      //      @UI.lineItem  : [{ position: 70 }]
      msgid   : msgid;
      //      @UI.lineItem  : [{ position: 80 }]
      msgno   : msgno;
      @UI.lineItem  : [{ position: 80 }]
      message : bapi_msg;
}
