@EndUserText.label: 'App Processa Retorno Extrato'
@ObjectModel.query.implementedBy: 'ABAP:ZCLFI_VAN_BAN_RETORNO'
define custom entity ZC_FI_VAN_BAN_RETORNO
{
      @UI.selectionField: [{ position: 10 }]
      @EndUserText.label: 'Variante'
      @Consumption.filter: { mandatory: true,
                             selectionType: #SINGLE }
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_VARIANTE', element: 'Variante' } }]
  key variant : variant;
      //      @UI.lineItem  : [{ position: 10 }]
      @UI.hidden: true
  key data    : datum;
      //      @UI.lineItem  : [{ position: 20 }]
      @UI.hidden: true
  key time    : uzeit;
      @UI.lineItem  : [{ position: 30 }]
      @EndUserText.label: 'Arquivo'
  key files   : febauszf;
      //      @UI.lineItem  : [{ position: 40 }]
      @UI.hidden: true
  key seqnr   : seqnr;
      @UI.hidden: true
      bukrs   : bukrs;
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
