@EndUserText.label: 'CDS Custon - Geração arquivo custodia'
@ObjectModel.query.implementedBy: 'ABAP:ZCLFI_GERACAO_ARQ_CUST'
define custom entity ZI_FI_GERACAO_ARQ_CUST_CE 
{
  
  key stream_data   : ze_stream_arq;
  key Bukrs         : bukrs;
  key Kunnr         : kunnr;
  key Ncheque       : ze_ncheque;
      Tipo : abap.char(3);
      txt: abap.char(1000);
}
