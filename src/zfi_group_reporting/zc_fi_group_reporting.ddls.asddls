@EndUserText.label: 'Projection view'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: ['Rldnr','Rdimen','Ryear','Docnr','Docln']

define root view entity ZC_FI_GROUP_REPORTING
  provider contract transactional_query
  as projection on ZI_FI_GROUP_REPORTING
{
      @Consumption.filter.mandatory:true
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_RLDNR', element: 'Rldnr' } }]
  key Rldnr,
      @Consumption.filter.mandatory:true
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_RLDNR', element: 'Rdimen' } }]
  key Rdimen,
      @Consumption.filter.mandatory:true
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_RLDNR', element: 'Ryear' } }]
  key Ryear,
  key Docnr,
  key Docln,
      Rrcty,
      @Consumption.filter.mandatory:true
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_RVERS', element: 'Rvers' } }]
      Rvers,
      Rtcur,
      Rhcur,
      Rkcur,
      Runit,
      @Consumption.filter.mandatory:true
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_POPER', element: 'Poper' } }]
      Poper,
      Docct,
      Rcomp,
      @Consumption.filter.mandatory:true
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_rbunit', element: 'Rbunit' } }]
      Rbunit,
      @Consumption.filter.mandatory:true
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_ritclg', element: 'Ritclg' } }]
      Ritclg,
      Ritem,
      Rbuptr,
      Rcongr,
      Robukrs,
      Sityp,
      Subit,
      Plevl,
      Rpflg,
      Rtflg,
      Docty,
      Yracq,
      Pracq,
      Coicu,
      Uppcu,
      Tsl,
      Hsl,
      Ksl,
      Msl,
      Sgtxt
      //      Autom,
      //      Activ,
      //      Bvorg,
      //      Budat,
      //      Wsdat,
      //      Refdocnr,
      //      Refryear,
      //      Refdocln,
      //      Refdocct,
      //      Refactiv,
      //      Timestamp,
      //      Cpudt,
      //      Cputm,
      //      Usnam,
      //      Rvsdocnr,
      //      Orndocnr,
      //      Bunnr,
      //      Coiac,
      //      Coinr,
      //      Revyear,
      //      Awtyp,
      //      Aworg,
      //      Logsys,
      //      Draft,
      //      Runid,
      //      Runreference,
      //      Ktopl,
      //      Racct,
      //      Xblnr,
      //      Zuonr,
      //      Rcntr,
      //      Prctr,
      //      Rfarea,
      //      Rbusa,
      //      Kokrs,
      //      Segment,
      //      Scntr,
      //      Pprctr,
      //      Sfarea,
      //      Sbusa,
      //      Rassc,
      //      Psegment,
      //      Aufnr,
      //      Kunnr,
      //      Lifnr,
      //      Matnr,
      //      MatklMm,
      //      Werks,
      //      Rmvct,
      //      //      PsPspPnr,
      //      //      PsPosid,
      //      //      PsPspid,
      //      Fkart,
      //      Vkorg,
      //      Vtweg,
      //      Spart,
      //      MatnrCopa,
      //      Matkl,
      //      Kdgrp,
      //      Land1,
      //      Brsch,
      //      Bzirk,
      //      Kunre,
      //      Kunwe,
      //      Konzs,
      //      Dataaging,
      //      Adhocitem,
      //      Adhocset,
      //      Adhocsetitem,
      //      Rcode,
      //      OrigType,
      //      OrigRef,
      //      DummyCjeInclEewPs,
//      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_STATUS_GROUP', element: 'ObjetoName' } }]
//      status,
//      message
}
