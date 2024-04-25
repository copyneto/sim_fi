@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help Variante'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_VH_VARIANTE as select from varid
left outer join varit on varid.variant = varit.variant
{        
    key varid.variant   as Variante,
        varid.report    as Report,
        varit.vtext     as Text,
        varid.ename     as Nome,
        @EndUserText.label: 'Data'
        varid.edat      as Data,
        @EndUserText.label: 'Hora'
        varid.etime     as Time,
        varid.aename    as Nome1,
        @EndUserText.label: 'Data'
        varid.aedat     as Data1,
        @EndUserText.label: 'Hora'
        varid.aetime    as Time1       
}
where varid.report = 'RFEBKA00'
