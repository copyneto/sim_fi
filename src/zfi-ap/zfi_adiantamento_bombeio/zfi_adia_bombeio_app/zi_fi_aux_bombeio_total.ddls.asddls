@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Adiantamento de Bombeio - Total'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_AUX_BOMBEIO_TOTAL

  as select from I_CompanyCode as _Company

  association [0..1] to ZI_SUM_SOL_ADI_BOMBEIO     as _SolAdiSUM  on _Company.CompanyCode = _SolAdiSUM.CompanyCode
  association [0..1] to ZI_SUM_ADI_PAG_BOMBEIO     as _AdiPagSUM  on _Company.CompanyCode = _AdiPagSUM.CompanyCode
  association [0..1] to ZI_SUM_FATURAS_BOMBEIO     as _FatSUM     on _Company.CompanyCode = _FatSUM.CompanyCode
  association [0..1] to ZI_SUM_FATURAS_DRC_BOMBEIO as _FatDRCSUM  on _Company.CompanyCode = _FatDRCSUM.CompanyCode
  association [0..1] to ZI_SUM_DEV_CANC_BOMBEIO    as _DevCancSUM on _Company.CompanyCode = _DevCancSUM.CompanyCode
  association [0..1] to ZI_SUM_REM_DIA             as _RemDiaSUM  on _Company.CompanyCode = _RemDiaSUM.CompanyCode
{

  key _Company.CompanyCode         as CompanyCode,
      _Company.Currency            as Currency,

      @Semantics.amount.currencyCode: 'Currency'
      _SolAdiSUM.Amount            as SolAdiSum,
      @Semantics.amount.currencyCode: 'Currency'
      _AdiPagSUM.Amount            as AdiPagSum,
      @Semantics.amount.currencyCode: 'Currency'
      _FatSUM.Amount               as FaturasSum,
      @Semantics.amount.currencyCode: 'Currency'
      _FatDRCSUM.AMOUNT            as FaturasDRCSum,
      @Semantics.amount.currencyCode: 'Currency'
      _DevCancSUM.Amount           as DevCancSum,
      @Semantics.amount.currencyCode: 'Currency'
      _RemDiaSUM.ValorTotalRemessa as ValorTotalRemessa,

      @EndUserText.label: 'Sugestão de pagamento'
      @Semantics.amount.currencyCode: 'Currency'
      case when (  coalesce( cast( _RemDiaSUM.ValorTotalRemessa as abap.dec(23,2) ), 0)
                + coalesce( cast( _AdiPagSUM.Amount as abap.dec(23,2) ), 0)
                + coalesce( cast( _FatSUM.Amount as abap.dec(23,2) ), 0)
                + coalesce( cast( _FatDRCSUM.AMOUNT as abap.dec(23,2) ), 0)
                + coalesce( cast( _DevCancSUM.Amount as abap.dec(23,2) ), 0) < 0 )

           then cast(  coalesce( cast( _RemDiaSUM.ValorTotalRemessa as abap.dec(23,2) ), 0)
                      + coalesce( cast( _AdiPagSUM.Amount as abap.dec(23,2) ), 0)
                      + coalesce( cast( _FatSUM.Amount as abap.dec(23,2) ), 0)
                      + coalesce( cast( _FatDRCSUM.AMOUNT as abap.dec(23,2) ), 0)
                      + coalesce( cast( _DevCancSUM.Amount as abap.dec(23,2) ), 0)
                      as abap.curr(23,2) )

           else cast( 0 as abap.curr(23,2) )

           end                     as PaymentAmount,

      case when ( coalesce( cast( _RemDiaSUM.ValorTotalRemessa as abap.dec(23,2) ), 0)
                + coalesce( cast( _AdiPagSUM.Amount as abap.dec(23,2) ), 0)
                + coalesce( cast( _FatSUM.Amount as abap.dec(23,2) ), 0)
                + coalesce( cast( _FatDRCSUM.AMOUNT as abap.dec(23,2) ), 0)
                + coalesce( cast( _DevCancSUM.Amount as abap.dec(23,2) ), 0) < 0 )
           then 3
           else 1
           end                     as PaymentAmountCrit
}
