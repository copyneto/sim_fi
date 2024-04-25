@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cockpit Bombeio - Faturas DRC'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COCKPIT_BOMBEIO_FAT_DRC
  as select from I_BR_NFDocument               as _NFItem

    inner join   ZI_FI_PARAM_BOMBEIO_CNPJ_CENT as _Parameter on _Parameter.Supplier = _NFItem.BR_NFPartner

  association        to parent ZI_FI_COCKPIT_BOMBEIO as _Bombeio       on  _Bombeio.CompanyCode = $projection.CompanyCode

  association [0..1] to I_BusinessPlace              as _BusinessPlace on  _BusinessPlace.CompanyCode   = $projection.CompanyCode
                                                                       and _BusinessPlace.BusinessPlace = $projection.BusinessPlace


{
  key _NFItem.CompanyCode                     as CompanyCode,
  key _NFItem.BR_NotaFiscal                   as BR_NotaFiscal,

      _NFItem.BusinessPlace                   as BusinessPlace,
      _BusinessPlace.BusinessPlaceDescription as BusinessPlaceDescription,
      _NFItem.BR_NFPartner                    as BR_NFPartner,
      _NFItem._BR_NFPartner.BR_NFPartnerName1 as BR_NFPartnerName,
      _NFItem.BR_NFAuthenticationDate         as BR_NFAuthenticationDate,
      _NFItem.BR_NFeNumber                    as BR_NFeNumber,
      _NFItem.BR_NFOriginalAmount             as BR_NFOriginalAmount,
      - _NFItem.BR_NFNetAmount                as BR_NFNetAmount,
      _NFItem.SalesDocumentCurrency           as Currency,

      /* Associations */
      _Bombeio

}
