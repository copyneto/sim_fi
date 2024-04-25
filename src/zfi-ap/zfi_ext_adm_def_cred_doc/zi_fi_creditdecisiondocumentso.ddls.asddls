@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Deferimentos de crédito doc. - Campos OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_CREDITDECISIONDOCUMENTSO

  as select from I_SalesOrder as _SalesOrder

  association [0..1] to I_PaymentMethod as _PaymentMethod on  _PaymentMethod.Country       = $projection.Country
                                                          and _PaymentMethod.PaymentMethod = $projection.PaymentMethod
{
  key cast( 'VBAK' as ukm_dcd_obj_type )                                          as CreditDecisionObjectType,

  key cast( ltrim( SalesOrder, '0' ) as ukm_dcd_obj_id )                          as CreditDecisionReferenceDoc,

      SalesDocApprovalStatus                                                      as SalesDocApprovalStatus,
      _SalesDocApprovalStatus.
      _Text[ 1:Language=$session.system_language ].SalesDocApprovalStatusDesc     as SalesDocApprovalStatusDesc,
      OverallSDDocumentRejectionSts                                               as OverallSDDocumentRejectionSts,
      _OverallSDDocumentRejectionSts.
      _Text[ 1:Language=$session.system_language ].OvrlSDDocumentRejectionStsDesc as OvrlSDDocumentRejectionStsDesc,
      CustomerPaymentTerms                                                        as CustomerPaymentTerms,
      _CustomerPaymentTerms.
      _Text[ 1:Language=$session.system_language ].CustomerPaymentTermsName       as CustomerPaymentTermsName,

      cast( 'BR' as land1 )                                                       as Country,

      PaymentMethod                                                               as PaymentMethod,
      _PaymentMethod,
      SalesOrderCondition                                                         as SalesOrderCondition

}
