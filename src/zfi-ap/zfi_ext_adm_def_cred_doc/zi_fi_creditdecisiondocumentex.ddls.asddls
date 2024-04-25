@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Deferimentos de crédito doc. - Campos OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_CREDITDECISIONDOCUMENTEX

  as select from ZI_FI_CREDITDECISIONDOCUMENTSO
{
  key CreditDecisionObjectType,
  key CreditDecisionReferenceDoc,
      SalesDocApprovalStatus,
      SalesDocApprovalStatusDesc,
      OverallSDDocumentRejectionSts,
      OvrlSDDocumentRejectionStsDesc,
      CustomerPaymentTerms,
      CustomerPaymentTermsName,
      Country,
      PaymentMethod,
      _PaymentMethod._Text[ 1:Language=$session.system_language ].PaymentMethodDescription,
      SalesOrderCondition

}
