extend view entity R_CreditDecisionDocumentTP with

association [0..1] to ZI_FI_CREDITDECISIONDOCUMENTEX as _ExtensionSD on  _ExtensionSD.CreditDecisionObjectType   = I_CreditDecisionDocument.CreditDecisionObjectType
                                                                     and _ExtensionSD.CreditDecisionReferenceDoc = I_CreditDecisionDocument.CreditDecisionReferenceDoc

{
  _ExtensionSD.SalesDocApprovalStatus,
  _ExtensionSD.SalesDocApprovalStatusDesc,
  _ExtensionSD.OverallSDDocumentRejectionSts,
  _ExtensionSD.OvrlSDDocumentRejectionStsDesc,
  _ExtensionSD.CustomerPaymentTerms,
  _ExtensionSD.CustomerPaymentTermsName,
  _ExtensionSD.PaymentMethod,
  _ExtensionSD.PaymentMethodDescription,
  _ExtensionSD.SalesOrderCondition
}
