extend view entity C_CreditDecisionDocumentTP with
{

  @Consumption: { valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_SALESDOCAPPROVALSTAT', element: 'SalesDocApprovalStatus' }}]}

  @EndUserText.label: 'Status de aprovação de documento'
  @ObjectModel.text.element: ['SalesDocApprovalStatusDesc']
  CreditDecisionDocument.SalesDocApprovalStatus,

  @EndUserText.label: 'Texto Status de aprovação de documento'
  CreditDecisionDocument.SalesDocApprovalStatusDesc,

  @Consumption: { valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_OVERALLSDDOCREJSTS', element: 'OverallSDDocumentRejectionSts' }}]}

  @EndUserText.label: 'Status da recusa (todos os itens)'
  @ObjectModel.text.element: ['OvrlSDDocumentRejectionStsDesc']
  CreditDecisionDocument.OverallSDDocumentRejectionSts,

  @EndUserText.label: 'Texto Status da recusa (todos os itens)'
  CreditDecisionDocument.OvrlSDDocumentRejectionStsDesc,

  @Consumption: { valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_CUSTOMERPAYMENTTERMS', element: 'CustomerPaymentTerms' }}]}

  @EndUserText.label: 'Condição de pagamento'
  @ObjectModel.text.element: ['CustomerPaymentTermsName']
  CreditDecisionDocument.CustomerPaymentTerms,

  @EndUserText.label: 'Texto Condição de pagamento'
  CreditDecisionDocument.CustomerPaymentTermsName,

  @Consumption: { valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_PAYMENTMETHOD', element: 'PaymentMethod' }}]}

  @EndUserText.label: 'Forma de pagamento'
  @ObjectModel.text.element: ['PaymentMethodDescription']
  CreditDecisionDocument.PaymentMethod,

  @EndUserText.label: 'Texto Forma de pagamento'
  CreditDecisionDocument.PaymentMethodDescription,

  @Consumption: { valueHelpDefinition: [{ entity: { name: 'ZI_FI_VH_SALESORDERCONDITION', element: 'SalesOrderCondition' }}]}

  @EndUserText.label: 'Nº condição do documento'
  CreditDecisionDocument.SalesOrderCondition
}
