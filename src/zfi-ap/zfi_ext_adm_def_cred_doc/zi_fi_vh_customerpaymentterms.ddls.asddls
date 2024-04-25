@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help - Condição de pagamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
} 
@ObjectModel.resultSet.sizeCategory: #XS

define view entity ZI_FI_VH_CUSTOMERPAYMENTTERMS
  as select from I_CustomerPaymentTerms
{
      @EndUserText.label: 'Condição de pagamento'
      @ObjectModel.text.element: ['CustomerPaymentTermsName']
      @UI.lineItem: [{ position: 10 }]
  key CustomerPaymentTerms                                                  as CustomerPaymentTerms,

      @Semantics.text: true
      @UI.lineItem: [{ position: 20 }]
      _Text[ 1:Language=$session.system_language ].CustomerPaymentTermsName as CustomerPaymentTermsName
}
