@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help - Forma de pagamento'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS

define view entity ZI_FI_VH_PAYMENTMETHOD
  as select from I_PaymentMethod
{
      @EndUserText.label: 'Forma de pagamento'
      @ObjectModel.text.element: ['PaymentMethodDescription']
      @UI.lineItem: [{ position: 10 }]
  key PaymentMethod                                                         as PaymentMethod,

      @Semantics.text: true
      @UI.lineItem: [{ position: 20 }]
      _Text[ 1:Language=$session.system_language ].PaymentMethodDescription as PaymentMethodDescription
}
where
  Country = 'BR'
