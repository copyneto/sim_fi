@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help Customer'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZI_FI_VH_CUSTOMER as select from I_Customer
{
    @ObjectModel.text.element: ['OrganizationBPName1']
    key Customer,
    OrganizationBPName1,
    @EndUserText.label: 'CNPJ'
    TaxNumber1,
    @EndUserText.label: 'CPF'
    TaxNumber2  
}
where DeletionIndicator <> 'X'
