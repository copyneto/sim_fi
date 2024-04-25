@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface Retirar Clientes'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_FI_RET_CLIENT as select from ztfi_retirar_cli as zret
left outer join I_Customer on zret.cliente = I_Customer.Customer
{
    key zret.cliente as Cliente,
    I_Customer.OrganizationBPName1 as Nome,
    @Semantics.user.createdBy: true
    zret.usuario_criacao as UsuarioCriacao,
    @Semantics.systemDateTime.createdAt: true 
    zret.data_criacao    as DataCriacao
}
