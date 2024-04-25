@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Interface Bloqueio de Clientes'
@Metadata.ignorePropagatedAnnotations: true

define root view entity ZI_FI_BLOQ_CLIENT
  as select from ztfi_bloq_client as zbloq
  left outer join I_Customer on zbloq.cliente = I_Customer.Customer
{
  key zbloq.cliente         as Cliente,
      zbloq.dias            as Dias,
      @Semantics.user.createdBy: true
      zbloq.usuario_criacao as UsuarioCriacao,
      @Semantics.systemDateTime.createdAt: true 
      zbloq.data_criacao    as DataCriacao,
      @Semantics.user.lastChangedBy: true
      zbloq.usuario_modif   as UsuarioModif,
      @Semantics.systemDateTime.lastChangedAt: true
      zbloq.data_modif      as DataModif,
      I_Customer.OrganizationBPName1 as Nome
}
