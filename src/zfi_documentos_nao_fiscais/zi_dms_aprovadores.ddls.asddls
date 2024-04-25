@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'App para Manutenção dos Aprovadores'
define root view entity ZI_DMS_APROVADORES
  as select from ztdms_aprovador as _Aprovadores
  association to ZI_CA_VH_USERS as _users on _Aprovadores.aprovador = _users.UserID
{
      @EndUserText.label: 'Aprovador'
  key _Aprovadores.aprovador             as Aprovador,

      @EndUserText.label: 'DescAprovador'
      _users.Nome                        as DescAprovador,

      @EndUserText.label: 'Status'
      _Aprovadores.status                as Status,

      @Semantics.user.createdBy: true
      @EndUserText.label: 'Criado por'
      _Aprovadores.created_by            as Criado_por,

      @Semantics.systemDateTime.createdAt: true
      @EndUserText.label: 'Criado em'
      _Aprovadores.created_at            as Criado_em,

      @Semantics.user.lastChangedBy: true
      @EndUserText.label: 'Modificado'
      _Aprovadores.last_changed_by       as Modificado_por,

      @Semantics.systemDateTime.lastChangedAt: true
      @EndUserText.label: 'Modificado em'
      _Aprovadores.last_changed_at       as Modificado_em,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      @EndUserText.label: 'Hora'
      _Aprovadores.local_last_changed_at as Hora

}
