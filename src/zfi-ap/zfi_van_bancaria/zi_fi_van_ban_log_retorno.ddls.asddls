@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programa de log de retorno'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_FI_VAN_BAN_LOG_RETORNO
  as select from ztfi_log_retorno
  association [1..1] to I_CompanyCode  as _CompanyCode on _CompanyCode.CompanyCode = $projection.Empresa
  association [1..1] to ZI_CA_VH_USERS as _User        on _User.UserID = $projection.Usuario
{
  key data     as Data,
  key hora     as Hora,
  key arquivo  as Arquivo,
  key seqnr    as Sequencial,
      bukrs    as Empresa,
      usuario  as Usuario,
      msgty    as MsgTipo,
      msgid    as MsgId,
      msgno    as Msgnumero,
      mensagem as Mensagem,

      case msgty
        when 'E' then 1
        when 'A' then 1
        when 'I' then 2
        when 'W' then 2
        when 'S' then 3
        else 0
      end      as Criticality,

      _CompanyCode,
      _User
}
