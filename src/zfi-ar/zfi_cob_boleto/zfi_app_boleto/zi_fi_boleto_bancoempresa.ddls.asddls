@AbapCatalog.sqlViewName: 'ZVFIBOLBANK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Buscar Banco empresa boleto'
define view ZI_FI_BOLETO_BANCOEMPRESA
  as select from bsid_view as _Fat
{

  key _Fat.bukrs                        as Empresa,
  key _Fat.belnr                        as Documento,
  key _Fat.buzei                        as Parcela,
  key _Fat.gjahr                        as Exercicio,

      concat( _Fat.bukrs , _Fat.hbkid ) as BancoEmpresaKey

}
