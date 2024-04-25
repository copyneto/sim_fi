@AbapCatalog.sqlViewName: 'ZVFIDCMAIL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Buscar email FIDC'
define view Zi_fi_email_FIDC
  as select distinct from but020 as BP

    left outer join       adr6   as _Mail1 on  BP.addrnumber     =  _Mail1.addrnumber
                                           and _Mail1.consnumber =  '001'
                                           and _Mail1.smtp_addr  <> ''
    left outer join       adr6   as _Mail2 on  BP.addrnumber     =  _Mail2.addrnumber
                                           and _Mail2.consnumber =  '002'
                                           and _Mail2.smtp_addr  <> ''
{

  key BP.partner       as Partner,
      _Mail1.smtp_addr as Email1,
      _Mail2.smtp_addr as Email2

}
where
  _Mail1.smtp_addr <> ''
