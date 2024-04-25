@AbapCatalog.sqlViewName: 'ZVFIFORMBOL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Boleto'
define view ZI_FI_DADOS_BOLETO
  as select from    acdoca                    as Doc
    left outer join I_HouseBankAccountLinkage as Banco   on  Doc.rbukrs = Banco.CompanyCode
                                                         and Doc.hbkid  = Banco.HouseBank

    left outer join t001                      as Empresa on Doc.rbukrs = Empresa.bukrs
{

  key Banco.CompanyCode                                                                         as Empresa,
  key Doc.belnr                                                                                 as Documento,
  key Doc.gjahr                                                                                 as Exercicio,
  key Doc.buzei                                                                                 as Item,
      Banco.HouseBank                                                                           as CodBanco,
      concat(left(Banco.BankInternalID, 3), concat('-', substring(Banco.BankInternalID, 4, 1))) as ChaveBanco,
      concat(right(Banco.BankInternalID, 4), concat('-', left(Banco.BankControlKey, 1)))        as Agencia,
      concat( ( Banco.BankAccount), concat('-', SUBSTRING(Banco.BankControlKey,2 ,1) ))           as Contadig,
      Banco.BankInternalID                                                                      as AgenciaSemCod,
      Banco.BankAccount                                                                         as Conta,
      Doc.bldat                                                                                 as DataDocumento,
      Doc.netdt                                                                                 as Vencimento,
      cast( Doc.osl as abap.dec(23,2) )                                                         as ValorDoc,
      //      Carteira.zlsch                                                                            as FormaPag,
      Banco.HouseBankAccount                                                                    as Carteira,
      Doc.kunnr                                                                                 as Cliente,
      Banco.BankControlKey                                                                      as Digito

}
