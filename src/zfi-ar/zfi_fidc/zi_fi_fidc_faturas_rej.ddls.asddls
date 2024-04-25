@AbapCatalog.sqlViewName: 'ZVFIDCFATREJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Buscar faturas FIDC Rejeitada'
define view ZI_FI_FIDC_FATURAS_REJ
  as select from bsid             as Fat

    inner join   kna1             as _Cli    on Fat.kunnr = _Cli.kunnr
    inner join   bseg             as _FatVec on  Fat.belnr = _FatVec.belnr
                                             and Fat.bukrs = _FatVec.bukrs
                                             and Fat.gjahr = _FatVec.gjahr
                                             and Fat.buzei = _FatVec.buzei
    inner join   j_1bnflin        as _NFIem  on Fat.vbeln = _NFIem.refkey
    inner join   j_1bnfdoc        as _NFDoc  on _NFIem.docnum = _NFDoc.docnum
    inner join   Zi_fi_email_FIDC as _Mail   on Fat.kunnr = _Mail.Partner
    inner join   j_1bnfe_active   as _Active on _NFDoc.docnum = _Active.docnum

{
  key Fat.bukrs,
  key Fat.gjahr,
  key Fat.belnr,
  key Fat.buzei,
      Fat.kkber,
      Fat.gsber,
      Fat.kunnr,
      Fat.xblnr,
      Fat.xref3,
      Fat.waers,
      Fat.wrbtr,
      Fat.budat,
      _NFDoc.cnpj_bupla                   as CNPJEmissor,
      _Cli.name1                          as NomeCliente,
      _Cli.stcd1                          as CNPJCliente,
      _Cli.telf1                          as TELEFONE1,
      _Cli.telf2                          as TELEFONE2,
      _NFDoc.docnum                       as DocNum,
      _NFDoc.stras                        as EndCliente,
      _NFDoc.house_num2                   as CompEnd,
      _NFDoc.ort02                        as Bairro,
      _NFDoc.ort01                        as Cidade,
      _NFDoc.regio                        as UF,
      _NFDoc.pstlz                        as CEP,
      _NFDoc.nftot                        as ValorNF,
      _FatVec.netdt                       as Vencimento,
      _Mail.Email1                        as Email1,
      _Mail.Email2                        as Email2,
      Concat( _Active.regio ,
              concat( _Active.nfyear ,
              concat( _Active.nfmonth,
              concat( _Active.stcd1,
              concat(_Active.model,
              concat( _Active.serie,
              concat( _Active.nfnum9,
              concat( _Active.docnum9,
              _Active.cdv ) ) ) ) ) ) ) ) as Chave,

      Fat.vbeln

}

where
      Fat.zlsch = 'E'
  and Fat.hbkid = 'FIDC'
  and Fat.xref3 = ''
  and Fat.vbeln = ''
