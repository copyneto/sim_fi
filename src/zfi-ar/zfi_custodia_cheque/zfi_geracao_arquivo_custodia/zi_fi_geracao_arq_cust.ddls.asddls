@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Geração arquivo custódia'

/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZI_FI_GERACAO_ARQ_CUST
  as select from ZI_FI_CUST_CHEQUE as _Cheque

  //  association[1..1] to t001 as _Empresa on _Empresa.bukrs = $projection.Bukrs
  association [1..1] to kna1                       as _Cliente     on  _Cliente.kunnr = $projection.Kunnr
  
  association [1..1] to ZI_FI_GERACAO_ARQ_INFO_DEPOS as _Depos on  _Depos.bukrs = $projection.Bukrs
                                                               and _Depos.belnr = $projection.Doc
                                                               and _Depos.gjahr = $projection.Gjahr
                                                               and _Depos.kunnr = $projection.Kunnr  
  //  association        to bsid                       as _Fat         on  _Fat.bukrs = $projection.Bukrs
  //                                                                   and _Fat.belnr = $projection.Doc
  //                                                                   and _Fat.gjahr = $projection.Gjahr
  //                                                                   and _Fat.kunnr = $projection.Kunnr
  //
  //  association        to t012k                      as _T012k       on  _T012k.bukrs = $projection.Bukrs
  //                                                                   and _T012k.hbkid = _Fat.hbkid

  association [1..1] to ZI_CUSTCHEQUE_COMP_CHEQUES as _ChequesComp on  _ChequesComp.Bukrs   = $projection.Bukrs
                                                                   and _ChequesComp.Kunnr   = $projection.Kunnr
                                                                   and _ChequesComp.Ncheque = $projection.Ncheque


{

  key _Cheque.Bukrs,             //Bukrs,
  key _Cheque.Kunnr,             //Kunnr,
  key _Cheque.Ncheque,           //Ncheque,
      _Cheque.Budat,             //Budat,
      _Cheque.Ncontrato,         //Ncontrato,
      _Cheque.Ncontratojuridico, //Ncontratojuridico,
      _Cheque.Bupla,             //Bupla,
      _Cheque.Zterm,             //Zterm,
      _Cheque.Nchamado,          //Nchamado,
      _Cheque.Valor,             //Valor,
      _Cheque.Moeda,             //Moeda,
      _Cheque.Status,
      _Cheque.Desc_status,
//      _Cheque.NovoStatus,
      _Cheque.Doc,
      _Cheque.Gjahr,
      _Cheque.DocEstorno,
      _Cheque.DescLocal,
      _Cheque.DescCond,
      _ChequesComp.TpDoc,
      _ChequesComp.ChaveLanc,
      _ChequesComp.CodRZE,
      _ChequesComp.Atribuicao,
      _ChequesComp.Refe,
      _ChequesComp.Vencimento,
      _ChequesComp.Xref1,
      _ChequesComp.Xref2,
      _Cheque.Zcamara,
      _Cheque.Zcmc7,
      _Cheque.Hktid,
      _Cheque.Bankn,
      _Cheque.DescEmp as NomeDepos,
      _Depos.hktid as AgenciaDepos,
      _Depos.bankn as ContaDepos,
      //_T012k.hktid    as AgenciaDepos,
      //_T012k.bankn    as ContaDepos,
      _Cliente.name1  as ClienteName,
      _Cliente.stcd2  as Cnpj
}
where
      _Cheque.Status     =  '01'
  and _Cheque.Doc        <> ''
  and _Cheque.DocEstorno =  ''
