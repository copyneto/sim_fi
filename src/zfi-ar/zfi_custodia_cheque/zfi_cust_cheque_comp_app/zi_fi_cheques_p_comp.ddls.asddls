@AbapCatalog.sqlViewName: 'ZVFICHQCOMP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Buscar cheques para compensar'
define view ZI_FI_CHEQUES_P_COMP
  as select from ztfi_cust_cheque as _Cheque


    inner join   bsid             as _Fat on  _Cheque.bukrs = _Fat.bukrs
                                          and _Cheque.gjahr = _Fat.gjahr
                                          and _Cheque.doc   = _Fat.belnr
                                          and _Cheque.kunnr = _Fat.kunnr

{
  key _Cheque.bukrs             as Bukrs,
  key _Cheque.kunnr             as Kunnr,
  key _Cheque.ncheque           as Ncheque,
      _Cheque.budat             as Budat,
      _Cheque.ncontrato         as Ncontrato,
      _Cheque.ncontratojuridico as Ncontratojuridico,
      _Cheque.bupla             as Bupla,
      _Cheque.zterm             as Zterm,
      _Cheque.nchamado          as Nchamado,
      _Cheque.valor             as Valor,
      _Cheque.moeda             as Moeda,
      _Cheque.status            as Status,
      _Cheque.doc               as Doc,
      _Cheque.gjahr             as Gjahr,
      _Cheque.buzei             as Buzei,
      _Cheque.doc_estorno       as DocEstorno,
      _Cheque.doc_devolucao     as DocDevolucao,
      _Cheque.doc_compensacao   as DocCompensacao,
      _Cheque.zcmc7             as Zcmc7,
      _Cheque.hktid             as Hktid,
      _Cheque.bankn             as Bankn,
      _Cheque.bkont             as Bkont,
      _Cheque.zcamara           as Zcamara

}
where
      _Cheque.doc_estorno     = ''
  and _Cheque.doc_devolucao   = ''
  and _Cheque.doc_compensacao = ''
