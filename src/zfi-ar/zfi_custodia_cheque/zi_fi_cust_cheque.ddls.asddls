@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Custódia de cheque'
define root view entity ZI_FI_CUST_CHEQUE
  as select from ztfi_cust_cheque as _Cheque

  association to ZI_FI_VH_STATUS_CHEQUE as _Status on  _Cheque.status = _Status.ObjetoId

  association to I_CompanyCodeVH        as _Emp    on  _Cheque.bukrs = _Emp.CompanyCode

  association to I_Customer             as _Cli    on  _Cheque.kunnr = _Cli.Customer
  association to zi_fi_ZTERM_VH         as _Cond   on  _Cheque.zterm = _Cond.Zterm
  association to P_BusinessPlace        as _Local  on  _Cheque.bukrs = _Local.bukrs
                                                   and _Cheque.zterm = _Local.branch


{
  key bukrs                 as Bukrs,
  key kunnr                 as Kunnr,
  key ncheque               as Ncheque,
      _Emp.CompanyCodeName  as DescEmp,
      _Cli.CustomerFullName as DescName,
      budat                 as Budat,
      ncontrato             as Ncontrato,
      ncontratojuridico     as Ncontratojuridico,
      bupla                 as Bupla,
      _Local._text.name     as DescLocal,
      zterm                 as Zterm,
      _Cond.Text1           as DescCond,
      nchamado              as Nchamado,
      @Semantics.amount.currencyCode : 'Moeda'
      valor                 as Valor,
      moeda                 as Moeda,
      status                as Status,
      _Status.ObjetoName    as Desc_status,
      ''                    as NovoStatus,
      doc                   as Doc,
      gjahr                 as Gjahr,
      doc_estorno           as DocEstorno,
      doc_devolucao         as DocDevolucao,
      doc_compensacao       as DocCompensacao,
      zcmc7                 as Zcmc7,
      hktid                 as Hktid,
      bankn                 as Bankn,
      bkont                 as Bkont,
      zcamara               as Zcamara,
      case doc
      when ''
       then 'Não Contabilizado'
      else 'Contabilizado'
      end                   as VHContabilizadotxt,
      case doc
      when ''
       then '0'
      else '1'
      end                   as VHContabilizado



}
