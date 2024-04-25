@AbapCatalog.sqlViewName: 'ZVFICHEQCOMP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Cheques Comp'
define view ZI_CUSTCHEQUE_COMP_CHEQUES
  as select from ztfi_custcheq_cp as _Cheque

    inner join   bsid             as _Fat       on  _Cheque.bukrs = _Fat.bukrs
                                                and _Cheque.gjahr = _Fat.gjahr
                                                and _Cheque.doc   = _Fat.belnr
                                                and _Cheque.kunnr = _Fat.kunnr

    inner join   Fclm_Bseg_Basic  as _Venc      on  _Venc.bukrs = _Fat.bukrs
                                                and _Venc.belnr = _Fat.belnr
                                                and _Venc.buzei = _Fat.buzei
                                                and _Venc.gjahr = _Fat.gjahr

    inner join   I_Customer       as _Cliente   on _Cheque.kunnr = _Cliente.Customer
    inner join   I_CompanyCodeVH  as _Emp       on _Cheque.bukrs = _Emp.CompanyCode

    inner join   F_Mmim_Blart     as _TpDocDesc on _TpDocDesc.AccountingDocumentType = _Fat.blart
{
  key _Cheque.bukrs                         as Bukrs,
  key _Cheque.kunnr                         as Kunnr,
  key _Cheque.ncheque                       as Ncheque,

      _Cheque.atribuido                     as marcado,
      case _Cheque.atribuido
       when 'X'
        then 3
       else 2
       end                                  as CriticalityAtri,

      case _Cheque.atribuido
      when 'X'
       then 'Marcado'
      else 'Não Marcado'
      end                                   as Atribuido,

      _Cheque.dinheiro                      as Dinheiro,
      _Cheque.valor                         as Valor,
      _Cheque.moeda                         as Moeda,
      _Cheque.doc                           as Doc,
      _Cheque.gjahr                         as Gjahr,
      _Fat.buzei                            as Item,
      _Fat.blart                            as TpDoc,
      _TpDocDesc.AccountingDocumentTypeName as TipoDocDesc,
      _Fat.bschl                            as ChaveLanc,
      _Fat.umskz                            as CodRZE,
      _Fat.zuonr                            as Atribuicao,
      _Fat.xblnr                            as Refe,
      _Fat.budat                            as DataLanc,
      _Venc.netdt                           as Vencimento,
      _Fat.xref1                            as Xref1,
      _Fat.xref2                            as Xref2,
      _Cliente.CustomerFullName             as Name,
      _Emp.CompanyCodeName                  as DescEmp
}
