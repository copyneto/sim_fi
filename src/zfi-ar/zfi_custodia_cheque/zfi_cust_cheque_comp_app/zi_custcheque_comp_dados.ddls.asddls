@AbapCatalog.sqlViewName: 'ZVFICUSTCHEQH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados Custodia'
define view ZI_CUSTCHEQUE_COMP_DADOS
  as select distinct from ztfi_cust_cheque    as _Cheque

    inner join            ZI_FI_RAIZCNPJ      as _Raiz    on _Cheque.kunnr = _Raiz.Kunnr
    inner join            I_Customer          as _Cliente on _Cheque.kunnr = _Cliente.Customer
    inner join            I_CompanyCodeVH     as _Emp     on _Cheque.bukrs = _Emp.CompanyCode

    left outer join       ZI_FI_SOMAR_CHEQUES as _Atrib   on  _Cheque.bukrs    = _Atrib.Bukrs
                                                          and _Cheque.kunnr    = _Atrib.Kunnr
                                                          and _Atrib.Atribuido = 'X'

    left outer join       ZI_FI_SOMAR_CHEQUES as _NAtrib  on  _Cheque.bukrs     = _NAtrib.Bukrs
                                                          and _Cheque.kunnr     = _NAtrib.Kunnr
                                                          and _NAtrib.Atribuido = ''

    left outer join       ZI_FI_SOMAR_FATURAS as _Fat     on  _Cheque.bukrs  = _Fat.Bukrs
                                                          and _Cheque.kunnr  = _Fat.Kunnr
                                                          and _Fat.Atribuido = 'X'

    left outer join       ZI_FI_SOMAR_FATURAS as _NFat    on  _Cheque.bukrs   = _NFat.Bukrs
                                                          and _Cheque.kunnr   = _NFat.Kunnr
                                                          and _NFat.Atribuido = ''

{
  key _Cheque.bukrs                              as Bukrs,
  key _Cheque.kunnr                              as Kunnr,
      _Raiz.raizcnpj                             as RaizCnpj,
      _Cliente.CustomerFullName                  as Name,
      _Emp.CompanyCodeName                       as DescEmp,
      _Cheque.moeda                              as Moeda,
      @Semantics.amount.currencyCode: 'Moeda'
      _Atrib.Valor                               as ValorChequeAtribuido,
      @Semantics.amount.currencyCode: 'Moeda'
      _NAtrib.Valor                              as ValorChequeNAtribuido,
      @Semantics.amount.currencyCode: 'Moeda'
      _Fat.Valor                                 as ValorFatAtribuido,
      @Semantics.amount.currencyCode: 'Moeda'
      _NFat.Valor                                as ValorFatNAtribuido,
      @Semantics.amount.currencyCode: 'Moeda'
      cast( _Atrib.Valor - _Fat.Valor as wrbtr ) as ValorResidual
}
