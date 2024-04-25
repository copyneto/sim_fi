@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Custódia de cheque - Header'
define root view entity ZI_CUSTCHEQUE_COMP_H
  as select from ZI_CUSTCHEQUE_COMP_DADOS as _Cheque

  composition [0..*] of ZI_CUSTCHEQUE_COMP_ICHEQUES as _Cheques
  composition [0..*] of ZI_CUSTCHEQUE_COMP_IFAT     as _Faturas


{
  key Bukrs,
  key Kunnr,
      RaizCnpj,
      Name,
      DescEmp,
      _Cheque.Moeda,
      _Cheque.ValorChequeAtribuido,
      _Cheque.ValorChequeNAtribuido,
      _Cheque.ValorFatAtribuido,
      _Cheque.ValorFatNAtribuido,
      _Cheque.ValorResidual,
      _Cheques,
      _Faturas
}
