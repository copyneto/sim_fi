@EndUserText.label: 'Projeção - Custodia Cheque Faturas'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_CUSTCHEQUE_COMP_IFAT
  as projection on ZI_CUSTCHEQUE_COMP_IFAT
{
  key Bukrs,
  key Kunnr,
  key Raizcnpj,
  key Doc,
  key Gjahr,
  key Buzei,
      Atribuido,
      CriticalityAtri,
      Wrbtr,
      Moeda,
      ClienteFat,
      blart,
      xblnr,
      bschl,
      umskz,
      zuonr,
      /* Associations */
      _Header : redirected to parent ZC_CUSTCHEQUE_COMP_H
}
