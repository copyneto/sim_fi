@EndUserText.label: 'Projeção - Custodia Cheque Cheques'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_CUSTCHEQUE_COMP_ICHEQUES
  as projection on ZI_CUSTCHEQUE_COMP_ICHEQUES
{
  key Bukrs,
  key Kunnr,
  key Ncheque,
      CriticalityAtri,
      Atribuido,
      Dinheiro,
      Valor,
      Moeda,
      Doc,
      Gjahr,
      Item,
      TpDoc,
      TipoDocDesc,
      ChaveLanc,
      CodRZE,
      Atribuicao,
      Refe,
      DataLanc,
      Vencimento,
      Xref1,
      Xref2,
      Name,
      DescEmp,
      /* Associations */
      _Header : redirected to parent ZC_CUSTCHEQUE_COMP_H
}
