@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Items Cheque para compensação'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_CUSTCHEQUE_COMP_ICHEQUES
  as select from ZI_CUSTCHEQUE_COMP_CHEQUES as Cheque


  association to parent ZI_CUSTCHEQUE_COMP_H as _Header on  $projection.Bukrs = _Header.Bukrs
                                                        and $projection.Kunnr = _Header.Kunnr
{
  key Bukrs,
  key Kunnr,
  key Ncheque,
      CriticalityAtri,
      Atribuido,
      @Semantics.amount.currencyCode: 'Moeda'
      Dinheiro,
      @Semantics.amount.currencyCode: 'Moeda'
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
      _Header
}
