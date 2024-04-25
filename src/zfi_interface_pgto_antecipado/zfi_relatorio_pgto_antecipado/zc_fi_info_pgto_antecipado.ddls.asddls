@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Relatório Solic. Pgto. Antecipado'
@Metadata.allowExtensions: true


define root view entity ZC_FI_INFO_PGTO_ANTECIPADO
  as select from ZI_FI_INFO_PGTO_ANTECIPADO

{
  key vbeln,
      bukrs,
      @ObjectModel.text.element: [ 'CustomerName' ]
      kunnr,
      valor,
      TransactionCurrency,
      criado_em,
      hora_registro,
      usuario,
      DocFatura,
      DocContabilFatura,
      Doc_adiantamento,
      Doc_compensacao,
      DocCompensacaoFatura,
      @ObjectModel.text.element: [ 'Status_Txt' ]
      status,
      Txid,
      flag_cancelamento,
      Status_Txt,
      CustomerName,
      Status_Pgto,
      ObjpageTitle,

      @ObjectModel.filter.enabled: false
      _Mensagens

}
