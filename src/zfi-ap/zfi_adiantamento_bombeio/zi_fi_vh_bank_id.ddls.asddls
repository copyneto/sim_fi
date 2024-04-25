@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help: Banco da Empresa'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_VH_BANK_ID
  as select from t012 as _bank

  association [0..*] to V_T012t_Ddl as _text on  _text.spras = $session.system_language
                                             and _text.bukrs = _bank.bukrs
                                             and _text.hbkid = _bank.hbkid
{

      @UI.lineItem : [{ position: 10, label: 'Empresa' }]
  key _bank.bukrs        as bukrs,

      @UI.lineItem : [{ position: 20, label: 'Banco da Empresa' }]
  key _bank.hbkid        as hbkid,

      @UI.lineItem : [{ position: 30, label: 'Chave do país/região do banco' }]
      _bank.banks        as banks,

      @UI.lineItem : [{ position: 40, label: 'Chave do banco' }]
      _bank.bankl        as bankl,

      @UI.lineItem : [{ position: 50, label: 'Descrição'  }]
      max( _text.text1 ) as text1
}
group by
  _bank.bukrs,
  _bank.hbkid,
  _bank.banks,
  _bank.bankl
