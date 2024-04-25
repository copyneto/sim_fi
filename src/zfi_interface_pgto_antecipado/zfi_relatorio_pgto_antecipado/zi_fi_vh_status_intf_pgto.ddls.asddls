@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Help Search: Centro'
@Search.searchable: true

define root view entity ZI_FI_VH_STATUS_INTF_PGTO
  as select from I_DomainFixedValueText


{

      @ObjectModel.text.element: ['DomainText']
      @Search.ranking: #MEDIUM
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
  key DomainValue,

      @Semantics.text: true
      @Search.defaultSearchElement: true
      @Search.ranking: #HIGH
      @Search.fuzzinessThreshold: 0.7
      DomainText

}
where
      Language                    = $session.system_language
  and 'ZD_STATUS_PGTO_ANTECIPADO' = SAPDataDictionaryDomain
