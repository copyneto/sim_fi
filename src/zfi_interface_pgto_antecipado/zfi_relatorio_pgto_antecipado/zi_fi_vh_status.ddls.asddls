@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help Relat. Solic. Pgto. Antecip'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.resultSet.sizeCategory: #XS
define root view entity ZI_FI_VH_STATUS as select from ztfi_adto_ov 
{
    key bukrs as Bukrs,
    key vbeln as Vbeln,

    status as Status

}

where bukrs is not initial
