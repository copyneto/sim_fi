@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help Aprovadores'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_DMS_VH_APROVADOR
  as select from    usr21                as _user
    left outer join ESH_N_USER_USER_NAME as _USER_NAME on _user.persnumber = _USER_NAME.persnumber

{
  key _user.bname          as UserID,
      _USER_NAME.name_text as Nome

}
