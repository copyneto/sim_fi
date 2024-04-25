@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help Documento DMS'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_DMS_VH_DOC

  as select from    I_DocumentInfoRecord                 as Doc
    association [0..1] to ZI_CA_VH_USERS                 as users  on Doc.ResponsiblePersonName  = users.UserID
    association [0..1] to I_DocumentInfoRecordDocStatusT as status on Doc.InternalDocumentStatus = status.InternalDocumentStatus
                                                                   and status.Language           = $session.system_language
{

  key Doc.DocumentInfoRecordDocType    as TpDoc,
  key Doc.DocumentInfoRecordDocNumber  as Doc,
  key Doc.DocumentInfoRecordDocVersion as VersDoc,
  key Doc.DocumentInfoRecordDocPart    as DocParc,
      Doc.ResponsiblePersonName        as Usuario,
      Doc.InternalDocumentStatus       as Status,
      status.DocumentStatusName        as DescStatus,
      users.Nome                       as DescName
}
where
  Doc.InternalDocumentStatus = 'H3'

