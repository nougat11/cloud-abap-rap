@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS View forRAPGeneratorBONode'
define view entity ZDMO_I_RAPGENERATORBONODE
  as projection on ZDMO_R_RapGeneratorBONode
  
  {
 key NodeUUID,
 HeaderUUID,
 ParentUUID,
 RootUUID,
 NodeNumber,
 IsRootNode,
 EntityName,
 ParentEntityName,
 DataSource,
 ParentDataSource,
 ViewTypeValue,
 FieldNameObjectID,
 FieldNameEtagMaster,
 FieldNameTotalEtag,
 FieldNameUUID,
 FieldNameParentUUID,
 FieldNameRootUUID,
 FieldNameCreatedBy,
 FieldNameCreatedAt,
 FieldNameLastChangedBy,
 FieldNameLastChangedAt,
 FieldNameLocLastChangedAt,
 CdsIView,
 CdsRView,
 CdsPView,
 MdeView,
 BehaviorImplementationClass,
 ServiceDefinition,
 ServiceBinding,
 ControlStructure,
 QueryImplementationClass,
 DraftTableName,
 HierarchyDistanceFromRoot,
 HierarchyDescendantCount,
 HierarchyDrillState,
 HierarchyPreorderRank,
 LocalLastChangedAt,
 /* Associations */
 _Child,
 _Parent,
 _RAPGeneratorBO : redirected to parent ZDMO_I_RAPGENERATORBO
}
