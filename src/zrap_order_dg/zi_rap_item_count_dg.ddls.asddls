@AbapCatalog.sqlViewName: 'ZITEMCOUNT'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Complexity'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_RAP_ITEM_COUNT_DG as select from zrap_aitem_dg
{
    orderuuid as Orderid,
    count(*) as item_count
}
group by orderuuid
