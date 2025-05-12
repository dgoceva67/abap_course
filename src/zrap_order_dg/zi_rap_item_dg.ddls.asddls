@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Item BO view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_RAP_ITEM_DG as select from zrap_aitem_dg
association to parent ZI_RAP_ORDER_DG as _Order on $projection.Orderuuid = _Order.Orderuuid

association [0..1] to I_Currency      as _Currency   on $projection.CurrencyCode    = _Currency.Currency 
{
    key itemuuid as Itemuuid,
    orderuuid as Orderuuid,
    name as Name,
    @Semantics.amount.currencyCode: 'CurrencyCode'
    price as Price,
    currency_code as CurrencyCode,
    quantity as Quantity,
    @Semantics.systemDateTime.lastChangedAt: true
    @EndUserText.label: 'Last Changed At'
    last_changed_at as LastChangedAt,
    
    /* associations */
       _Order,
       _Currency
}
