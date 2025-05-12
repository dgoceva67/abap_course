@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Item BO projection view'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_RAP_ITEM_DG as projection on ZI_RAP_ITEM_DG as Item
{
    key Itemuuid,
    Orderuuid,
    Name,
    @Semantics.amount.currencyCode: 'CurrencyCode'
    Price,
    @Consumption.valueHelpDefinition: [{entity: {name: 'I_Currency', element: 'Currency' }}]
    CurrencyCode,
    Quantity,
    LastChangedAt,
    /* Associations */
    _Currency,
    _Order : redirected to parent ZC_RAP_ORDER_DG
}
