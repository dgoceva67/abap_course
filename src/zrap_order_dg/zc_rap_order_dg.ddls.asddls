@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Order BO projection view'
@Search.searchable: true
@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZC_RAP_ORDER_DG as projection on ZI_RAP_ORDER_DG as Orders
{
    key Orderuuid,
    @Search.defaultSearchElement: true
    Orderid,
    Name,
    @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_RAP_STATUS_DG', element: 'StatusID'} }]
    @ObjectModel.text.element: ['StatusText']
    @Search.defaultSearchElement: true
    Status,
    StatusText,
    @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Customer', element: 'CustomerID'} }]
    @ObjectModel.text.element: ['CustomerName']
    @Search.defaultSearchElement: true
    CustomerId as CustomerId,
    _Customer.LastName as CustomerName,
    @Search.defaultSearchElement: true
    CreationDate,
    CancellationDate,
    CompletionDate,
    @Consumption.valueHelpDefinition: [{ entity: { name: 'I_Country', element: 'Country'} }]
    @ObjectModel.text.element: ['CustomerName']
    @Search.defaultSearchElement: true
    DeliveryCountry,
    @Consumption.valueHelpDefinition: [{entity: {name: 'I_Currency', element: 'Currency' }}]
    CurrencyCode,
    TotalPrice,
    Complexity,
    LastChangedAt,
    /* Associations */
    _Customer,
    _Item : redirected to composition child ZC_RAP_ITEM_DG
}
