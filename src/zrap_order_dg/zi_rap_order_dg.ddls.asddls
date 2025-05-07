@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Order BO view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_RAP_ORDER_DG as select from zrap_aorder_dg as Orders
composition [1..*] of ZI_RAP_ITEM_DG as _Item

association [0..1] to ZI_RAP_ITEM_COUNT_DG as _ItemCount on $projection.Orderuuid = _ItemCount.Orderid
      
association [0..1] to /DMO/I_Customer as _Customer on $projection.CustomerId = _Customer.CustomerID
association [0..1] to I_Country as _Country on $projection.DeliveryCountry = _Country.Country
{
    key orderuuid as Orderuuid,
    orderid as Orderid,
    name as Name,
    status as Status,
    customer_id as CustomerId,
    creation_date as CreationDate,
    cancellation_date as CancellationDate,
    completion_date as CompletionDate,
    delivery_country as DeliveryCountry,
    total_price as TotalPrice,
    _ItemCount.item_count as ItemCount,
    case 
      when _ItemCount.item_count < 3 then 'Easy'
      when _ItemCount.item_count > 5 then 'Complex'
      else 'Medium'
    end as Complexity,
    

/* associations */
       _Item,
       _ItemCount,
       _Customer,
       _Country
}
