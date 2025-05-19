CLASS lhc_Order DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Order RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR order RESULT result.

    METHODS setstatuscancelled FOR MODIFY
      IMPORTING keys FOR ACTION order~setStatusCancelled RESULT result.

    METHODS setstatuscompleted FOR MODIFY
      IMPORTING keys FOR ACTION order~setStatusCompleted RESULT result.

    METHODS setcreationdate FOR DETERMINE ON MODIFY
      IMPORTING keys FOR order~setcreationdate.

    METHODS propagateCurrencyCode FOR DETERMINE ON MODIFY
      IMPORTING keys FOR order~propagateCurrencyCode.

    METHODS setinitialstatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR order~setinitialstatus.

    METHODS calculateorderid FOR DETERMINE ON SAVE
      IMPORTING keys FOR order~calculateorderid.

    METHODS validatecustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR order~validatecustomer.

    METHODS validatedeliverycountry FOR VALIDATE ON SAVE
      IMPORTING keys FOR order~validatedeliverycountry.

    METHODS validatecurrencycode FOR VALIDATE ON SAVE
      IMPORTING keys FOR order~validateCurrencyCode.

    METHODS validateOrderName FOR VALIDATE ON SAVE
      IMPORTING keys FOR order~validateOrderName.

ENDCLASS.

CLASS lhc_Order IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.
    SELECT SINGLE Statusid
      FROM zi_rap_status_dg
      WHERE status_text = 'In Process'
      INTO @DATA(lv_in_process_status).

    " Read the travel status of the existing travels
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders)
      FAILED failed.

    result =
      VALUE #(
        FOR order IN orders
          LET is_disabled =   COND #( WHEN order-Status <> lv_in_process_status
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled  )
             IN
            ( %tky                 = order-%tky
              %action-setStatusCompleted = is_disabled
              %action-setStatusCancelled = is_disabled
*              %action-Edit = is_disabled
              %features-%update = is_disabled
              %features-%delete = is_disabled
              %assoc-_Item = is_disabled
             ) ).
  ENDMETHOD.

  METHOD setInitialStatus.
    DATA lv_default_status TYPE zde_status_dg.

    SELECT SINGLE Statusid
    FROM zi_rap_status_dg
    WHERE status_text = 'In Process'
    INTO @lv_default_status.

    " Read relevant order instance data
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    " Remove all order instance data with defined status
    DELETE orders WHERE Status IS NOT INITIAL.
    CHECK orders IS NOT INITIAL.

    " Set default order status
    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
    ENTITY Order
      UPDATE
        FIELDS ( Status )
        WITH VALUE #( FOR order IN orders
                      ( %tky         = order-%tky
                        Status = lv_default_status ) )
    REPORTED DATA(update_reported).

    reported = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.

  METHOD calculateOrderID.
    " check if OrderID is already filled
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        FIELDS ( OrderID ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    " remove lines where OrderID is already filled.
    DELETE orders WHERE OrderID IS NOT INITIAL.

    " anything left ?
    CHECK orders IS NOT INITIAL.

    " Select max order ID
    SELECT SINGLE
        FROM  zrap_aorder_dg
        FIELDS MAX( orderid ) AS orderID
        INTO @DATA(max_orderid).

    " Set the order ID
    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
    ENTITY order
      UPDATE
        FROM VALUE #( FOR order IN orders INDEX INTO i (
          %tky              = order-%tky
          OrderID          = max_orderid + i
          %control-OrderID = if_abap_behv=>mk-on ) )
    REPORTED DATA(update_reported).

    reported = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.

  METHOD validateCustomer.
    " Read relevant order instance data
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        FIELDS ( CustomerID ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    customers = CORRESPONDING #( orders DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.
    IF customers IS NOT INITIAL.
      " Check if customer ID exist
      SELECT FROM /dmo/customer FIELDS customer_id
        FOR ALL ENTRIES IN @customers
        WHERE customer_id = @customers-customer_id
        INTO TABLE @DATA(customers_db).
    ENDIF.

    " Raise msg for non existing and initial customerID
    LOOP AT orders INTO DATA(order).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky        = order-%tky
                       %state_area = 'VALIDATE_CUSTOMER' )
        TO reported-order.

      IF order-CustomerID IS INITIAL OR NOT line_exists( customers_db[ customer_id = order-CustomerID ] ).
        APPEND VALUE #(  %tky = order-%tky ) TO failed-order.

        APPEND VALUE #(  %tky        = order-%tky
                         %state_area = 'VALIDATE_CUSTOMER'
                         %msg        = NEW zcm_rap_order_dg(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zcm_rap_order_dg=>customer_unknown
                                           customerid = order-CustomerID )
                         %element-CustomerID = if_abap_behv=>mk-on )
          TO reported-order.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateDeliveryCountry.
    " Read relevant order instance data
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        FIELDS ( DeliveryCountry ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    DATA delivery_countries TYPE SORTED TABLE OF I_Country WITH UNIQUE KEY country.

    " Optimization of DB select: extract distinct non-initial delivery countries
    delivery_countries = CORRESPONDING #( orders DISCARDING DUPLICATES MAPPING country = DeliveryCountry EXCEPT * ).
    DELETE delivery_countries WHERE country IS INITIAL.

    IF delivery_countries IS NOT INITIAL.
      " Check if agency ID exist
      SELECT FROM I_Country FIELDS country
        FOR ALL ENTRIES IN @delivery_countries
        WHERE country = @delivery_countries-country
        INTO TABLE @DATA(delivery_countries_db).
    ENDIF.

    " Raise msg for non existing and initial deliverycountry
    LOOP AT orders INTO DATA(order).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky               = order-%tky
                       %state_area        = 'VALIDATE_DELIVERY_COUNTRY' )
        TO reported-order.

      IF order-DeliveryCountry IS INITIAL OR NOT line_exists( delivery_countries_db[ country = order-DeliveryCountry ] ).
        APPEND VALUE #( %tky = order-%tky ) TO failed-order.

        APPEND VALUE #( %tky        = order-%tky
                        %state_area = 'VALIDATE_DELIVERY_COUNTRY'
                        %msg        = NEW zcm_rap_order_dg(
                                          severity = if_abap_behv_message=>severity-error
                                          textid   = zcm_rap_order_dg=>delivery_country_unknown
                                          deliverycountry = order-DeliveryCountry )
                        %element-DeliveryCountry = if_abap_behv=>mk-on )
          TO reported-order.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

 METHOD validateOrderName.
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
     ENTITY Order
     FIELDS ( name )
     WITH CORRESPONDING #( keys )
     RESULT DATA(orders).

    LOOP AT orders INTO DATA(order).
      CLEAR reported-order.

      IF order-Name IS INITIAL OR strlen( order-Name ) < 3.
        APPEND VALUE #(
          %tky = order-%tky
          %msg = NEW zcm_rap_order_dg(
            severity = if_abap_behv_message=>severity-error
            textid   = zcm_rap_order_dg=>invalid_order_name
            ordername = order-Name )
          %element-name = if_abap_behv=>mk-on )
          TO reported-order.

        APPEND VALUE #( %tky = order-%tky ) TO failed-order.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateCurrencyCode.
    " Read relevant order instance data
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        FIELDS ( CurrencyCode ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    DATA currency_codes TYPE SORTED TABLE OF I_Currency WITH UNIQUE KEY currency.

    " Optimization of DB select: extract distinct non-initial currency codes
    currency_codes = CORRESPONDING #( orders DISCARDING DUPLICATES MAPPING currency = CurrencyCode EXCEPT * ).
    DELETE currency_codes WHERE currency IS INITIAL.

    IF currency_codes IS NOT INITIAL.
      " Check if currency exist
      SELECT FROM I_Currency FIELDS currency
        FOR ALL ENTRIES IN @currency_codes
        WHERE currency = @currency_codes-currency
        INTO TABLE @DATA(currency_codes_db).
    ENDIF.

    " Raise msg for non existing and initial currencycode
    LOOP AT orders INTO DATA(order).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky               = order-%tky
                       %state_area        = 'VALIDATE_CURRENCY_CODE' )
        TO reported-order.

      IF order-CurrencyCode IS INITIAL OR NOT line_exists( currency_codes_db[ currency = order-CurrencyCode ] ).
        APPEND VALUE #( %tky = order-%tky ) TO failed-order.

        APPEND VALUE #( %tky        = order-%tky
                        %state_area = 'VALIDATE_CURRENCY_CODE'
                        %msg        = NEW zcm_rap_order_dg(
                                          severity = if_abap_behv_message=>severity-error
                                          textid   = zcm_rap_order_dg=>currency_code_unknown
                                          currencycode = order-CurrencyCode )
                        %element-CurrencyCode = if_abap_behv=>mk-on )
          TO reported-order.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD propagateCurrencyCode.
    DATA update TYPE TABLE FOR UPDATE ZI_RAP_Order_dg\\Item.

    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
      FIELDS ( CurrencyCode )
      WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
     ENTITY Order BY \_Item
     FIELDS ( itemuuid orderuuid )
     WITH CORRESPONDING #( orders )
     RESULT DATA(items).

    LOOP AT orders INTO DATA(order).
      " Update all items of that order
      LOOP AT items INTO DATA(item).
        APPEND VALUE #( %tky = item-%tky
        CurrencyCode =  order-CurrencyCode ) TO update.
      ENDLOOP.
    ENDLOOP.

    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
    ENTITY Item
        UPDATE FIELDS ( CurrencyCode ) WITH update
        REPORTED DATA(update_reported).

  ENDMETHOD.

  METHOD setCreationDate.
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        FIELDS ( CreationDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    DELETE orders WHERE CreationDate IS NOT INITIAL.
    CHECK orders IS NOT INITIAL.

    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
         UPDATE
           FIELDS ( CreationDate )
           WITH VALUE #( FOR order IN orders
                           ( %tky         = order-%tky
                             CreationDate = cl_abap_context_info=>get_system_date(  ) ) ).
  ENDMETHOD.

  METHOD setStatusCancelled.
    SELECT SINGLE Statusid
      FROM zi_rap_status_dg
      WHERE status_text = 'Cancelled'
      INTO @DATA(lv_cancelled_status).


    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
         UPDATE
           FIELDS ( Status CancellationDate )
           WITH VALUE #( FOR key IN keys
                           ( %tky         = key-%tky
                             Status = lv_cancelled_status
                             CancellationDate = cl_abap_context_info=>get_system_date(  ) ) )
      FAILED failed
      REPORTED reported.

    " Fill the response table
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    result = VALUE #( FOR order IN orders
                        ( %tky   = order-%tky
                          %param = order ) ).
  ENDMETHOD.

  METHOD setStatusCompleted.
    SELECT SINGLE Statusid
      FROM zi_rap_status_dg
      WHERE status_text = 'Completed'
      INTO @DATA(lv_completed_status).

    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
         UPDATE
           FIELDS ( Status CompletionDate )
           WITH VALUE #( FOR key IN keys
                           ( %tky         = key-%tky
                             Status = lv_completed_status
                             CompletionDate = cl_abap_context_info=>get_system_date(  ) ) )
      FAILED failed
      REPORTED reported.

    " Fill the response table
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    result = VALUE #( FOR order IN orders
                        ( %tky   = order-%tky
                          %param = order ) ).
  ENDMETHOD.

ENDCLASS.
