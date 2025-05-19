CLASS lhc_Item DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR item RESULT result.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Item~calculateTotalPrice.

    METHODS initCurrencyCode FOR DETERMINE ON SAVE
      IMPORTING keys FOR Item~initCurrencyCode.

    METHODS validateItemName FOR VALIDATE ON SAVE
      IMPORTING keys FOR Item~validateItemName.

    METHODS validatePrice FOR VALIDATE ON SAVE
      IMPORTING keys FOR Item~validatePrice.

    METHODS validateQuantity FOR VALIDATE ON SAVE
      IMPORTING keys FOR Item~validateQuantity.

ENDCLASS.

CLASS lhc_Item IMPLEMENTATION.

  METHOD get_instance_features.
    TYPES: BEGIN OF lt_item_status,
        itemuuid    TYPE sysuuid_x16,
        orderuuid   TYPE sysuuid_x16,
        status      TYPE zde_status_dg,
    END OF lt_item_status.
    DATA lv_item_status TYPE STANDARD TABLE OF lt_item_status.

    SELECT SINGLE Statusid
      FROM zi_rap_status_dg
      WHERE status_text = 'In Process'
      INTO @DATA(lv_in_process_status).

    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Item BY \_Order
      FIELDS ( orderuuid Status )
      WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    result =
      VALUE #(
        FOR order IN orders
          LET is_disabled =   COND #( WHEN order-Status <> lv_in_process_status
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled  )
             IN
            ( %tky                 = order-%tky
*              %action-Edit = is_disabled
              %features-%update = is_disabled
              %features-%delete = is_disabled
             ) ).
  ENDMETHOD.

  METHOD calculateTotalPrice.
    DATA update TYPE TABLE FOR UPDATE ZI_RAP_Order_dg.
    DATA lv_default_status TYPE zde_status_dg.

    SELECT SINGLE Statusid
    FROM zi_rap_status_dg
    WHERE status_text = 'In Process'
    INTO @lv_default_status.

    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Item BY \_Order
        FIELDS ( Orderuuid Status TotalPrice ) WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    " Remove all order instance data with defined status
*    DELETE orders WHERE Status <> lv_default_status.
    CHECK orders IS NOT INITIAL.

    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order BY \_Item
      FIELDS ( Orderuuid Quantity Price )
      WITH CORRESPONDING #( orders )
      RESULT DATA(items).

    LOOP AT orders ASSIGNING FIELD-SYMBOL(<order>).
      CLEAR <order>-TotalPrice.

      LOOP AT items INTO DATA(item).
        IF <order>-Orderuuid = item-Orderuuid.
          <order>-TotalPrice += item-Quantity * item-Price.
        ENDIF.
      ENDLOOP.
      APPEND VALUE #( %tky = <order>-%tky
                      TotalPrice =  <order>-TotalPrice ) TO update.

    ENDLOOP.
    " write back the modified total_price of orders
    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
        UPDATE FIELDS ( TotalPrice )
        WITH CORRESPONDING #( orders ).

  ENDMETHOD.

  METHOD initCurrencyCode.
    DATA update TYPE TABLE FOR UPDATE ZI_RAP_Order_dg\\Item.

    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Item BY \_Order
      FIELDS ( orderuuid CurrencyCode )
      WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    LOOP AT orders INTO DATA(order).
      LOOP AT keys INTO DATA(key).
        APPEND VALUE #( %tky = key-%tky
        CurrencyCode =  order-CurrencyCode ) TO update.
      ENDLOOP.
    ENDLOOP.

    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
    ENTITY Item
        UPDATE FIELDS ( CurrencyCode ) WITH update
        REPORTED DATA(update_reported).

  ENDMETHOD.

  METHOD validateItemName.
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
     ENTITY Item
     FIELDS ( name )
     WITH CORRESPONDING #( keys )
     RESULT DATA(items).

    LOOP AT items INTO DATA(item).
      CLEAR reported-item.

      IF item-Name IS INITIAL OR strlen( item-Name ) < 3.
        APPEND VALUE #(
          %tky = item-%tky
          %msg = NEW zcm_rap_order_dg(
            severity = if_abap_behv_message=>severity-error
            textid   = zcm_rap_order_dg=>invalid_item_name
            itemname = item-Name )
          %element-name = if_abap_behv=>mk-on )
          TO reported-item.

        APPEND VALUE #( %tky = item-%tky ) TO failed-item.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validatePrice.
   READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Item
      FIELDS ( price )
      WITH CORRESPONDING #( keys )
      RESULT DATA(items).

    LOOP AT items INTO DATA(item).
      IF item-price <= 0.
        APPEND VALUE #( %tky = item-%tky ) TO failed-Item.
        APPEND VALUE #(
          %tky = item-%tky
          %msg = NEW zcm_rap_order_dg(
                    severity = if_abap_behv_message=>severity-error
                    textid   = zcm_rap_order_dg=>price_greather_than_0 )
          %element-price = if_abap_behv=>mk-on )
          TO reported-Item.
      ENDIF.
    ENDLOOP.

    ENDMETHOD.

  METHOD validateQuantity.
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Item
      FIELDS ( quantity )
      WITH CORRESPONDING #( keys )
      RESULT DATA(items).

    LOOP AT items INTO DATA(item).
      IF item-quantity <= 0.
        APPEND VALUE #( %tky = item-%tky ) TO failed-Item.
        APPEND VALUE #(
          %tky = item-%tky
          %msg = NEW zcm_rap_order_dg(
                    severity = if_abap_behv_message=>severity-error
                    textid   = zcm_rap_order_dg=>quantity_greather_than_0 )
          %element-quantity = if_abap_behv=>mk-on )
          TO reported-Item.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

 ENDCLASS.
