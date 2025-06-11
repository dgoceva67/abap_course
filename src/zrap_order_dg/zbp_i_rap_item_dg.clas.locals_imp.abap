CLASS lhc_Item DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_item_order_key,
             itemuuid  TYPE sysuuid_x16,
             orderuuid TYPE sysuuid_x16,
           END OF ty_item_order_key.
    CLASS-DATA item_order_key TYPE STANDARD TABLE OF ty_item_order_key.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR item RESULT result.

    METHODS calculateOnModify FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Item~calculateOnModify.

    METHODS initCurrencyCode FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Item~initCurrencyCode.

    METHODS validateItemName FOR VALIDATE ON SAVE
      IMPORTING keys FOR Item~validateItemName.

    METHODS validatePrice FOR VALIDATE ON SAVE
      IMPORTING keys FOR Item~validatePrice.

    METHODS validateQuantity FOR VALIDATE ON SAVE
      IMPORTING keys FOR Item~validateQuantity.

    METHODS delete_precheck FOR PRECHECK
      IMPORTING keys FOR DELETE Item.

    METHODS calculateOnDelete FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Item~calculateOnDelete.

ENDCLASS.

CLASS lhc_Item IMPLEMENTATION.

  METHOD get_instance_features.
    DATA count_items TYPE i.

    SELECT SINGLE Statusid
      FROM zi_rap_status_dg
      WHERE status_text = 'In Process'
      INTO @DATA(lv_in_process_status).

    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Item BY \_Order
      FIELDS ( orderuuid Status )
      WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order BY \_Item
      FIELDS ( orderuuid itemuuid )
      WITH CORRESPONDING #( orders )
      RESULT DATA(items).

    LOOP AT orders INTO DATA(order).
      IF order-Status = lv_in_process_status.
        LOOP AT items INTO DATA(item).
          IF item-orderuuid = order-orderuuid.
            count_items += 1.
          ENDIF.
        ENDLOOP.
        DATA(is_disabled_deleted) =  COND #( WHEN ( count_items <= 1 )
                                     THEN if_abap_behv=>fc-o-disabled
                                     ELSE if_abap_behv=>fc-o-enabled  ).
        DATA(is_disabled) = if_abap_behv=>fc-o-enabled.
      ELSE.
        is_disabled_deleted = if_abap_behv=>fc-o-disabled.
        is_disabled = if_abap_behv=>fc-o-disabled.
      ENDIF.
      LOOP AT items INTO DATA(key).
        IF key-orderuuid = order-orderuuid.
          APPEND VALUE #(
                  %tky    = key-%tky
                  %update = is_disabled
                  %delete = is_disabled_deleted
                  %field-CurrencyCode = if_abap_behv=>fc-f-read_only
           ) TO result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

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
                    textid   = zcm_rap_order_dg=>price_greather_than_0
                    price = item-Price )
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
      IF item-quantity < 1.
        APPEND VALUE #( %tky = item-%tky ) TO failed-Item.
        APPEND VALUE #(
          %tky = item-%tky
          %msg = NEW zcm_rap_order_dg(
                    severity = if_abap_behv_message=>severity-error
                    textid   = zcm_rap_order_dg=>quantity_greather_than_0
                    quantity = item-Quantity )
          %element-quantity = if_abap_behv=>mk-on )
          TO reported-Item.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete_precheck.
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Item
      FIELDS ( itemuuid orderuuid )
      WITH CORRESPONDING #( keys )
      RESULT DATA(items).

    LOOP AT items INTO DATA(item).
      APPEND VALUE #(
        itemuuid  = item-itemuuid
        orderuuid = item-orderuuid )
        TO item_order_key.

    ENDLOOP.

  ENDMETHOD.

  METHOD calculateOnDelete.
    DATA order_keys TYPE TABLE FOR ACTION IMPORT zi_rap_order_dg~calculateTotalPrice.

    LOOP AT keys REFERENCE INTO DATA(key).
      DATA(orderuuid) = VALUE #( item_order_key[ itemuuid =
        key->Itemuuid ]-orderuuid OPTIONAL ).

      IF orderuuid IS INITIAL.
        CONTINUE.
      ENDIF.

      IF NOT line_exists( order_keys[ KEY id
                                      Orderuuid = orderuuid
                                      %is_draft = key->%is_draft ] ).
        INSERT VALUE #(
          Orderuuid = orderuuid
          %is_draft = key->%is_draft ) INTO TABLE order_keys.
      ENDIF.
    ENDLOOP.

    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
      EXECUTE calculateTotalPrice
      FROM order_keys
      REPORTED DATA(execute_reported).

    reported = CORRESPONDING #( DEEP execute_reported ).

  ENDMETHOD.

  METHOD calculateonmodify.
    READ ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Item BY \_Order
      FIELDS ( orderuuid )
      WITH CORRESPONDING #( keys )
      RESULT DATA(orders).

    MODIFY ENTITIES OF zi_rap_order_dg IN LOCAL MODE
      ENTITY Order
      EXECUTE calculateTotalPrice
      FROM CORRESPONDING #( orders )
      REPORTED DATA(execute_reported).

    reported = CORRESPONDING #( DEEP execute_reported ).

  ENDMETHOD.

ENDCLASS.
