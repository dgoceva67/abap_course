CLASS zcm_rap_order_dg DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_abap_behv_message .
    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    CONSTANTS:
      BEGIN OF customer_unknown,
        msgid TYPE symsgid VALUE 'ZRAP_ORDER_MSG_DG',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'CUSTOMERID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF customer_unknown .
    CONSTANTS:
      BEGIN OF delivery_country_unknown,
        msgid TYPE symsgid VALUE 'ZRAP_ORDER_MSG_DG',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'DELIVERYCOUNTRY',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF delivery_country_unknown .
    CONSTANTS:
      BEGIN OF price_greather_than_0,
        msgid TYPE symsgid VALUE 'ZRAP_ORDER_MSG_DG',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'PRICE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF price_greather_than_0 .
    CONSTANTS:
      BEGIN OF quantity_greather_than_0,
        msgid TYPE symsgid VALUE 'ZRAP_ORDER_MSG_DG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'QUATITY',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF quantity_greather_than_0 .

    CONSTANTS:
      BEGIN OF items_count_greather_than_0,
        msgid TYPE symsgid VALUE 'ZRAP_ORDER_MSG_DG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF items_count_greather_than_0 .

    CONSTANTS:
      BEGIN OF invalid_order_name,
        msgid TYPE symsgid VALUE 'ZRAP_ORDER_MSG_DG',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'ORDERNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_order_name .

    CONSTANTS:
      BEGIN OF invalid_item_name,
        msgid TYPE symsgid VALUE 'ZRAP_ORDER_MSG_DG',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'ITEMNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_item_name .
    CONSTANTS:
      BEGIN OF currency_code_unknown,
        msgid TYPE symsgid VALUE 'ZRAP_ORDER_MSG_DG',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'CURRENCYCODE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF currency_code_unknown .

    METHODS constructor
      IMPORTING
        severity        TYPE if_abap_behv_message=>t_severity DEFAULT if_abap_behv_message=>severity-error
        textid          LIKE if_t100_message=>t100key OPTIONAL
        previous        TYPE REF TO cx_root OPTIONAL
        customerid      TYPE /dmo/customer_id OPTIONAL
        currencycode    TYPE /dmo/currency_code OPTIONAL
        deliverycountry TYPE land1 OPTIONAL
        price           TYPE /dmo/price OPTIONAL
        ordername       TYPE string OPTIONAL
        itemname        TYPE string OPTIONAL
        quantity        TYPE zde_quantity_dg OPTIONAL.

    DATA customerid TYPE string READ-ONLY.
    DATA deliverycountry TYPE string READ-ONLY.
    DATA price TYPE string  READ-ONLY.
    DATA quantity TYPE string READ-ONLY.
    DATA ordername TYPE string READ-ONLY.
    DATA itemname TYPE string READ-ONLY.
    DATA currencycode TYPE string READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCM_RAP_ORDER_DG IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->if_abap_behv_message~m_severity = severity.
    me->customerid = customerid.
    me->deliverycountry = deliverycountry.
    me->price = price.
    me->quantity = quantity.
    me->ordername = ordername.
    me->itemname = itemname.
    me->currencycode = currencycode.

  ENDMETHOD.
ENDCLASS.
