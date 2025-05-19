CLASS zcl_rap_utility_dg DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_RAP_UTILITY_DG IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DELETE FROM ZRAP_ANITEM_DG.
    out->write( 'Delete all items.' ).

    DELETE FROM ZRAP_AORDER_DG.
    out->write( 'Delete all orders.' ).

    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.
