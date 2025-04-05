CLASS zcl_dg_select DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dg_select IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA connection TYPE REF TO lcl_connection.
    DATA connections TYPE TABLE OF REF TO lcl_connection.

    TRY.
        connection = new #(
                i_carrier_id    = 'LH'
                i_connection_id = '0400'
            ).

        APPEND connection TO connections.

    CATCH cx_abap_invalid_value.
        out->write( 'Method call failed LH and 0400' ).
    ENDTRY.

   TRY.
        connection = new #(
                i_carrier_id    = 'AA'
                i_connection_id = '0017'
            ).

        APPEND connection TO connections.

      CATCH cx_abap_invalid_value.
        out->write( 'Method call failed AA and 0017' ).
    ENDTRY.

   TRY.
        connection = new #(
                i_carrier_id    = 'SO'
                i_connection_id = '0001'
            ).

        APPEND connection TO connections.

    CATCH cx_abap_invalid_value.
        out->write( 'Method call failed SO and 0001' ).
    ENDTRY.

    LOOP AT connections INTO connection.
        out->write( connection->get_output( ) ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
