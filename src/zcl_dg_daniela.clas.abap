CLASS zcl_dg_daniela DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    INTERFACES zif_abap_course_basics .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA r_output TYPE string.
    CLASS-DATA: cdate TYPE d,
                ctime TYPE t,
                ztravel_table TYPE STANDARD TABLE OF ZTRAVEL_DGT.

ENDCLASS.



CLASS zcl_dg_daniela IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

**********************************************************************
*   Task 1.
**********************************************************************

    out->write( zif_abap_course_basics~hello_world( 'Daniela' ) ).

**********************************************************************
*   Task 2.
**********************************************************************

    DATA(result) = zif_abap_course_basics~calculator(
        iv_first_number = 2
        iv_second_number = 3
        iv_operator = '*' ).
    IF result IS INITIAL.
        out->write( r_output ).
    ELSE.
        out->write( | 2 * 3 = { result } | ).

    ENDIF.

**********************************************************************
*   Task 3.
**********************************************************************

    out->write( 'The Fizz-Buzz string is:' &&
        zif_abap_course_basics~fizz_buzz( ) ).

**********************************************************************
*   Task 4.
**********************************************************************

    DATA(d_result) = zif_abap_course_basics~date_parsing( `12 April 2017` ).
    IF d_result IS INITIAL.
        out->write( r_output ).
    ELSE.
        out->write( d_result ).
    ENDIF.
    d_result = zif_abap_course_basics~date_parsing( `12 4 2017` ).
    IF d_result IS INITIAL.
        out->write( r_output ).
    ELSE.
        out->write( | { d_result DATE = USER } | ).
    ENDIF.

**********************************************************************
*   Task 5.
**********************************************************************

     out->write(
        'The result for: "This is scrabble score result" is:' &&
        zif_abap_course_basics~scrabble_score(
        'This is scrabble score result' ) ).

**********************************************************************
*   Task 6.
**********************************************************************

    DATA(lv_timestamp) = zif_abap_course_basics~get_current_date_time(  ).
    out->write( | The current timestamp is: { lv_timestamp } | ).
    out->write( | The current date is: { cdate DATE = USER } | ).
    out->write( | The current time is: { ctime TIME = USER } | ).

**********************************************************************
*   Task 7.
**********************************************************************

    DATA:
     et_travel_ids_task7_1 TYPE zif_abap_course_basics=>ltty_travel_id,
     et_travel_ids_task7_2 TYPE zif_abap_course_basics=>ltty_travel_id,
     et_travel_ids_task7_3 TYPE zif_abap_course_basics=>ltty_travel_id.

    zif_abap_course_basics~internal_tables(
        IMPORTING
            et_travel_ids_task7_1 = et_travel_ids_task7_1
            et_travel_ids_task7_2 = et_travel_ids_task7_2
            et_travel_ids_task7_3 = et_travel_ids_task7_3
     ).

    out->write( et_travel_ids_task7_1 ).
    out->write( et_travel_ids_task7_2 ).
    out->write( et_travel_ids_task7_3 ).
    out->write( ztravel_table ).

**********************************************************************
*   Task 8.
**********************************************************************

    DATA:
     et_travel_ids_task8_1 TYPE zif_abap_course_basics=>ltty_travel_id,
     et_travel_ids_task8_2 TYPE zif_abap_course_basics=>ltty_travel_id,
     et_travel_ids_task8_3 TYPE zif_abap_course_basics=>ltty_travel_id.

    zif_abap_course_basics~open_sql(
        IMPORTING
            et_travel_ids_task8_1 = et_travel_ids_task8_1
            et_travel_ids_task8_2 = et_travel_ids_task8_2
            et_travel_ids_task8_3 = et_travel_ids_task8_3
     ).

    out->write( et_travel_ids_task8_1 ).
    out->write( et_travel_ids_task8_2 ).
    out->write( et_travel_ids_task8_3 ).

  ENDMETHOD.


  METHOD zif_abap_course_basics~calculator.

    CASE iv_operator.
        WHEN '+'.
            rv_result = iv_first_number + iv_second_number.
        WHEN '-'.
            rv_result =  iv_first_number - iv_second_number.
        WHEN '*'.
            rv_result =  iv_first_number * iv_second_number.
        WHEN '/'.
            rv_result =  iv_first_number / iv_second_number.
        WHEN OTHERS.
            r_output =  `Wrong operator and/or operands.`.
            RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abap_course_basics~date_parsing.

    TYPES:
      BEGIN OF t_name,
        name TYPE string,
        idx TYPE i,
      END OF t_name.

    DATA: m_names TYPE TABLE OF t_name,
        day TYPE n LENGTH 2,
        month TYPE n LENGTH 2,
        year TYPE n LENGTH 4.

    m_names = VALUE #(
        ( idx =  1 name = `January` )
        ( idx =  2 name = `February` )
        ( idx =  3 name = `March` )
        ( idx =  4 name = `April` )
        ( idx =  5 name = `May` )
        ( idx =  6 name = `June` )
        ( idx =  7 name = `July` )
        ( idx =  8 name = `August` )
        ( idx =  9 name = `September` )
        ( idx = 10 name = `October` )
        ( idx = 11 name = `November` )
        ( idx = 12 name = `December` )
    ).

    SPLIT iv_date AT ' ' INTO DATA(s_day) DATA(s_month) DATA(s_year).

    IF strlen( s_month ) > 2.
        READ TABLE m_names WITH KEY name = s_month INTO DATA(single).
        IF sy-subrc = 0.
            s_month = single-idx.
        ELSE.
            r_output = `No such month.`.
            RETURN.
        ENDIF.
    ENDIF.

    TRY.
        day = s_day.
        month = s_month.
        year = s_year.
        r_output = s_year && s_month && s_day.
        rv_result = year && month && day.
    CATCH CX_SY_CONVERSION_ERROR.
        r_output =  `Conversion error.`.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abap_course_basics~fizz_buzz.

    CONSTANTS how_many TYPE i VALUE 100.

    DATA: counter TYPE i VALUE 1,
          iv_result TYPE string VALUE '',
          digit TYPE i.

    DO how_many TIMES.
        IF counter > 9.
            digit = counter DIV 10.
            CASE digit.
                WHEN 3.
                    iv_result = iv_result && 'Fizz'.
                WHEN 5.
                    iv_result = iv_result && 'Buzz'.
                WHEN OTHERS.
                    iv_result = iv_result && digit.
            ENDCASE.
        ENDIF.

        digit = counter MOD 10.
        CASE digit.
            WHEN 3.
                iv_result = iv_result && 'Fizz'.
            WHEN 5.
                iv_result = iv_result && 'Buzz'.
            WHEN OTHERS.
                iv_result = iv_result && digit.
        ENDCASE.

        counter = counter + 1.

    ENDDO.

    RETURN iv_result.

  ENDMETHOD.


  METHOD zif_abap_course_basics~get_current_date_time.

  GET TIME STAMP FIELD DATA(lv_timestamp).

  CONVERT TIME STAMP lv_timestamp TIME ZONE 'EET' INTO
        DATE cdate TIME ctime.

  RETURN lv_timestamp.

  ENDMETHOD.


  METHOD zif_abap_course_basics~hello_world.

     rv_result = | 'Hello { IV_NAME }, your system user id is <{ sy-uname }>'|.
  ENDMETHOD.


  METHOD zif_abap_course_basics~internal_tables.

    IF lcl_helper_class=>is_empty_table( ) = abap_true.
        lcl_helper_class=>init_travel_table(  ).
    ENDIF.

    SELECT
        FROM    ZTRAVEL_DGT
        FIELDS  TRAVEL_ID
        WHERE   AGENCY_ID = '070001' AND
                BOOKING_FEE = 20 AND
                CURRENCY_CODE = 'JPY'
        INTO    TABLE @et_travel_ids_task7_1.

    SELECT
        FROM    ZTRAVEL_DGT
        FIELDS  TRAVEL_ID
        WHERE   TOTAL_PRICE > 2000 AND CURRENCY_CODE = 'USD'
*        WHERE   CURRENCY_CONVERSION(
*                    amount = TOTAL_PRICE,
*                    source_currency = CURRENCY_CODE,
*                    target_currency = 'USD',
*                    exchange_rate_date = @sy-datum ) > 2000
        INTO    TABLE @et_travel_ids_task7_2.

    SELECT      *
        FROM    ZTRAVEL_DGT
        INTO    TABLE @ztravel_table.

    LOOP AT ztravel_table INTO DATA(row).
        IF row-currency_code <> 'EUR'.
            DELETE ztravel_table INDEX sy-tabix.
        ENDIF.
    ENDLOOP.
    SORT ztravel_table BY total_price begin_date.

    SELECT
        FROM    ZTRAVEL_DGT
        FIELDS  TRAVEL_ID
        INTO    TABLE @et_travel_ids_task7_3
        UP TO 10 ROWS.

  ENDMETHOD.


  METHOD zif_abap_course_basics~open_sql.
        SELECT  TRAVEL_ID
        FROM    ZTRAVEL_DGT
        WHERE   AGENCY_ID = '070001' AND
                BOOKING_FEE = 20 AND
                CURRENCY_CODE = 'JPY'
        INTO    TABLE @et_travel_ids_task8_1.

        SELECT  TRAVEL_ID
        FROM    ZTRAVEL_DGT
*        WHERE   TOTAL_PRICE > 2000 AND CURRENCY_CODE = 'USD'
        WHERE   CURRENCY_CONVERSION(
                    amount = TOTAL_PRICE,
                    source_currency = CURRENCY_CODE,
                    target_currency = 'USD',
                    exchange_rate_date = @sy-datum ) > 2000
        INTO    TABLE @et_travel_ids_task8_2.

        SELECT  TRAVEL_ID
        FROM    ZTRAVEL_DGT
        INTO    TABLE @et_travel_ids_task8_3
        UP TO 10 ROWS.

  ENDMETHOD.


  METHOD zif_abap_course_basics~scrabble_score.

    DATA: codeA TYPE i,
          code TYPE i,
          index TYPE i,
          len TYPE i,
          ch TYPE c.

    DATA(lo_conv) = cl_abap_conv_codepage=>create_out( ).
    codeA = lo_conv->convert( 'A' ).

    len = strlen( iv_word ).

    DO len TIMES.
        ch = iv_word+index(1).
        IF ch >= 'A' AND ch <= 'Z' OR ch >= 'a' AND ch <= 'z'.
          code = lo_conv->convert( substring( val = iv_word off = index len = 1 ) ).
          rv_result = rv_result + code - codeA + 1.
        ENDIF.
        index = index + 1..
    ENDDO.

    RETURN rv_result.

  ENDMETHOD.

ENDCLASS.
