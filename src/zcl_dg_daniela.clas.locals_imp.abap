*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lcl_helper_class definition.

  public section.

    CLASS-METHODS init_travel_table.
    CLASS-METHODS is_empty_table
        RETURNING VALUE(is_empty) TYPE abap_bool.

  protected section.
  private section.

endclass.

class lcl_helper_class implementation.

  METHOD init_travel_table.
    SELECT * FROM ZTRAVEL_DGT INTO TABLE @DATA(lt_ztravel).
        DELETE ZTRAVEL_DGT FROM TABLE @lt_ztravel.
        COMMIT WORK AND WAIT.

        INSERT ZTRAVEL_DGT FROM
        ( SELECT FROM /dmo/travel
            FIELDS  uuid( )          AS travel_uuid,
                    travel_id        AS travel_id,
                    agency_id        AS agensy_id,
                    customer_id      AS customer_id,
                    begin_date       AS begin_date,
                    end_date         AS end_date,
                    booking_fee      AS booking_fee,
                    total_price      AS total_price,
                    currency_code    AS currency_code,
                    description      AS description,
                    CASE status
                    WHEN 'B' THEN 'A'   " ACCEPTED
                    WHEN 'X' THEN 'X'   " CANCELLED
                    ELSE 'O'            " OPEN
                    END              AS overall_status,
                    createdby        AS createdby,
                    createdat        AS createdat,
                    lastchangedby    AS last_changed_by,
                    lastchangedat    AS last_changed_at
         ORDER BY travel_id ).
         COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD is_empty_table.
    SELECT COUNT( * ) FROM ZTRAVEL_DGT INTO @DATA(rows).
    IF rows = 0.
       is_empty = abap_true.
    ELSE.
       is_empty = abap_false.
    ENDIF.

  ENDMETHOD.

endclass.
