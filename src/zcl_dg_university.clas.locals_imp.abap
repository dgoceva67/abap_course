*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lcl_data_validator definition .

  public section.
    CLASS-METHODS is_valid_name
        IMPORTING VALUE(iv_name) TYPE string
        RETURNING VALUE(rv_valid) TYPE abap_bool.

    CLASS-METHODS is_valid_location
        IMPORTING VALUE(iv_location) TYPE ZUNIVERSITY_LOCATION
        RETURNING VALUE(rv_valid) TYPE abap_bool.

    CLASS-METHODS is_valid_age
        IMPORTING VALUE(iv_age) TYPE ZSTUDENT_AGE
        RETURNING VALUE(rv_valid) TYPE abap_bool.

    CLASS-METHODS is_valid_major
        IMPORTING VALUE(iv_major) TYPE ZSTUDENT_MAJOR
        RETURNING VALUE(rv_valid) TYPE abap_bool.

    CLASS-METHODS is_valid_email
        IMPORTING VALUE(iv_email) TYPE ZSTUDENT_EMAIL
        RETURNING VALUE(rv_valid) TYPE abap_bool.

  protected section.
  private section.

endclass.

class lcl_data_validator implementation.

  method is_valid_name.
    DATA lv_pattern TYPE string VALUE '^\p{Lu}\p{Ll}+(?:[ -]\p{Lu}\p{Ll}+)*$'.

    FIND PCRE lv_pattern in iv_name
        MATCH COUNT DATA(lv_match_count).
    rv_valid = xsdbool( lv_match_count > 0 ).
  endmethod.

  method is_valid_location.
    DATA lv_pattern TYPE string VALUE '^(ul\.|bul\.|zh\.k\.)\s?[A-Za-z0-9\s\-\.]+(?:,\s?(bl\.|vh\.|et\.|ap\.)\s?[A-Za-z0-9]+)*$'.

    FIND PCRE lv_pattern in iv_location
        MATCH COUNT DATA(lv_match_count).
    rv_valid = xsdbool( lv_match_count > 0 ).

  endmethod.

  method is_valid_age.
    rv_valid = xsdbool( iv_age > 18 AND iv_age < 100 ).
  endmethod.

  method is_valid_major.
    DATA lv_pattern TYPE string VALUE '^([A-Z][a-z]+(?:\s|-)?)+$'.
    FIND PCRE lv_pattern in iv_major
        MATCH COUNT DATA(lv_match_count).
    rv_valid = xsdbool( lv_match_count > 0 ).

  endmethod.

  method is_valid_email.
    DATA lv_pattern TYPE string VALUE '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'.
    FIND PCRE lv_pattern in iv_email
        MATCH COUNT DATA(lv_match_count).
    rv_valid = xsdbool( lv_match_count > 0 ).

  endmethod.

endclass.

class lcl_university definition.

  public section.
    TYPES: BEGIN OF ty_student_info,
           student_id   TYPE    zstudent_dgt-student_id,
           name         TYPE    zstudent_dgt-name,
           age          TYPE    zstudent_dgt-age,
           major        TYPE    zstudent_dgt-major,
           email        TYPE    zstudent_dgt-email,
    END OF ty_student_info.
    TYPES ty_student_info_table TYPE
        STANDARD TABLE OF ty_student_info WITH EMPTY KEY.

    CLASS-METHODS create_university
        IMPORTING
            iv_university_name TYPE zuniversity_name
            iv_university_location TYPE zuniversity_location
        RETURNING value(rv_university_id) TYPE zuniversity_id
        RAISING
          cx_uuid_error
          zcx_university_error .
    CLASS-METHODS add_student
        IMPORTING
            iv_student_id TYPE zstudent_id
            iv_university_id TYPE zuniversity_id
        RAISING zcx_student_error
                zcx_university_error.
    CLASS-METHODS delete_student
        IMPORTING iv_student_id TYPE zstudent_id
        RAISING
          zcx_student_error.
    CLASS-METHODS list_students
        IMPORTING
            iv_university_id TYPE zuniversity_id
        RETURNING VALUE(rv_students) TYPE ty_student_info_table
        RAISING
          zcx_university_error .
  private section.

endclass.

class lcl_university implementation.

  method create_university.
    DATA lv_valid TYPE abap_bool.

    lv_valid = lcl_data_validator=>is_valid_name( iv_university_name ).
    IF lv_valid = abap_false.
        RAISE EXCEPTION TYPE zcx_university_error
            EXPORTING
                textid          = zcx_university_error=>university_name_invalid
                university_name = iv_university_name.
    ENDIF.

    lv_valid = lcl_data_validator=>is_valid_location( iv_university_location ).
    IF lv_valid = abap_false.
        RAISE EXCEPTION TYPE zcx_university_error
            EXPORTING
                textid          = zcx_university_error=>university_location_invalid
                university_location = iv_university_location.
    ENDIF.

    DATA(lv_university_id) = cl_system_uuid=>create_uuid_c32_static( ).
    DATA(ls_university) = VALUE zuniversity_dgt(
        id            = lv_university_id
        name          = iv_university_name
        location      = iv_university_location ).

    INSERT zuniversity_dgt FROM @ls_university.
    IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_university_error
            EXPORTING
                textid = zcx_university_error=>university_cant_created.
    ENDIF.
    COMMIT WORK AND WAIT.

    rv_university_id = lv_university_id.

  endmethod.

  method add_student.
    SELECT count( * ) FROM zuniversity_dgt
        WHERE zuniversity_dgt~id = @iv_university_id
        INTO @DATA(counter).
    IF counter = 0.
        RAISE EXCEPTION TYPE zcx_university_error
        EXPORTING
            textid        = zcx_university_error=>university_not_found
            university_id = iv_university_id.
    ENDIF.

    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @counter.
    IF counter = 0.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid      = zcx_student_error=>student_not_found
                student_id  = iv_student_id.
    ENDIF.

    UPDATE zstudent_dgt SET zstudent_dgt~university_id = @iv_university_id
        WHERE zstudent_dgt~student_id = @iv_student_id.
    IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid      = zcx_student_error=>student_cant_updated
                student_id  = iv_student_id.
    ENDIF.
    COMMIT WORK AND WAIT.

  endmethod.

  method delete_student.
    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @DATA(counter).
    IF counter = 0.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid      = zcx_student_error=>student_not_found
                student_id  = iv_student_id.
   ENDIF.

    UPDATE zstudent_dgt SET zstudent_dgt~university_id = ''
        WHERE zstudent_dgt~student_id = @iv_student_id.
    IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid      = zcx_student_error=>student_cant_updated
                student_id  = iv_student_id.
    ENDIF.
    COMMIT WORK AND WAIT.

  endmethod.

  method list_students.
    SELECT
        student_id,
        name,
        age,
        major,
        email
    FROM zstudent_dgt
    WHERE university_id = @iv_university_id
    INTO TABLE @rv_students.
    IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_university_error
            EXPORTING
                textid          = zcx_university_error=>university_error
                university_id   = iv_university_id.
    ENDIF.

  endmethod.

endclass.

class lcl_student definition.

  public section.
    CLASS-METHODS create_student
        IMPORTING
            iv_student_name TYPE zstudent_name
            iv_student_age TYPE zstudent_age
            iv_major TYPE zstudent_major
            iv_email TYPE zstudent_email
        RETURNING VALUE(rv_student_id) TYPE zstudent_id
        RAISING
          cx_uuid_error
          zcx_student_error.
    CLASS-METHODS get_student
        IMPORTING
            iv_student_id TYPE zstudent_id
        RETURNING VALUE(rs_student) TYPE zstudent_info
        RAISING
          zcx_student_error.
    CLASS-METHODS update_student
        IMPORTING
            iv_student_id TYPE zstudent_id
            iv_name TYPE zstudent_name
            iv_age TYPE zstudent_age
            iv_major TYPE zstudent_major
            iv_email TYPE zstudent_email
        RAISING
          zcx_student_error.
  protected section.
  private section.
    CLASS-METHODS is_valid_student_data
         IMPORTING
            iv_name TYPE zstudent_name
            iv_age TYPE zstudent_age
            iv_major TYPE zstudent_major
            iv_email TYPE zstudent_email
        RAISING
          zcx_student_error.

endclass.

class lcl_student implementation.

  method create_student.

    is_valid_student_data(
        iv_name     = iv_student_name
        iv_age      = iv_student_age
        iv_major    = iv_major
        iv_email    = iv_email ).
    DATA(lv_student_id) = cl_system_uuid=>create_uuid_c32_static( ).
    DATA(ls_student) = VALUE zstudent_dgt(
        student_id    = lv_student_id
        name          = iv_student_name
        age           = iv_student_age
        major         = iv_major
        email         = iv_email ).

    INSERT zstudent_dgt FROM @ls_student.
    IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid      = zcx_student_error=>student_cant_created.
    ENDIF.
    COMMIT WORK AND WAIT.

    rv_student_id = lv_student_id.

  endmethod.

  method get_student.
    SELECT SINGLE
        s~student_id,
        s~name,
        s~age,
        s~major,
        s~email,
        u~name as university_name,
        u~location
    FROM zstudent_dgt as s
    LEFT OUTER join zuniversity_dgt as u
    ON s~university_id = u~id
    WHERE s~student_id = @iv_student_id
    INTO @rs_student.

   IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid      = zcx_student_error=>student_not_found
                student_id  = iv_student_id.
    ENDIF.

  endmethod.

  method update_student.
    is_valid_student_data(
        iv_name     = iv_name
        iv_age      = iv_age
        iv_major    = iv_major
        iv_email    = iv_email ).

    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @DATA(counter).
    IF counter = 0.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid      = zcx_student_error=>student_not_found
                student_id  = iv_student_id.
    ENDIF.

    UPDATE zstudent_dgt SET zstudent_dgt~name = @iv_name,
        zstudent_dgt~age = @iv_age,
        zstudent_dgt~email = @iv_email,
        zstudent_dgt~major = @iv_major
        WHERE zstudent_dgt~student_id = @iv_student_id.
    IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid      = zcx_student_error=>student_cant_updated
                student_id  = iv_student_id.
    ENDIF.

    COMMIT WORK AND WAIT.

  endmethod.

  method is_valid_student_data.
    DATA lv_valid TYPE abap_bool.

    lv_valid = lcl_data_validator=>is_valid_name( iv_name ).
    if lv_valid = abap_false.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid          = zcx_student_error=>student_name_invalid
                student_name = iv_name.
    ENDIF.

    lv_valid = lcl_data_validator=>is_valid_age( iv_age ).
    if lv_valid = abap_false.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid          = zcx_student_error=>student_age_invalid
                student_age = iv_age.
    ENDIF.

   lv_valid = lcl_data_validator=>is_valid_major( iv_major ).
    if lv_valid = abap_false.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid          = zcx_student_error=>student_major_invalid
                student_major = iv_major.
    ENDIF.

    lv_valid = lcl_data_validator=>is_valid_email( iv_email ).
    if lv_valid = abap_false.
        RAISE EXCEPTION TYPE zcx_student_error
            EXPORTING
                textid          = zcx_student_error=>student_email_invalid
                student_email = iv_email.
    ENDIF.

  endmethod.

endclass.
