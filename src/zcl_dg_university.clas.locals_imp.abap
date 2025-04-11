*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
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
          zcx_student_error .
  private section.

endclass.

class lcl_university implementation.

  method create_university.
    DATA(lv_university_id) = cl_system_uuid=>create_uuid_c32_static( ).
    DATA(ls_university) = VALUE zuniversity_dgt(
        id            = lv_university_id
        name          = iv_university_name
        location      = iv_university_location ).

    INSERT zuniversity_dgt FROM @ls_university.
    IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_university_error(
            iv_message = '|University { ls_university_id } not found.|').
    ENDIF.
    COMMIT WORK AND WAIT.

    rv_university_id = lv_university_id.

  endmethod.

  method add_student.
    SELECT count( * ) FROM zuniversity_dgt
        WHERE zuniversity_dgt~id = @iv_university_id
        INTO @DATA(counter).
    IF counter = 0.
        RAISE EXCEPTION NEW zcx_university_error(
        iv_message = |University ID { iv_university_id } not found.| ).
    ENDIF.

    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @counter.
    IF counter = 0.
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |Student ID { iv_student_id } not found.| ).
    ENDIF.

    UPDATE zstudent_dgt SET zstudent_dgt~university_id = @iv_university_id
        WHERE zstudent_dgt~student_id = @iv_student_id.
    IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |Student ID { iv_student_id } ban not be updated.| ).
    ENDIF.
    COMMIT WORK AND WAIT.

  endmethod.

  method delete_student.
    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @DATA(counter).
    IF counter = 0.
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |Student ID { iv_student_id } not found.| ).
   ENDIF.

    UPDATE zstudent_dgt SET zstudent_dgt~university_id = ''
        WHERE zstudent_dgt~student_id = @iv_student_id.
    IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |Student ID { iv_student_id } can not be updated.| ).
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
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |There is an error with { iv_university_id } University.| ).
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

endclass.

class lcl_student implementation.

  method create_student.
    DATA(lv_student_id) = cl_system_uuid=>create_uuid_c32_static( ).
    DATA(ls_student) = VALUE zstudent_dgt(
        student_id    = lv_student_id
        name          = iv_student_name
        age           = iv_student_age
        major         = iv_major
        email         = iv_email ).

    INSERT zstudent_dgt FROM @ls_student.
    IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |Student can not be created.| ).
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
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |Student ID { iv_student_id } not found.| ).
    ENDIF.

  endmethod.

  method update_student.
    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @DATA(counter).
    IF counter = 0.
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |Student ID { iv_student_id } not found.| ).
    ENDIF.

    UPDATE zstudent_dgt SET zstudent_dgt~name = @iv_name,
        zstudent_dgt~age = @iv_age,
        zstudent_dgt~email = @iv_email,
        zstudent_dgt~major = @iv_major
        WHERE zstudent_dgt~student_id = @iv_student_id.
    IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_student_error(
        iv_message = |Student ID { iv_student_id } can not be updated.| ).
    ENDIF.

    COMMIT WORK AND WAIT.

  endmethod.

endclass.
