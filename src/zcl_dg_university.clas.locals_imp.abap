*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lcl_university definition.

  public section.
    CLASS-METHODS create_university
        IMPORTING
            iv_university_name TYPE zuniversity_name
            iv_university_location TYPE zuniversity_location
        RETURNING value(rv_university_id) TYPE zuniversity_id.
    CLASS-METHODS add_student
        IMPORTING
            iv_student_id TYPE zstudent_id
            iv_university_id TYPE zuniversity_id.
    CLASS-METHODS delete_student
        IMPORTING iv_student_id TYPE zstudent_id.
    CLASS-METHODS list_students
        IMPORTING
            iv_university_id TYPE zuniversity_id
        RETURNING VALUE(rv_students) TYPE zstudent_dgt.
    CLASS-METHODS get_output
        RETURNING VALUE(rv_output) TYPE string.
  private section.
    CLASS-DATA rv_output TYPE string.

endclass.

class lcl_university implementation.

  method create_university.

  endmethod.

  method add_student.
    SELECT count( * ) FROM zuniversity_dgt
        WHERE zuniversity_dgt~id = @iv_university_id
        INTO @DATA(counter).
    IF counter = 0.
        rv_output = 'No such university'.
        RETURN.
    ENDIF.

    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @counter.
    IF counter = 0.
        rv_output = 'No such student'.
        RETURN.
    ENDIF.

    UPDATE zstudent_dgt SET zstudent_dgt~university_id = @iv_university_id
        WHERE zstudent_dgt~student_id = @iv_student_id.
    COMMIT WORK AND WAIT.
    rv_output = 'Student add to university successfully'.

  endmethod.

  method delete_student.
    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @DATA(counter).
    IF counter = 0.
        rv_output = 'No such student'.
        RETURN.
    ENDIF.
    UPDATE zstudent_dgt SET zstudent_dgt~university_id = ''
        WHERE zstudent_dgt~student_id = @iv_student_id.
    COMMIT WORK AND WAIT.
    rv_output = 'Student remove from university successfully'.
  endmethod.

  method list_students.

  endmethod.


  method get_output.
    RETURN rv_output.
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
        RETURNING VALUE(rv_student_id) TYPE zstudent_id.
    CLASS-METHODS get_student
        IMPORTING
            iv_student_id TYPE zstudent_id
        RETURNING VALUE(rs_student) TYPE zstudent_dgt.
    CLASS-METHODS update_student
        IMPORTING
            iv_student_id TYPE zstudent_id
            iv_name TYPE zstudent_name
            iv_age TYPE zstudent_age
            iv_major TYPE zstudent_major
            iv_email TYPE zstudent_email.
    CLASS-METHODS get_output
        RETURNING VALUE(rv_output) TYPE string.
  protected section.
  private section.
    CLASS-DATA rv_output TYPE string.

endclass.

class lcl_student implementation.

  method create_student.

  endmethod.

  method get_student.

  endmethod.

  method update_student.
    SELECT count( * ) FROM zstudent_dgt
        WHERE zstudent_dgt~student_id = @iv_student_id
        INTO @DATA(counter).
    IF counter = 0.
        rv_output = 'No such student'.
        RETURN.
    ENDIF.
    UPDATE zstudent_dgt SET zstudent_dgt~name = @iv_name,
        zstudent_dgt~age = @iv_age,
        zstudent_dgt~email = @iv_email,
        zstudent_dgt~major = @iv_major
        WHERE zstudent_dgt~student_id = @iv_student_id.
    COMMIT WORK AND WAIT.
    rv_output = 'Student updated successfully'.

  endmethod.

  method get_output.
    RETURN rv_output.
  endmethod.

endclass.
