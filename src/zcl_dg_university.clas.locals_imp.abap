*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lcl_university definition.

  public section.
    METHODS create_university
        IMPORTING
            iv_university_name TYPE zuniversity_name
            iv_university_location TYPE zuniversity_location
        RETURNING value(rv_university_id) TYPE zuniversity_id.
    METHODS add_student
        IMPORTING
            iv_student_id TYPE zstudent_id
            iv_university_id TYPE zuniversity_id.
    METHODS delete_student
        IMPORTING iv_student_id TYPE zstudent_id.
    METHODS list_students
        IMPORTING
            iv_university_id TYPE zuniversity_id
        RETURNING VALUE(rv_students) TYPE zstudent_dgt.
  private section.

endclass.

class lcl_university implementation.

  method create_university.

  endmethod.

  method add_student.

  endmethod.

  method delete_student.
    UPDATE zstudent_dgt SET zstudent_dgt~university_id = ''
        WHERE zstudent_dgt~student_id = @iv_student_id.
  endmethod.

  method list_students.

  endmethod.


endclass.
