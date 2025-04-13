CLASS zcl_dg_university DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS delete_all.
ENDCLASS.


CLASS zcl_dg_university IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    TRY.
        delete_all( ).
        DATA(university_id) = lcl_university=>create_university(
            iv_university_name = 'Sample University'
            iv_university_location = 'Example of location'
        ).
        out->write( |University { university_id } is created successfully. | ).

        DATA(student_id) = lcl_student=>create_student(
            iv_student_name = 'My Name'
            iv_student_age = 20
            iv_major = 'CSE'
            iv_email = 'my.name@univ.com' ).
        out->write( |Student { student_id } is created successfully. | ).

        lcl_university=>add_student(
            iv_student_id = student_id
            iv_university_id = '11' ).
*            iv_university_id = university_id ).
        out->write( |Student { student_id } is added to the University { university_id }| ).

        DATA(list) = lcl_university=>list_students( university_id ).
        out->write( |Student's list from the University { university_id }| ).
        out->write( list ).

        lcl_university=>delete_student( student_id ).
        out->write( |Student { student_id } is removed from University| ).

        lcl_student=>update_student(
            iv_student_id = student_id
            iv_name = 'New Name'
            iv_age = 22
            iv_major = 'CyberSecurity'
            iv_email = 'new.name@univ.com' ).
        out->write( |Student { student_id } is updated successfully. | ).

        DATA(student) = lcl_student=>get_student( student_id ).
        out->write( student ).

    CATCH cx_uuid_error zcx_university_error zcx_student_error INTO DATA(lx_error).
        "handle exception
        out->write( lx_error->get_longtext( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD delete_all.
    DELETE FROM zstudent_dgt.
    DELETE FROM zuniversity_dgt.
  ENDMETHOD.

ENDCLASS.
