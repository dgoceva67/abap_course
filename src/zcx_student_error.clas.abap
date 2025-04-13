CLASS zcx_student_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    DATA:   student_id TYPE ZSTUDENT_ID READ-ONLY,
            student_name TYPE ZSTUDENT_NAME READ-ONLY,
            student_age TYPE ZSTUDENT_AGE READ-ONLY,
            student_major TYPE ZSTUDENT_MAJOR READ-ONLY,
            student_email TYPE ZSTUDENT_EMAIL READ-ONLY.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
         student_id TYPE ZSTUDENT_ID OPTIONAL
         student_name TYPE ZSTUDENT_NAME OPTIONAL
         student_age TYPE ZSTUDENT_AGE OPTIONAL
         student_major TYPE ZSTUDENT_MAJOR OPTIONAL
         student_email TYPE ZSTUDENT_EMAIL OPTIONAL .

    CONSTANTS:
      begin of student_cant_created,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '003',
        attr1 type scx_attrname value 'attr1',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of student_cant_created.

    constants:
      begin of student_not_found,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '004',
        attr1 type scx_attrname value 'student_id',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of student_not_found.

    constants:
      begin of student_cant_updated,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '005',
        attr1 type scx_attrname value 'student_id',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of student_cant_updated.

    constants:
      begin of student_name_invalid,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '009',
        attr1 type scx_attrname value 'student_name',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of student_name_invalid.

    constants:
      begin of student_age_invalid,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '010',
        attr1 type scx_attrname value 'student_age',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of student_age_invalid.

    constants:
      begin of student_major_invalid,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '011',
        attr1 type scx_attrname value 'student_major',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of student_major_invalid.

    constants:
      begin of student_email_invalid,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '012',
        attr1 type scx_attrname value 'student_email',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of student_email_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_student_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    IF student_id IS NOT INITIAL.
        me->student_id = student_id.
    ENDIF.
    IF student_name IS NOT INITIAL.
        me->student_name = student_name.
    ENDIF.
    IF student_age IS NOT INITIAL.
        me->student_age = student_age.
    ENDIF.
    IF student_major IS NOT INITIAL.
        me->student_major = student_major.
    ENDIF.
    IF student_email IS NOT INITIAL.
        me->student_email = student_email.
    ENDIF.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
