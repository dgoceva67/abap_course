CLASS zcx_student_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    DATA student_id TYPE ZSTUDENT_ID READ-ONLY.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
         student_id TYPE ZSTUDENT_ID OPTIONAL .

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
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
