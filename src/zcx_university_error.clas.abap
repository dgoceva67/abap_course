CLASS zcx_university_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    DATA university_id TYPE ZUNIVERSITY_ID READ-ONLY.

    METHODS
      constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
         university_id TYPE ZUNIVERSITY_ID OPTIONAL.

    constants:
      begin of university_not_found,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '001',
        attr1 type scx_attrname value 'university_id',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of university_not_found.

    CONSTANTS:
      begin of university_cant_created,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '002',
        attr1 type scx_attrname value 'attr1',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of university_cant_created.

    CONSTANTS:
      begin of university_error,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '006',
        attr1 type scx_attrname value 'attr1',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of university_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_university_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    IF university_id IS NOT INITIAL.
        me->university_id = university_id.
    ENDIF.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
