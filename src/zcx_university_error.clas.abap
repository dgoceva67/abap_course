CLASS zcx_university_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    DATA:   university_id TYPE ZUNIVERSITY_ID READ-ONLY,
            university_name TYPE ZUNIVERSITY_NAME READ-ONLY,
            university_location TYPE ZUNIVERSITY_LOCATION READ-ONLY.

    METHODS
      constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
         university_id TYPE ZUNIVERSITY_ID OPTIONAL
         university_name TYPE ZUNIVERSITY_NAME OPTIONAL
         university_location TYPE ZUNIVERSITY_LOCATION OPTIONAL.

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

    constants:
      begin of university_name_invalid,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '007',
        attr1 type scx_attrname value 'university_name',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of university_name_invalid.

    constants:
      begin of university_location_invalid,
        msgid type symsgid value 'ZCL_DG_MESSAGE',
        msgno type symsgno value '008',
        attr1 type scx_attrname value 'university_location',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of university_location_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_UNIVERSITY_ERROR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    IF university_id IS NOT INITIAL.
        me->university_id = university_id.
    ENDIF.
    IF university_name IS NOT INITIAL.
        me->university_name = university_name.
    ENDIF.
    IF university_location IS NOT INITIAL.
        me->university_location = university_location.
    ENDIF.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
