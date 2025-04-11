CLASS zcx_university_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    METHODS
      constructor
      IMPORTING
        !iv_message  TYPE string
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL.
    METHODS get_text REDEFINITION.
    METHODS get_longtext REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA lv_text TYPE string.
ENDCLASS.



CLASS zcx_university_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    lv_text = iv_message.
  ENDMETHOD.
  METHOD get_longtext.
    result = lv_text.
  ENDMETHOD.

  METHOD get_text.
    result = lv_text.
  ENDMETHOD.

ENDCLASS.
