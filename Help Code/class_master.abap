
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
*       CLASS lcl_class_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class_name DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.
    METHODS extract_data.
ENDCLASS.                    "

DATA go_class_name TYPE REF TO lcl_class_name.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_finance_rep IMPLEMENTATION.

  METHOD execute.
    extract_data( ).

  ENDMETHOD.                    "execute

  METHOD extract_data.

  ENDMETHOD.                    "extract_data

ENDCLASS.    


START-OF-SELECTION.
  CREATE OBJECT go_class_name.
  go_class_name->execute( ).
