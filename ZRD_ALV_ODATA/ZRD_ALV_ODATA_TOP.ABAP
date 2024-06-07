*&---------------------------------------------------------------------*
*&  Include           ZRD_ALV_ODATA_TOP
*&---------------------------------------------------------------------*
REPORT zrd_alv_odata_top.

TABLES sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_noexp RADIOBUTTON GROUP g1,
            p_exp   RADIOBUTTON GROUP g1,
            p_post  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN PUSHBUTTON 1(25) text-002 USER-COMMAND btn.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'BTN'.
    sscrfields-ucomm = 'ONLI'.
  ENDIF.


CLASS lcl_alv_display_odata DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_makt,
             matnr TYPE string,
             spras TYPE string,
             maktx TYPE string,
             maktg TYPE string,
           END OF ty_makt.

    TYPES: BEGIN OF ty_mara,
             matnr TYPE string,
             ernam TYPE string,
             mtart TYPE string,
             matkl TYPE string,
             ntgew TYPE string,
           END OF ty_mara.

    TYPES: BEGIN OF ty_deserialize,
             matnr     TYPE string,
             quantity  TYPE string,
             timestamp TYPE string,
             delind    TYPE string,
             BEGIN OF tomaktnav,
               results TYPE STANDARD TABLE OF ty_makt WITH EMPTY KEY,
             END OF tomaktnav,
             tomaranav TYPE ty_mara,
           END OF ty_deserialize ,
           tt_deserialize TYPE TABLE OF ty_deserialize.

    METHODS execute.

  PRIVATE SECTION.

    METHODS get_method.
    METHODS display_tables IMPORTING iv_response TYPE string.
    METHODS create_fieldcatalog CHANGING ct_fieldcat_header TYPE slis_t_fieldcat_alv
                                         ct_fieldcat_mara   TYPE slis_t_fieldcat_alv
                                         ct_fieldcat_makt   TYPE slis_t_fieldcat_alv .
    METHODS convert_timestamp CHANGING ct_time_table TYPE tt_deserialize.
    METHODS post_method.
ENDCLASS.

DATA go_alv_odata TYPE REF TO lcl_alv_display_odata.
