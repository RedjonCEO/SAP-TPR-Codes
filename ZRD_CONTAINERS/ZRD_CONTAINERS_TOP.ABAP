*&---------------------------------------------------------------------*
*&  Include           ZRD_CONTAINERS_TOP
*&---------------------------------------------------------------------*
REPORT zrd_containers.

DATA:BEGIN OF gs_screen01,
       ok_code TYPE sy-ucomm,
       cn      TYPE zrd_cn,
     END OF gs_screen01.

PARAMETERS:p_chb  AS CHECKBOX  USER-COMMAND chb1,
           p_file TYPE rlgrap-filename,
           d_file TYPE string OBLIGATORY.

DATA: BEGIN OF gv_exclude OCCURS 0,
        fcode LIKE sy-ucomm,
      END OF gv_exclude .

TYPES:BEGIN OF wa_header,
        name TYPE c LENGTH 20,
      END OF wa_header.

TYPES:BEGIN OF ty_structure,
        cn    TYPE zrd_finaldt-cn,
        matnr TYPE zrd_finaldt-matnr,
        maktx TYPE zrd_finaldt-maktx,
        quant TYPE zrd_finaldt-quant,
        shift TYPE c LENGTH 10,
        dats  TYPE zrd_finaldt-dats,
        usern TYPE zrd_finaldt-usern,
        del   TYPE zrd_finaldt-del,
      END OF ty_structure.

DATA:  gt_inoutab TYPE TABLE OF zrd_finaldt,
       gs_inoutab TYPE zrd_finaldt,
       gs_downtab TYPE ty_structure,
       gt_downtab TYPE STANDARD TABLE OF ty_structure,
       gv_edit    TYPE abap_bool VALUE abap_false,
       ls_header  TYPE STANDARD TABLE OF wa_header.



*&SPWIZARD: DECLARATION OF TABLECONTROL 'ZRD_TAB_CTRL' ITSELF
CONTROLS: zrd_tab_ctrl TYPE TABLEVIEW USING SCREEN 0001.
