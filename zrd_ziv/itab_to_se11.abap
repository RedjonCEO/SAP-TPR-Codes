*&---------------------------------------------------------------------*
*& Report  ZIV_HMC_DHUB_VEHICLE_SEND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ziv_hmc_dhub_vehicle_send.

TABLES vlcvehicle.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_werks FOR vlcvehicle-zwerks_orig OBLIGATORY,
                s_mmsta FOR vlcvehicle-mmsta.
PARAMETERS :    p_file  TYPE filename,
                p_sepr  TYPE char1 DEFAULT ';',
                p_disp  AS CHECKBOX DEFAULT ''.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_dhub_vehicle_send DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dhub_vehicle_send DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_vehicles,
      vguid TYPE vlcvehicle-vguid,
      vhvin TYPE vlcvehicle-vhvin,
      mmsta TYPE vlcvehicle-mmsta,
      zzvp  TYPE zsdipc_vp-zzvp,
      END OF ty_vehicles,
      tt_vehicles TYPE STANDARD TABLE OF ty_vehicles.

    DATA: mt_vehicles TYPE tt_vehicles,
          mo_salv     TYPE REF TO cl_salv_table.

    METHODS extract_data.
    METHODS upload_to_directory.

ENDCLASS.                    "lcl_dhub_vehicle_send DEFINITION

DATA go_dhub_vehicle_send TYPE REF TO lcl_dhub_vehicle_send.

*----------------------------------------------------------------------*
*       CLASS lcl_dhub_vehicle_send IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dhub_vehicle_send IMPLEMENTATION.

  METHOD execute.
    extract_data( ).
    upload_to_directory( ).

  ENDMETHOD.                    "execute

  METHOD extract_data.

    SELECT v~vguid
           v~vhvin
           v~mmsta
           z~zzvp
      MAX( z~zversion ) AS zversion
      FROM vlcvehicle AS v
      JOIN zsdipc_vp AS z ON z~vguid = v~vguid
      INTO CORRESPONDING FIELDS OF TABLE mt_vehicles
      WHERE v~zwerks_orig IN s_werks
      AND   v~mmsta       IN s_mmsta
      AND   v~vhvin IS NOT NULL
      GROUP BY v~vguid v~vhvin v~mmsta z~zzvp.


    IF p_disp = abap_false.
      RETURN.
    ENDIF.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = mt_vehicles.
      CATCH cx_salv_msg .
    ENDTRY.
    CALL METHOD mo_salv->display.

  ENDMETHOD.                    "extract_data



  METHOD upload_to_directory.

    DATA: lv_filename  TYPE string,
          lv_ts        TYPE timestamp,
          lv_string_ts TYPE string,
          lv_tab_row   TYPE string,
          lv_header    TYPE string,
          ls_vehicles  like LINE OF mt_vehicles.

    lv_filename = p_file.
    GET TIME STAMP FIELD lv_ts.
    MOVE lv_ts to lv_string_ts.

    CONCATENATE lv_string_ts(14) 'csv' INTO lv_string_ts SEPARATED BY '.'.
    CONCATENATE lv_filename lv_string_ts INTO lv_filename SEPARATED BY '_'.

    OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    CONCATENATE 'Vehicle Identif. No' 'Status' 'VP code' INTO lv_header SEPARATED BY p_sepr.
    TRANSFER lv_header TO lv_filename.

    LOOP AT mt_vehicles INTO ls_vehicles.

      CONCATENATE ls_vehicles-vhvin ls_vehicles-mmsta ls_vehicles-zzvp INTO lv_tab_row SEPARATED BY p_sepr.
      TRANSFER lv_tab_row TO lv_filename.

    ENDLOOP.
    CLOSE DATASET lv_filename.


  ENDMETHOD.                    "upload_to_directory

ENDCLASS.                    "lcl_dhub_vehicle_send IMPLEMENTATION


START-OF-SELECTION.

  CREATE OBJECT go_dhub_vehicle_send.

  go_dhub_vehicle_send->execute( ).
