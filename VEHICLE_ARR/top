*&---------------------------------------------------------------------*
*&  Include           ZIV_HMC_VEHICLE_ARR_TOP
*&---------------------------------------------------------------------*

REPORT ziv_hmc_vehicle_arr.

DATA: BEGIN OF gs_screen100,
  ok_code  TYPE sy-ucomm,
  END OF gs_screen100.

TABLES: ziv_hmc_batch_hd,ziv_hmc_batch_it, vlcvehicle.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_batch  FOR ziv_hmc_batch_hd-zbatch,
                s_prdslt FOR ziv_hmc_batch_hd-zprod_slot,
                s_dstprt FOR ziv_hmc_batch_hd-zdest_port.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_spccod FOR ziv_hmc_batch_it-zhmc_spec_code,
                s_mmsta  FOR vlcvehicle-mmsta DEFAULT 'ZM49' TO 'ZM50' NO-EXTENSION,
                s_vhvin  FOR vlcvehicle-vhvin,
                s_vhcle  FOR vlcvehicle-vhcle,
                s_invc   FOR ziv_hmc_batch_it-zinvoice,
                s_vessel FOR ziv_hmc_batch_it-zvessel,
                s_sailng FOR ziv_hmc_batch_it-zsailing.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'S_MMSTA-LOW' OR 'S_MMSTA-HIGH'.
        screen-input = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_vehicle_arr DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS execute.

    METHODS: handle_user_command
    FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm .

  PRIVATE SECTION.

    DATA: BEGIN OF ms_technical,
            icon_wrong_status      TYPE char30,
            icon_correct_status    TYPE char30,
            icon_warning_status    TYPE char30,
            edit_cells             TYPE lvc_t_styl,
          END OF ms_technical.

    TYPES ty_icon TYPE char100.

    TYPES: BEGIN OF ty_logs,
      status  TYPE char30,
      vhcle   TYPE vlcvehicle-vhcle,
      msg_no  TYPE i,
      message TYPE char100,
      END OF ty_logs,
      tt_logs TYPE STANDARD TABLE OF ty_logs.

    TYPES: BEGIN OF ty_vehicle,
      checkbox        TYPE xfeld,
      zbatch          TYPE ziv_hmc_batch_hd-zbatch,
      zprod_slot      TYPE ziv_hmc_batch_hd-zprod_slot,
      zdest_port      TYPE ziv_hmc_batch_hd-zdest_port,
      vhcle           TYPE vlcvehicle-vhcle,
      vhsar           TYPE vlcvehicle-vhsar,
      vhusg           TYPE vlcvehicle-vhusg,
      kunnr           TYPE vlcvehicle-kunnr,
      endcu           TYPE vlcvehicle-endcu,
      zdcr01          TYPE vlcvehicle-zdcr01,
      zhmc_spec_code  TYPE ziv_hmc_batch_it-zhmc_spec_code,
      zhmc_color      TYPE ziv_hmc_batch_it-zhmc_color,
      zhmc_tyres      TYPE ziv_hmc_batch_it-zhmc_tyres,
      vhvin           TYPE ziv_hmc_batch_it-vhvin,
      zengine         TYPE ziv_hmc_batch_it-zengine,
      zinvoice        TYPE ziv_hmc_batch_it-zinvoice,
      zinvoice_date   TYPE ziv_hmc_batch_it-zinvoice_date,
      zvessel         TYPE ziv_hmc_batch_it-zvessel,
      zsailing        TYPE ziv_hmc_batch_it-zsailing,
      loctn           TYPE vlcvehicle-loctn,
      loctn_desc      TYPE char30,
      zspec_tyres     TYPE ziv_hmc_batch_it-zspec_tyres,
      ebeln           TYPE ekpo-ebeln,
      netpr           TYPE ekpo-netpr,
      belnr           TYPE ekbe-belnr,
      elab_icon       TYPE char30,
      elab_msg        TYPE char50,
      mmsta           TYPE vlcvehicle-mmsta,
      mmsta_desc      TYPE char100,
      loekz           TYPE ekpo-loekz,
      zda01           TYPE vlcvehicle-zda01,
      style           TYPE lvc_t_styl,
      blocked         TYPE abap_bool,
      cuobj           TYPE vlcvehicle-cuobj,
      END OF ty_vehicle,
      tt_vehicle_tab TYPE STANDARD TABLE OF ty_vehicle.

    DATA: mt_vehicle_tab TYPE tt_vehicle_tab,
          mt_cvlc02t     TYPE STANDARD TABLE OF cvlc02t,
          mo_grid        TYPE REF TO cl_gui_alv_grid,
          mo_top         TYPE REF TO cl_dd_document,
          mo_top_cnt     TYPE REF TO cl_gui_container,
          mv_selected    TYPE abap_bool VALUE abap_false,
          mt_logs        TYPE tt_logs,
          mo_split_col   TYPE REF TO cl_gui_splitter_container,
          mo_split_row   TYPE REF TO cl_gui_splitter_container,
          mo_salv_logs   TYPE REF TO cl_salv_table.

    METHODS extract_data.
    METHODS display_logs.
    METHODS display_vehicles.
    METHODS start_container.
    METHODS set_logs_visibility IMPORTING iv_visible TYPE i.
    METHODS change_edit_status.
    METHODS init_technical_vars.
    METHODS modify_vehicle_data.
    METHODS change_status IMPORTING iv_vhcle  TYPE vlcvehicle-vhcle.
    METHODS add_edit_cell       IMPORTING iv_fieldname TYPE fieldname.

    METHODS on_data_changed
    FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING  er_data_changed .

    METHODS: handle_toolbar
    FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING e_object .

    METHODS handle_logs_ucomm
    FOR EVENT added_function OF cl_salv_events_table
    IMPORTING e_salv_function.

    METHODS get_icon IMPORTING iv_name        TYPE char30
                               iv_info        TYPE char50
    RETURNING value(rv_icon) TYPE ty_icon.

    METHODS top_of_page
    FOR EVENT top_of_page OF cl_gui_alv_grid.

    METHODS print_top_of_page
    FOR EVENT print_top_of_page OF cl_gui_alv_grid.

ENDCLASS.                    "

DATA go_vehicle_arr TYPE REF TO lcl_vehicle_arr.
