**&---------------------------------------------------------------------*
**&  Include           ZIV_HMC_VEHICLE_UPD_TOP
**&---------------------------------------------------------------------*

REPORT ziv_hmc_vehicle_upd.

DATA: BEGIN OF gs_screen100,
  ok_code  TYPE sy-ucomm,
  END OF gs_screen100.

TABLES: ziv_hmc_batch_hd, ziv_hmc_batch_it , vlcvehicle, ziv_hmc_proforma.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_batch  FOR ziv_hmc_batch_hd-zbatch,
                s_prslot FOR ziv_hmc_batch_hd-zprod_slot,
                s_desprt FOR ziv_hmc_batch_hd-zdest_port.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_spcod  FOR ziv_hmc_batch_it-zhmc_spec_code,
                s_mmsta  FOR vlcvehicle-mmsta,
                s_vhvin  FOR vlcvehicle-vhvin.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
*       CLASS lcl_vehicle_update DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_vehicle_update DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS execute.

    METHODS: handle_user_command
    FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm .

  PRIVATE SECTION.

    DATA: BEGIN OF ms_technical,
            icon_wrong_status      TYPE char30,
            icon_correct_status    TYPE char30,
            edit_cells             TYPE lvc_t_styl,
          END OF ms_technical.

    TYPES: BEGIN OF ty_logs,
      status  TYPE char30,
      ebeln   TYPE ekpo-ebeln,
      vhcle   TYPE vlcvehicle-vhcle,
      msg_no  TYPE i,
      message TYPE char100,
      END OF ty_logs,
      tt_logs TYPE STANDARD TABLE OF ty_logs.

    TYPES ty_icon TYPE char100.

    TYPES: BEGIN OF ty_icons,
            error     TYPE ty_icon,
            warning   TYPE ty_icon,
            success   TYPE ty_icon,
            info      TYPE ty_icon,
          END OF ty_icons.

    TYPES: BEGIN OF ty_vehicles,
      checkbox           TYPE char1,
      zbatch             TYPE ziv_hmc_batch_hd-zbatch,
      posnr              TYPE ziv_hmc_batch_it-posnr,
      zprod_slot         TYPE ziv_hmc_batch_hd-zprod_slot,
      zdest_port         TYPE ziv_hmc_batch_hd-zdest_port,
      vhcle              TYPE ziv_hmc_batch_it-vhcle,
      vhsar              TYPE vlcvehicle-vhsar,
      vhusg              TYPE vlcvehicle-vhusg,
      kunnr              TYPE vlcvehicle-kunnr,
      endcu              TYPE vlcvehicle-endcu,
      zdcr01             TYPE vlcvehicle-zdcr01,
      zhmc_spec_code     TYPE ziv_hmc_batch_it-zhmc_spec_code,
      zhmc_color         TYPE ziv_hmc_batch_it-zhmc_color,
      zhmc_tyres         TYPE ziv_hmc_batch_it-zhmc_tyres,
      vhvin              TYPE ziv_hmc_batch_it-vhvin,
      zengine            TYPE ziv_hmc_batch_it-zengine,
      zinvoice           TYPE ziv_hmc_batch_it-zinvoice,
      zinvoice_date      TYPE ziv_hmc_batch_it-zinvoice_date,
      zvessel            TYPE ziv_hmc_batch_it-zvessel,
      zsailing           TYPE ziv_hmc_batch_it-zsailing,
      zspec_tyres        TYPE ziv_hmc_batch_it-zspec_tyres,
      elab_icon          TYPE char30,
      mmsta              TYPE vlcvehicle-mmsta,
      mmsta_desc         TYPE char100,
      ebeln              TYPE ekpo-ebeln,
      netpr              TYPE ekpo-netpr,
      werks              TYPE ekpo-werks,
      lgort              TYPE ekpo-lgort,
      style              TYPE lvc_t_styl,
      blocked            TYPE abap_bool,
      zunitprice         TYPE ziv_hmc_proforma-zunitprice,
      zcolor             TYPE ziv_hmc_proforma-zcolor,
      zquantity          TYPE ziv_hmc_proforma-zquantity,
      zwerks_orig        TYPE vlcvehicle-zwerks_orig,
    END OF ty_vehicles,
    tt_vehicles TYPE STANDARD TABLE OF ty_vehicles.

    DATA: mt_vehicles           TYPE tt_vehicles,
          mo_grid               TYPE REF TO cl_gui_alv_grid,
          mv_selected           TYPE abap_bool VALUE abap_false,
          mt_logs               TYPE tt_logs,
          mo_split_col          TYPE REF TO cl_gui_splitter_container,
          mo_salv_logs          TYPE REF TO cl_salv_table,
          ms_icons              TYPE ty_icons,
          mt_cvlc02t            TYPE STANDARD TABLE OF cvlc02t,
          mo_top                TYPE REF TO cl_dd_document,
          mo_top_cnt            TYPE REF TO cl_gui_container.

    METHODS extract_data.
    METHODS append_logs IMPORTING it_logs  TYPE  cnv_pe_t_bapireturn
                                  iv_ebeln TYPE ebeln
                                  iv_vhcle TYPE vlcvehicle-vhcle
                        RETURNING value(rv_error) TYPE abap_bool.
    METHODS display_logs.
    METHODS save_data.
    METHODS modify_vehicle_data IMPORTING it_vehicles TYPE tt_vehicles.
    METHODS check_table_data RETURNING value(rv_error) TYPE abap_bool.
    METHODS register_good_receipt.
    METHODS display_vehicles.
    METHODS start_container.
    METHODS change_edit_status.
    METHODS init_technical_vars.
    METHODS add_edit_cell       IMPORTING iv_fieldname TYPE fieldname.
    METHODS set_logs_visibility IMPORTING iv_visible TYPE i.
    METHODS get_status_icons    RETURNING value(rs_icons) TYPE ty_icons.

    METHODS on_data_changed
    FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING  er_data_changed .

    METHODS get_icon IMPORTING iv_name        TYPE char30
                               iv_info        TYPE char50
    RETURNING value(rv_icon) TYPE ty_icon.

    METHODS: handle_toolbar
    FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING e_object .

    METHODS handle_logs_ucomm
    FOR EVENT added_function OF cl_salv_events_table
    IMPORTING e_salv_function.


    METHODS: handle_toolbar_logs
    FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING e_object .

    METHODS change_status IMPORTING iv_vhcle  TYPE vlcvehicle-vhcle
                                    iv_ebeln  TYPE ebeln.

    METHODS top_of_page
    FOR EVENT top_of_page OF cl_gui_alv_grid.

    METHODS print_top_of_page
    FOR EVENT print_top_of_page OF cl_gui_alv_grid.


ENDCLASS.                    "lcl_vehicle_update DEFINITION



DATA go_vehicle_upd TYPE REF TO lcl_vehicle_update.
