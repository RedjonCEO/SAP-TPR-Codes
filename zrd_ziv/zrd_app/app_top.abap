*&---------------------------------------------------------------------*
*&  Include           ZIV_HMC_PROFORMA_APP_TOP
*&---------------------------------------------------------------------*
REPORT ziv_hmc_proforma_app.

TABLES: ziv_hmc_batch_hd , ziv_hmc_prof_hd.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_zbatch  FOR ziv_hmc_batch_hd-zbatch,
                s_prslot  FOR ziv_hmc_batch_hd-zprod_slot,
                s_dsport  FOR ziv_hmc_batch_hd-zdest_port,
                s_prof    FOR ziv_hmc_prof_hd-zproforma,
                s_prdt    FOR ziv_hmc_prof_hd-zpro_date,
                s_recdt   FOR ziv_hmc_prof_hd-zpro_rec_date,
                s_upddt   FOR ziv_hmc_prof_hd-zpro_upd_date.

SELECTION-SCREEN END OF BLOCK b01.

*----------------------------------------------------------------------*
*       CLASS lcl_approval DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_approval DEFINITION.

  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.

    CONSTANTS: BEGIN OF cs_status,
                approved TYPE ziv_hmc_prof_hd-zstatus VALUE 'A',
                rejected TYPE ziv_hmc_prof_hd-zstatus VALUE 'R',
                new      TYPE ziv_hmc_prof_hd-zstatus VALUE ' ',
               END OF cs_status.

    DATA: mo_grid             TYPE REF TO cl_gui_alv_grid,
          mo_proforma_grid    TYPE REF TO cl_gui_alv_grid,
          mo_iv_prof_grid     TYPE REF TO cl_gui_alv_grid,
          mt_header_data      TYPE  zcl_iv_hmc_automatic_checks=>tt_header,
          mo_split_col        TYPE REF TO cl_gui_splitter_container,
          mo_split_row        TYPE REF TO cl_gui_splitter_container.

    DATA: BEGIN OF ms_technical,
            icon_status_approved TYPE char30,
            icon_status_rejected TYPE char30,
            icon_status_new      TYPE char30,
            last_col             TYPE lvc_fname,
            last_row             TYPE i,
          END OF ms_technical.

    METHODS init_technical_vars.
    METHODS extract_data.
    METHODS display_header_tab.
    METHODS get_fcat_config RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.
    METHODS display_config_tab      CHANGING ct_zconfig_tab   TYPE zcl_iv_hmc_automatic_checks=>tt_conf_qty.
    METHODS display_quantity_tab    CHANGING ct_zquantity_tab TYPE zcl_iv_hmc_automatic_checks=>tt_conf_qty.
    METHODS display_price_tab       CHANGING ct_zprice_tab    TYPE zcl_iv_hmc_automatic_checks=>tt_price.
    METHODS display_proforma_tab    IMPORTING iv_source TYPE char5
                                     CHANGING ct_zproforma_tab  TYPE zcl_iv_hmc_automatic_checks=>tt_proforma.
    METHODS start_containers.
    METHODS approve_reject_proforma IMPORTING iv_ucomm TYPE sy-ucomm.
    METHODS set_split_row_visibility     IMPORTING iv_visible TYPE i.
    METHODS set_split_col_visibility     IMPORTING iv_visible TYPE i.

    METHODS get_icon_tooltip IMPORTING iv_name TYPE icon-name
                                       iv_info TYPE char30
                             RETURNING value(rv_icon) TYPE char30.

    METHODS: handle_button_click
    FOR EVENT button_click OF cl_gui_alv_grid
    IMPORTING es_col_id es_row_no.

    METHODS handle_grid_toolbar
    FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING e_object e_interactive.

    METHODS handle_grid_proforma_toolbar
    FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING e_object e_interactive.

    METHODS handle_hdr_ucomm
    FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm.

    METHODS handle_proforma_ucomm
    FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm.

ENDCLASS.                    "lcl_approval DEFINITION

DATA: go_approval TYPE REF TO lcl_approval,
      gv_ok_code  TYPE sy-ucomm.
