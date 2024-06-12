*&---------------------------------------------------------------------*
*&  Include           ZRD_TVARVC_CLONE_TOP
*&---------------------------------------------------------------------*
REPORT zrd_tvarvc_clone.

DATA: gv_edit_mode TYPE abap_bool VALUE abap_false,
      gv_tabstrip  TYPE char1 VALUE 'P'.

CLASS lcl_tvarvc_clone DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_parameters,
             name      TYPE rvari_vnam,
             value     TYPE rvari_val_255,
             case_sens TYPE flag,
           END OF ty_parameters,
           tt_parameters TYPE TABLE OF ty_parameters,
           BEGIN OF ty_sel_options,
             name      TYPE rvari_vnam,
             option    TYPE char255,
             low_limit TYPE rvari_val_255,
             upp_limit TYPE rvari_val_255,
             multi_sel TYPE char255,
             case_sens TYPE flag,
             interval  TYPE flag,
             sign    TYPE char1,
           END OF ty_sel_options,
           tt_sel_options TYPE TABLE OF ty_sel_options.

    DATA: mt_param_table  TYPE STANDARD TABLE OF ty_parameters,
          mt_param_change TYPE STANDARD TABLE OF ty_parameters,
          mt_sel_table    TYPE STANDARD TABLE OF ty_sel_options,
          mt_sel_change   TYPE STANDARD TABLE OF ty_sel_options,
          mt_fcat_p       TYPE lvc_t_fcat,
          mt_fcat_s       TYPE lvc_t_fcat,
          mo_grid_param   TYPE REF TO cl_gui_alv_grid,
          mo_grid_selop   TYPE REF TO cl_gui_alv_grid.

    METHODS   execute.
    METHODS set_icons.
    METHODS save_edits.

    CLASS-METHODS handle_grid_toolbar
                  FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.

    METHODS handle_grid_user_command
                  FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS on_hotspot_click
                  FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no.

    METHODS on_param_data_changed
                  FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed .

    METHODS on_sel_data_changed
                  FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed .

  PRIVATE SECTION.
    METHODS create_fieldcatalog.
    METHODS retrieve_data.
    METHODS set_option_icon IMPORTING iv_opti TYPE char2 iv_inex TYPE char1 OPTIONAL
    CHANGING cv_result TYPE icons-text.
ENDCLASS.

DATA go_tvarvc_clone TYPE REF TO lcl_tvarvc_clone.
