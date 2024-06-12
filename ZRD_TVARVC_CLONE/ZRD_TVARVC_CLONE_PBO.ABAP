*&---------------------------------------------------------------------*
*&  Include           ZRD_TVARVC_CLONE_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  DATA: lv_container_p TYPE scrfname VALUE 'GS_DIALOG1-CUST_CONT',
        lo_cus_cont_p  TYPE REF TO cl_gui_custom_container,
        lv_container_s TYPE scrfname VALUE 'GS_DIALOG2-CUST_CONT',
        lo_cus_cont_s  TYPE REF TO cl_gui_custom_container,
        ls_layout      TYPE lvc_s_layo,
        lv_result      TYPE icons-text.

  SET PF-STATUS 'PF_GS01'.

  IF gv_edit_mode = abap_false.
    SET TITLEBAR 'TB_GS01'.
    ls_layout-edit = ' '.
  ELSEIF gv_edit_mode = abap_true.
    ls_layout-edit = 'X'.
    SET TITLEBAR 'TB_GS01_V2'.
  ENDIF.

  ls_layout-sel_mode = 'A'.

  IF lo_cus_cont_p IS INITIAL.
    CREATE OBJECT lo_cus_cont_p EXPORTING container_name = lv_container_p.
    CREATE OBJECT go_tvarvc_clone->mo_grid_param EXPORTING i_parent = lo_cus_cont_p.
    SET HANDLER:  lcl_tvarvc_clone=>handle_grid_toolbar
                  go_tvarvc_clone->handle_grid_user_command
                  go_tvarvc_clone->on_hotspot_click
                  go_tvarvc_clone->on_param_data_changed FOR go_tvarvc_clone->mo_grid_param.
  ENDIF.

  IF lo_cus_cont_s IS INITIAL.
    CREATE OBJECT lo_cus_cont_s EXPORTING container_name = lv_container_s.
    CREATE OBJECT go_tvarvc_clone->mo_grid_selop EXPORTING i_parent = lo_cus_cont_s.
    SET HANDLER:  lcl_tvarvc_clone=>handle_grid_toolbar
                  go_tvarvc_clone->handle_grid_user_command
                  go_tvarvc_clone->on_hotspot_click
                  go_tvarvc_clone->on_sel_data_changed FOR go_tvarvc_clone->mo_grid_selop.
  ENDIF.

  go_tvarvc_clone->set_icons( ).

  CALL METHOD go_tvarvc_clone->mo_grid_param->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = go_tvarvc_clone->mt_fcat_p
      it_outtab       = go_tvarvc_clone->mt_param_table.

  CALL METHOD go_tvarvc_clone->mo_grid_selop->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = go_tvarvc_clone->mt_fcat_s
      it_outtab       = go_tvarvc_clone->mt_sel_table.
ENDMODULE.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'ZTABSTRIP'
CONSTANTS: BEGIN OF c_ztabstrip,
             tab1 LIKE sy-ucomm VALUE 'ZTABSTRIP_FC1',
             tab2 LIKE sy-ucomm VALUE 'ZTABSTRIP_FC2',
           END OF c_ztabstrip.
*&SPWIZARD: DATA FOR TABSTRIP 'ZTABSTRIP'
CONTROLS:  ztabstrip TYPE TABSTRIP.
DATA:      BEGIN OF g_ztabstrip,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'ZRD_TVARVC_CLONE',
             pressed_tab LIKE sy-ucomm VALUE c_ztabstrip-tab1,
           END OF g_ztabstrip.

*&SPWIZARD: OUTPUT MODULE FOR TS 'ZTABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE ztabstrip_active_tab_set OUTPUT.
  ztabstrip-activetab = g_ztabstrip-pressed_tab.
  CASE g_ztabstrip-pressed_tab.
    WHEN c_ztabstrip-tab1.
      g_ztabstrip-subscreen = '0002'.
    WHEN c_ztabstrip-tab2.
      g_ztabstrip-subscreen = '0003'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'ZTABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE ztabstrip_active_tab_get INPUT.
  gs_screen01-ok_code = sy-ucomm.
  CASE gs_screen01-ok_code.
    WHEN c_ztabstrip-tab1.
      g_ztabstrip-pressed_tab = c_ztabstrip-tab1.
    WHEN c_ztabstrip-tab2.
      g_ztabstrip-pressed_tab = c_ztabstrip-tab2.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
