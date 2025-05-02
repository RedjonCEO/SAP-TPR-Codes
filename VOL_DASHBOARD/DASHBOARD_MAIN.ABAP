*&---------------------------------------------------------------------*
*& Report ZLPBNS_PROMO_MD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlpbns_vol_dash_md.

INCLUDE zlpbns_vol_dash_md_top.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_vkorg TYPE vkorg OBLIGATORY DEFAULT 'O994',
            p_markt TYPE zlpbns_promo_mdh-market OBLIGATORY.
SELECT-OPTIONS: s_bonus  FOR zlpbns_vol_mdh-bonus_id,
                s_valfrm FOR zlpbns_vol_mdh-valid_from,
                s_valto  FOR zlpbns_vol_mdh-valid_to,
                s_bcateg FOR zlpbns_vol_mdh-bcategory,
                s_cntper FOR zlpbns_vol_mdh-count_period,
                s_cntrul FOR zlpbns_vol_mdh-count_rule,
                s_zqrtcu FOR zlpbns_vol_mdh-zqrt_catch_up,
                s_zdele  FOR zlpbns_vol_mdh-zdele,
                s_creon  FOR zlpbns_vol_mdh-credate.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-t02.
PARAMETERS: p_vari TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b02.
CLASS lcl_bonus DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_bonus,
             icon              TYPE icon_d,
             vkorg             TYPE vkorg,
             market            TYPE zlpbns_vol_mdh-market,
             bonus_id          TYPE zlpbns_vol_mdh-bonus_id,
             bonus_desc        TYPE zlpbns_vol_mdht-text,
             bcategory         TYPE zbcategory,
             bcategory_desc    TYPE ddtext,
             valid_from        TYPE zlpbns_vol_mdh-valid_from,
             valid_to          TYPE zlpbns_vol_mdh-valid_to,
             count_period      TYPE zlpbns_vol_mdh-count_period,
             count_period_desc TYPE ddtext,
             count_rule        TYPE zlpbns_vol_mdh-count_rule,
             count_rule_desc   TYPE ddtext,
             zqrt_catch_up     TYPE zlpbns_vol_mdh-zqrt_catch_up,
             zdele             TYPE zlpbns_vol_mdh-zdele,
             credate           TYPE zlpbns_vol_mdh-credate,
             cretime           TYPE zlpbns_vol_mdh-cretime,
             crename           TYPE zlpbns_vol_mdh-crename,
             moddate           TYPE zlpbns_vol_mdh-moddate,
             modtime           TYPE zlpbns_vol_mdh-modtime,
             modname           TYPE zlpbns_vol_mdh-modname,
           END OF ty_bonus,
           tt_bonus TYPE STANDARD TABLE OF ty_bonus.

    TYPES: BEGIN OF ty_model,
             comm_model_code       TYPE zlpbns_vol_model-comm_model_code,
             comm_model_code_descr TYPE zlmiv_pc_link_cm-comm_model_code_descr,
             fuel_type             TYPE zlpbns_vol_model-fuel_type,
             fuel_type_desc        TYPE ddtext,
             saved                 TYPE abap_bool,
           END OF ty_model.

    TYPES: BEGIN OF ty_target,
             kunnr       TYPE zlpbns_vol_trgt-kunnr,
             name1       TYPE kna1-name1,
             zyear       TYPE zlpbns_vol_trgt-zyear,
             zmonth_from TYPE zlpbns_vol_trgt-zmonth_from,
             zmonth_to   TYPE zlpbns_vol_trgt-zmonth_to,
             target      TYPE zlpbns_vol_trgt-target,
             saved       TYPE abap_bool,
           END OF ty_target.

    TYPES: BEGIN OF ty_incent,
             tratio_min            TYPE zlpbns_vol_inc-tratio_min,
             tratio_max            TYPE zlpbns_vol_inc-tratio_max,
             comm_model_code       TYPE zlpbns_vol_inc-comm_model_code,
             comm_model_code_descr TYPE zlmiv_pc_link_cm-comm_model_code_descr,
             fuel_type             TYPE zlpbns_vol_model-fuel_type,
             fuel_type_desc        TYPE ddtext,
             sell_type             TYPE zlpbns_vol_inc-sell_type,
             incentive_perc        TYPE zlpbns_vol_inc-incentive_perc,
             incentive             TYPE zlpbns_vol_inc-incentive,
             zcurr                 TYPE zlpbns_vol_inc-zcurr,
             saved                 TYPE abap_bool,
           END OF ty_incent.

    TYPES: BEGIN OF ty_vin,
             vhvin           TYPE zlpbns_vol_vin-vhvin,
             excl_type       TYPE zlpbns_vol_vin-excl_type,
             excl_type_descr TYPE ddtext,
             saved           TYPE abap_bool,
           END OF ty_vin.

    TYPES: BEGIN OF ty_sale,
             sell_type       TYPE zlpbns_vol_stype-sell_type,
             excl_type       TYPE zlpbns_vol_stype-excl_type,
             excl_type_descr TYPE ddtext,
             saved           TYPE abap_bool,
           END OF ty_sale.

    DATA: mv_ok_code      TYPE sy-ucomm,
          mv_state        TYPE string,
          ms_bonus_header TYPE ty_bonus,
          mv_bonus_id     TYPE zlpbns_vol_mdh-bonus_id,
          mv_valid_to     TYPE char15,
          mv_valid_from   TYPE char15,
          mv_last_tab     TYPE char30.

    DATA: mt_model  TYPE STANDARD TABLE OF ty_model,
          mt_target TYPE STANDARD TABLE OF ty_target,
          mt_incent TYPE STANDARD TABLE OF ty_incent,
          mt_vin    TYPE STANDARD TABLE OF ty_vin,
          mt_sale   TYPE STANDARD TABLE OF ty_sale.

    METHODS    execute IMPORTING iv_state TYPE string
                                 is_bonus TYPE ty_bonus OPTIONAL.
    METHODS pbo.
    METHODS pai.
    METHODS pai_exit200.
    METHODS check_changed_data.
    METHODS get_result EXPORTING es_bonus TYPE ty_bonus
                                 ev_saved TYPE abap_bool.
* Tabstrip PBO
    METHODS ts_pbo.

  PRIVATE SECTION.

    DATA: mv_saved           TYPE abap_bool,
          mt_messages_header TYPE esp1_message_tab_type,
          mt_messages_item   TYPE esp1_message_tab_type,
          mt_messages        TYPE esp1_message_tab_type.

    DATA: mo_model_container  TYPE REF TO cl_gui_custom_container,
          mo_model            TYPE REF TO cl_gui_alv_grid,
          mt_model_fcat       TYPE lvc_t_fcat,

          mo_target_container TYPE REF TO cl_gui_custom_container,
          mo_target           TYPE REF TO cl_gui_alv_grid,
          mt_target_fcat      TYPE lvc_t_fcat,

          mo_incent_container TYPE REF TO cl_gui_custom_container,
          mo_incent           TYPE REF TO cl_gui_alv_grid,
          mt_incent_fcat      TYPE lvc_t_fcat,

          mo_vin_container    TYPE REF TO cl_gui_custom_container,
          mo_vin              TYPE REF TO cl_gui_alv_grid,
          mt_vin_fcat         TYPE lvc_t_fcat,

          mo_sale_container   TYPE REF TO cl_gui_custom_container,
          mo_sale             TYPE REF TO cl_gui_alv_grid,
          mt_sale_fcat        TYPE lvc_t_fcat.

    DATA: mt_deleted_model  TYPE STANDARD TABLE OF ty_model,
          mt_deleted_target TYPE STANDARD TABLE OF ty_target,
          mt_deleted_incent TYPE STANDARD TABLE OF ty_incent,
          mt_deleted_vin    TYPE STANDARD TABLE OF ty_vin,
          mt_deleted_sale   TYPE STANDARD TABLE OF ty_sale.

    METHODS modify_screen.
    METHODS hide_show_tabs.
    METHODS lock_tables.
    METHODS unlock_tables.
    METHODS popup_to_confirm IMPORTING iv_text TYPE string RETURNING VALUE(rv_answer) TYPE char1.
    METHODS refresh_tabstrip_alv.
    METHODS save.
    METHODS clear_bonus IMPORTING iv_clear_header TYPE abap_bool DEFAULT abap_false.
    METHODS create_entries.
    METHODS update_entries.
    METHODS check_header_data.
    METHODS check_item_data.
    METHODS insert_row IMPORTING iv_tab TYPE string.
    METHODS delete_rows IMPORTING iv_tab           TYPE string
                                  it_selected_rows TYPE lvc_t_roid.
    METHODS delete_all IMPORTING iv_tab TYPE string.
    METHODS add_message IMPORTING iv_msgno    TYPE esp1_message_wa_type-msgno
                                  iv_msgty    TYPE esp1_message_wa_type-msgty
                                  iv_msgv1    TYPE esp1_message_wa_type-msgv1 OPTIONAL
                                  iv_msgv2    TYPE esp1_message_wa_type-msgv2 OPTIONAL
                                  iv_msgv3    TYPE esp1_message_wa_type-msgv3 OPTIONAL
                                  iv_msgv4    TYPE esp1_message_wa_type-msgv4 OPTIONAL
                        CHANGING  ct_messages TYPE esp1_message_tab_type.
    METHODS add_message_sys CHANGING  ct_messages TYPE esp1_message_tab_type.
    METHODS display_messages.
    METHODS fill_tables.
    METHODS model_fieldcatalog.
    METHODS target_fieldcatalog.
    METHODS incent_fieldcatalog.
    METHODS vin_fieldcatalog.
    METHODS sale_fieldcatalog..
    METHODS update_fcat.

    METHODS on_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
    METHODS on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
    METHODS on_hotspot      FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id
                                                                                 e_row_id.
    METHODS grid_toolbar    FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object
                                                                           e_interactive.

ENDCLASS.

CLASS lcl_bonus IMPLEMENTATION.
  METHOD execute.

    g_ts_bonus-pressed_tab = c_ts_bonus-tab1. "Reset the tabstrip tab

    ms_bonus_header = is_bonus.
    mv_state = iv_state.
    CLEAR mv_saved.

    CASE mv_state.
      WHEN gc_state-new.
        mv_bonus_id = '$$$'.
        ms_bonus_header = VALUE #( vkorg    = p_vkorg
                                   market   = p_markt
                                   bonus_id = mv_bonus_id
                                   credate  = sy-datum
                                   cretime  = sy-uzeit
                                   crename  = sy-uname ).
        RETURN.
      WHEN gc_state-view.
        mv_bonus_id = ms_bonus_header-bonus_id.
        CONDENSE ms_bonus_header-bonus_id.
    ENDCASE.

    fill_tables( ).
  ENDMETHOD.
  METHOD pbo.

    modify_screen( ).
    check_changed_data( ).

    IF mv_ok_code <> 'FC_SAVE'.
      CLEAR mv_saved.
    ELSE.
      CLEAR mv_ok_code.
    ENDIF.

    update_fcat( ).

    mv_last_tab = g_ts_promo-pressed_tab.

  ENDMETHOD.
  METHOD pai.
    CASE mv_ok_code.

      WHEN 'FC_DELETE'.

        lock_tables( ).
        IF gv_locked = abap_false.
          unlock_tables( ).
          RETURN.
        ENDIF.

        IF popup_to_confirm( iv_text = 'Do you want to delete the bonus?' ) = '2'.
          RETURN.
        ENDIF.

        CASE mv_state.
          WHEN gc_state-new.
            clear_bonus( iv_clear_header = abap_true ).
            MESSAGE s019(zlpbns_bonus) DISPLAY LIKE 'E'.
            LEAVE TO SCREEN 0.
          WHEN gc_state-edit OR gc_state-view.
            ms_bonus_header-zdele  = abap_true.
            save( ).
        ENDCASE.
        unlock_tables( ).
      WHEN 'FC_SAVE'.
        unlock_tables( ).
        save( ).
      WHEN 'FC_EDIT'.
        lock_tables( ).
        IF gv_locked = abap_false.
          unlock_tables( ).
          RETURN.
        ENDIF.
        mv_state = gc_state-edit.
      WHEN 'FC_DISPLAY'.
        mv_state = gc_state-view.
      WHEN c_ts_bonus-tab1. "Model
        g_ts_bonus-pressed_tab = c_ts_bonus-tab1.
      WHEN c_ts_bonus-tab2. "TARGET
        g_ts_bonus-pressed_tab = c_ts_bonus-tab2.
      WHEN c_ts_bonus-tab3. "INCENT
        g_ts_bonus-pressed_tab = c_ts_bonus-tab3.
      WHEN c_ts_bonus-tab4. "Vin
        g_ts_bonus-pressed_tab = c_ts_bonus-tab4.
      WHEN c_ts_bonus-tab5. "SALE
        g_ts_bonus-pressed_tab = c_ts_bonus-tab5.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD pai_exit200.
    CASE mv_ok_code.
      WHEN 'FC_BACK'.
        IF mv_saved IS INITIAL.
          IF popup_to_confirm( iv_text = 'Unsaved data will be lost. Do you want to continue?' ) = '2'.
            RETURN.
          ENDIF.
        ENDIF.
        clear_bonus( ).
        unlock_tables( ).
        LEAVE TO SCREEN 0.
      WHEN 'FC_EXIT'.
        unlock_tables( ).
        LEAVE PROGRAM.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD  lock_tables.

    IF gv_locked = abap_true.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = 'ZLPBNS_VOL_MDH'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = 'ZLPBNS_VOL_MODEL'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = 'ZLPBNS_VOL_TRGT'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = 'ZLPBNS_VOL_INC'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = 'ZLPBNS_VOL_VIN'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = 'ZLPBNS_VOL_STYPE'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    gv_locked = abap_true.

  ENDMETHOD.


  METHOD  unlock_tables.

    IF gv_locked = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZLPBNS_VOL_MDH'.

    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZLPBNS_VOL_MODEL'.

    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZLPBNS_VOL_TRGT'.

    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZLPBNS_VOL_INC'.

    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZLPBNS_VOL_VIN'.

    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZLPBNS_VOL_STYPE'.

    gv_locked = abap_false.

  ENDMETHOD.

  METHOD modify_screen.

    LOOP AT SCREEN.
      READ TABLE gt_tabstrip_header ASSIGNING FIELD-SYMBOL(<ls_header>) WITH KEY name = screen-name.
      IF sy-subrc = 0.

        ASSIGN (<ls_header>-name) TO FIELD-SYMBOL(<lv_tab_text>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        ASSIGN (<ls_header>-data_tab) TO FIELD-SYMBOL(<ls_data>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <ls_data> IS INITIAL.
          <lv_tab_text> = <ls_header>-empty.
        ELSE.
          <lv_tab_text> = <ls_header>-full.
        ENDIF.
      ENDIF.

      CASE screen-group1.
        WHEN 'HD'.
          IF mv_state = gc_state-view.
            screen-input = 0.
          ELSE.
            screen-input = 1.
          ENDIF.

          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

    IF mv_state = gc_state-view OR mv_state = gc_state-edit.
      hide_show_tabs( ).
    ENDIF.

  ENDMETHOD.
  METHOD hide_show_tabs.

    SELECT *
      FROM zlpbns_dash_parm
      INTO TABLE @DATA(lt_param)
      WHERE vkorg = @p_vkorg
      AND bcategory = @ms_bonus_header-bcategory.

    LOOP AT lt_param ASSIGNING FIELD-SYMBOL(<ls_param>).
      CASE <ls_param>-tab_type.
        WHEN 'N'.
          CONTINUE.
        WHEN 'M'.
          DATA(lv_datatab) = |MT_| && <ls_param>-tab.
          READ TABLE gt_tabstrip_header ASSIGNING FIELD-SYMBOL(<ls_header>) WITH KEY data_tab = lv_datatab.
          IF sy-subrc = 0.
            LOOP AT SCREEN.
              IF screen-name = <ls_header>-name.
                screen-intensified = 1.
                screen-display_3d = 1.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          ENDIF.
        WHEN abap_false.
          lv_datatab = |MT_| && <ls_param>-tab.
          READ TABLE gt_tabstrip_header ASSIGNING <ls_header> WITH KEY data_tab = lv_datatab.
          IF sy-subrc = 0.
            LOOP AT SCREEN.
              IF screen-name = <ls_header>-name.
                screen-active = 0.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_result.

    es_bonus = ms_bonus_header.
    ev_saved = mv_saved.

  ENDMETHOD.
  METHOD update_fcat.

    FIELD-SYMBOLS: <lo_grid>         TYPE REF TO cl_gui_alv_grid.

    DATA(lv_tabstrip_name) = g_ts_promo-pressed_tab+3.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF mv_state = gc_state-view.
      <lo_grid>->set_ready_for_input( 0 ).

    ELSE.
      <lo_grid>->set_ready_for_input( 1 ).
    ENDIF.

*    <lo_grid>->refresh_table_display(
*      EXCEPTIONS
*        finished       = 1                " Display was Ended (by Export)
*        OTHERS         = 2
*    ).
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

  ENDMETHOD.
  METHOD check_changed_data.

    FIELD-SYMBOLS: <lo_grid>         TYPE REF TO cl_gui_alv_grid.

    DATA(lv_tabstrip_name) = mv_last_tab+3.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    <lo_grid>->check_changed_data( ).
  ENDMETHOD.
  METHOD ts_pbo.

    DATA: ls_layout            TYPE lvc_s_layo,
          lt_toolbar_excluding TYPE ui_functions.

    FIELD-SYMBOLS: <lo_container>    TYPE REF TO cl_gui_custom_container,
                   <lo_grid>         TYPE REF TO cl_gui_alv_grid,
                   <lt_outtab>       TYPE ANY TABLE,
                   <lt_fieldcatalog> TYPE lvc_t_fcat.

    DATA(lv_tabstrip_name) = g_ts_bonus-pressed_tab+3.

    DATA(lv_container_name) = |MO_{ lv_tabstrip_name }_CONTAINER|.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.
    DATA(lv_data_tab_name) = |MT_{ lv_tabstrip_name }|.
    DATA(lv_fcat_name) = |MT_{ lv_tabstrip_name }_FCAT|.
    DATA(lv_fcat_method_name) = |{ lv_tabstrip_name }_FIELDCATALOG|.

    ASSIGN (lv_container_name) TO <lo_container>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_data_tab_name) TO <lt_outtab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_fcat_name) TO <lt_fieldcatalog>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF <lo_container> IS BOUND.
      <lo_grid>->refresh_table_display(
        EXCEPTIONS
          finished       = 1                " Display was Ended (by Export)
          OTHERS         = 2
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      RETURN.

    ENDIF.

    <lo_container> = NEW cl_gui_custom_container(
      container_name = |{ lv_tabstrip_name }_CONTAINER|
    ).

    <lo_grid> = NEW cl_gui_alv_grid(
      i_parent         = <lo_container>
    ).

    ls_layout = VALUE #( cwidth_opt = abap_true
                         sel_mode = 'D' ).

    lt_toolbar_excluding = VALUE #( ( '&LOCAL&CUT' )
                                   ( '&LOCAL&COPY'      )
*                                   ( '&LOCAL&PASTE'     )
                                   ( '&LOCAL&UNDO'      )
                                   ( '&&SEP00'          )
                                   ( '&CHECK'           )
                                   ( '&REFRESH'         )
                                   ( '&&SEP02'          )
                                   ( '&LOCAL&APPEND'    )
                                   ( '&LOCAL&INSERT_ROW')
                                   ( '&LOCAL&DELETE_ROW')
                                   ( '&LOCAL&COPY_ROW'  )
*                                   ( '&LOCAL&PASTE_NEW_ROW' )
                                   ( '&&SEP03'          )
                                   ( '&MB_SUM'          )
                                   ( '&MB_SUBTOT'       )
                                   ( '&&SEP05'          )
                                   ( '&PRINT_BACK'      )
                                   ( '&MB_VIEW'         )
                                   ( '&GRAPH'           )
                                   ( '&&SEP07'          )
                                   ( '&INFO'            ) ).

    CALL METHOD (lv_fcat_method_name).
*      EXPORTING
*        iv_edit = COND abap_bool( WHEN mv_state = gc_state-view
*                                  THEN abap_false
*                                  ELSE abap_true ).

    SET HANDLER on_user_command FOR <lo_grid>.
    SET HANDLER on_data_changed FOR <lo_grid>.
    SET HANDLER on_hotspot FOR <lo_grid>.
    SET HANDLER grid_toolbar FOR <lo_grid>.

    <lo_grid>->register_edit_event(
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1                " Error
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    <lo_grid>->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excluding
      CHANGING
        it_outtab                     = <lt_outtab>
        it_fieldcatalog               = <lt_fieldcatalog>
      EXCEPTIONS
        invalid_parameter_combination = 1                " Wrong Parameter
        program_error                 = 2                " Program Errors
        too_many_lines                = 3                " Too many Rows in Ready for Input Grid
        OTHERS                        = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  METHOD on_user_command.

    DATA: lt_selected_rows TYPE lvc_t_roid.

    FIELD-SYMBOLS: <lo_grid>  TYPE REF TO cl_gui_alv_grid,
                   <lt_table> TYPE ANY TABLE.

    check_header_data( ).

    IF mt_messages_header IS NOT INITIAL.
      MESSAGE TEXT-e07 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lv_tabstrip_name) = CONV string( g_ts_bonus-pressed_tab+3 ).

    DATA(lv_data_tab_name) = |MT_{ lv_tabstrip_name }|.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    <lo_grid>->get_selected_rows(
      IMPORTING
        et_row_no     = lt_selected_rows
    ).

    CASE e_ucomm.
      WHEN 'FC_INSERT_ROW'.
        insert_row( iv_tab = lv_tabstrip_name ).
      WHEN 'FC_DELETE_ROW'.
        IF lt_selected_rows IS INITIAL.
          MESSAGE TEXT-e02 TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        delete_rows( iv_tab = lv_tabstrip_name
                     it_selected_rows = lt_selected_rows ).
      WHEN 'FC_DELETE_ALL'.
        ASSIGN (lv_data_tab_name) TO <lt_table>.
        IF sy-subrc = 0.
          LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table_row>).
            APPEND VALUE #( row_id = sy-tabix ) TO lt_selected_rows.
          ENDLOOP.
          SORT lt_selected_rows BY row_id DESCENDING.
          delete_rows( iv_tab = lv_tabstrip_name
                       it_selected_rows = lt_selected_rows ).
        ENDIF.
    ENDCASE.

    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
  ENDMETHOD.
  METHOD on_data_changed.

    check_header_data( ).

    IF mt_messages_header IS NOT INITIAL.
      MESSAGE TEXT-e07 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_mod_cells>).

      CASE <ls_mod_cells>-fieldname.
        WHEN 'COMM_MODEL_CODE'.

          SELECT SINGLE comm_model_code_descr
            FROM zlmiv_pc_link_cm
            INTO @DATA(lv_mod_desc)
            WHERE comm_model_code = @<ls_mod_cells>-value
              AND lingua = @sy-langu.

          er_data_changed->modify_cell(
             EXPORTING
               i_row_id    = <ls_mod_cells>-row_id
               i_fieldname = 'COMM_MODEL_CODE_DESCR'
               i_value     = lv_mod_desc
           ).
          CLEAR lv_mod_desc.
        WHEN 'FUEL_TYPE'.

          SELECT SINGLE ddtext
            FROM dd07t
            INTO @DATA(lv_fueldesc)
            WHERE domname  = 'TESTTTTT'
            AND domvalue_l = @<ls_mod_cells>-value
            AND ddlanguage = @sy-langu.

          er_data_changed->modify_cell(
             EXPORTING
               i_row_id    = <ls_mod_cells>-row_id
               i_fieldname = 'FUEL_TYPE_DESC'
               i_value     = lv_fueldesc
           ).
        WHEN 'KUNNR'.

          SELECT SINGLE name1
            FROM kna1
            INTO @DATA(lv_name1)
            WHERE kunnr = @<ls_mod_cells>-value.

          er_data_changed->modify_cell(
             EXPORTING
               i_row_id    = <ls_mod_cells>-row_id
               i_fieldname = 'NAME1'
               i_value     = lv_name1
           ).
          CLEAR lv_name1.

        WHEN 'EXCL_TYPE'.

          SELECT SINGLE ddtext
            FROM dd07t
            INTO @DATA(lv_excldesc)
            WHERE domname  = 'TESTTTTT'
            AND domvalue_l = @<ls_mod_cells>-value
            AND ddlanguage = @sy-langu.

          er_data_changed->modify_cell(
             EXPORTING
               i_row_id    = <ls_mod_cells>-row_id
               i_fieldname = 'EXCL_TYPE_DESCR'
               i_value     = lv_excldesc
           ).
          CLEAR lv_excldesc.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD on_hotspot.
  ENDMETHOD.
  METHOD grid_toolbar.
    DATA: lt_add_func TYPE ttb_button.

    IF mv_state = gc_state-view.
      RETURN.
    ENDIF.

    lt_add_func = VALUE #( ( function = 'FC_INSERT_ROW' icon = icon_insert_row text = 'Insert' )
                           ( function = 'FC_DELETE_ROW' icon = icon_delete_row text = 'Delete Rows' )
                           ( function = 'FC_DELETE_ALL' icon = icon_delete text = 'Remove ALL' ) ).

    APPEND LINES OF lt_add_func TO e_object->mt_toolbar.
  ENDMETHOD.
  METHOD model_fieldcatalog.
    mt_model_fcat = VALUE #( ( fieldname = 'COMM_MODEL_CODE'       ref_table = 'ZLPBNS_VOL_MODEL' ref_field = 'COMM_MODEL_CODE'  edit = abap_true )
                             ( fieldname = 'COMM_MODEL_CODE_DESCR' ref_table = 'ZLMIV_PC_LINK_CM' ref_field = 'COMM_MODEL_CODE_DESCR' scrtext_s = 'Descr.'
                              scrtext_m  = 'Model descr.'          scrtext_l = 'Commercial model decscr.')
                             ( fieldname = 'FUEL_TYPE'             ref_table = 'ZLPBNS_VOL_MODEL' ref_field = 'FUEL_TYPE'        edit = abap_true )
                             ( fieldname = 'FUEL_TYPE_DESC'        scrtext_s = 'Descript.'        scrtext_m = 'Fuel Typ. descr.'
                             scrtext_l   = 'Fuel type description' ) ).
  ENDMETHOD.
  METHOD target_fieldcatalog.
    mt_target_fcat = VALUE #( ( fieldname = 'KUNNR'       ref_table = 'ZLPBNS_VOL_TRGT' ref_field = 'KUNNR'       edit = abap_true )
                              ( fieldname = 'NAME1'       ref_table = 'KNA1'            ref_field = 'NAME1'     scrtext_s = 'Name'
                               scrtext_m  = 'Cost. name'  scrtext_l = 'Costumer name')
                              ( fieldname = 'ZYEAR'       ref_table = 'ZLPBNS_VOL_TRGT' ref_field = 'ZYEAR'       edit = abap_true )
                              ( fieldname = 'ZMONTH_FROM' ref_table = 'ZLPBNS_VOL_TRGT' ref_field = 'ZMONTH_FROM' edit = abap_true )
                              ( fieldname = 'ZMONTH_TO'   ref_table = 'ZLPBNS_VOL_TRGT' ref_field = 'ZMONTH_TO'   edit = abap_true )
                              ( fieldname = 'TARGET'      ref_table = 'ZLPBNS_VOL_TRGT' ref_field = 'TARGET'      edit = abap_true ) ).
  ENDMETHOD.
  METHOD incent_fieldcatalog.

    mt_incent_fcat = VALUE #( ( fieldname = 'TRATIO_MIN'            ref_table = 'ZLPBNS_VOL_INC'   ref_field = 'TRATIO_MIN'       edit = abap_true )
                              ( fieldname = 'TRATIO_MAX'            ref_table = 'ZLPBNS_VOL_INC'   ref_field = 'TRATIO_MAX'       edit = abap_true )
                              ( fieldname = 'COMM_MODEL_CODE'       ref_table = 'ZLPBNS_VOL_INC'   ref_field = 'COMM_MODEL_CODE'  edit = abap_true )
                              ( fieldname = 'COMM_MODEL_CODE_DESCR' ref_table = 'ZLMIV_PC_LINK_CM' ref_field = 'COMM_MODEL_CODE_DESCR' scrtext_s = 'Descr.'
                               scrtext_m  = 'Model descr.'          scrtext_l = 'Commercial model decscr.')
                              ( fieldname = 'FUEL_TYPE'             ref_table = 'ZLPBNS_VOL_MODEL' ref_field = 'FUEL_TYPE'        edit = abap_true )
                              ( fieldname = 'FUEL_TYPE_DESC'        scrtext_s = 'Descript.'        scrtext_m = 'Fuel Typ. descr.'
                                scrtext_l = 'Fuel type description' )
                              ( fieldname = 'SELL_TYPE'             ref_table = 'ZLPBNS_VOL_INC'   ref_field = 'SELL_TYPE'        edit = abap_true )
                              ( fieldname = 'INCENTIVE_PERC'        ref_table = 'ZLPBNS_VOL_INC'   ref_field = 'ZRSTDK_INCENTIVE_PERC' edit = abap_true )
                              ( fieldname = 'INCENTIVE_AMOUNT'      ref_table = 'ZLPBNS_VOL_INC'   ref_field = 'INCENTIVE' edit = abap_true   cfieldname = 'ZCURR' )
                              ( fieldname = 'ZCURR'                 ref_table = 'ZLPBNS_VOL_INC'   ref_field = 'ZCURR'            edit = abap_true )
                               ).

  ENDMETHOD.
  METHOD vin_fieldcatalog.

    mt_vin_fcat = VALUE #( ( fieldname = 'VHVIN'            ref_table = 'ZLPBNS_VOL_VIN' ref_field = 'VHVIN'       edit = abap_true )
                           ( fieldname = 'EXCL_TYPE'        ref_table = 'ZLPBNS_VOL_VIN' ref_field = 'EXCL_TYPE'   edit = abap_true )
                           ( fieldname = 'EXCL_TYPE_DESCR'  scrtext_s = 'Descript.'      scrtext_m = 'Excl. Typ. descr.'
                             scrtext_l = 'Exclude type description' ) ).

  ENDMETHOD.
  METHOD sale_fieldcatalog.

    mt_sale_fcat = VALUE #( ( fieldname = 'SELL_TYPE'        ref_table = 'ZLPBNS_VOL_STYPE' ref_field = 'SELL_TYPE'       edit = abap_true )
                            ( fieldname = 'EXCL_TYPE'        ref_table = 'ZLPBNS_VOL_STYPE' ref_field = 'EXCL_TYPE'   edit = abap_true )
                            ( fieldname = 'EXCL_TYPE_DESCR'  scrtext_s = 'Descript.'      scrtext_m = 'Excl. Typ. descr.'
                              scrtext_l = 'Exclude type description'  ) ).

  ENDMETHOD.
  METHOD save.

    check_changed_data( ).

    check_header_data( ).

    check_item_data( ).

    display_messages( ).

    IF mt_messages IS NOT INITIAL.
      MESSAGE TEXT-e08 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE mv_state.
      WHEN gc_state-new.
        IF mv_saved = abap_true.
          RETURN.
        ENDIF.
        create_entries( ).
      WHEN gc_state-edit OR gc_state-view.
        update_entries( ).
    ENDCASE.


    IF mt_messages IS NOT INITIAL.
      ROLLBACK WORK.
      display_messages( ).
      MESSAGE s005(zlpbns_bonus) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    display_messages( ).

    COMMIT WORK.
    mv_saved = abap_true.

    CASE mv_state.
      WHEN gc_state-new OR gc_state-copy.
        MESSAGE |Bonus { ms_bonus_header-bonus_id } { TEXT-s01 }| TYPE 'S'.
      WHEN gc_state-edit OR gc_state-view.
        MESSAGE |Bonus { ms_bonus_header-bonus_id } { TEXT-s02 }| TYPE 'S'.
    ENDCASE.

    clear_bonus( ).
    LEAVE TO SCREEN 0.

  ENDMETHOD.
  METHOD create_entries.

    DATA: ls_bonus_mdh  TYPE zlpbns_vol_mdh,
          ls_bonus_mdht TYPE zlpbns_vol_mdht,
          ls_model      TYPE zlpbns_vol_model,
          ls_target     TYPE zlpbns_vol_trgt,
          ls_incent     TYPE zlpbns_vol_inc,
          ls_vin        TYPE zlpbns_vol_vin,
          ls_sale       TYPE zlpbns_vol_stype,
          lt_sale       TYPE STANDARD TABLE OF zlpbns_vol_stype.

    SELECT MAX( bonus_id )
      FROM zlpbns_vol_mdh
      INTO @DATA(lv_last_bonus).

    CONDENSE lv_last_bonus.

    IF sy-subrc = 0.
      mv_bonus_id = ms_bonus_header-bonus_id = CONV char3( CONV i( lv_last_bonus ) + 1 ). " To be changed
    ELSE.
      mv_bonus_id = ms_bonus_header-bonus_id = '000'.
    ENDIF.

    CONDENSE mv_bonus_id.

    ls_bonus_mdh  = CORRESPONDING #( ms_bonus_header ).
    ls_bonus_mdht = CORRESPONDING #( ms_bonus_header ).
    ls_bonus_mdht-text      = ms_bonus_header-bonus_desc.
    ls_bonus_mdht-spras     = sy-langu.

    INSERT zlpbns_vol_mdh  FROM ls_bonus_mdh.
    IF sy-subrc <> 0.
      MESSAGE e020(zlpbns_bonus) WITH '' INTO DATA(lv_dummy).
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    INSERT zlpbns_vol_mdht FROM ls_bonus_mdht.
    IF sy-subrc <> 0.
      MESSAGE e020(zlpbns_bonus) WITH '' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    LOOP AT mt_model ASSIGNING FIELD-SYMBOL(<ls_model>).
      CLEAR ls_model.
      ls_model = CORRESPONDING #( <ls_model> ).
      ls_model-market   = ms_bonus_header-market.
      ls_model-bonus_id = ms_bonus_header-bonus_id.

      INSERT zlpbns_vol_model FROM ls_model.
      IF sy-subrc <> 0.
        MESSAGE e004(zlpbns_bonus)
        WITH 'Model' 'material' ls_model-comm_model_code INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_header ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_target ASSIGNING FIELD-SYMBOL(<ls_target>).
      CLEAR ls_target.
      ls_target = CORRESPONDING #( <ls_target> ).
      ls_target-market   = ms_bonus_header-market.
      ls_target-bonus_id = ms_bonus_header-bonus_id.

      INSERT zlpbns_vol_trgt FROM ls_target.
      IF sy-subrc <> 0.
        MESSAGE e004(zlpbns_bonus)
        WITH 'Target' 'target' <ls_target>-target INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_header ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_incent ASSIGNING FIELD-SYMBOL(<ls_incent>).
      CLEAR ls_incent.
      ls_incent = CORRESPONDING #( <ls_incent> ).
      ls_incent-market   = ms_bonus_header-market.
      ls_incent-bonus_id = ms_bonus_header-bonus_id.

      INSERT zlpbns_vol_inc FROM ls_incent.
      IF sy-subrc <> 0.
        MESSAGE e004(zlpbns_bonus)
        WITH 'Incentive' 'incentive' ls_incent-incentive INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_header ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_vin ASSIGNING FIELD-SYMBOL(<ls_vin>).
      CLEAR ls_vin.
      ls_vin = CORRESPONDING #( <ls_vin> ).
      ls_vin-market = ms_bonus_header-market.
      ls_vin-bonus_id = ms_bonus_header-bonus_id.

      INSERT zlpbns_vol_vin FROM ls_vin.
      IF sy-subrc <> 0.
        MESSAGE e004(zlpbns_bonus)
        WITH 'Vin exclusion' 'vin' ls_vin-vhvin INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_header ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_sale ASSIGNING FIELD-SYMBOL(<ls_sale>).
      CLEAR: ls_sale, lt_sale.
      ls_sale = CORRESPONDING #( <ls_sale> ).
      ls_sale-market = ms_bonus_header-market.
      ls_sale-bonus_id = ms_bonus_header-bonus_id.

      INSERT zlpbns_vol_stype FROM TABLE lt_sale.

      IF sy-subrc <> 0.
        MESSAGE e004(zlpbns_bonus)
        WITH 'Sale exclusion' 'sale' ls_sale-sell_type INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_header ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD update_entries.

    DATA: ls_vol_mdh  TYPE zlpbns_vol_mdh,
          ls_vol_mdht TYPE zlpbns_vol_mdht,
          ls_model    TYPE zlpbns_vol_model,
          ls_target   TYPE zlpbns_vol_trgt,
          ls_incent   TYPE zlpbns_vol_inc,
          ls_vin      TYPE zlpbns_vol_vin,
          ls_sale     TYPE zlpbns_vol_stype,
          lt_sale     TYPE STANDARD TABLE OF zlpbns_vol_stype.

    ls_vol_mdh = CORRESPONDING #( ms_bonus_header ).
    ls_vol_mdh-modname = ms_bonus_header-modname = sy-uname.
    ls_vol_mdh-moddate = ms_bonus_header-moddate = sy-datum.
    ls_vol_mdh-modtime = ms_bonus_header-modtime = sy-uzeit.

    ls_vol_mdht-market   = ms_bonus_header-market.
    ls_vol_mdht-bonus_id = ms_bonus_header-bonus_id.
    ls_vol_mdht-text     = ms_bonus_header-bonus_desc.
    ls_vol_mdht-spras    = sy-langu.

    UPDATE zlpbns_vol_mdh FROM ls_vol_mdh.
    IF sy-subrc <> 0.
      MESSAGE e018(zlpbns_bonus) WITH '' INTO DATA(lv_dummy).
      add_message_sys( CHANGING ct_messages = mt_messages_item ).
    ENDIF.

    UPDATE zlpbns_vol_mdht FROM ls_vol_mdht.
    IF sy-subrc <> 0.
      MESSAGE e018(zlpbns_bonus) WITH '' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_item ).
    ENDIF.

    LOOP AT mt_model ASSIGNING FIELD-SYMBOL(<ls_model>) WHERE saved IS INITIAL.
      CLEAR ls_model.
      ls_model = CORRESPONDING #( <ls_model> ).
      ls_model-market   = ms_bonus_header-market.
      ls_model-bonus_id = ms_bonus_header-bonus_id.

      INSERT zlpbns_vol_model FROM ls_model.
      IF sy-subrc <> 0.
        MESSAGE e007(zlpbns_bonus)
        WITH 'Model' 'material' ls_model-comm_model_code INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_model ASSIGNING FIELD-SYMBOL(<ls_deleted_model>).
      CLEAR ls_model.
      ls_model = CORRESPONDING #( <ls_deleted_model> ).
      ls_model-market   = ms_bonus_header-market.
      ls_model-bonus_id = ms_bonus_header-bonus_id.

      DELETE zlpbns_vol_model FROM ls_model.

      IF sy-subrc <> 0.
        MESSAGE e013(zlpbns_bonus)
        WITH 'Model' 'material' ls_model-comm_model_code INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_target ASSIGNING FIELD-SYMBOL(<ls_target>).
      CLEAR ls_target.
      ls_target = CORRESPONDING #( <ls_target> ).
      ls_target-market   = ms_bonus_header-market.
      ls_target-bonus_id = ms_bonus_header-bonus_id.

      MODIFY zlpbns_vol_trgt FROM ls_target.
      IF sy-subrc <> 0.
        MESSAGE e007(zlpbns_bonus)
        WITH 'Target' 'target' <ls_target>-target INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_target ASSIGNING FIELD-SYMBOL(<ls_deleted_target>).
      CLEAR ls_target.
      ls_target = CORRESPONDING #( <ls_deleted_target> ).
      ls_target-market   = ms_bonus_header-market.
      ls_target-bonus_id = ms_bonus_header-bonus_id.

      DELETE zlpbns_vol_trgt FROM ls_target.
      IF sy-subrc <> 0.
        MESSAGE e013(zlpbns_bonus)
        WITH 'Target' 'target' <ls_target>-target INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_incent ASSIGNING FIELD-SYMBOL(<ls_incent>).
      CLEAR ls_incent.
      ls_incent = CORRESPONDING #( <ls_incent> ).
      ls_incent-market   = ms_bonus_header-market.
      ls_incent-bonus_id = ms_bonus_header-bonus_id.

      MODIFY zlpbns_vol_inc FROM ls_incent.
      IF sy-subrc <> 0.
        MESSAGE e007(zlpbns_bonus)
        WITH 'Incentive' 'incentive' ls_incent-incentive INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_incent ASSIGNING FIELD-SYMBOL(<ls_deleted_incent>).
      CLEAR ls_incent.
      ls_incent = CORRESPONDING #( <ls_incent> ).
      ls_incent-market   = ms_bonus_header-market.
      ls_incent-bonus_id = ms_bonus_header-bonus_id.

      DELETE zlpbns_vol_inc FROM ls_incent.
      IF sy-subrc <> 0.
        MESSAGE e013(zlpbns_bonus)
        WITH 'Incentive' 'incentive' ls_incent-incentive INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_vin ASSIGNING FIELD-SYMBOL(<ls_vin>).
      CLEAR ls_vin.
      ls_vin = CORRESPONDING #( <ls_vin> ).
      ls_vin-market   = ms_bonus_header-market.
      ls_vin-bonus_id = ms_bonus_header-bonus_id.

      INSERT zlpbns_vol_vin FROM ls_vin.
      IF sy-subrc <> 0.
        MESSAGE e007(zlpbns_bonus)
        WITH 'Vin exclusion' 'vin' ls_vin-vhvin INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_vin ASSIGNING FIELD-SYMBOL(<ls_deleted_vin>).
      CLEAR ls_vin.
      ls_vin = CORRESPONDING #( <ls_vin> ).
      ls_vin-market   = ms_bonus_header-market.
      ls_vin-bonus_id = ms_bonus_header-bonus_id.

      DELETE zlpbns_vol_vin FROM ls_vin.
      IF sy-subrc <> 0.
        MESSAGE e013(zlpbns_bonus)
        WITH 'Vin exclusion' 'vin' ls_vin-vhvin INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_sale ASSIGNING FIELD-SYMBOL(<ls_sale>).
      CLEAR: ls_sale, lt_sale.
      ls_sale = CORRESPONDING #( <ls_sale> ).
      ls_sale-market    = ms_bonus_header-market.
      ls_sale-bonus_id  = ms_bonus_header-bonus_id.

      INSERT zlpbns_vol_stype FROM TABLE lt_sale.
      IF sy-subrc <> 0.
        MESSAGE e007(zlpbns_bonus)
        WITH 'Sale exclusion' 'sale' ls_sale-sell_type INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_sale ASSIGNING FIELD-SYMBOL(<ls_deleted_sale>).
      CLEAR: ls_sale, lt_sale.
      ls_sale = CORRESPONDING #( <ls_sale> ).
      ls_sale-market    = ms_bonus_header-market.
      ls_sale-bonus_id  = ms_bonus_header-bonus_id.

      DELETE zlpbns_vol_stype FROM TABLE lt_sale.

      IF sy-subrc <> 0.
        MESSAGE e013(zlpbns_bonus)
        WITH 'Sale exclusion' 'sale' ls_sale-sell_type INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD insert_row.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    DATA(lv_data_tab_name) = |MT_{ iv_tab }|.

    ASSIGN (lv_data_tab_name) TO <lt_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO <lt_table>.

    refresh_tabstrip_alv( ).

  ENDMETHOD.
  METHOD delete_rows.

    DATA: lo_line_type TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table>         TYPE STANDARD TABLE,
                   <lt_deleted_table> TYPE STANDARD TABLE.

    DATA(lv_data_tab_name) = |MT_{ iv_tab }|.
    DATA(lv_deleted_data_tab_name) = |MT_DELETED_{ iv_tab }|.
    DATA(lv_data_type) = |TY_{ iv_tab }|.

    ASSIGN (lv_data_tab_name) TO <lt_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_deleted_data_tab_name) TO <lt_deleted_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE DATA lo_line_type TYPE (lv_data_type).
    ASSIGN lo_line_type->* TO FIELD-SYMBOL(<ls_line>).

    LOOP AT it_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected_rows>).

      CLEAR <ls_line>.
      READ TABLE <lt_table> INTO <ls_line> INDEX <ls_selected_rows>-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT 'SAVED' OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_saved>).
      IF sy-subrc = 0 AND <lv_saved> = abap_true.
        APPEND <ls_line> TO <lt_deleted_table>. "Append only entries saved in DB to the deletion table
      ENDIF.

      DELETE <lt_table> INDEX <ls_selected_rows>-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    refresh_tabstrip_alv( ).
  ENDMETHOD.
  METHOD delete_all.

    DATA: lo_line_type TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table>         TYPE STANDARD TABLE,
                   <lt_deleted_table> TYPE STANDARD TABLE.

    DATA(lv_data_tab_name) = |MT_{ iv_tab }|.
    DATA(lv_deleted_data_tab_name) = |MT_DELETED_{ iv_tab }|.
    DATA(lv_data_type) = |TY_{ iv_tab }|.

    ASSIGN (lv_data_tab_name) TO <lt_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_deleted_data_tab_name) TO <lt_deleted_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE DATA lo_line_type TYPE (lv_data_type).
    ASSIGN lo_line_type->* TO FIELD-SYMBOL(<ls_line>).

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).

      ASSIGN COMPONENT 'SAVED' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<lv_saved>).
      IF sy-subrc = 0 AND <lv_saved> = abap_true.
        APPEND <ls_table> TO <lt_deleted_table>. "Append only entries saved in DB to the deletion table
      ENDIF.
    ENDLOOP.

    CLEAR <lt_table>.

    refresh_tabstrip_alv( ).
  ENDMETHOD.
  METHOD refresh_tabstrip_alv.

    FIELD-SYMBOLS: <lo_grid> TYPE REF TO cl_gui_alv_grid.

    DATA(lv_tabstrip_name) = g_ts_promo-pressed_tab+3.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    <lo_grid>->refresh_table_display(
      EXCEPTIONS
        finished       = 1                " Display was Ended (by Export)
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  METHOD add_message.

    APPEND VALUE #( msgid = 'ZLPBNS_BONUS'
                    msgty = iv_msgty
                    msgno = iv_msgno
                    msgv1 = iv_msgv1
                    msgv2 = iv_msgv2
                    msgv3 = iv_msgv3
                    msgv4 = iv_msgv4 ) TO ct_messages.

  ENDMETHOD.
  METHOD add_message_sys.

    APPEND VALUE #( msgid = sy-msgid
                    msgty = sy-msgty
                    msgno = sy-msgno
                    msgv1 = sy-msgv1
                    msgv2 = sy-msgv2
                    msgv3 = sy-msgv3
                    msgv4 = sy-msgv4 ) TO ct_messages.

  ENDMETHOD.
  METHOD check_header_data.

    CLEAR mt_messages_header.

    IF ms_bonus_header-market IS INITIAL.
      MESSAGE e001(zlpbns_bonus)
      WITH 'Market' INTO DATA(lv_dummy).
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    IF ms_bonus_header-bonus_id IS INITIAL.
      MESSAGE e001(zlpbns_bonus)
      WITH 'Bonus ID' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    IF ms_bonus_header-bonus_desc IS INITIAL.
      MESSAGE e001(zlpbns_bonus)
      WITH 'Bonus Desc' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    IF ms_bonus_header-count_period IS INITIAL.
      MESSAGE e001(zlpbns_bonus)
      WITH 'Count period' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    IF ms_bonus_header-count_rule IS INITIAL.
      MESSAGE e001(zlpbns_bonus)
      WITH 'Count rule' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    IF ms_bonus_header-zqrt_catch_up IS INITIAL.
      MESSAGE e001(zlpbns_bonus)
      WITH 'Catch up' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    IF ms_bonus_header-valid_from IS INITIAL.
      MESSAGE e001(zlpbns_bonus)
      WITH 'Valid from' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    IF ms_bonus_header-valid_to IS INITIAL.
      MESSAGE e001(zlpbns_bonus)
      WITH 'Valid to' INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages_header ).
    ENDIF.

    IF ms_bonus_header-valid_from IS NOT INITIAL AND
       ms_bonus_header-valid_to   IS NOT INITIAL.

      IF ms_bonus_header-valid_from > ms_bonus_header-valid_to.
        MESSAGE e002(zlpbns_bonus) WITH '' INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_header ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD check_item_data.

    DATA: lv_tabix TYPE sy-tabix.

    SELECT *
      FROM tcurx
      INTO TABLE @DATA(lt_tcurx).

    LOOP AT mt_model ASSIGNING FIELD-SYMBOL(<ls_model>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        IF <ls_model>-fuel_type IS INITIAL.
          MESSAGE w000(zlpbns_bonus)
          WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
          |'Model table errors'| INTO DATA(lv_dummy).
          add_message_sys( CHANGING ct_messages = mt_messages_item ).
        ENDIF.
      ENDIF.

      IF <ls_model> IS INITIAL.
        IF <ls_model>-fuel_type IS INITIAL.
          MESSAGE e010(zlpbns_bonus)
          WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
          INTO lv_dummy.
          add_message_sys( CHANGING ct_messages = mt_messages_item ).
        ENDIF.
        CONTINUE.
      ENDIF.

      IF <ls_model>-comm_model_code IS INITIAL.
        IF <ls_model>-fuel_type IS INITIAL.
          MESSAGE e008(zlpbns_bonus)
          WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
          |'Model'| INTO lv_dummy.
          add_message_sys( CHANGING ct_messages = mt_messages_item ).
        ENDIF.
      ENDIF.

      IF <ls_model>-fuel_type IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Fuel type'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    DATA(lt_model) = mt_model.
    SORT lt_model BY comm_model_code.

    LOOP AT lt_model ASSIGNING <ls_model>.
      DATA(lv_next_index) = sy-tabix + 1.
      READ TABLE lt_model ASSIGNING FIELD-SYMBOL(<ls_mod_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_mod_row>-comm_model_code = <ls_model>-comm_model_code.
        MESSAGE e014(zlpbns_bonus)
        WITH CONV syst_msgv( <ls_model>-comm_model_code ) INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lv_tabix.
    LOOP AT mt_target ASSIGNING FIELD-SYMBOL(<ls_target>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        MESSAGE w000(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Target table errors'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_target> IS INITIAL.
        MESSAGE e010(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        CONTINUE.
      ENDIF.

      IF <ls_target>-kunnr IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Customer'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.


      IF <ls_target>-zyear IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Year'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        CONTINUE.
      ENDIF.

      IF <ls_target>-zmonth_from IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Month from'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_target>-zmonth_to IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Month to'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_target>-target IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Target'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    DATA(lt_target) = mt_target.
    CLEAR lv_next_index.
    SORT lt_target BY kunnr zyear zmonth_from zmonth_to.

    LOOP AT lt_target ASSIGNING <ls_target>.
      lv_next_index = sy-tabix + 1.
      READ TABLE lt_target ASSIGNING FIELD-SYMBOL(<ls_chas_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_chas_row>-kunnr = <ls_target>-kunnr.
        MESSAGE e014(zlpbns_bonus)
        WITH CONV syst_msgv( |{ <ls_chas_row>-kunnr }|       && '|'
                          && |{ <ls_chas_row>-zyear }|       && '|'
                          && |{ <ls_chas_row>-zmonth_from }| && '|'
                          && |{ <ls_chas_row>-zmonth_to }|   && '|' )
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lv_tabix.
    LOOP AT mt_incent ASSIGNING FIELD-SYMBOL(<ls_incent>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        MESSAGE w000(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Incentives table errors'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_incent> IS INITIAL.
        MESSAGE e010(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        CONTINUE.
      ENDIF.

      IF <ls_incent>-tratio_min IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Tratio min'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_incent>-tratio_max IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Tratio max'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_incent>-comm_model_code IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Model'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_incent>-fuel_type IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Fuel type'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_incent>-sell_type IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Sell type'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_incent>-incentive_perc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF <ls_incent>-incentive IS INITIAL AND <ls_incent>-incentive_perc IS INITIAL.
        MESSAGE e009(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Incentive'| |'Incentive percentage'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        CONTINUE.
      ENDIF.

      IF <ls_incent>-incentive IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Incentive'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_incent>-zcurr IS INITIAL.
        MESSAGE e008(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'Currency'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      READ TABLE lt_tcurx TRANSPORTING NO FIELDS WITH KEY currkey = <ls_incent>-zcurr.

      IF sy-subrc <> 0.
        MESSAGE e011(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        |'{ <ls_incent>-zcurr }'| INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    DATA(lt_incent) = mt_incent.
    CLEAR lv_next_index.
    SORT lt_incent BY tratio_min tratio_max comm_model_code fuel_type sell_type.

    LOOP AT lt_incent ASSIGNING <ls_incent>.
      lv_next_index = sy-tabix + 1.
      READ TABLE lt_incent ASSIGNING FIELD-SYMBOL(<ls_incent_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF  <ls_incent>-tratio_min      = <ls_incent_row>-tratio_min
      AND <ls_incent>-tratio_max      = <ls_incent_row>-tratio_max
      AND <ls_incent>-comm_model_code = <ls_incent_row>-comm_model_code
      AND <ls_incent>-fuel_type       = <ls_incent_row>-fuel_type
      AND <ls_incent>-sell_type       = <ls_incent_row>-sell_type.
        .
        MESSAGE e014(zlpbns_bonus)
        WITH CONV syst_msgv( |{ <ls_incent>-tratio_min }|      && '|'
                          && |{ <ls_incent>-tratio_max }|      && '|'
                          && |{ <ls_incent>-comm_model_code }| && '|'
                          && |{ <ls_incent>-fuel_type }|       && '|'
                          && |{ <ls_incent>-sell_type }|       && '|' )
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lv_tabix.
    LOOP AT mt_vin ASSIGNING FIELD-SYMBOL(<ls_vin>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        MESSAGE w000(zlpbns_bonus)
        WITH |'Vin exclusion table errors'|
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_vin> IS INITIAL.
        MESSAGE e010(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    DATA(lt_vin) = mt_vin.
    CLEAR lv_next_index.
    SORT lt_vin BY vhvin.

    LOOP AT lt_vin ASSIGNING <ls_vin>.
      lv_next_index = sy-tabix + 1.
      READ TABLE lt_vin ASSIGNING FIELD-SYMBOL(<ls_vin_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_vin>-vhvin = <ls_vin_row>-vhvin.
        MESSAGE e014(zlpbns_bonus)
        WITH CONV syst_msgv( <ls_vin>-vhvin )
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lv_tabix.
    LOOP AT mt_sale ASSIGNING FIELD-SYMBOL(<ls_sale>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        MESSAGE w000(zlpbns_bonus)
        WITH |'Sales exclusion table errors'|
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.

      IF <ls_sale> IS INITIAL.
        MESSAGE e010(zlpbns_bonus)
        WITH condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
        INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
      ENDIF.
    ENDLOOP.

    DATA(lt_sale) = mt_sale.
    CLEAR lv_next_index.
    SORT lt_sale BY sell_type.

    LOOP AT lt_sale ASSIGNING <ls_sale>.
      lv_next_index = sy-tabix + 1.

      READ TABLE lt_sale ASSIGNING FIELD-SYMBOL(<ls_sale_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_sale>-sell_type = <ls_sale_row>-sell_type.
        MESSAGE e014(zlpbns_bonus)
        WITH CONV syst_msgv( <ls_sale>-sell_type ) INTO lv_dummy.
        add_message_sys( CHANGING ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD display_messages.

    CLEAR mt_messages.

    IF mt_messages_header IS NOT INITIAL.
      MESSAGE w000(zlpbns_bonus)
      WITH CONV syst_msgv( 'Header messages' ) INTO DATA(lv_dummy).
      add_message_sys( CHANGING ct_messages = mt_messages ).

      APPEND LINES OF mt_messages_header TO mt_messages.
      CLEAR mt_messages_header.
    ENDIF.

    LOOP AT mt_messages_item TRANSPORTING NO FIELDS WHERE msgty CA 'E'.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      MESSAGE w000(zlpbns_bonus)
      WITH CONV syst_msgv( 'Item messages' ) INTO lv_dummy.
      add_message_sys( CHANGING ct_messages = mt_messages ).
      APPEND LINES OF mt_messages_item TO mt_messages.
      CLEAR mt_messages_item.
    ENDIF.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = mt_messages.

  ENDMETHOD.
  METHOD clear_bonus.

    IF iv_clear_header IS NOT INITIAL.
      CLEAR ms_bonus_header.
    ENDIF.

    CLEAR: mt_model,
           mt_incent,
           mt_target,
           mt_vin,
           mt_sale,
           mt_deleted_model,
           mt_deleted_target,
           mt_deleted_incent,
           mt_deleted_vin,
           mt_deleted_sale,
           mt_messages,
           mt_messages_header,
           mt_messages_item,
           mv_last_tab.
  ENDMETHOD.
  METHOD fill_tables.

    SELECT m~comm_model_code,
           c~comm_model_code_descr,
           m~fuel_type,
           d~ddtext AS fuel_type_desc,
           @abap_true AS saved
      FROM zlpbns_vol_model AS m
      LEFT JOIN zlmiv_pc_link_cm AS c ON c~comm_model_code = m~comm_model_code
      LEFT JOIN dd07t AS d ON d~domvalue_l = m~fuel_type
        AND d~domname  = 'TESTTTTT'
      INTO CORRESPONDING FIELDS OF TABLE @mt_model
      WHERE m~bonus_id = @ms_bonus_header-bonus_id
        AND m~market   = @ms_bonus_header-market.

    SELECT t~kunnr,
           k~name1,
           t~zyear,
           t~zmonth_from,
           t~zmonth_to,
           t~target,
           @abap_true AS saved
      FROM zlpbns_vol_trgt AS t
      LEFT JOIN kna1 AS k ON k~kunnr = t~kunnr
      INTO CORRESPONDING FIELDS OF TABLE @mt_target
      WHERE t~bonus_id = @ms_bonus_header-bonus_id
        AND t~market = @ms_bonus_header-market.

    SELECT i~tratio_min,
           i~tratio_max,
           i~comm_model_code,
           c~comm_model_code_descr,
           i~fuel_type,
           d~ddtext AS fuel_type_desc,
           i~sell_type,
           i~incentive_perc,
           i~incentive,
           i~zcurr,
           @abap_true AS saved
     FROM zlpbns_vol_inc AS i
      LEFT JOIN zlmiv_pc_link_cm AS c ON c~comm_model_code = i~comm_model_code
      LEFT JOIN dd07t AS d ON d~domvalue_l = i~fuel_type
        AND d~domname  = 'TESTTTTT'
     INTO CORRESPONDING FIELDS OF TABLE @mt_incent
      WHERE i~bonus_id = @ms_bonus_header-bonus_id
        AND i~market = @ms_bonus_header-market.

    SELECT v~vhvin,
           v~excl_type,
           d~ddtext AS excl_type_descr,
           @abap_true AS saved
      FROM zlpbns_vol_vin AS v
      LEFT JOIN dd07t AS d ON d~domvalue_l = v~excl_type
        AND d~domname = 'ZRSTDK_ZEXCMOD'
        AND d~ddlanguage = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @mt_vin
      WHERE v~bonus_id = @ms_bonus_header-bonus_id
        AND v~market = @ms_bonus_header-market.

    SELECT s~sell_type,
           s~excl_type,
           d~ddtext AS excl_type_descr,
           @abap_true AS saved
      FROM zlpbns_vol_stype AS s
      LEFT JOIN dd07t AS d ON d~domvalue_l = s~excl_type
        AND d~domname = 'ZRSTDK_ZEXCMOD'
        AND d~ddlanguage = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @mt_sale
      WHERE s~bonus_id = @ms_bonus_header-bonus_id
        AND s~market = @ms_bonus_header-market.

  ENDMETHOD.

  METHOD popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = iv_text
        text_button_1         = 'Yes'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'No'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ENDMETHOD.
ENDCLASS.

DATA: go_bonus           TYPE REF TO lcl_bonus.

CLASS lcl_zlpbns_bonus_md DEFINITION.
  PUBLIC SECTION.

    DATA: mv_ok_code         TYPE sy-ucomm.

    CONSTANTS: mc_header_table(30) TYPE c VALUE 'ZLPBNS_VOL_MDH'.

    METHODS execute.
    METHODS pai.
    METHODS pbo_0100.
    METHODS adjust_data.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_model,
             market        TYPE zlpbns_promo_mdh-market,
             promo         TYPE zlpbns_promo_mdh-promo,
             matnr         TYPE zlpbns_pr_model-matnr,
             descr_veicolo TYPE zrsdap_psa_pc016-descr_veicolo,
             perc_am       TYPE zlpbns_pr_model-perc_am,
             amount        TYPE zlpbns_pr_model-amount,
             curr          TYPE zlpbns_pr_model-curr,
           END OF ty_model.

    TYPES: BEGIN OF ty_vin,
             market           TYPE zlpbns_promo_mdh-market,
             promo            TYPE zlpbns_promo_mdh-promo,
             vhvin            TYPE zlpbns_pr_vin-vhvin,
             zmmw0_descr_veic TYPE zmmw0_descr_veic,
             perc_am          TYPE zlpbns_pr_model-perc_am,
             amount           TYPE zlpbns_pr_model-amount,
             curr             TYPE zlpbns_pr_model-curr,
           END OF ty_vin.

    DATA: mo_salv_bonus      TYPE REF TO cl_salv_table,
          mo_bonus_container TYPE REF TO cl_gui_custom_container,
          mt_bonus           TYPE STANDARD TABLE OF lcl_bonus=>ty_bonus.

    DATA: mo_salv_model      TYPE REF TO cl_salv_table,
          mo_model_list_cont TYPE REF TO cl_gui_custom_container,
          mt_model           TYPE STANDARD TABLE OF ty_model.

    DATA: mo_salv_vin      TYPE REF TO cl_salv_table,
          mo_vin_container TYPE REF TO cl_gui_custom_container,
          mt_vin           TYPE STANDARD TABLE OF ty_vin.

    CONSTANTS: BEGIN OF mc_rc,
                 no_data    TYPE i VALUE 1, " No data was found
                 select_one TYPE i VALUE 2, " Select only one row
               END OF mc_rc.

    METHODS get_data.
    METHODS display_alv.
    METHODS salv_functions.
    METHODS salv_events.
    METHODS salv_fieldcatalog.

    METHODS new_bonus.
    METHODS view_bonus IMPORTING it_selected_rows  TYPE salv_t_row.

    METHODS create_tabstrip_text_table.
    METHODS get_status_text.
    METHODS append_master_data IMPORTING is_bonus TYPE lcl_bonus=>ty_bonus.

    METHODS on_user_command FOR EVENT added_function OF cl_salv_events_table IMPORTING e_salv_function.
    METHODS on_hotspot FOR EVENT link_click OF cl_salv_events_table IMPORTING column row.

ENDCLASS.

CLASS lcl_zlpbns_bonus_md IMPLEMENTATION.
  METHOD execute.

    create_tabstrip_text_table( ).

    get_status_text( ).

    get_data( ).

    adjust_data( ).

    display_alv( ).

    CALL SCREEN 100.
  ENDMETHOD.
  METHOD get_data.

    CONSTANTS: lc_bcateg TYPE string VALUE 'ZBCATEGORY',
               lc_period TYPE string VALUE 'ZLPBNS_COUNT_PERIOD',
               lc_rule   TYPE string VALUE 'ZCOUNT_RULE'.

    SELECT md~market,
           md~bonus_id,
           t~text AS bonus_descr,
           md~bcategory,
           md~valid_from,
           md~valid_to,
           md~count_period,
           md~count_rule,
           md~zqrt_catch_up,
           md~zdele,
           md~credate,
           md~cretime,
           md~crename,
           md~moddate,
           md~modtime,
           md~modname
      FROM zlpbns_vol_mdh AS md
      LEFT JOIN zlpbns_vol_mdht AS t ON t~bonus_id = md~bonus_id
      INTO TABLE @DATA(lt_bonus_temp)
      WHERE md~market         = @p_markt
        AND md~valid_from    IN @s_valfrm
        AND md~valid_to      IN @s_valto
        AND md~zdele         IN @s_zdele
        AND md~zqrt_catch_up IN @s_zqrtcu
        AND md~bonus_id      IN @s_bonus
        AND md~count_rule    IN @s_cntrul
        AND md~count_period  IN @s_cntper
        AND md~credate       IN @s_creon.

    SELECT domname,
           domvalue_l,
           ddtext
      FROM dd07t
      INTO TABLE @DATA(lt_domvalues)
      WHERE domname IN ( @lc_bcateg, @lc_period, @lc_rule )
      AND ddlanguage = @sy-langu
      ORDER BY domname, domvalue_l.

    LOOP AT lt_bonus_temp ASSIGNING FIELD-SYMBOL(<ls_bonus_temp>).

      APPEND INITIAL LINE TO mt_bonus ASSIGNING FIELD-SYMBOL(<ls_bonus>).
      <ls_bonus> = CORRESPONDING #( <ls_bonus_temp> ).
      <ls_bonus>-vkorg = p_vkorg.
      <ls_bonus>-bonus_desc = <ls_bonus_temp>-bonus_descr.

      DATA(lv_bonus_id) = EXACT zbcategory( |{ <ls_bonus_temp>-bcategory ALPHA = IN }| ).

      READ TABLE lt_domvalues ASSIGNING FIELD-SYMBOL(<ls_domvalue>) WITH KEY domname = lc_bcateg domvalue_l  = lv_bonus_id BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_bonus>-bcategory_desc = <ls_domvalue>-ddtext.
      ENDIF.

      READ TABLE lt_domvalues ASSIGNING <ls_domvalue> WITH KEY domname = lc_period domvalue_l  = <ls_bonus_temp>-count_period BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_bonus>-count_period_desc = <ls_domvalue>-ddtext.
      ENDIF.

      READ TABLE lt_domvalues ASSIGNING <ls_domvalue> WITH KEY domname = lc_rule domvalue_l  = <ls_bonus_temp>-count_rule BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_bonus>-count_rule_desc = <ls_domvalue>-ddtext.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD display_alv.

    IF mo_bonus_container IS BOUND.
      RETURN.
    ENDIF.

    mo_bonus_container = NEW cl_gui_custom_container( container_name = 'BONUS_CONTAINER' ).

    TRY.
        cl_salv_table=>factory(
      EXPORTING
        r_container    = mo_bonus_container
          IMPORTING
            r_salv_table   = mo_salv_bonus
          CHANGING
            t_table        = mt_bonus
        ).
      CATCH cx_salv_msg.
    ENDTRY.

    DATA(lo_layout) = mo_salv_bonus->get_layout( ).
    DATA(lv_key) = VALUE salv_s_layout_key( report = sy-repid ).

    lo_layout->set_initial_layout( p_vari ).
    lo_layout->set_key( lv_key ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    mo_salv_bonus->get_selections( )->set_selection_mode(
        value = if_salv_c_selection_mode=>row_column
    ).

    salv_functions( ).
    salv_events( ).
    salv_fieldcatalog( ).
    mo_salv_bonus->display( ).
  ENDMETHOD.
  METHOD pai.

    CASE sy-ucomm.
      WHEN 'FC_BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'FC_EXIT'.
        LEAVE PROGRAM.
    ENDCASE.

  ENDMETHOD.
  METHOD on_user_command.

    DATA: lt_selected_data TYPE lcl_bonus=>tt_bonus.
    DATA(lt_selected_rows) = mo_salv_bonus->get_selections( )->get_selected_rows( ).

    LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<lv_row>).
      READ TABLE mt_bonus ASSIGNING FIELD-SYMBOL(<ls_promo>) INDEX <lv_row>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      APPEND <ls_promo> TO lt_selected_data.
    ENDLOOP.

    IF lt_selected_data IS INITIAL AND e_salv_function <> 'NEW'.
      MESSAGE TEXT-e02 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE e_salv_function.
      WHEN 'VIEW'.
        IF lines( lt_selected_data ) <> 1.
          MESSAGE TEXT-e05 TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    CASE e_salv_function.
      WHEN 'NEW'.
        new_bonus( ).
      WHEN 'VIEW'.
        view_bonus( lt_selected_rows ).
    ENDCASE.

    mo_salv_bonus->refresh( ).
  ENDMETHOD.
  METHOD pbo_0100.

  ENDMETHOD.

  METHOD salv_functions.

    DATA(lo_functions) = mo_salv_bonus->get_functions( ).
    lo_functions->set_all( ).

    TRY.

        lo_functions->add_function(
          EXPORTING
            name     = 'NEW'
            icon     = CONV string( icon_create )
            text     = 'New'
            tooltip  = 'Create new bonus'
            position = if_salv_c_function_position=>right_of_salv_functions ).

        lo_functions->add_function(
          EXPORTING
            name     = 'VIEW'
            icon     = CONV string( icon_display )
            text     = 'Show ID'
            tooltip  = 'Show ID'
            position = if_salv_c_function_position=>right_of_salv_functions ).

      CATCH cx_salv_existing.
      CATCH cx_salv_wrong_call.
    ENDTRY.
  ENDMETHOD.
  METHOD salv_events.

    DATA(lo_events) = mo_salv_bonus->get_event( ).

    SET HANDLER on_user_command FOR lo_events.
    SET HANDLER on_hotspot FOR lo_events.
  ENDMETHOD.
  METHOD salv_fieldcatalog.

    DATA: lo_column TYPE REF TO cl_salv_column_table.
    DATA(lo_columns) = mo_salv_bonus->get_columns( ).

    lo_columns->set_optimize( ).

    TRY.
        lo_column ?= lo_columns->get_column( 'ICON' ).
        lo_column->set_icon( ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_column->set_long_text('View Bonus').

        lo_column ?= lo_columns->get_column( 'VKORG' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'MARKET' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'BONUS_ID' ).
        lo_column->set_key( ).
        lo_column->set_icon( ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_root INTO DATA(lx_root).

    ENDTRY.
  ENDMETHOD.
  METHOD new_bonus.

    DATA: ls_bonus TYPE lcl_bonus=>ty_bonus,
          lv_saved TYPE abap_bool.

    IF go_bonus IS NOT BOUND.
      go_bonus = NEW lcl_bonus( ).
    ENDIF.

    go_bonus->execute( iv_state = gc_state-new ).

    CALL SCREEN 200.

    go_bonus->get_result(
      IMPORTING
        es_bonus = ls_bonus
        ev_saved = lv_saved ).

    CLEAR mt_bonus.
    get_data( ).
    adjust_data( ).
    mo_salv_bonus->refresh( ).

  ENDMETHOD.
  METHOD view_bonus.

    DATA: ls_bonus TYPE lcl_bonus=>ty_bonus,
          lv_saved TYPE abap_bool.

    READ TABLE it_selected_rows ASSIGNING FIELD-SYMBOL(<lv_row>) INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE mt_bonus ASSIGNING FIELD-SYMBOL(<ls_bonus>) INDEX <lv_row>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF go_bonus IS NOT BOUND.
      go_bonus = NEW lcl_bonus( ).
    ENDIF.
    go_bonus->execute( iv_state = gc_state-view
                    is_bonus = <ls_bonus> ).

    CALL SCREEN 200.

    go_bonus->get_result(
      IMPORTING
        es_bonus = ls_bonus
        ev_saved = lv_saved ).

    CLEAR mt_bonus.
    get_data( ).
    adjust_data( ).
    mo_salv_bonus->refresh( ).

  ENDMETHOD.
  METHOD create_tabstrip_text_table.

    DATA: lv_empty TYPE char24,
          lv_full  TYPE char24.

    WRITE icon_wf_workitem_cancel AS ICON TO lv_empty.
    WRITE icon_wf_workitem_completed AS ICON TO lv_full.
    gt_tabstrip_header = VALUE #( ( name = 'TS_BONUS_TAB1' data_tab = 'MT_MODEL'  empty = |{ lv_empty } Model|             full = |{ lv_full } Model| )
                                  ( name = 'TS_BONUS_TAB2' data_tab = 'MT_TARGET' empty = |{ lv_empty } Target|            full = |{ lv_full } Target| )
                                  ( name = 'TS_BONUS_TAB3' data_tab = 'MT_INCENT' empty = |{ lv_empty } Incentive|         full = |{ lv_full } Incentive| )
                                  ( name = 'TS_BONUS_TAB4' data_tab = 'MT_VIN'    empty = |{ lv_empty } Vehicle exclusion| full = |{ lv_full } Vehicle exclusion| )
                                  ( name = 'TS_BONUS_TAB5' data_tab = 'MT_SALE'   empty = |{ lv_empty } Sale exclusion|    full = |{ lv_full } Sale exclusion| ) ).
  ENDMETHOD.
  METHOD append_master_data.

    APPEND INITIAL LINE TO mt_bonus ASSIGNING FIELD-SYMBOL(<ls_bonus>).

    <ls_bonus> = CORRESPONDING #( is_bonus ).

    IF <ls_bonus>-zdele = abap_true.
      <ls_bonus>-icon = icon_enter_more.
    ELSE.
      <ls_bonus>-icon = icon_display_more.
    ENDIF.

  ENDMETHOD.
  METHOD adjust_data.

    LOOP AT mt_bonus ASSIGNING FIELD-SYMBOL(<ls_bonus>).
      IF <ls_bonus>-zdele = abap_true.
        <ls_bonus>-icon = icon_enter_more.
      ELSE.
        <ls_bonus>-icon = icon_display_more.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD on_hotspot.

    READ TABLE mt_bonus ASSIGNING FIELD-SYMBOL(<ls_promo>) INDEX row.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_selected_rows) = mo_salv_bonus->get_selections( )->get_selected_rows( ).
    APPEND row TO lt_selected_rows.

    CASE column.
      WHEN 'ICON'.
        view_bonus( lt_selected_rows ).
      WHEN 'BONUS_ID'.
        view_bonus( lt_selected_rows ).
    ENDCASE.
    mo_salv_bonus->refresh( ).
  ENDMETHOD.
  METHOD get_status_text.
    SELECT status,
           status_desc
      FROM zlpbns_pr_stat_t
      INTO CORRESPONDING FIELDS OF TABLE @gt_status_text
      WHERE spras = @sy-langu.
  ENDMETHOD.
ENDCLASS.

DATA: go_zlpbns_bonus_md TYPE REF TO lcl_zlpbns_bonus_md.

START-OF-SELECTION.

  go_zlpbns_bonus_md = NEW lcl_zlpbns_bonus_md( ).

  IF go_zlpbns_bonus_md IS BOUND.
    go_zlpbns_bonus_md->execute( ).
  ENDIF.
  INCLUDE zlpbns_vol_dash_md_0100m01.
