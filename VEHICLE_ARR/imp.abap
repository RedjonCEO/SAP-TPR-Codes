*&---------------------------------------------------------------------*
*&  Include           ZIV_HMC_VEHICLE_ARR_IMP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_class_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_vehicle_arr IMPLEMENTATION.

  METHOD execute.

    start_container( ).
    display_vehicles( ).

    IF mt_vehicle_tab IS INITIAL.
      MESSAGE 'No data found'(003) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL SCREEN 100.
  ENDMETHOD.                    "execute


  METHOD on_data_changed.
    FIELD-SYMBOLS: <lt_data> TYPE tt_vehicle_tab.
    ASSIGN er_data_changed->mp_mod_rows->* TO <lt_data>.
  ENDMETHOD.                    "on_data_changed


  METHOD extract_data.

    SELECT   h~zbatch
             h~zprod_slot
             h~zdest_port
             v~vhcle
             v~vhsar
             v~vhusg
             v~kunnr
             v~endcu
             v~zdcr01
             t~zhmc_spec_code
             t~zhmc_color
             t~zhmc_tyres
             v~vhvin
             t~zengine
             t~zinvoice
             t~zinvoice_date
             t~zvessel
             t~zsailing
             v~loctn
             a~name1 AS loctn_desc
             t~zspec_tyres
             v~mmsta
             p~ebeln
             p~netpr
             p~loekz
             b~belnr
             v~zda01
             c~statut AS mmsta_desc
             v~cuobj
     FROM ziv_hmc_batch_hd AS h
      JOIN ziv_hmc_batch_it AS t ON t~zbatch = h~zbatch AND t~zdeleted = space
      JOIN vlcvehicle  AS v ON v~vhcle = t~vhcle
      LEFT JOIN ekpo        AS p ON p~ebeln = t~ebeln
      LEFT JOIN ekbe        AS b ON b~ebeln = t~ebeln
      LEFT JOIN cvlc02t     AS c ON c~statu = v~mmsta   AND c~spras    = sy-langu
      LEFT JOIN adrc        AS a ON a~addrnumber = v~loctn
      INTO CORRESPONDING FIELDS OF TABLE mt_vehicle_tab
      WHERE h~zbatch        IN s_batch
      AND   h~zprod_slot    IN s_prdslt
      AND   v~mmsta         IN s_mmsta
      AND   h~zdest_port    IN s_dstprt
      AND   v~vhvin         IN s_vhvin
      AND   v~vhcle         IN s_vhcle.

    SELECT * FROM cvlc02t
      INTO TABLE mt_cvlc02t
      FOR ALL ENTRIES IN mt_vehicle_tab
      WHERE statu = mt_vehicle_tab-mmsta
      AND   spras = sy-langu.

    change_edit_status( ).

  ENDMETHOD.                    "extract_data


  METHOD change_edit_status.

    DATA: ls_cvlc02t TYPE cvlc02t,
          ls_style TYPE lvc_s_styl.

    FIELD-SYMBOLS: <ls_vehicles> TYPE ty_vehicle,
                   <ls_style>    TYPE lvc_s_styl.

    mo_grid->check_changed_data( ).

    LOOP AT mt_vehicle_tab ASSIGNING <ls_vehicles>.

      IF <ls_vehicles>-loekz IS NOT INITIAL.

        <ls_vehicles>-blocked   = abap_true.
        <ls_vehicles>-elab_icon = ms_technical-icon_wrong_status.
        <ls_vehicles>-elab_msg  = 'PO deleted, action not possible'(007).
        DELETE <ls_vehicles>-style WHERE fieldname = 'CHECKBOX'.

      ELSEIF ( <ls_vehicles>-mmsta = 'ZM50' ).

        <ls_vehicles>-blocked   = abap_true.
        <ls_vehicles>-elab_icon = ms_technical-icon_correct_status.
        <ls_vehicles>-elab_msg  = 'Action completed'(008).
        DELETE <ls_vehicles>-style WHERE fieldname = 'CHECKBOX'.

      ELSEIF ( <ls_vehicles>-ebeln IS NOT INITIAL ) AND
             ( <ls_vehicles>-belnr IS INITIAL ).

        <ls_vehicles>-blocked   = abap_true.
        <ls_vehicles>-elab_icon = ms_technical-icon_wrong_status.
        <ls_vehicles>-elab_msg  = 'GR missing, action not possible'(004).
        DELETE <ls_vehicles>-style WHERE fieldname = 'CHECKBOX'.

      ELSEIF ( <ls_vehicles>-belnr IS INITIAL ) AND
             ( <ls_vehicles>-ebeln IS INITIAL ).

        <ls_vehicles>-blocked   = abap_true.
        <ls_vehicles>-elab_icon = ms_technical-icon_wrong_status.
        <ls_vehicles>-elab_msg  = 'PO missing, action not possible'(005).
        DELETE <ls_vehicles>-style WHERE fieldname = 'CHECKBOX'.

      ELSE.

        <ls_vehicles>-blocked    = abap_false.
        <ls_vehicles>-elab_icon  = ms_technical-icon_correct_status.
        <ls_vehicles>-elab_msg   = 'Action possible'(006).
        READ TABLE <ls_vehicles>-style ASSIGNING <ls_style> WITH  KEY fieldname = 'CHECKBOX'.

        IF sy-subrc <> 0.
          ls_style-fieldname       = 'CHECKBOX'.
          ls_style-style           = cl_gui_alv_grid=>mc_style_enabled.
          APPEND ls_style TO  <ls_vehicles>-style .
          CLEAR ls_style.
        ENDIF.
      ENDIF.

      READ TABLE mt_cvlc02t INTO ls_cvlc02t WITH  KEY statu = <ls_vehicles>-mmsta.
      <ls_vehicles>-mmsta_desc = ls_cvlc02t-statut.

      CLEAR ls_cvlc02t.

    ENDLOOP.

  ENDMETHOD.                    "change_edit_status


  METHOD start_container.

    DATA:  lo_cont   TYPE REF TO cl_gui_custom_container.

    IF mo_grid IS BOUND.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_cont
      EXPORTING
        container_name              = 'CUSTOM_CONTAINER'
        repid                       = sy-repid
        dynnr                       = '0100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT mo_split_col
      EXPORTING
        parent            = lo_cont
        rows              = 2
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    mo_split_col->set_row_sash( id    = 1
                                   type  = cl_gui_splitter_container=>type_sashvisible
                                   value = cl_gui_splitter_container=>false ).

    mo_split_col->set_row_height(
      EXPORTING
        id                = 1
        height             = 5
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    CREATE OBJECT mo_split_row
      EXPORTING
        parent            = mo_split_col->get_container( row = 2 column = 1 )
        rows              = 1
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    set_logs_visibility( cl_gui_splitter_container=>false ).
    mo_top_cnt = mo_split_col->get_container( row = 1 column = 1 ).

    CREATE OBJECT mo_top
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent          = mo_split_row->get_container( row = 1 column = 1 )
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "start_container



  METHOD handle_toolbar.
    DATA: ls_toolbar TYPE stb_button.

    LOOP AT e_object->mt_toolbar INTO ls_toolbar.
      CASE ls_toolbar-function.
        WHEN '&LOCAL&CUT'
          OR '&LOCAL&APPEND'
          OR '&LOCAL&INSERT_ROW'
          OR '&LOCAL&DELETE_ROW'
          OR '&LOCAL&PASTE'
          OR '&INFO'
          OR '&CHECK'
          OR '&REFRESH'
          OR '&GRAPH'
          OR '&LOCAL&COPY'
          OR '&LOCAL&COPY_ROW'
          OR '&LOCAL&UNDO'.

          DELETE e_object->mt_toolbar.

          CONTINUE.
      ENDCASE.
    ENDLOOP.

    IF mv_selected = abap_true.
      ls_toolbar-quickinfo  = 'Deselect all'.
      ls_toolbar-text       = 'Deselect all'.
      ls_toolbar-icon       = icon_deselect_all.
    ELSE.
      ls_toolbar-quickinfo  = 'Select all'.
      ls_toolbar-text       = 'Select all'.
      ls_toolbar-icon       = icon_select_all.
    ENDIF.

    ls_toolbar-function   = 'FC_CHECK'.
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 1.

    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar .
    CLEAR ls_toolbar.

    ls_toolbar-function   = 'FC_IMPORT'.
    ls_toolbar-text       = 'Import'.
    ls_toolbar-icon       = icon_import.
    ls_toolbar-quickinfo  = 'Gate Release'.
    APPEND ls_toolbar TO e_object->mt_toolbar.



  ENDMETHOD.    "handle_toolbar

  METHOD handle_user_command.

    DATA: ls_vehicles    LIKE LINE OF mt_vehicle_tab,
          lv_no_selected TYPE abap_bool VALUE abap_false.

    CLEAR mt_logs.

    mo_grid->check_changed_data( ).

    READ TABLE mt_vehicle_tab TRANSPORTING NO FIELDS WITH KEY checkbox = abap_true.
    IF sy-subrc <> 0.
      lv_no_selected = abap_true.
    ENDIF.

    CASE e_ucomm.

      WHEN 'FC_CHECK'.

        IF mv_selected = abap_true.
          ls_vehicles-checkbox = abap_false.
          mv_selected = abap_false.
        ELSE.
          ls_vehicles-checkbox = abap_true.
          mv_selected = abap_true.
        ENDIF.
        MODIFY mt_vehicle_tab FROM ls_vehicles TRANSPORTING checkbox WHERE  mmsta = 'ZM49' AND blocked = abap_false .

        IF sy-subrc <> 0.
          MESSAGE s039(ziv_hmc) DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        mo_grid->refresh_table_display( ).

      WHEN 'FC_IMPORT'.

        IF lv_no_selected = abap_true.
          MESSAGE s038(ziv_hmc) DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        modify_vehicle_data( ).
        change_edit_status( ).

    ENDCASE.

    mo_grid->refresh_table_display( ).

  ENDMETHOD.                    "handle_user_command



  METHOD modify_vehicle_data.

    TYPES: BEGIN OF ty_par_loc,
           zdest_port TYPE ziv_hmc_par_loc-zdest_port,
           loctn      TYPE ziv_hmc_par_loc-loctn,
           END OF ty_par_loc.

    TYPES: BEGIN OF ty_mapping,
           action          TYPE vlc_action,
           spec_tyres      TYPE ziv_hmc_batch_it-zspec_tyres,
           sd_i0100        TYPE cux_value,
           sd_ba100        TYPE cux_value,
           END OF ty_mapping.

    DATA: lt_par_loc      TYPE STANDARD TABLE OF ty_par_loc,
          ls_mapping      TYPE  ty_mapping,
          ls_bapicucfg    TYPE bapicucfg,
          lt_bapicuins    TYPE STANDARD TABLE OF bapicuins,
          lt_bapicuprt    TYPE STANDARD TABLE OF bapicuprt,
          lt_bapicuval    TYPE STANDARD TABLE OF bapicuval,
          ls_bapicuval    TYPE  bapicuval,
          lv_actenh       TYPE zactenh,
          lt_vhcle        TYPE ziv_hmc_vhcle_tt,
          ls_bapicucfg_es TYPE bapicucfg,
          lv_cuobj        TYPE cuobj,
          ls_messages     TYPE ty_logs,
          lv_new_cuobj    TYPE inob-cuobj,
          lv_objectid     TYPE inob-obtab,
          lv_object       TYPE inob-objek,
          lv_new(1)       TYPE c.

    FIELD-SYMBOLS: <ls_vehicle> TYPE ty_vehicle,
                   <ls_par_loc> TYPE ty_par_loc.

    zcl_iv_hmc_utilities=>init_mapping(
      iv_progname = sy-cprog ).

    zcl_iv_hmc_utilities=>map_values(
      EXPORTING
        iv_structure = 'VLCVEHICLE'
      CHANGING
        cs_map       = ls_mapping
    ).

    SELECT zdest_port
           loctn
    FROM ziv_hmc_par_loc
    INTO TABLE lt_par_loc
    FOR ALL ENTRIES IN mt_vehicle_tab
    WHERE zdest_port = mt_vehicle_tab-zdest_port
    AND action = ls_mapping-action.


    LOOP AT mt_vehicle_tab ASSIGNING <ls_vehicle> WHERE checkbox = abap_true.
      READ TABLE lt_par_loc ASSIGNING <ls_par_loc> WITH KEY zdest_port = <ls_vehicle>-zdest_port.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      <ls_vehicle>-loctn  = <ls_par_loc>-loctn.

      UPDATE vlcvehicle  SET   loctn   = <ls_par_loc>-loctn
                               zda01   = sy-datum
                         WHERE vhcle   = <ls_vehicle>-vhcle.

      CALL FUNCTION 'ZSD_CHCK_ACT_ENH'
        EXPORTING
          cod_enh  = 'HMC_SPEC_TYRES'
        IMPORTING
          flag_act = lv_actenh.

      IF lv_actenh = abap_true.

        CALL FUNCTION 'VELO03_GET_CONFIGURATION'
          EXPORTING
            cuobj_iv                 = <ls_vehicle>-cuobj
          IMPORTING
            bapicucfg_es             = ls_bapicucfg
          TABLES
            bapicuins_et             = lt_bapicuins
            bapicuprt_et             = lt_bapicuprt
            bapicuval_et             = lt_bapicuval
          EXCEPTIONS
            conf_not_found           = 1
            conversion_not_performed = 2
            OTHERS                   = 3.

        IF sy-subrc <> 0.

          ls_messages-status  = ms_technical-icon_wrong_status.
          ls_messages-vhcle   = <ls_vehicle>-vhcle.
          ls_messages-message = 'Configorations not found'(013).
          ls_messages-msg_no  = 1.
          APPEND ls_messages TO mt_logs.
          CLEAR ls_messages.

        ELSE.

          ls_bapicuval-value = ls_mapping-sd_ba100.
          MODIFY lt_bapicuval FROM ls_bapicuval TRANSPORTING value WHERE charc = 'SD_BA100'.

          ls_bapicuval-value     = ls_mapping-sd_i0100.
          ls_bapicuval-charc     = 'SD_I0100'.
          ls_bapicuval-config_id = 1.
          ls_bapicuval-inst_id   = 1.
          APPEND ls_bapicuval TO lt_bapicuval.
          CLEAR ls_bapicuval.


          CALL FUNCTION 'VELO03_WRITE_SINGLE_CONFIG'
            EXPORTING
              return_written_config_iv = 'X'
              bapicucfg_is             = ls_bapicucfg
              cuobj_iv                 = <ls_vehicle>-cuobj
              check_config_iv          = ' '
              check_date_iv            = sy-datum
            IMPORTING
              bapicucfg_es             = ls_bapicucfg_es
              cuobj_ev                 = lv_cuobj
            TABLES
              bapicuins_it             = lt_bapicuins
              bapicuprt_it             = lt_bapicuprt
              bapicuval_it             = lt_bapicuval
            EXCEPTIONS
              no_conversion            = 1
              no_set_config            = 2
              no_config_to_db          = 3
              OTHERS                   = 4.

          IF sy-subrc <> 0.

            ls_messages-status  = ms_technical-icon_wrong_status.
            ls_messages-vhcle   = <ls_vehicle>-vhcle.
            ls_messages-message = 'Error in updating configurations'(014).
            ls_messages-msg_no  = 1.
            APPEND ls_messages TO mt_logs.
            CLEAR ls_messages.
            CONTINUE.

          ENDIF.

          CLEAR: lv_objectid, lv_object.

          lv_objectid = 'VLCVEHICLE'.
          lv_object   = <ls_vehicle>-vhcle.

          CALL FUNCTION 'CUD0_CONFIGURATION_TO_DB'
            EXPORTING
              instance           = lv_cuobj
              objectid           = lv_objectid
              object             = lv_object
              force_new_instance = lv_new
            IMPORTING
              new_instance       = lv_new_cuobj
            EXCEPTIONS
              no_changes         = 2
              instance_not_found = 4.

          IF sy-subrc = 4.

            ls_messages-status  = ms_technical-icon_wrong_status.
            ls_messages-vhcle   = <ls_vehicle>-vhcle.
            ls_messages-message = 'Error in updating configurations'(014).
            ls_messages-msg_no  = 1.
            APPEND ls_messages TO mt_logs.
            CLEAR ls_messages.
            CONTINUE.

          ELSE.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.

            ls_messages-status  = ms_technical-icon_correct_status.
            ls_messages-vhcle   = <ls_vehicle>-vhcle.
            ls_messages-message = 'Configurations updated successfully'(015).
            ls_messages-msg_no  = 1.
            APPEND ls_messages TO mt_logs.
            CLEAR ls_messages.

          ENDIF.
        ENDIF.
      ENDIF.

      change_status( iv_vhcle = <ls_vehicle>-vhcle ).

    ENDLOOP.

    IF mt_logs IS NOT INITIAL.
      display_logs( ).
    ENDIF.

    CLEAR: lt_vhcle , mt_logs.

  ENDMETHOD.                    "modify_vehicle_data


  METHOD change_status.

    DATA: lv_tabix    TYPE i,
          ls_messages TYPE ty_logs,
          lt_msg      TYPE ziv_hmc_msg_tt,
          lt_vhcle    TYPE ziv_hmc_vhcle_tt,
          ls_msg      LIKE LINE OF lt_msg,
          ls_cvlc02t  LIKE LINE OF mt_cvlc02t,
          ls_vehicle  TYPE ty_vehicle,
          lv_error    TYPE flag.

    READ TABLE mt_logs TRANSPORTING NO FIELDS WITH KEY vhcle = iv_vhcle.

    IF sy-subrc = 0.
      lv_tabix = 1.
    ENDIF.

    APPEND iv_vhcle TO lt_vhcle.

    CALL FUNCTION 'ZIV_HMC_RUN_ACTION'
      EXPORTING
        iv_action      = 'ZV1P'
        iv_vari        = 'ZV1P_BATCH'
        it_vhcle       = lt_vhcle
        iv_do_not_disp = 'X'
      IMPORTING
        et_msg         = lt_msg.


    LOOP AT lt_msg INTO ls_msg.

      lv_tabix = lv_tabix + 1.

      CASE ls_msg-lights.
        WHEN 1.
          ls_messages-status = ms_technical-icon_wrong_status.
          lv_error = abap_true.
        WHEN 2.
          ls_messages-status = ms_technical-icon_warning_status.
        WHEN 3.
          ls_messages-status = ms_technical-icon_correct_status.
      ENDCASE.

      ls_messages-vhcle   = iv_vhcle.
      ls_messages-message = ls_msg-natxt.
      ls_messages-msg_no  = lv_tabix.
      APPEND ls_messages TO mt_logs.
      CLEAR ls_messages.

    ENDLOOP.

    IF lv_error = abap_true.
      RETURN.
    ENDIF.

    READ TABLE mt_cvlc02t INTO ls_cvlc02t WITH KEY statu = 'ZM50'.

    ls_vehicle-mmsta      = 'ZM50'.
    ls_vehicle-mmsta_desc = ls_cvlc02t-statut.
    ls_vehicle-checkbox   = abap_false.

    MODIFY mt_vehicle_tab FROM ls_vehicle TRANSPORTING mmsta mmsta_desc checkbox WHERE vhcle = iv_vhcle.

    mo_grid->refresh_table_display( ).

  ENDMETHOD.                    "change_status



  METHOD display_logs.
    DATA : lo_functions TYPE REF TO cl_salv_functions_list,
           lo_columns   TYPE REF TO cl_salv_columns_table,
           lo_column    TYPE REF TO cl_salv_column_table,
           lo_events    TYPE REF TO cl_salv_events_table,
           lv_icon      TYPE string,
           ls_logs      TYPE ty_logs,
           lv_msg_no    TYPE i.

    set_logs_visibility( cl_gui_splitter_container=>true  ).

    IF mo_salv_logs IS BOUND.
      mo_salv_logs->refresh( ).
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_split_row->get_container( row = 1 column = 2 )
          IMPORTING
            r_salv_table = mo_salv_logs
          CHANGING
            t_table      = mt_logs ).

      CATCH cx_salv_msg.
    ENDTRY.

    lo_columns = mo_salv_logs->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    TRY.
        lo_column ?= lo_columns->get_column('STATUS').
        lo_column->set_long_text( 'Status' ).
        lo_column->set_short_text( 'Status' ).
        lo_column->set_medium_text( 'Status' ).

        lo_column ?= lo_columns->get_column('MSG_NO').
        lo_column->set_long_text( 'Message number' ).
        lo_column->set_short_text( 'Msg nr.' ).
        lo_column->set_medium_text( 'Message nr.' ).

        lo_column ?= lo_columns->get_column('MESSAGE').
        lo_column->set_long_text( 'Message' ).
        lo_column->set_short_text( 'Message' ).
        lo_column->set_medium_text( 'Message' ).

      CATCH cx_salv_not_found.
    ENDTRY.

    lo_functions = mo_salv_logs->get_functions( ).
    lo_functions->set_all( 'X' ).

    TRY.
        lv_icon = icon_close.
        lo_functions->add_function(
          EXPORTING
            name     = 'FC_CLOSE'
            icon     = lv_icon
            tooltip  = 'Close'
            position = if_salv_c_function_position=>right_of_salv_functions
        ).
      CATCH cx_salv_existing.
      CATCH cx_salv_wrong_call.
    ENDTRY.

    lo_events = mo_salv_logs->get_event( ).
    SET HANDLER handle_logs_ucomm FOR lo_events.

    mo_salv_logs->get_display_settings( )->set_list_header(  'Results' ).
    mo_salv_logs->display( ).

  ENDMETHOD.                    "display_logs



  METHOD set_logs_visibility.
    DATA lv_height TYPE i.

    IF iv_visible = cl_gui_splitter_container=>true.
      lv_height = 50.
    ENDIF.

    mo_split_row->set_column_width(
      EXPORTING
        id                = 2
        width             = lv_height
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).

    mo_split_row->set_column_sash( id    = 2
                                   type  = cl_gui_splitter_container=>type_sashvisible
                                   value = iv_visible ).

  ENDMETHOD.                    "set_logs_visibility



  METHOD handle_logs_ucomm.

    CASE e_salv_function.
      WHEN 'FC_CLOSE'.
        set_logs_visibility( iv_visible = cl_gui_splitter_container=>false ).
    ENDCASE.

  ENDMETHOD.                    "handle_logs_ucomm


  METHOD display_vehicles.

    DATA: lt_fcat        TYPE lvc_t_fcat,
          ls_fcat        TYPE lvc_s_fcat,
          ls_layout      TYPE lvc_s_layo,
          ls_variant     TYPE disvariant,
          lv_vehicles    TYPE char1.

    init_technical_vars( ).
    extract_data( ).

    ls_fcat-fieldname = 'CHECKBOX'.
    ls_fcat-checkbox  = abap_true.
    ls_fcat-key       = 'X'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZBATCH'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    ls_fcat-key       = 'X'.
    ls_fcat-style      = 3.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZPROD_SLOT'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_HD'.
    ls_fcat-key       = 'X'.
    ls_fcat-style      = 3.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZDEST_PORT'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_HD'.
    ls_fcat-key       = 'X'.
    ls_fcat-style      = 3.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'VHCLE'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    ls_fcat-key       = 'X'.
    ls_fcat-style      = 3.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'VHSAR'.
    ls_fcat-ref_table = 'VLCVEHICLE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'VHUSG'.
    ls_fcat-ref_table = 'VLCVEHICLE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'KUNNR'.
    ls_fcat-ref_table = 'VLCVEHICLE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ENDCU'.
    ls_fcat-ref_table = 'VLCVEHICLE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZDCR01'.
    ls_fcat-ref_table = 'VLCVEHICLE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZHMC_SPEC_CODE'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZHMC_COLOR'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZHMC_TYRES'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'VHVIN'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZENGINE'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZINVOICE'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZINVOICE_DATE'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZVESSEL'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZSAILING'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'LOCTN'.
    ls_fcat-ref_table = 'VLCVEHICLE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'LOCTN_DESC'.
    ls_fcat-reptext   = 'Location Description'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZSPEC_TYRES'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'EBELN'.
    ls_fcat-ref_table = 'EKPO'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'NETPR'.
    ls_fcat-reptext   = 'Purchase order value from Proforma'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'BELNR'.
    ls_fcat-ref_table = 'EKBE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ELAB_ICON'.
    ls_fcat-reptext   = 'Elaboration'.
    ls_fcat-icon      = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ELAB_MSG'.
    ls_fcat-reptext   = 'Elaboration message'.
    ls_fcat-icon      = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'MMSTA'.
    ls_fcat-ref_table = 'VLCVEHICLE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'MMSTA_DESC'.
    ls_fcat-reptext   = 'Status description'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.


    ls_fcat-fieldname = 'ZDA01'.
    ls_fcat-ref_table = 'VLCVEHICLE'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_layout-sel_mode    = 'D'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-stylefname  = 'STYLE'.
    ls_layout-cwidth_opt  = abap_true.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.

    SET HANDLER : handle_toolbar
                  handle_user_command
                  top_of_page
                  print_top_of_page
                  on_data_changed
                  FOR mo_grid.

    mo_grid->set_ready_for_input( ).

    mo_grid->set_table_for_first_display(
       EXPORTING
         i_save             = 'X'
         is_layout          = ls_layout
         is_variant         = ls_variant
       CHANGING
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_vehicle_tab ).


    mo_top->initialize_document( ).
    mo_grid->list_processing_events(
     EXPORTING
       i_event_name = 'TOP_OF_PAGE'
       i_dyndoc_id  = mo_top ).

  ENDMETHOD.                    "display_vehicles


  METHOD top_of_page.

    DATA: lv_text  TYPE sdydo_text_element,
          lo_html  TYPE REF TO cl_gui_html_viewer.

    lv_text = lines( mt_vehicle_tab ).

    CONDENSE lv_text NO-GAPS.

    CONCATENATE 'Vehicles' lv_text INTO lv_text SEPARATED BY space.

    mo_top->add_text_as_heading(
      EXPORTING
        text          = lv_text
        heading_level = 2
    ).

    CREATE OBJECT lo_html
      EXPORTING
        parent             = mo_top_cnt
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_top->html_control = lo_html.

    CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
      EXPORTING
        document = mo_top
        bottom   = space.

    mo_top->merge_document( ).

    mo_top->display_document(
     EXPORTING
       reuse_control      = 'X'
       parent             = mo_top_cnt
     EXCEPTIONS
       html_display_error = 1
       OTHERS             = 2 ).
  ENDMETHOD.                    "top_of_page



  METHOD print_top_of_page.
    DATA lv_text TYPE char30.
    lv_text = lines( mt_vehicle_tab ).

    WRITE:  1 'Vehicles', 10 lv_text.
  ENDMETHOD.                    "print_top_of_page


  METHOD get_icon.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = iv_name
        info                  = iv_info
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.

  ENDMETHOD.                    "get_icon


  METHOD init_technical_vars.

    ms_technical-icon_wrong_status = get_icon( iv_name = 'ICON_RED_LIGHT'
                                               iv_info = 'Data Missing'(009) ).

    ms_technical-icon_correct_status = get_icon( iv_name = 'ICON_GREEN_LIGHT'
                                                 iv_info = 'Action Possible'(010) ).

    ms_technical-icon_correct_status = get_icon( iv_name = 'ICON_YELLOW_LIGHT'
                                                 iv_info = 'Warning' ).

    add_edit_cell( 'CHECKBOX' ).

  ENDMETHOD.                    "init_technical_vars


  METHOD add_edit_cell.

    DATA ls_edit_cell TYPE lvc_s_styl.

    ls_edit_cell-fieldname = iv_fieldname.
    ls_edit_cell-style     = cl_gui_alv_grid=>mc_style_enabled.
    INSERT ls_edit_cell INTO TABLE ms_technical-edit_cells.

  ENDMETHOD.                    "add_edit_cell


ENDCLASS.                    "lcl_class_name IMPLEMENTATION
