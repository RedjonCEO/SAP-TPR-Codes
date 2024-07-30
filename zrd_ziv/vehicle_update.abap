*&---------------------------------------------------------------------*
*& Report  ZIV_HMC_VEHICLE_UPD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ziv_hmc_vehicle_upd.

DATA: BEGIN OF gs_screen100,
  ok_code  TYPE sy-ucomm,
  END OF gs_screen100.


INCLUDE ziv_hmc_vehicle_upd_pbo.


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
    END OF ty_vehicles,
    tt_vehicles TYPE STANDARD TABLE OF ty_vehicles.

    DATA: mt_vehicles           TYPE tt_vehicles,
          mt_changed            TYPE tt_vehicles,
          mo_grid               TYPE REF TO cl_gui_alv_grid,
          mv_selected           TYPE abap_bool VALUE abap_false,
          mt_logs               TYPE tt_logs,
          mo_split_col          TYPE REF TO cl_gui_splitter_container,
          mo_split_row          TYPE REF TO cl_gui_splitter_container,
          mo_salv_logs          TYPE REF TO cl_salv_table,
          mt_bapireturn         TYPE TABLE OF bapireturn,
          ms_icons              TYPE ty_icons.

    METHODS extract_data.
    METHODS append_logs IMPORTING it_logs  TYPE  cnv_pe_t_bapireturn
                                  iv_ebeln TYPE ebeln
                        RETURNING value(rv_error) TYPE abap_bool.
    METHODS display_logs.
    METHODS save_data.
    METHODS modify_vehicle_data IMPORTING is_vehicles TYPE ty_vehicles.
    METHODS check_table_data RETURNING value(rv_error) TYPE abap_bool.
    METHODS register_good_receipt.
    METHODS display_vehicles.
    METHODS start_container.
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


ENDCLASS.                    "lcl_vehicle_update DEFINITION



DATA go_vehicle_upd TYPE REF TO lcl_vehicle_update.
INCLUDE ziv_hmc_vehicle_upd_pai.


*----------------------------------------------------------------------*
*       CLASS lcl_vehicle_update IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_vehicle_update IMPLEMENTATION.


  METHOD on_data_changed.
    FIELD-SYMBOLS: <lt_data> TYPE tt_vehicles.
    ASSIGN er_data_changed->mp_mod_rows->* TO <lt_data>.
    mt_changed = <lt_data>.
  ENDMETHOD.                    "on_data_changed

  METHOD execute.

    start_container( ).
    display_vehicles( ).

    IF mt_vehicles IS INITIAL.
      MESSAGE 'No data found'(003) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.                    "execute



  METHOD extract_data.


    FIELD-SYMBOLS: <ls_vehicles> TYPE ty_vehicles.

    SELECT hd~zbatch
           it~posnr
           hd~zprod_slot
           hd~zdest_port
           it~vhcle
           vlc~vhsar
           vlc~vhusg
           vlc~kunnr
           vlc~endcu
           vlc~zdcr01
           it~zhmc_spec_code
           it~zhmc_color
           it~zhmc_tyres
           vlc~vhvin
           vlc~zengine
           it~zinvoice
           it~zinvoice_date
           it~zvessel
           it~zsailing
           vlc~mmsta
           ek~ebeln
           ek~netpr
           ek~werks
           ek~lgort
    FROM ziv_hmc_batch_hd AS hd
    JOIN ziv_hmc_batch_it AS it
      ON it~zbatch = hd~zbatch
      AND it~zdeleted = space
    JOIN vlcvehicle AS vlc
      ON vlc~vhcle = it~vhcle
    JOIN ekpo AS ek
      ON ek~ebeln = it~ebeln
      AND ek~loekz = space
    INTO CORRESPONDING FIELDS OF TABLE mt_vehicles
    WHERE vlc~mmsta     IN s_mmsta
    AND   vlc~vhvin     IN s_vhvin
    AND   hd~zbatch     IN s_batch
    AND   hd~zprod_slot IN s_prslot
    AND   hd~zdest_port IN s_desprt.


    LOOP AT mt_vehicles ASSIGNING <ls_vehicles>.

      IF <ls_vehicles>-vhvin IS INITIAL.
        <ls_vehicles>-style = ms_technical-edit_cells.
      ENDIF.

      IF <ls_vehicles>-mmsta <> 'ZM34'.
        <ls_vehicles>-elab_icon = ms_technical-icon_wrong_status.
        <ls_vehicles>-blocked = abap_true.
        DELETE <ls_vehicles>-style WHERE fieldname = 'CHECKBOX'.
      ELSE.
        <ls_vehicles>-elab_icon = ms_technical-icon_correct_status.
        <ls_vehicles>-blocked = abap_false.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.                    "extract_data


  METHOD get_status_icons.

    rs_icons-error   = get_icon( iv_name = 'ICON_RED_LIGHT'     iv_info = 'Errore'  ).
    rs_icons-warning = get_icon( iv_name = 'ICON_YELLOW_LIGHT'  iv_info = 'Warning'  ).
    rs_icons-info    = get_icon( iv_name = 'ICON_INFORMATION'   iv_info = 'Information'  ).
    rs_icons-success = get_icon( iv_name = 'ICON_GREEN_LIGHT'   iv_info = 'Success'  ).

  ENDMETHOD.                    "get_status_icons


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
        rows              = 1
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    set_logs_visibility( cl_gui_splitter_container=>false ).

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent          = mo_split_col->get_container( row = 1 column = 1 )
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


  METHOD set_logs_visibility.
    DATA lv_height TYPE i.

    IF iv_visible = cl_gui_splitter_container=>true.
      lv_height = 50.
    ENDIF.

    mo_split_col->set_column_width(
      EXPORTING
        id                = 2     " Column ID
        width             = lv_height    " NPlWidth
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).

    mo_split_col->set_column_sash( id    = 2
                                   type  = cl_gui_splitter_container=>type_sashvisible
                                   value = iv_visible ).

  ENDMETHOD.                    "set_logs_visibility


  METHOD handle_user_command.

    DATA: ls_vehicles    LIKE LINE OF mt_vehicles,
          lv_no_selected TYPE abap_bool VALUE abap_false.

    CLEAR mt_logs.

    READ TABLE mt_vehicles TRANSPORTING NO FIELDS WITH KEY checkbox = abap_true.
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
        MODIFY mt_vehicles FROM ls_vehicles TRANSPORTING checkbox WHERE  mmsta = 'ZM33' AND blocked = abap_false .

        IF sy-subrc <> 0.
          MESSAGE s039(ziv_hmc) DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        mo_grid->refresh_table_display( ).
      WHEN 'FC_SAVE'.

        IF lv_no_selected = abap_true.
          MESSAGE s038(ziv_hmc) DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF check_table_data( ) = abap_true.
          RETURN.
        ENDIF.

        save_data( ).

      WHEN 'FC_REGISTER'.

        IF lv_no_selected = abap_true.
          MESSAGE s038(ziv_hmc) DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF check_table_data( ) = abap_true.
          RETURN.
        ENDIF.

        register_good_receipt( ).

    ENDCASE.

  ENDMETHOD.                    "handle_user_command


  METHOD save_data.

    FIELD-SYMBOLS : <ls_vehicles> TYPE ty_vehicles.

    LOOP AT mt_vehicles ASSIGNING <ls_vehicles> WHERE checkbox IS NOT INITIAL.

      UPDATE vlcvehicle SET   vhvin   = <ls_vehicles>-vhvin
                              zengine = <ls_vehicles>-zengine
                              zdi01   = sy-datum
                        WHERE vhcle   = <ls_vehicles>-vhcle.

      UPDATE ziv_hmc_batch_it SET zinvoice      = <ls_vehicles>-zinvoice
                                  zinvoice_date = <ls_vehicles>-zinvoice_date
                                  zvessel       = <ls_vehicles>-zvessel
                                  zsailing      = <ls_vehicles>-zsailing
                                  zspec_tyres   = <ls_vehicles>-zspec_tyres
                              WHERE zbatch      = <ls_vehicles>-zbatch
                              AND   posnr       = <ls_vehicles>-posnr.
    ENDLOOP.

    MESSAGE s003(ziv_hmc) DISPLAY LIKE 'S'.

  ENDMETHOD.                    "save_data



  METHOD register_good_receipt.

    DATA: ls_gm_header   TYPE bapi2017_gm_head_01,
          lt_gm_items    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          ls_gm_items    TYPE  bapi2017_gm_item_create,
          lt_gm_messages TYPE STANDARD TABLE OF bapiret2,
          ls_gm_messages TYPE bapiret2,
          ls_po_header   TYPE bapiekkol,
          lt_po_items    TYPE STANDARD TABLE OF bapiekpo,
          lt_po_messages TYPE STANDARD TABLE OF bapireturn,
          ls_po_messages TYPE  bapireturn,
          ls_vehicles    LIKE LINE OF mt_vehicles,
          lv_error       TYPE flag VALUE abap_false,
          lt_vhcle       TYPE ziv_hmc_vhcle_tt.

    FIELD-SYMBOLS: <ls_po_items> TYPE bapiekpo,
                   <ls_gm_items> TYPE bapi2017_gm_item_create.

    ms_icons = get_status_icons( ).

    LOOP AT mt_vehicles INTO ls_vehicles WHERE checkbox IS NOT INITIAL.

      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder = ls_vehicles-ebeln
          items         = 'X'
        IMPORTING
          po_header     = ls_po_header
        TABLES
          po_items      = lt_po_items
          return        = lt_po_messages.

      IF append_logs(
          it_logs  = lt_po_messages
          iv_ebeln = ls_vehicles-ebeln ) = abap_true.
        lv_error = abap_true.
        CONTINUE.
      ENDIF.

      LOOP AT lt_po_items ASSIGNING <ls_po_items>.

        MOVE-CORRESPONDING <ls_po_items> TO ls_gm_items.

        ls_gm_items-stge_loc       = <ls_po_items>-store_loc.
        ls_gm_items-move_type      = '101'.
        ls_gm_items-stck_type      = '3'.
        ls_gm_items-ref_doc        = ls_vehicles-zinvoice.
        ls_gm_items-entry_qnt      = <ls_po_items>-quantity.
        ls_gm_items-entry_uom      = <ls_po_items>-unit.
        ls_gm_items-batch          = ls_vehicles-zbatch.

        APPEND ls_gm_items TO lt_gm_items.
        CLEAR  ls_gm_items.

      ENDLOOP.

      ls_gm_header-ref_doc_no = ls_vehicles-zinvoice.
      ls_gm_header-pstng_date = sy-datum.
      ls_gm_header-doc_date   = sy-datum.
      ls_gm_header-pr_uname   = sy-uname.


      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header = ls_gm_header
          goodsmvt_code   = '01'
        TABLES
          goodsmvt_item   = lt_gm_items
          return          = lt_gm_messages.

      LOOP AT lt_gm_messages INTO ls_gm_messages.

        ls_po_messages-code       = ls_gm_messages-id.
        ls_po_messages-log_msg_no = ls_gm_messages-log_msg_no.
        ls_po_messages-message    = ls_gm_messages-message.
        ls_po_messages-log_no     = ls_gm_messages-log_no.

        APPEND ls_po_messages TO lt_po_messages.
        CLEAR ls_po_messages.
      ENDLOOP.

      lv_error = append_logs(
                   it_logs  = lt_po_messages
                   iv_ebeln = ls_vehicles-ebeln ).

      IF lv_error = abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        modify_vehicle_data( is_vehicles = ls_vehicles ).

        change_status( iv_vhcle = ls_vehicles-vhcle
                       iv_ebeln = ls_vehicles-ebeln ).

      ENDIF.

    ENDLOOP.


    display_logs( ).

  ENDMETHOD.                    "register_good_receipt



  METHOD change_status.

    DATA: lv_tabix    TYPE i,
          ls_messages TYPE ty_logs,
          lt_msg      TYPE ziv_hmc_msg_tt,
          ls_msg      LIKE LINE OF lt_msg,
          lt_vhcle    TYPE ziv_hmc_vhcle_tt.


    APPEND iv_vhcle TO lt_vhcle.

    CALL FUNCTION 'ZIV_HMC_RUN_ACTION'
      EXPORTING
        iv_action      = 'ZPWG'
        iv_vari        = ' '
        it_vhcle       = lt_vhcle
        iv_do_not_disp = 'X'
      IMPORTING
        et_msg         = lt_msg.


    LOOP AT lt_msg INTO ls_msg.

      lv_tabix = sy-tabix.

      CASE ls_msg-lights.
        WHEN 1.
          ls_messages-status = ms_icons-error.
        WHEN 2.
          ls_messages-status = ms_icons-warning.
        WHEN 3.
          ls_messages-status = ms_icons-success.
      ENDCASE.

      ls_messages-vhcle   = iv_vhcle.
      ls_messages-ebeln   = iv_ebeln.
      ls_messages-message = ls_msg-natxt.
      ls_messages-msg_no  = lv_tabix.
      APPEND ls_messages TO mt_logs.
      CLEAR ls_messages.

    ENDLOOP.
  ENDMETHOD.                    "change_status


  METHOD modify_vehicle_data.

    UPDATE vlcvehicle  SET   werks   = is_vehicles-werks
                             lgort   = is_vehicles-lgort
                       WHERE vhcle   = is_vehicles-vhcle.

    UPDATE zcopa_delta  SET    zcstvancong = is_vehicles-netpr
                        WHERE  vhcle       = is_vehicles-vhcle.

    UPDATE zcopa_dettagli  SET    zcost   = is_vehicles-netpr
                           WHERE  vhcle   = is_vehicles-vhcle.


  ENDMETHOD.                    "modify_vehicle_data




  METHOD append_logs.

    DATA : ls_return   TYPE bapireturn,
           ls_messages TYPE ty_logs,
           lv_tabix    TYPE i.

    LOOP AT it_logs INTO ls_return.
      lv_tabix = sy-tabix.

      CASE ls_return-type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_error = abap_true.
          ls_messages-status = ms_icons-error.
        WHEN 'I'.
          ls_messages-status = ms_icons-info.
        WHEN 'W'.
          ls_messages-status = ms_icons-warning.
        WHEN 'S'.
          ls_messages-status = ms_icons-success.
      ENDCASE.

      ls_messages-ebeln   = iv_ebeln.
      ls_messages-message = ls_return-message.
      ls_messages-msg_no  = lv_tabix.
      APPEND ls_messages TO mt_logs.
      CLEAR ls_messages.

    ENDLOOP.


  ENDMETHOD.                    "append_logs


  METHOD handle_toolbar_logs.
    DATA: ls_toolbar TYPE stb_button.

    LOOP AT e_object->mt_toolbar INTO ls_toolbar.
      CASE ls_toolbar-function.
        WHEN '&INFO'
          OR '&CHECK'
          OR '&REFRESH'
          OR '&GRAPH' .

          DELETE e_object->mt_toolbar.

          CONTINUE.
      ENDCASE.
    ENDLOOP.

    ls_toolbar-function   = 'FC_CLOSE'.
    ls_toolbar-icon       = icon_close.
    ls_toolbar-quickinfo  = 'Close'.
    APPEND ls_toolbar TO e_object->mt_toolbar.


  ENDMETHOD.                    "handle_toolbar_logs


  METHOD check_table_data.

    DATA: lt_cells  TYPE lvc_t_cell,
          ls_cells  TYPE lvc_s_cell,
          lv_tabix  TYPE i.

    FIELD-SYMBOLS : <ls_vehicles>       TYPE ty_vehicles,
                    <ls_vehicles_mmsta> TYPE ty_vehicles.

    LOOP AT mt_vehicles ASSIGNING <ls_vehicles> WHERE checkbox IS NOT INITIAL.

      lv_tabix = sy-tabix.

      IF <ls_vehicles>-vhvin IS  INITIAL.
        ls_cells-row_id-index = lv_tabix.
        ls_cells-col_id-fieldname = 'VHVIN'.
        APPEND ls_cells TO lt_cells.
      ENDIF.

      IF <ls_vehicles>-zengine IS  INITIAL.
        ls_cells-row_id-index = lv_tabix.
        ls_cells-col_id-fieldname = 'ZENGINE'.
        APPEND ls_cells TO lt_cells.
      ENDIF.

      IF <ls_vehicles>-zinvoice IS  INITIAL.
        ls_cells-row_id-index = lv_tabix.
        ls_cells-col_id-fieldname = 'ZINVOICE'.
        APPEND ls_cells TO lt_cells.
      ENDIF.

      IF <ls_vehicles>-zinvoice_date IS  INITIAL.
        ls_cells-row_id-index = lv_tabix.
        ls_cells-col_id-fieldname = 'ZINVOICE_DATE'.
        APPEND ls_cells TO lt_cells.
      ENDIF.

      IF <ls_vehicles>-zvessel IS  INITIAL.
        ls_cells-row_id-index = lv_tabix.
        ls_cells-col_id-fieldname = 'ZVESSEL'.
        APPEND ls_cells TO lt_cells.
      ENDIF.

      IF <ls_vehicles>-zsailing IS  INITIAL.
        ls_cells-row_id-index = lv_tabix.
        ls_cells-col_id-fieldname = 'ZSAILING'.
        APPEND ls_cells TO lt_cells.
      ENDIF.
    ENDLOOP.

    IF lt_cells IS INITIAL.
      RETURN.
    ENDIF.

    rv_error = abap_true.
    MESSAGE s030(ziv_hmc) DISPLAY LIKE 'E'.
    mo_grid->set_selected_cells( it_cells = lt_cells ).

  ENDMETHOD.                    "check_table_data


  METHOD display_logs.
    DATA : lo_functions TYPE REF TO cl_salv_functions_list,
           lo_columns   TYPE REF TO cl_salv_columns_table,
           lo_column    TYPE REF TO cl_salv_column_table,
           lo_events    TYPE REF TO cl_salv_events_table,
           lv_icon      TYPE string.

    set_logs_visibility( cl_gui_splitter_container=>true  ).

    IF mo_salv_logs IS BOUND.
      mo_salv_logs->refresh( ).
      RETURN.
    ENDIF.


    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_split_col->get_container( row = 1 column = 2 )
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

    mo_salv_logs->get_display_settings( )->set_list_header(  'Check Results' ).

    mo_salv_logs->display( ).

  ENDMETHOD.                    "display_logs


  METHOD handle_logs_ucomm.

    CASE e_salv_function.
      WHEN 'FC_CLOSE'.
        set_logs_visibility( iv_visible = cl_gui_splitter_container=>false ).
    ENDCASE.

  ENDMETHOD.                    "handle_logs_ucomm


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




  METHOD display_vehicles.

    DATA: lt_fcat    TYPE lvc_t_fcat,
          ls_fcat    TYPE lvc_s_fcat,
          ls_layout  TYPE lvc_s_layo,
          ls_variant TYPE disvariant.


    init_technical_vars( ).

    extract_data( ).

    ls_fcat-fieldname = 'CHECKBOX'.
    ls_fcat-checkbox  = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZBATCH'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZPROD_SLOT'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZDEST_PORT'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'VHCLE'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
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

    ls_fcat-fieldname = 'ZSPEC_TYRES'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ELAB_ICON'.
    ls_fcat-reptext   = 'Elaboration'.
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

    ls_fcat-fieldname = 'EBELN'.
    ls_fcat-ref_table = 'EKPO'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'NETPR'.
    ls_fcat-ref_table = 'EKPO'.
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
         it_outtab          = mt_vehicles ).

  ENDMETHOD.                    "display_vehicles


  METHOD init_technical_vars.

    ms_technical-icon_wrong_status = get_icon( iv_name = 'ICON_RED_LIGHT'
                                               iv_info = 'Wrong primary status'(004) ).

    ms_technical-icon_correct_status = get_icon( iv_name = 'ICON_GREEN_LIGHT'
                                                 iv_info = 'Correct primary status'(005) ).

    add_edit_cell( 'VHVIN' ).
    add_edit_cell( 'ZENGINE' ).
    add_edit_cell( 'ZINVOICE' ).
    add_edit_cell( 'ZINVOICE_DATE' ).
    add_edit_cell( 'ZVESSEL' ).
    add_edit_cell( 'ZSAILING' ).
    add_edit_cell( 'ZSPEC_TYRES' ).
    add_edit_cell( 'CHECKBOX' ).
  ENDMETHOD.                    "init_technical_vars


  METHOD add_edit_cell.

    DATA ls_edit_cell TYPE lvc_s_styl.

    ls_edit_cell-fieldname = iv_fieldname.
    ls_edit_cell-style     = cl_gui_alv_grid=>mc_style_enabled.
    INSERT ls_edit_cell INTO TABLE ms_technical-edit_cells.

  ENDMETHOD.                    "add_edit_cell


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

    ls_toolbar-function   = 'FC_SAVE'.
    ls_toolbar-text       = 'Save'.
    ls_toolbar-icon       = icon_system_save.
    ls_toolbar-quickinfo  = 'Save data'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-function   = 'FC_REGISTER'.
    ls_toolbar-text       = 'Register Good Receipt'.
    ls_toolbar-icon       = icon_te_receipts.
    ls_toolbar-quickinfo  = 'Register good receipt'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.    "handle_toolbar

ENDCLASS.                    "lcl_vehicle_update IMPLEMENTATION


START-OF-SELECTION.

  CREATE OBJECT go_vehicle_upd.
  go_vehicle_upd->execute( ).
