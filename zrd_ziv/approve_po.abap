REPORT ziv_hmc_approve_po.

TABLES: ziv_hmc_batch_hd, ziv_hmc_prof_hd, vlcvehicle.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_batch  FOR ziv_hmc_batch_hd-zbatch,
                s_vhcle  FOR vlcvehicle-vhcle,
                s_prslot FOR ziv_hmc_batch_hd-zprod_slot,
                s_desprt FOR ziv_hmc_batch_hd-zdest_port,
                s_stsdt  FOR ziv_hmc_prof_hd-zstatus_date.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER: p_test TYPE flag DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.


*----------------------------------------------------------------------*
*       CLASS lcl_vehicle_update DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_approve_po DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.
    TYPES ty_icon TYPE char100.

    TYPES:BEGIN OF ty_vehicles,
            expand(1)      TYPE c,
            checked        TYPE abap_bool,
            finished       TYPE ty_icon,
            zproforma      TYPE ziv_hmc_proforma-zproforma,
            vhcle          TYPE vlcvehicle-vhcle,
            matnr          TYPE vlcvehicle-matnr,
            zbatch         TYPE ziv_hmc_prof_hd-zbatch,
            ebeln          TYPE bapimepoheader-po_number,
            posnr          TYPE ziv_hmc_batch_it-posnr,
            zhmc_spec_code TYPE ziv_hmc_proforma-zhmc_spec_code,
            zhmc_tyres     TYPE ziv_hmc_proforma-zhmc_tyres ,
            zhmc_color     TYPE ziv_hmc_proforma-zhmc_color,
            zunitprice     TYPE ziv_hmc_proforma-zunitprice,
            zcolor         TYPE ziv_hmc_proforma-zcolor,
            zquantity      TYPE ziv_hmc_proforma-zquantity,
            zprice         TYPE ziv_hmc_proforma-ztotal,
            zcurr          TYPE ziv_hmc_prof_hd-zcurr,
          END OF ty_vehicles,
          tt_vehicles TYPE STANDARD TABLE OF ty_vehicles.

    TYPES: BEGIN OF ty_messages,
            vhcle          TYPE vlcvehicle-vhcle,
            status         TYPE ty_icon,
            time           TYPE sy-uzeit,
            msg_no         TYPE i,
            message        TYPE bapiret2-message,
          END OF ty_messages.

    TYPES: BEGIN OF ty_icons,
            error     TYPE ty_icon,
            warning   TYPE ty_icon,
            success   TYPE ty_icon,
            info      TYPE ty_icon,
          END OF ty_icons.

    DATA : mt_vehicles TYPE tt_vehicles,
           mt_messages TYPE STANDARD TABLE OF ty_messages,
           ms_icons    TYPE ty_icons,
           mt_param    TYPE STANDARD TABLE OF ziv_hmc_param,
           mo_hier     TYPE REF TO cl_salv_hierseq_table.

    CONSTANTS: cv_act_zpwg  TYPE vlcbatchact-action   VALUE 'ZPWG',
               cv_var_batch TYPE vlcbatchact-execvari VALUE 'ZPWG_BATCH'.

    METHODS prepare_data.
    METHODS change_status IMPORTING iv_vhcle  TYPE vlcvehicle-vhcle
                                    iv_action TYPE vlcbatchact-action
                                    iv_vari   TYPE vlcbatchact-execvari
                          RETURNING value(rv_error) TYPE flag.
    METHODS create_po.
    METHODS display_messages.
    METHODS get_status_icons RETURNING value(rs_icons) TYPE ty_icons.
    METHODS get_icon
    IMPORTING iv_name        TYPE char30
              iv_tooltip     TYPE char50
    RETURNING value(rv_icon) TYPE ty_icon.

    METHODS on_user_comm FOR EVENT added_function OF cl_salv_events_hierseq IMPORTING e_salv_function.
    METHODS post_po.
ENDCLASS.                    "lcl_vehicle_update DEFINITION

DATA go_approve_po TYPE REF TO lcl_approve_po.

*----------------------------------------------------------------------*
*       CLASS lcl_vehicle_update IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_approve_po IMPLEMENTATION.

  METHOD execute.

    prepare_data( ).
    ms_icons = get_status_icons( ).

    IF p_test IS INITIAL.
      create_po( ).
    ENDIF.

    display_messages( ).


  ENDMETHOD.                    "execute
  METHOD get_status_icons.

    rs_icons-error   = get_icon( iv_name = 'ICON_RED_LIGHT'     iv_tooltip = 'Errore'  ).
    rs_icons-warning = get_icon( iv_name = 'ICON_YELLOW_LIGHT'  iv_tooltip = 'Warning'  ).
    rs_icons-info    = get_icon( iv_name = 'ICON_INFORMATION'   iv_tooltip = 'Information'  ).
    rs_icons-success = get_icon( iv_name = 'ICON_GREEN_LIGHT'   iv_tooltip = 'Success'  ).

  ENDMETHOD.                    "get_status_icons
  METHOD get_icon.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = iv_name
        info                  = iv_tooltip
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
  ENDMETHOD.                    "get_icon

  METHOD create_po.

    DATA: ls_po_hdr      TYPE bapimepoheader,
          ls_po_hdrx     TYPE bapimepoheaderx,
          lt_return      TYPE STANDARD TABLE OF bapiret2,
          ls_return      TYPE bapiret2,
          ls_poitem      TYPE bapimepoitem,
          lt_poitem      TYPE STANDARD TABLE OF bapimepoitem,
          ls_poitemx     TYPE bapimepoitemx,
          lt_poitemx     TYPE STANDARD TABLE OF bapimepoitemx,
          ls_messages    TYPE ty_messages,
          lv_error       TYPE flag VALUE abap_false,
          lv_tabix       TYPE i,
          ls_map_po_hdr  TYPE bapimepoheader,
          ls_map_po_hdrx TYPE bapimepoheaderx,
          ls_map_po_itm  TYPE bapimepoitem,
          ls_map_po_itmx TYPE bapimepoitemx.

    DATA lv_fieldname TYPE char30.


    FIELD-SYMBOLS: <ls_vehicles> TYPE ty_vehicles,
                   <ls_param>    TYPE ziv_hmc_param,
                   <lv_value>    TYPE any.

    zcl_iv_hmc_utilities=>init_mapping( iv_progname = sy-repid ).

    zcl_iv_hmc_utilities=>map_values(
      EXPORTING
        iv_structure = 'BAPIMEPOHEADER'    " Structure name
      CHANGING
        cs_map       = ls_map_po_hdr     " Map to str
        cs_mapx      = ls_map_po_hdrx    " Map to strx
    ).

    zcl_iv_hmc_utilities=>map_values(
      EXPORTING
        iv_structure = 'BAPIMEPOITEM'    " Structure name
      CHANGING
        cs_map       = ls_map_po_itm     " Map to str
        cs_mapx      = ls_map_po_itmx    " Map to strx
    ).

    LOOP AT mt_vehicles ASSIGNING <ls_vehicles> WHERE checked IS NOT INITIAL
                                                  AND finished <> ms_icons-success.
      CLEAR <ls_vehicles>-checked.
      IF <ls_vehicles>-ebeln IS INITIAL.

        ls_po_hdr  = ls_map_po_hdr.
        ls_po_hdrx = ls_map_po_hdrx.

        ls_poitem  = ls_map_po_itm.
        ls_poitemx = ls_map_po_itmx.

        ls_poitem-po_item   = 10.
        ls_poitem-material  = <ls_vehicles>-matnr.
        ls_poitem-val_type  = <ls_vehicles>-vhcle.
        ls_poitem-batch     = <ls_vehicles>-vhcle.
        ls_poitem-net_price = <ls_vehicles>-zprice.
        ls_poitem-quantity  = 1.
        APPEND ls_poitem TO lt_poitem.
        CLEAR ls_poitem.

        ls_poitemx-po_item   = 10.
        ls_poitemx-material  = abap_true.
        ls_poitemx-val_type  = abap_true.
        ls_poitemx-batch     = abap_true.
        ls_poitemx-net_price = abap_true.
        ls_poitemx-quantity  = abap_true.
        APPEND ls_poitemx TO lt_poitemx.
        CLEAR ls_poitemx.

        CALL FUNCTION 'BAPI_PO_CREATE1'
          EXPORTING
            poheader         = ls_po_hdr
            poheaderx        = ls_po_hdrx
          IMPORTING
            exppurchaseorder = <ls_vehicles>-ebeln
          TABLES
            return           = lt_return
            poitem           = lt_poitem
            poitemx          = lt_poitemx.

        LOOP AT lt_return INTO ls_return.
          lv_tabix = sy-tabix.

          CASE ls_return-type.
            WHEN 'E' OR 'A' OR 'X'.
              lv_error = abap_true.
              ls_messages-status = ms_icons-error.
            WHEN 'I'.
              ls_messages-status = ms_icons-info.
            WHEN 'W'.
              ls_messages-status = ms_icons-warning.
            WHEN 'S'.
              ls_messages-status = ms_icons-success.
          ENDCASE.

          ls_messages-vhcle   = <ls_vehicles>-vhcle.
          ls_messages-message = ls_return-message.
          ls_messages-time    = sy-uzeit.
          ls_messages-msg_no  = lv_tabix.
          APPEND ls_messages TO mt_messages.
          CLEAR ls_messages.

        ENDLOOP.


        CLEAR: ls_po_hdr,
               lt_return,
               lt_poitem,
               lt_poitemx.

        IF lv_error = abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CLEAR lv_error.

          <ls_vehicles>-finished = ms_icons-error.

          ls_messages-vhcle   = <ls_vehicles>-vhcle.
          ls_messages-message = sy-uline(50).
          ls_messages-time    = sy-uzeit.
          APPEND ls_messages TO mt_messages.
          CLEAR ls_messages.

          CONTINUE.
        ENDIF.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        UPDATE ziv_hmc_batch_it
           SET ebeln  = <ls_vehicles>-ebeln
         WHERE zbatch = <ls_vehicles>-zbatch
           AND posnr  = <ls_vehicles>-posnr.

      ENDIF.

      lv_error = change_status(
          iv_vhcle  = <ls_vehicles>-vhcle
          iv_action = cv_act_zpwg
          iv_vari   = cv_var_batch
      ).

      ls_messages-vhcle   = <ls_vehicles>-vhcle.
      ls_messages-message = sy-uline(50).
      ls_messages-time    = sy-uzeit.
      APPEND ls_messages TO mt_messages.
      CLEAR ls_messages.

      IF lv_error IS NOT INITIAL.
        <ls_vehicles>-finished = ms_icons-error.
        CLEAR lv_error.
        CONTINUE.
      ENDIF.

      <ls_vehicles>-finished = ms_icons-success.

    ENDLOOP.

  ENDMETHOD.                    "create_po


  METHOD display_messages.

    DATA: lt_binding   TYPE salv_t_hierseq_binding,
          ls_binding   TYPE salv_s_hierseq_binding,
          lr_columns   TYPE REF TO cl_salv_columns_hierseq,
          lr_column    TYPE REF TO cl_salv_column,
          lr_functions TYPE REF TO cl_salv_functions_list,
          lr_level     TYPE REF TO cl_salv_hierseq_level,
          lv_icon      TYPE string,
          lo_event     TYPE REF TO cl_salv_events_hierseq.

    ls_binding-master = 'VHCLE'.
    ls_binding-slave  = 'VHCLE'.
    APPEND ls_binding TO lt_binding.

    TRY.
        cl_salv_hierseq_table=>factory(
          EXPORTING
            t_binding_level1_level2  = lt_binding
          IMPORTING
            r_hierseq                = mo_hier
          CHANGING
            t_table_level1           = mt_vehicles
            t_table_level2           = mt_messages ).
      CATCH cx_salv_data_error cx_salv_not_found.
    ENDTRY.

    IF p_test IS INITIAL.
      lr_functions = mo_hier->get_functions( ).
      lr_functions->set_all( abap_true ).
    ELSE.
      mo_hier->set_screen_status(
        EXPORTING
          report        = sy-repid     " ABAP Program: Current Main Program
          pfstatus      = 'SALV_TABLE_STANDARD'    " Screens, Current GUI Status
          set_functions = mo_hier->c_functions_all    " ALV: Data Element for Constants
      ).

      TRY.
          mo_hier->get_selections( level = 1 )->set_selection_mode( cl_salv_selections=>if_salv_c_selection_mode~multiple ).
        CATCH cx_salv_not_found.
      ENDTRY.
    ENDIF.

    TRY.
        lr_columns = mo_hier->get_columns( 1 ).
        lr_columns->set_optimize( ).

        lr_column = lr_columns->get_column( 'FINISHED' ).
        lr_column->set_short_text( value = 'Status' ).

        lr_column = lr_columns->get_column( 'ZUNITPRICE' ).
        lr_column->set_currency_column( value = 'ZCURR'   ).

        lr_column = lr_columns->get_column( 'ZCOLOR' ).
        lr_column->set_currency_column( value = 'ZCURR'   ).

        lr_column = lr_columns->get_column( 'ZPRICE' ).
        lr_column->set_currency_column( value = 'ZCURR'   ).

        lr_column = lr_columns->get_column( 'ZQUANTITY' ).
        lr_column->set_decimals( value = '0' ).

        lr_columns->get_column( 'CHECKED' )->set_technical( ).

      CATCH cx_salv_not_found.    " ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_salv_data_error.    " ALV: General Error Class (Checked During Syntax Check)

    ENDTRY.

    TRY.
        lr_columns->set_expand_column( 'EXPAND' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_columns = mo_hier->get_columns( 2 ).
        lr_columns->set_optimize( ).

        lr_column = lr_columns->get_column( 'STATUS' ).
        lr_column->set_short_text( value = 'Status' ).


        lr_column = lr_columns->get_column( 'VHCLE' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_level = mo_hier->get_level( 1 ).
        lr_level->set_items_expanded( ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lo_event = mo_hier->get_event( ).
    SET HANDLER on_user_comm FOR lo_event.

    mo_hier->display( ).

  ENDMETHOD.                    "display_messages
  METHOD on_user_comm.

    CASE e_salv_function.
      WHEN 'FC_POST_PO'.
        post_po( ).
    ENDCASE.

  ENDMETHOD.                    "ON_USER_COMM
  METHOD post_po.
    DATA lt_selected TYPE salv_t_row.
    FIELD-SYMBOLS: <lv_index> TYPE i,
                   <ls_vehc> TYPE ty_vehicles.

    TRY.
        lt_selected = mo_hier->get_selections( level = 1 )->get_selected_rows( ).
      CATCH cx_salv_not_found.    " ALV: General Error Class (Checked During Syntax Check)
    ENDTRY.

    IF lt_selected IS INITIAL.
      MESSAGE 'Select at least 1 vehicle before saving'(003) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_selected ASSIGNING <lv_index>.
      READ TABLE mt_vehicles ASSIGNING <ls_vehc> INDEX <lv_index>.
      IF sy-subrc = 0.
        <ls_vehc>-checked = abap_true.
      ENDIF.
    ENDLOOP.

    create_po( ).

    mo_hier->refresh( ).

  ENDMETHOD.                    "POST_PO
  METHOD change_status.

    DATA: lt_vhcle    TYPE ziv_hmc_vhcle_tt,
          lt_msg      TYPE ziv_hmc_msg_tt,
          ls_msg      LIKE LINE OF lt_msg,
          lv_tabix    TYPE i,
          ls_messages TYPE ty_messages.

    APPEND iv_vhcle TO lt_vhcle.

    CALL FUNCTION 'ZIV_HMC_RUN_ACTION'
      EXPORTING
        iv_action      = iv_action
        iv_vari        = iv_vari
        it_vhcle       = lt_vhcle
        iv_do_not_disp = 'X'
      IMPORTING
        ev_error       = rv_error
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
      ls_messages-message = ls_msg-natxt.
      ls_messages-msg_no  = lv_tabix.
      APPEND ls_messages TO mt_messages.
      CLEAR ls_messages.

    ENDLOOP.

  ENDMETHOD.                    "change_status


  METHOD prepare_data.
    DATA: lt_vhcle  TYPE ziv_hmc_vhcle_tt,
          lv_error  TYPE flag.

    FIELD-SYMBOLS: <ls_vehicles> TYPE ty_vehicles.

    SELECT pf~zhmc_spec_code
           pf~zhmc_tyres
           pf~zhmc_color
           vlc~vhcle
           it~zbatch
           it~ebeln
           it~posnr
           hd~zproforma
           hd~zcurr
           pf~zunitprice
           pf~zcolor
           pf~zquantity
           vlc~matnr
  FROM ziv_hmc_batch_it   AS it

    JOIN ziv_hmc_prof_hd  AS hd
      ON hd~zbatch  = it~zbatch
     AND hd~zstatus = 'A'

    JOIN ziv_hmc_proforma AS pf
      ON pf~zproforma = hd~zproforma
     AND pf~zhmc_spec_code = it~zhmc_spec_code
     AND pf~zhmc_tyres     = it~zhmc_tyres
     AND pf~zhmc_color     = it~zhmc_color

    JOIN vlcvehicle       AS vlc
      ON vlc~vhcle = it~vhcle
     AND vlc~mmsta  = 'ZM33'

  INTO CORRESPONDING FIELDS OF TABLE mt_vehicles
     WHERE  hd~zstatus_date    IN s_stsdt
       AND  it~zdeleted        =  space
       AND  it~vhcle           IN s_vhcle
       AND EXISTS ( SELECT *
                   FROM ziv_hmc_batch_hd AS bhd
                  WHERE it~zbatch       = bhd~zbatch
                    AND bhd~zprod_slot  IN s_prslot
                    AND bhd~zdest_port  IN s_desprt
                    AND bhd~zbatch      IN s_batch
                    AND bhd~zdeleted    = space ).
    IF sy-subrc <> 0.
      MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    LOOP AT mt_vehicles ASSIGNING <ls_vehicles>.
      IF <ls_vehicles>-zquantity <> 0.
        <ls_vehicles>-zprice =  <ls_vehicles>-zunitprice + ( <ls_vehicles>-zcolor  / <ls_vehicles>-zquantity ).
      ELSE.
        <ls_vehicles>-zprice =  <ls_vehicles>-zunitprice.
      ENDIF.

      IF p_test IS INITIAL.
        <ls_vehicles>-checked = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "prepare_data

ENDCLASS.                    "lcl_vehicle_update IMPLEMENTATION


START-OF-SELECTION.

  CREATE OBJECT go_approve_po.
  go_approve_po->execute( ).
