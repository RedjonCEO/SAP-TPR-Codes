*&---------------------------------------------------------------------*
*& Report  ZIV_HMC_VEHICLE_UPD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ziv_hmc_vehicle_upd.

TABLES: ziv_hmc_batch_hd, ziv_hmc_prof_hd.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_batch  FOR ziv_hmc_batch_hd-zbatch,
                s_prslot FOR ziv_hmc_batch_hd-zprod_slot,
                s_desprt FOR ziv_hmc_batch_hd-zdest_port,
                s_stsdt  FOR ziv_hmc_prof_hd-zstatus_date.

SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
*       CLASS lcl_vehicle_update DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_vehicle_update DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.
    TYPES ty_icon TYPE char100.

    TYPES:BEGIN OF ty_vehicles,
            expand(1)      TYPE c,
            zproforma      TYPE ziv_hmc_proforma-zproforma,
            vhcle          TYPE vlcvehicle-vhcle,
            matnr          TYPE vlcvehicle-matnr,
            zbatch         TYPE ziv_hmc_prof_hd-zbatch,
            zhmc_spec_code TYPE ziv_hmc_proforma-zhmc_spec_code,
            zhmc_tyres     TYPE ziv_hmc_proforma-zhmc_tyres ,
            zhmc_color     TYPE ziv_hmc_proforma-zhmc_color,
            zunitprice     TYPE ziv_hmc_proforma-zunitprice,
            zcolor         TYPE ziv_hmc_proforma-zcolor,
            zquantity      TYPE ziv_hmc_proforma-zquantity,
            zprice         TYPE ziv_hmc_proforma-ztotal,
            zcurr          TYPE ziv_hmc_prof_hd-zcurr,
          END OF ty_vehicles.

    TYPES: BEGIN OF ty_messages,
            vhcle          TYPE vlcvehicle-vhcle,
            status         TYPE ty_icon,
            msg_no         TYPE i,
            message        TYPE bapiret2-message,
          END OF ty_messages.

    TYPES: BEGIN OF ty_icons,
            error     TYPE ty_icon,
            warning   TYPE ty_icon,
            success   TYPE ty_icon,
            info      TYPE ty_icon,
          END OF ty_icons.

    DATA : mt_vehicles TYPE STANDARD TABLE OF ty_vehicles,
           mt_messages TYPE STANDARD TABLE OF ty_messages,
           ms_icons    TYPE ty_icons.

    CONSTANTS: cv_act_zpwg  TYPE vlcbatchact-action   VALUE 'ZPWG',
               cv_var_batch TYPE vlcbatchact-execvari VALUE 'ZPWG_BATCH',
               cv_act_zm34  TYPE vlcbatchact-action   VALUE 'ZM34',
               cv_var_unk   TYPE vlcbatchact-execvari VALUE ''.

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

ENDCLASS.                    "lcl_vehicle_update DEFINITION

DATA go_vehicle_update TYPE REF TO lcl_vehicle_update.

*----------------------------------------------------------------------*
*       CLASS lcl_vehicle_update IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_vehicle_update IMPLEMENTATION.

  METHOD execute.

    prepare_data( ).

    create_po( ).

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

    DATA: ls_po_hdr   TYPE bapimepoheader,
          ls_po_hdrx  TYPE bapimepoheaderx,
          lt_return   TYPE STANDARD TABLE OF bapiret2,
          ls_return   TYPE bapiret2,
          ls_poitem   TYPE bapimepoitem,
          lt_poitem   TYPE STANDARD TABLE OF bapimepoitem,
          ls_poitemx  TYPE bapimepoitemx,
          lt_poitemx  TYPE STANDARD TABLE OF bapimepoitemx,
          ls_messages TYPE ty_messages,
          lv_error    TYPE flag VALUE abap_false,
          lv_tabix    TYPE i.

    FIELD-SYMBOLS <ls_vehicles> TYPE ty_vehicles.

    ms_icons = get_status_icons( ).

    LOOP AT mt_vehicles ASSIGNING <ls_vehicles> .

      lv_error = change_status(
          iv_vhcle  = <ls_vehicles>-vhcle
          iv_action = cv_act_zpwg
          iv_vari   = cv_var_batch
      ).

      IF lv_error IS NOT INITIAL.
        CLEAR lv_error.
        CONTINUE.
      ENDIF.

      <ls_vehicles>-zprice = ( <ls_vehicles>-zunitprice + <ls_vehicles>-zcolor ) / <ls_vehicles>-zquantity.

      ls_po_hdr-doc_type  = 'ZPWG'.
      ls_po_hdr-purch_org = 'ITP1'.
      ls_po_hdr-pur_group = 'PWG'.
      ls_po_hdr-comp_code = 'G021'.
      ls_po_hdr-vendor    = '0000267860'.
      ls_po_hdr-currency  = 'EUR'.

      ls_po_hdrx-doc_type  = abap_true.
      ls_po_hdrx-purch_org = abap_true.
      ls_po_hdrx-pur_group = abap_true.
      ls_po_hdrx-comp_code = abap_true.
      ls_po_hdrx-vendor    = abap_true.
      ls_po_hdrx-currency  = abap_true.

      ls_poitem-po_item   = 10.
      ls_poitem-material  = <ls_vehicles>-matnr.
      ls_poitem-val_type  = <ls_vehicles>-vhcle.
      ls_poitem-batch     = <ls_vehicles>-vhcle.
      ls_poitem-net_price = <ls_vehicles>-zprice.
      ls_poitem-quantity  = 1.
      ls_poitem-plant     = 'L021'.
      ls_poitem-stge_loc  = 'L021'.
      APPEND ls_poitem TO lt_poitem.

      ls_poitemx-po_item   = 10.
      ls_poitemx-material  = abap_true.
      ls_poitemx-val_type  = abap_true.
      ls_poitemx-batch     = abap_true.
      ls_poitemx-net_price = abap_true.
      ls_poitemx-quantity  = abap_true.
      ls_poitemx-plant     = abap_true.
      ls_poitemx-stge_loc  = abap_true.
      APPEND ls_poitemx TO lt_poitemx.


      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader  = ls_po_hdr
          poheaderx = ls_po_hdrx
        TABLES
          return    = lt_return
          poitem    = lt_poitem
          poitemx   = lt_poitemx.

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
        CONTINUE.
      ENDIF.


      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.


      lv_error = change_status(
          iv_vhcle  = <ls_vehicles>-vhcle
          iv_action = cv_act_zm34
          iv_vari   = cv_var_unk
      ).

      IF lv_error IS NOT INITIAL.
        CLEAR lv_error.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "create_po


  METHOD display_messages.

    DATA: lo_hier      TYPE REF TO cl_salv_hierseq_table,
          lt_binding   TYPE salv_t_hierseq_binding,
          ls_binding   TYPE salv_s_hierseq_binding,
          lr_columns   TYPE REF TO cl_salv_columns_hierseq,
          lr_column    TYPE REF TO cl_salv_column,
          lr_functions TYPE REF TO cl_salv_functions_list,
          lr_level     TYPE REF TO cl_salv_hierseq_level.


    ls_binding-master = 'VHCLE'.
    ls_binding-slave  = 'VHCLE'.
    APPEND ls_binding TO lt_binding.

    TRY.
        cl_salv_hierseq_table=>factory(
          EXPORTING
            t_binding_level1_level2  = lt_binding
          IMPORTING
            r_hierseq                = lo_hier
          CHANGING
            t_table_level1           = mt_vehicles
            t_table_level2           = mt_messages ).
      CATCH cx_salv_data_error cx_salv_not_found.
    ENDTRY.

    lr_functions = lo_hier->get_functions( ).
    lr_functions->set_all( abap_true ).

    TRY.
        lr_columns = lo_hier->get_columns( 1 ).
        lr_columns->set_optimize( ).

        lr_column = lr_columns->get_column( 'ZUNITPRICE' ).
        lr_column->set_currency_column( value = 'ZCURR'   ).

        lr_column = lr_columns->get_column( 'ZCOLOR' ).
        lr_column->set_currency_column( value = 'ZCURR'   ).

        lr_column = lr_columns->get_column( 'ZPRICE' ).
        lr_column->set_currency_column( value = 'ZCURR'   ).

        lr_column = lr_columns->get_column( 'ZQUANTITY' ).
        lr_column->set_decimals( value = '0' ).

      CATCH cx_salv_not_found.    " ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_salv_data_error.    " ALV: General Error Class (Checked During Syntax Check)

    ENDTRY.

    TRY.
        lr_columns->set_expand_column( 'EXPAND' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_columns = lo_hier->get_columns( 2 ).
        lr_columns->set_optimize( ).

        lr_column = lr_columns->get_column( 'STATUS' ).
        lr_column->set_short_text( value = 'Status' ).


        lr_column = lr_columns->get_column( 'VHCLE' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lr_level = lo_hier->get_level( 1 ).
        lr_level->set_items_expanded( ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lo_hier->display( ).

  ENDMETHOD.                    "display_messages
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


    SELECT pf~zhmc_spec_code
           pf~zhmc_tyres
           pf~zhmc_color
           vlc~vhcle
           it~zbatch
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
       AND EXISTS ( SELECT *
                   FROM ziv_hmc_batch_hd AS bhd
                  WHERE it~zbatch       = bhd~zbatch
                    AND bhd~zprod_slot  IN s_prslot
                    AND bhd~zdest_port  IN s_desprt
                    AND bhd~zbatch      IN s_batch ).
    IF sy-subrc <> 0.
      MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.                    "prepare_data

ENDCLASS.                    "lcl_vehicle_update IMPLEMENTATION


START-OF-SELECTION.

  CREATE OBJECT go_vehicle_update.
  go_vehicle_update->execute( ).
