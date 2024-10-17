*&---------------------------------------------------------------------*
*&  Include           ZIV_HMC_VEHICLE_UPD_IMP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_vehicle_update IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_vehicle_update IMPLEMENTATION.

  METHOD on_data_changed.
    FIELD-SYMBOLS: <lt_data> TYPE tt_vehicles.
    ASSIGN er_data_changed->mp_mod_rows->* TO <lt_data>.
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
           it~zspec_tyres
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
           pf~zunitprice
           pf~zcolor
           pf~zquantity
           vlc~zwerks_orig
    FROM ziv_hmc_batch_hd AS hd
    JOIN ziv_hmc_batch_it AS it
      ON it~zbatch = hd~zbatch
    JOIN vlcvehicle AS vlc
      ON vlc~vhcle = it~vhcle
    JOIN ekpo AS ek
      ON ek~ebeln = it~ebeln
    JOIN ziv_hmc_prof_hd  AS ph ON ph~zbatch = hd~zbatch
    JOIN ziv_hmc_proforma AS pf ON pf~zproforma = ph~zproforma
      AND pf~zhmc_spec_code = it~zhmc_spec_code
      AND pf~zhmc_color     = it~zhmc_color
      AND pf~zhmc_tyres     = it~zhmc_tyres
    INTO CORRESPONDING FIELDS OF TABLE mt_vehicles
    WHERE vlc~mmsta     IN s_mmsta
    AND   vlc~vhvin     IN s_vhvin
    AND   hd~zbatch     IN s_batch
    AND   hd~zprod_slot IN s_prslot
    AND   hd~zdest_port IN s_desprt
    AND   ek~loekz       = space
    AND   it~zdeleted    = space
    AND   ph~zstatus     = 'A'.

    SELECT * FROM cvlc02t
      INTO TABLE mt_cvlc02t
      FOR ALL ENTRIES IN mt_vehicles
      WHERE statu = mt_vehicles-mmsta
      AND   spras = sy-langu.

    change_edit_status( ).

  ENDMETHOD.                    "extract_data


  METHOD change_edit_status.


    DATA: ls_cvlc02t TYPE cvlc02t,
          ls_style TYPE lvc_s_styl.

    FIELD-SYMBOLS: <ls_vehicles> TYPE ty_vehicles,
                   <ls_style>    TYPE lvc_s_styl.

    mo_grid->check_changed_data( ).

    LOOP AT mt_vehicles ASSIGNING <ls_vehicles>.

      IF <ls_vehicles>-vhvin IS INITIAL .
        <ls_vehicles>-style = ms_technical-edit_cells.
      ELSEIF <ls_vehicles>-vhvin IS NOT INITIAL AND <ls_vehicles>-mmsta = 'ZM34'.
        <ls_vehicles>-style = ms_technical-edit_cells.
      ELSE.
        CLEAR <ls_vehicles>-style.
      ENDIF.

      IF <ls_vehicles>-mmsta = 'ZM49'.
        <ls_vehicles>-elab_icon = ms_technical-icon_correct_status.
        <ls_vehicles>-blocked = abap_true.
        DELETE <ls_vehicles>-style WHERE fieldname = 'CHECKBOX'.

      ELSEIF <ls_vehicles>-mmsta = 'ZM34'.

        <ls_vehicles>-elab_icon = ms_technical-icon_correct_status.
        <ls_vehicles>-blocked    = abap_false.

        READ TABLE <ls_vehicles>-style ASSIGNING <ls_style> WITH  KEY fieldname = 'CHECKBOX'.

        IF sy-subrc <> 0.
          ls_style-fieldname       = 'CHECKBOX'.
          ls_style-style           = cl_gui_alv_grid=>mc_style_enabled.
          APPEND ls_style TO  <ls_vehicles>-style .
          CLEAR ls_style.
        ENDIF.

      ELSE.
        <ls_vehicles>-elab_icon = ms_technical-icon_wrong_status.
        <ls_vehicles>-blocked = abap_true.
        DELETE <ls_vehicles>-style WHERE fieldname = 'CHECKBOX'.
      ENDIF.

      READ TABLE mt_cvlc02t INTO ls_cvlc02t WITH  KEY statu = <ls_vehicles>-mmsta.
      <ls_vehicles>-mmsta_desc = ls_cvlc02t-statut.

      CLEAR ls_cvlc02t.

    ENDLOOP.

  ENDMETHOD.                    "change_edit_status


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
        rows              = 2
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    mo_split_col->set_row_height(
      EXPORTING
        id                = 1
        height             = 5
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    set_logs_visibility( cl_gui_splitter_container=>false ).
    mo_top_cnt = mo_split_col->get_container( row = 1 column = 1 ).

    CREATE OBJECT mo_top
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent          = mo_split_col->get_container( row = 2 column = 1 )
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
        id                = 2
        width             = lv_height
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

    mo_grid->check_changed_data( ).

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
        MODIFY mt_vehicles FROM ls_vehicles TRANSPORTING checkbox WHERE  mmsta = 'ZM34' AND blocked = abap_false .

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
        change_edit_status( ).

    ENDCASE.

    mo_grid->refresh_table_display( ).

  ENDMETHOD.                    "handle_user_command


  METHOD save_data.

    FIELD-SYMBOLS : <ls_vehicles> TYPE ty_vehicles.

    LOOP AT mt_vehicles ASSIGNING <ls_vehicles> WHERE checkbox IS NOT INITIAL.

      UPDATE vlcvehicle SET   vhvin   = <ls_vehicles>-vhvin
                              zengine = <ls_vehicles>-zengine
                              zdi01   = sy-datum
                        WHERE vhcle   = <ls_vehicles>-vhcle.

      UPDATE ziv_hmc_batch_it SET vhvin         = <ls_vehicles>-vhvin
                                  zengine       = <ls_vehicles>-zengine
                                  zinvoice      = <ls_vehicles>-zinvoice
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

    TYPES: BEGIN OF ty_ebeln,
      ebeln TYPE ekpo-ebeln,
      END OF ty_ebeln.

    DATA: ls_gm_header      TYPE bapi2017_gm_head_01,
          lt_gm_items       TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          ls_gm_items       TYPE  bapi2017_gm_item_create,
          lt_gm_messages    TYPE STANDARD TABLE OF bapiret2,
          ls_gm_messages    TYPE bapiret2,
          ls_po_header      TYPE bapiekkol,
          lt_po_items       TYPE STANDARD TABLE OF bapiekpo,
          lt_po_messages    TYPE STANDARD TABLE OF bapireturn,
          ls_po_messages    TYPE  bapireturn,
          ls_vehicles       LIKE LINE OF mt_vehicles,
          lv_error          TYPE flag VALUE abap_false,
          lt_vhcle          TYPE tt_vehicles,
          ls_goodsmvt_code  TYPE bapi2017_gm_code,
          lt_gm_ebeln       TYPE STANDARD TABLE OF ty_ebeln.


    FIELD-SYMBOLS: <ls_po_items> TYPE bapiekpo,
                   <ls_gm_items> TYPE bapi2017_gm_item_create.

    ms_icons = get_status_icons( ).
    ls_goodsmvt_code-gm_code = '01'.

    SELECT ebeln
      FROM ekbe
      INTO TABLE lt_gm_ebeln
      FOR ALL ENTRIES IN mt_vehicles
      WHERE ebeln = mt_vehicles-ebeln
      AND   bewtp = 'E'.

    LOOP AT mt_vehicles INTO ls_vehicles WHERE checkbox IS NOT INITIAL.

      READ TABLE lt_gm_ebeln TRANSPORTING NO FIELDS WITH KEY ebeln = ls_vehicles-ebeln.

      IF sy-subrc <> 0.


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
            iv_vhcle = ls_vehicles-vhcle
            iv_ebeln = ls_vehicles-ebeln ) = abap_true.
          lv_error = abap_true.
          CLEAR: lt_po_messages, lt_po_items.
          CONTINUE.
        ENDIF.

        LOOP AT lt_po_items ASSIGNING <ls_po_items>.

          MOVE-CORRESPONDING <ls_po_items> TO ls_gm_items.

          ls_gm_items-stge_loc       = <ls_po_items>-store_loc.
          ls_gm_items-move_type      = '101'.
          ls_gm_items-mvt_ind        = 'B'.
          ls_gm_items-stck_type      = '3'.
          ls_gm_items-entry_qnt      = <ls_po_items>-quantity.
          ls_gm_items-entry_uom      = <ls_po_items>-unit.
          ls_gm_items-batch          = ls_vehicles-vhcle.

          APPEND ls_gm_items TO lt_gm_items.
          CLEAR  ls_gm_items.

        ENDLOOP.

        CLEAR lt_po_items.

        ls_gm_header-ref_doc_no = ls_vehicles-zinvoice.
        ls_gm_header-pstng_date = sy-datum.
        ls_gm_header-doc_date   = sy-datum.
        ls_gm_header-pr_uname   = sy-uname.
        ls_gm_header-header_txt = 'GM Vehicle'.


        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            goodsmvt_header = ls_gm_header
            goodsmvt_code   = ls_goodsmvt_code
          TABLES
            goodsmvt_item   = lt_gm_items
            return          = lt_gm_messages.

        CLEAR lt_gm_items.

        IF lt_gm_messages IS INITIAL .

          ls_po_messages-type       = 'S'.

          CONCATENATE 'Good movement for PO : ' ls_vehicles-ebeln 'posted successfully'
          INTO ls_po_messages-message SEPARATED BY space.

          APPEND ls_po_messages TO lt_po_messages.
          CLEAR ls_po_messages.

        ELSE.

          LOOP AT lt_gm_messages INTO ls_gm_messages.

            ls_po_messages-type       = ls_gm_messages-type.
            ls_po_messages-code       = ls_gm_messages-id.
            ls_po_messages-log_msg_no = ls_gm_messages-log_msg_no.
            ls_po_messages-message    = ls_gm_messages-message.
            ls_po_messages-log_no     = ls_gm_messages-log_no.

            APPEND ls_po_messages TO lt_po_messages.
            CLEAR ls_po_messages.
          ENDLOOP.
        ENDIF.

        lv_error = append_logs(
                     it_logs  = lt_po_messages
                     iv_vhcle = ls_vehicles-vhcle
                     iv_ebeln = ls_vehicles-ebeln ).

        CLEAR: lt_po_messages, lt_gm_messages.

        IF lv_error = abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CONTINUE.
        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          APPEND  ls_vehicles TO lt_vhcle.


        ENDIF.
      ENDIF.

      change_status( iv_vhcle = ls_vehicles-vhcle
                        iv_ebeln = ls_vehicles-ebeln ).

    ENDLOOP.

    IF lt_vhcle IS NOT INITIAL.
      modify_vehicle_data( it_vehicles = lt_vhcle ).
    ENDIF.

    display_logs( ).

  ENDMETHOD.                    "register_good_receipt



  METHOD change_status.

    DATA: lv_tabix    TYPE i,
          ls_messages TYPE ty_logs,
          lt_msg      TYPE ziv_hmc_msg_tt,
          ls_msg      LIKE LINE OF lt_msg,
          lt_vhcle    TYPE ziv_hmc_vhcle_tt,
          ls_cvlc02t  LIKE LINE OF mt_cvlc02t,
          ls_vehicle  TYPE ty_vehicles,
          lv_error    TYPE flag.


    APPEND iv_vhcle TO lt_vhcle.

    CALL FUNCTION 'ZIV_HMC_RUN_ACTION'
      EXPORTING
        iv_action      = 'ZGRP'
        iv_vari        = 'ZGRP_BATCH'
        it_vhcle       = lt_vhcle
        iv_do_not_disp = 'X'
      IMPORTING
        et_msg         = lt_msg.


    LOOP AT lt_msg INTO ls_msg.

      lv_tabix = sy-tabix.

      CASE ls_msg-lights.
        WHEN 1.
          ls_messages-status = ms_icons-error.
          lv_error = abap_true.
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

    IF lv_error = abap_true.
      RETURN.
    ENDIF.

    READ TABLE mt_cvlc02t INTO ls_cvlc02t WITH KEY statu = 'ZM49'.

    ls_vehicle-mmsta = 'ZM49'.
    ls_vehicle-mmsta_desc = ls_cvlc02t-statut.

    MODIFY mt_vehicles FROM ls_vehicle TRANSPORTING mmsta mmsta_desc WHERE vhcle = iv_vhcle.

    mo_grid->refresh_table_display( ).

  ENDMETHOD.                    "change_status


  METHOD modify_vehicle_data.


    TYPES: BEGIN OF ty_vhcle_data,
            vhcle     TYPE vlcvehicle-vhcle,
            zgommedef TYPE vlcvehicle-zgommedef,
            matnr     TYPE vlcvehicle-matnr,
            vguid     TYPE vlcvehicle-vguid,
           END OF ty_vhcle_data,
      tt_vhcle_data TYPE STANDARD TABLE OF ty_vhcle_data.

    TYPES: BEGIN OF ty_errors,
            van    TYPE vlcvehicle-vhcle,
            tipologia(15),
            sub_tipo(2),
            vp     TYPE zsdipc_vp-zzvp,
            codice TYPE atwrt,
          END OF ty_errors,
          tt_errors  TYPE STANDARD TABLE OF ty_errors.

    DATA: lt_vhcle_range TYPE RANGE OF vlcvehicle-vhcle,
          ls_vhcle_range LIKE LINE OF lt_vhcle_range,
          ls_vhcle       TYPE ty_vehicles,
          lt_delta       TYPE STANDARD TABLE OF zcopa_delta,
          ls_delta       TYPE zcopa_delta,
          lt_dettagli    TYPE STANDARD TABLE OF zcopa_dettagli,
          ls_dettagli    TYPE zcopa_dettagli,
          ls_map_delta   TYPE zcopa_delta,
          ls_vehicle     TYPE vlcvehicle,
          lt_vhcle_data  TYPE tt_vhcle_data,
          ls_vhcle_data  TYPE ty_vhcle_data,
          lt_gomme       TYPE STANDARD TABLE OF zco_atwrt ,
          lt_ccm         TYPE STANDARD TABLE OF zco_atwrt ,
          lt_vp          TYPE STANDARD TABLE OF zco_atwrt ,
          ls_gomme       TYPE zco_atwrt ,
          lt_colori      TYPE STANDARD TABLE OF zco_atwrt ,
          ls_colori      TYPE zco_atwrt ,
          lt_cs          TYPE STANDARD TABLE OF zco_atwrt ,
          ls_cs          TYPE zco_atwrt ,
          lv_zzvp        TYPE zsdipc_vp-zzvp,
          lv_zseq        TYPE zcopa_dettagli-zseq,
          lt_errors      TYPE tt_errors,
          ls_messages    TYPE ty_logs.

    CLEAR lt_vhcle_range.

    zcl_iv_hmc_utilities=>init_mapping( iv_progname = 'ZIV_HMC_VEHICLE_UPD' ).

    zcl_iv_hmc_utilities=>map_values(
      EXPORTING
        iv_structure = 'ZCOPA_DELTA'    " Structure name
      CHANGING
        cs_map       = ls_map_delta     " Map to str
    ).

    zcl_iv_hmc_utilities=>map_values(
      EXPORTING
        iv_structure = 'VLCVEHICLE'    " Structure name
      CHANGING
        cs_map       = ls_vehicle     " Map to str
    ).

    SELECT zgommedef
           matnr
           vguid
           vhcle
    FROM vlcvehicle
      INTO CORRESPONDING FIELDS OF TABLE lt_vhcle_data
      FOR ALL ENTRIES IN it_vehicles
      WHERE vhcle = it_vehicles-vhcle.

    LOOP AT it_vehicles INTO ls_vhcle.

      CLEAR ls_vhcle_range.
      ls_vhcle_range-sign = 'I'.
      ls_vhcle_range-option = 'EQ'.
      ls_vhcle_range-low = ls_vhcle-vhcle.
      APPEND ls_vhcle_range TO lt_vhcle_range.

      UPDATE vlcvehicle  SET   loctn   = ls_vehicle-loctn
                               werks   = ls_vhcle-werks
                               lgort   = ls_vhcle-lgort
                         WHERE vhcle   = ls_vhcle-vhcle.

      CLEAR: ls_delta , ls_dettagli.

      ls_delta-vhcle         = ls_vhcle-vhcle.
      ls_delta-gjahr         = sy-datum(4).
      ls_delta-werks         = ls_vhcle-zwerks_orig.
      ls_delta-zcstvp        = ls_vhcle-zunitprice.
      ls_delta-prctr         = ls_map_delta-prctr.
      ls_delta-zzprodotto    = ls_map_delta-zzprodotto.
      ls_delta-zcstcol       = ls_vhcle-zcolor / ls_vhcle-zquantity.
      ls_delta-zcstvancong   = ls_delta-zcstvp + ls_delta-zcstcol + ls_delta-zcstruo + ls_delta-zcstcs.

      CLEAR ls_vhcle_data.

      SELECT SINGLE zzvp
      FROM zsdipc_vp
      INTO lv_zzvp
      WHERE vhcle = ls_vhcle-vhcle.

      READ TABLE lt_vhcle_data INTO ls_vhcle_data WITH KEY vhcle = ls_vhcle-vhcle.

      ls_dettagli-vhcle         = ls_vhcle-vhcle.
      ls_dettagli-gjahr         = sy-datum(4).
      ls_dettagli-zelem_van     = 'VP'.
      ls_dettagli-zseq          = '01'.
      ls_dettagli-werks         = 'C044'.
      ls_dettagli-zdate         = sy-datum.
      ls_dettagli-erdat         = sy-datum.
      ls_dettagli-zstatus       = 'D'.
      ls_dettagli-zevento_van   =  'ASSEGN'.
      ls_dettagli-zcost         = ls_delta-zcstvp.
      ls_dettagli-zcodice_elem  = lv_zzvp.

      APPEND ls_dettagli TO lt_dettagli.
      CLEAR ls_dettagli.

      CALL FUNCTION 'ZCO_COSTI_VAN'
        EXPORTING
          cod_van             = ls_vhcle_data-vhcle
          cod_ruote           = ls_vhcle_data-zgommedef
          matnr               = ls_vhcle_data-matnr
          p_vguid             = ls_vhcle_data-vguid
        TABLES
          log                 = lt_errors
          tab_cs              = lt_cs
          tab_ccm             = lt_ccm
          tab_colori          = lt_colori
          tab_vp              = lt_vp
          tab_gomme           = lt_gomme
        EXCEPTIONS
          codice_vp_not_found = 1
          instance_not_found  = 2
          OTHERS              = 3.

      LOOP AT lt_cs INTO ls_cs.

        MOVE sy-tabix TO lv_zseq.

        ls_dettagli-vhcle         = ls_vhcle-vhcle.
        ls_dettagli-gjahr         = sy-datum(4).
        ls_dettagli-zelem_van     = 'CS'.
        ls_dettagli-zseq          = lv_zseq.
        ls_dettagli-werks         = 'C044'.
        ls_dettagli-zdate         = sy-datum.
        ls_dettagli-erdat         = sy-datum.
        ls_dettagli-zstatus       = 'D'.
        ls_dettagli-zevento_van   = 'ASSEGN'.
        ls_dettagli-zcodice_elem  = ls_cs-valore.
        ls_dettagli-zcost  = 0.
        APPEND ls_dettagli TO lt_dettagli.
        CLEAR ls_dettagli.
      ENDLOOP.

      LOOP AT lt_colori INTO ls_colori.
        ls_dettagli-vhcle         = ls_vhcle-vhcle.
        ls_dettagli-gjahr         = sy-datum(4).
        ls_dettagli-zelem_van     = 'COL'.
        ls_dettagli-zseq          = '01'.
        ls_dettagli-werks         = 'C044'.
        ls_dettagli-zdate         = sy-datum.
        ls_dettagli-erdat         = sy-datum.
        ls_dettagli-zstatus       = 'D'.
        ls_dettagli-zevento_van   = 'ASSEGN'.
        ls_dettagli-zcodice_elem  = ls_colori-valore.
        ls_dettagli-zcost         = ls_delta-zcstcol.
        APPEND ls_dettagli TO lt_dettagli.
        CLEAR ls_dettagli.
      ENDLOOP.

      LOOP AT lt_gomme INTO ls_gomme.
        ls_dettagli-vhcle         = ls_vhcle-vhcle.
        ls_dettagli-gjahr         = sy-datum(4).
        ls_dettagli-zelem_van     = 'TYRE'.
        ls_dettagli-zseq          = '01'.
        ls_dettagli-werks         = 'C044'.
        ls_dettagli-zdate         = sy-datum.
        ls_dettagli-erdat         = sy-datum.
        ls_dettagli-zstatus       = 'D'.
        ls_dettagli-zevento_van   = 'ASSEGN'.
        ls_dettagli-zcodice_elem  = ls_gomme-valore.
        ls_dettagli-zcost  = 0.
        APPEND ls_dettagli TO lt_dettagli.
        CLEAR ls_dettagli.
      ENDLOOP.

      APPEND ls_delta    TO lt_delta.

    ENDLOOP.

    INSERT zcopa_delta    FROM TABLE lt_delta.
    INSERT zcopa_dettagli FROM TABLE lt_dettagli.

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
      ls_messages-vhcle   = iv_vhcle.
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
           lv_icon      TYPE string,
           ls_logs      TYPE ty_logs,
           lv_msg_no    TYPE i.

    set_logs_visibility( cl_gui_splitter_container=>true  ).

    IF mo_salv_logs IS BOUND.
      mo_salv_logs->refresh( ).
      RETURN.
    ENDIF.

    LOOP AT mt_logs INTO ls_logs .
      AT NEW ebeln.
        lv_msg_no = 1.
      ENDAT.
      ls_logs-msg_no = lv_msg_no.
      lv_msg_no = lv_msg_no + 1.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = mo_split_col->get_container( row = 2 column = 2 )
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

    DATA: lt_fcat     TYPE lvc_t_fcat,
          ls_fcat     TYPE lvc_s_fcat,
          ls_layout   TYPE lvc_s_layo,
          ls_variant  TYPE disvariant,
          lv_vehicles TYPE char1.


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
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZDEST_PORT'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_HD'.
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
         it_outtab          = mt_vehicles ).

    mo_top->initialize_document( ).
    mo_grid->list_processing_events(
     EXPORTING
       i_event_name = 'TOP_OF_PAGE'
       i_dyndoc_id  = mo_top ).

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


  METHOD top_of_page.

    DATA: lv_text  TYPE sdydo_text_element,
          lo_html  TYPE REF TO cl_gui_html_viewer.

    lv_text = lines( mt_vehicles ).

    CONDENSE lv_text NO-GAPS.

    CONCATENATE 'Vehicles' lv_text INTO lv_text SEPARATED BY space.

    mo_top->add_text_as_heading(
      EXPORTING
        text          = lv_text
        sap_fontstyle = cl_dd_document=>large
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
    lv_text = lines( mt_vehicles ).

    WRITE:  1 'Vehicles', 10 lv_text.
  ENDMETHOD.                    "print_top_of_page




ENDCLASS.                    "lcl_vehicle_update IMPLEMENTATION
