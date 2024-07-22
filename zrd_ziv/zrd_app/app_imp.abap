*&---------------------------------------------------------------------*
*&  Include           ZIV_HMC_PROFORMA_APP_IMP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_approval IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_approval IMPLEMENTATION.

  METHOD execute.
    init_technical_vars( ).
    extract_data( ).

    IF mt_header_data IS  INITIAL.
      MESSAGE 'No data found'(015) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    start_containers( ).

    CALL SCREEN 100.

  ENDMETHOD.                    "execute
  METHOD start_containers.

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

    CREATE OBJECT mo_split_row
      EXPORTING
        parent            = lo_cont
        rows              = 2
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    CREATE OBJECT mo_split_col
      EXPORTING
        parent            = mo_split_row->get_container( row = 2 column = 1 )
        rows              = 1
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.


    set_split_row_visibility( cl_gui_splitter_container=>false ).

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

    CREATE OBJECT mo_proforma_grid
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

    CREATE OBJECT mo_iv_prof_grid
      EXPORTING
        i_parent          = mo_split_col->get_container( row = 1 column = 2 )
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

    display_header_tab( ).

  ENDMETHOD.                "execute

  METHOD get_icon_tooltip.

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

  ENDMETHOD.                    "get_icon_tooltip
  METHOD set_split_row_visibility.
    DATA lv_height TYPE i.

    IF iv_visible = cl_gui_splitter_container=>true.
      lv_height = 50.
    ENDIF.

    mo_split_row->set_row_height(
      EXPORTING
        id                = 2
        height            = lv_height
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    mo_split_row->set_row_sash( id    = 2
                                type  = cl_gui_splitter_container=>type_sashvisible
                                value = iv_visible ).

  ENDMETHOD.                    "set_item_visibility
  METHOD set_split_col_visibility.
    DATA lv_width TYPE i.

    IF iv_visible = cl_gui_splitter_container=>true.
      lv_width = 50.
    ENDIF.

    mo_split_col->set_column_width(
      EXPORTING
        id                = 2    " Column ID
        width             = lv_width     " NPlWidth
       EXCEPTIONS
         cntl_error        = 1
         cntl_system_error = 2
         OTHERS            = 3
    ).

    mo_split_col->set_column_sash(
      EXPORTING
        id                = 2    " Column Splitter Bar ID
        type              = cl_gui_splitter_container=>type_sashvisible    " Attribute
        value             = iv_visible    " Value
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).

  ENDMETHOD.                    "set_item_visibility
  METHOD init_technical_vars.

    ms_technical-icon_status_approved = get_icon_tooltip( iv_name = 'ICON_ALLOW'
                                                          iv_info = 'Proforma approved'(012) ).
    ms_technical-icon_status_rejected = get_icon_tooltip( iv_name = 'ICON_REJECT'
                                                          iv_info = 'Proforma rejected'(013) ).
    ms_technical-icon_status_new = get_icon_tooltip( iv_name = 'ICON_WRI'
                                                     iv_info = 'New proforma'(014) ).

  ENDMETHOD.                    "init_technical_vars

  METHOD extract_data.

    DATA: lo_autocheck TYPE REF TO zcl_iv_hmc_automatic_checks,
          ls_errors    TYPE zcl_iv_hmc_automatic_checks=>ty_errors,
          ls_prof      TYPE zcl_iv_hmc_automatic_checks=>ty_proforma.

    FIELD-SYMBOLS: <ls_hdr>  TYPE zcl_iv_hmc_automatic_checks=>ty_header.

    SELECT hd~zbatch
           hd~zprod_slot
           hd~zdest_port
           pf~zproforma
           pf~zpro_date
           pf~zpro_rec_date
           pf~zpro_upd_date
           pf~zpro_upd_uname
           pf~zcurr
           pf~zstatus
           pf~zstatus_date
           pf~zstatus_user
           pf~zstatus_comment

    FROM  ziv_hmc_batch_hd AS hd
    JOIN  ziv_hmc_prof_hd  AS pf ON pf~zbatch = hd~zbatch
    INTO CORRESPONDING FIELDS OF TABLE mt_header_data
    WHERE hd~zbatch          IN s_zbatch
    AND   hd~zprod_slot      IN s_prslot
    AND   hd~zdest_port      IN s_dsport
    AND   pf~zproforma       IN s_prof
    AND   pf~zpro_date       IN s_prdt
    AND   pf~zpro_rec_date   IN s_recdt
    AND   pf~zpro_upd_date   IN s_upddt
    AND   hd~zdeleted        = space.


    LOOP AT mt_header_data ASSIGNING <ls_hdr>.

      CREATE OBJECT lo_autocheck
        EXPORTING
          iv_proforma = <ls_hdr>-zproforma.

      lo_autocheck->execute_summary_check(
        IMPORTING
          es_errors = ls_errors
      ).

      IF ls_errors-config = abap_true.
        <ls_hdr>-zconf_check_btn = icon_red_light.
      ELSE.
        <ls_hdr>-zconf_check_btn = icon_green_light.
      ENDIF.

      IF ls_errors-qty   = abap_true.
        <ls_hdr>-zqty_check_btn = icon_red_light.
      ELSE.
        <ls_hdr>-zqty_check_btn = icon_green_light.
      ENDIF.

      IF ls_errors-price  = abap_true.
        <ls_hdr>-zprc_check_btn = icon_red_light.
      ELSE.
        <ls_hdr>-zprc_check_btn = icon_green_light.
      ENDIF.

      CASE <ls_hdr>-zstatus.
        WHEN cs_status-approved.
          <ls_hdr>-zstatus_icon = ms_technical-icon_status_approved.
        WHEN cs_status-rejected.
          <ls_hdr>-zstatus_icon = ms_technical-icon_status_rejected.
        WHEN cs_status-new.
          <ls_hdr>-zstatus_icon = ms_technical-icon_status_new.
      ENDCASE.

      <ls_hdr>-zproforma_tab = lo_autocheck->ms_proforma-prof_tab.
      <ls_hdr>-prof_iv_tab   = lo_autocheck->mt_iv_proforma.

      ls_prof-zcurr = <ls_hdr>-zcurr.
      MODIFY <ls_hdr>-zproforma_tab FROM ls_prof
      TRANSPORTING zcurr WHERE zcurr <> ls_prof-zcurr.

    ENDLOOP.

  ENDMETHOD.                    "extract_data

  METHOD handle_button_click.

    FIELD-SYMBOLS: <ls_hdr>  TYPE zcl_iv_hmc_automatic_checks=>ty_header.

    READ TABLE mt_header_data ASSIGNING <ls_hdr> INDEX es_row_no-row_id.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ms_technical-last_row = es_row_no-row_id
      AND ms_technical-last_col = es_col_id-fieldname.
      RETURN.
    ENDIF.

    ms_technical-last_row = es_row_no-row_id.
    ms_technical-last_col = es_col_id-fieldname.

    CASE es_col_id-fieldname.
      WHEN 'ZCONF_CHECK_BTN' OR 'ZQTY_CHECK_BTN'.

        set_split_col_visibility( iv_visible = cl_gui_splitter_container=>false ).

        IF <ls_hdr>-technical-read_conf_qty IS INITIAL.
          <ls_hdr>-technical-read_conf_qty = abap_true.

          <ls_hdr>-check_conf_qty =
          zcl_iv_hmc_automatic_checks=>check_conf_qty(
              it_iveco    = <ls_hdr>-prof_iv_tab
              it_hyundai  = <ls_hdr>-zproforma_tab ).
        ENDIF.

        CASE es_col_id-fieldname.
          WHEN 'ZCONF_CHECK_BTN' .
            display_config_tab(
            CHANGING
              ct_zconfig_tab = <ls_hdr>-check_conf_qty ).

          WHEN 'ZQTY_CHECK_BTN' .
            display_quantity_tab(
             CHANGING
              ct_zquantity_tab = <ls_hdr>-check_conf_qty ).
        ENDCASE.

      WHEN 'ZPRC_CHECK_BTN'.
        set_split_col_visibility( iv_visible = cl_gui_splitter_container=>false ).

        IF <ls_hdr>-technical-read_price IS INITIAL.
          <ls_hdr>-technical-read_price = abap_true.

          <ls_hdr>-check_price =
          zcl_iv_hmc_automatic_checks=>check_price(
              it_iveco    = <ls_hdr>-prof_iv_tab
              it_hyundai  = <ls_hdr>-zproforma_tab ).
        ENDIF.

        display_price_tab(
        CHANGING
          ct_zprice_tab = <ls_hdr>-check_price ).

      WHEN 'ZPROFORMA'.

        set_split_col_visibility( iv_visible = cl_gui_splitter_container=>true ).

        display_proforma_tab(
        EXPORTING
          iv_source        = 'HMC'
        CHANGING
          ct_zproforma_tab = <ls_hdr>-zproforma_tab ).

        display_proforma_tab(
        EXPORTING
          iv_source        = 'IV'
        CHANGING
          ct_zproforma_tab = <ls_hdr>-prof_iv_tab ).

    ENDCASE.

    set_split_row_visibility( iv_visible = cl_gui_splitter_container=>true ).

  ENDMETHOD.                    "handle_button_click


  METHOD handle_grid_toolbar.

    DATA ls_toolbar TYPE stb_button.

    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar .
    CLEAR ls_toolbar.

    ls_toolbar-function  = 'APPROVE'.
    ls_toolbar-icon      = icon_allow.
    ls_toolbar-text      = 'Approve'(011).
    ls_toolbar-quickinfo = 'Approve'(011).

    APPEND ls_toolbar TO e_object->mt_toolbar .
    CLEAR ls_toolbar.

    ls_toolbar-function  = 'REJECT'.
    ls_toolbar-icon      = icon_reject.
    ls_toolbar-text      = 'Reject'(010).
    ls_toolbar-quickinfo = 'Reject'(010).

    APPEND ls_toolbar TO e_object->mt_toolbar .
    CLEAR ls_toolbar.

  ENDMETHOD.                    "handle_grid_toolbar

  METHOD handle_grid_proforma_toolbar.

    DATA ls_toolbar TYPE stb_button.

    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar .
    CLEAR ls_toolbar.

    ls_toolbar-function  = 'CLOSE'.
    ls_toolbar-icon      = icon_close.
    ls_toolbar-quickinfo = 'Close'(009).

    APPEND ls_toolbar TO e_object->mt_toolbar .
    CLEAR ls_toolbar.

  ENDMETHOD.                    "handle_grid_proforma_toolbar


  METHOD handle_proforma_ucomm.

    CASE e_ucomm.
      WHEN 'CLOSE'.
        set_split_row_visibility( iv_visible = cl_gui_splitter_container=>false ).
    ENDCASE.


  ENDMETHOD.                    "handle_hdr_ucomm
  METHOD handle_hdr_ucomm.

    CASE e_ucomm.
      WHEN 'APPROVE' OR 'REJECT'.
        approve_reject_proforma( iv_ucomm = e_ucomm ).
    ENDCASE.

  ENDMETHOD.                    "handle_grid_user_command
  METHOD approve_reject_proforma.

    DATA: lt_rows           TYPE lvc_t_row,
          ls_rows           TYPE lvc_s_row,
          ls_header_data    TYPE zcl_iv_hmc_automatic_checks=>ty_header,
          lv_answer         TYPE char1,
          lv_action         TYPE vlcbatchact-action,
          lv_execvari       TYPE vlcbatchact-execvari,
          lv_check_failed   TYPE flag,
          lv_error          TYPE flag,
          lt_proforma       TYPE ziv_tt_proforma,
          ls_proforma       TYPE ziv_ty_proforma,
          lv_value          TYPE string .

    FIELD-SYMBOLS <ls_header_data> TYPE zcl_iv_hmc_automatic_checks=>ty_header.

    mo_grid->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rows ).

    IF lt_rows IS INITIAL.
      MESSAGE 'Select at least one proforma for approval'(008) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_rows INTO ls_rows.

      READ TABLE mt_header_data INTO ls_header_data INDEX ls_rows-index.

      IF ls_header_data-zstatus IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CASE icon_red_light.
        WHEN ls_header_data-zconf_check_btn
          OR ls_header_data-zqty_check_btn
          OR ls_header_data-zprc_check_btn.
          lv_check_failed = abap_true.
      ENDCASE.

      APPEND ls_header_data-zproforma TO lt_proforma.
    ENDLOOP.

    IF lt_proforma IS INITIAL.
      MESSAGE 'Select at least 1 proforma that has not been approved or rejected'(007)
       TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lv_check_failed IS NOT INITIAL.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Warning   '(004)
          text_question         = 'Warning. One of the automatic checks has failed. You want to proceed?'(003)
          text_button_1         = 'Continue'(005)
          icon_button_1         = 'ICON_EXECUTE_OBJECT'
          text_button_2         = 'Cancel'(006)
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF lv_answer <> 1.
        RETURN.
      ENDIF.

    ENDIF.

    CASE iv_ucomm.
      WHEN 'APPROVE'.
        lv_action     = 'ZPAP' .
        lv_execvari   = 'ZPAP_BATCH'.

      WHEN 'REJECT'.
        lv_action     = 'ZPRJ' .
        lv_execvari   = 'ZPRJ_BATCH'.
    ENDCASE.

    CALL FUNCTION 'POPUP_GET_STRING'
      EXPORTING
        label = 'Please enter comment'(002)
      IMPORTING
        value = lv_value.

    IF lv_value IS NOT INITIAL.
      ls_proforma-zstatus_comment = lv_value.
      MODIFY lt_proforma FROM ls_proforma TRANSPORTING zstatus_comment
       WHERE zstatus_comment IS INITIAL.
    ENDIF.

    CALL FUNCTION 'ZIV_HMC_PROCESS_ACTION_AP_RJ'
      EXPORTING
        iv_action   = lv_action
        iv_vari     = lv_execvari
      IMPORTING
        ev_error    = lv_error
      CHANGING
        ct_proforma = lt_proforma.

    IF lv_error = abap_true.
      RETURN.
    ENDIF.

    LOOP AT lt_proforma INTO ls_proforma.
      READ TABLE mt_header_data ASSIGNING <ls_header_data> WITH KEY zproforma = ls_proforma-zproforma.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      MOVE-CORRESPONDING ls_proforma TO <ls_header_data>.

      CASE ls_proforma-zstatus.
        WHEN 'A'.
          <ls_header_data>-zstatus_icon = ms_technical-icon_status_approved.
        WHEN 'R'.
          <ls_header_data>-zstatus_icon = ms_technical-icon_status_rejected.
        WHEN OTHERS.
          <ls_header_data>-zstatus_icon = ms_technical-icon_status_new.
      ENDCASE.

    ENDLOOP.

    mo_grid->refresh_table_display( ).

  ENDMETHOD.                    "approve_proforma
  METHOD display_header_tab.

    DATA: lt_fcat    TYPE lvc_t_fcat,
          ls_fcat    TYPE lvc_s_fcat,
          ls_layout  TYPE lvc_s_layo,
          ls_variant TYPE disvariant.

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

    ls_fcat-fieldname = 'ZPROFORMA'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    ls_fcat-style     = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZPRO_DATE'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZPRO_REC_DATE'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZPRO_UPD_UNAME'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZPRO_UPD_DATE'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname  = 'ZSTATUS_ICON'.
    ls_fcat-reptext    = 'Status'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname  = 'ZSTATUS_DATE'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZSTATUS_USER'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZSTATUS_COMMENT'.
    ls_fcat-ref_table = 'ZIV_HMC_PROF_HD'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZCONF_CHECK_BTN'.
    ls_fcat-scrtext_m = 'Configuration check'.
    ls_fcat-icon      = abap_true.
    ls_fcat-style     = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZQTY_CHECK_BTN'.
    ls_fcat-scrtext_m = 'Quantity check'.
    ls_fcat-icon      = abap_true.
    ls_fcat-style     = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZPRC_CHECK_BTN'.
    ls_fcat-scrtext_m = 'Price check'.
    ls_fcat-icon      = abap_true.
    ls_fcat-style     = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_layout-sel_mode    = 'D'.
    ls_layout-col_opt     = abap_true.
    ls_layout-cwidth_opt  = abap_true.

    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.

    SET HANDLER: handle_button_click
                 handle_hdr_ucomm
                 handle_grid_toolbar FOR mo_grid.

    mo_grid->set_table_for_first_display(
    EXPORTING
      i_save             = 'X'
      is_layout          = ls_layout
      is_variant         = ls_variant
    CHANGING
      it_fieldcatalog    = lt_fcat
      it_outtab          = mt_header_data ).

  ENDMETHOD.                    "display_header_TAB

  METHOD display_proforma_tab.

    DATA: lt_fcat        TYPE lvc_t_fcat,
          ls_fcat        TYPE lvc_s_fcat,
          ls_layout      TYPE lvc_s_layo,
          ls_variant     TYPE disvariant,
          lv_seltext     TYPE char40,
          ls_header_data TYPE zcl_iv_hmc_automatic_checks=>ty_header,
          lo_grid        TYPE REF TO cl_gui_alv_grid,
          lt_sort        TYPE lvc_t_sort,
          ls_sort        TYPE lvc_s_sort.

    ls_fcat-fieldname = 'ZHMC_SPEC_CODE'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZHMC_COLOR'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZHMC_TYRES'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname  = 'ZQUANTITY'.
    ls_fcat-coltext    = 'Quantity'.
    ls_fcat-decimals_o = '0'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZBASIC'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZOPT'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZFOB'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZFREIGHT'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZINS'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZUNITPRICE'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZSUM'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZCOLOR'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname  = 'ZTOTAL'.
    ls_fcat-ref_table  = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    CASE iv_source.
      WHEN 'IV'.
        lo_grid = mo_iv_prof_grid.
        ls_layout-grid_title = 'IVECO:'.
      WHEN OTHERS.
        lo_grid = mo_proforma_grid.
        ls_layout-grid_title = 'HMC:'.
    ENDCASE.

    READ TABLE mt_header_data INTO ls_header_data INDEX ms_technical-last_row.
    CONCATENATE ls_layout-grid_title 'Pro Forma nr.' ls_header_data-zproforma
       INTO ls_layout-grid_title SEPARATED BY space.

    ls_layout-sel_mode    = 'D'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-cwidth_opt  = abap_true.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.

    SET HANDLER: handle_grid_proforma_toolbar
                 handle_proforma_ucomm FOR lo_grid.

    CLEAR: ls_sort , lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 1.
    ls_sort-fieldname = 'ZHMC_SPEC_CODE'.
    APPEND ls_sort TO lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 2.
    ls_sort-fieldname = 'ZHMC_COLOR'.
    APPEND ls_sort TO lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 3.
    ls_sort-fieldname = 'ZHMC_TYRES'.
    APPEND ls_sort TO lt_sort.

    lo_grid->set_table_for_first_display(
    EXPORTING
      i_save             = 'X'
      is_layout          = ls_layout
      is_variant         = ls_variant
    CHANGING
      it_fieldcatalog    = lt_fcat
      it_outtab          = ct_zproforma_tab
      it_sort            = lt_sort ).

  ENDMETHOD.                    "display_price_tab
  METHOD display_price_tab.

    DATA: lt_fcat        TYPE lvc_t_fcat,
          ls_fcat        TYPE lvc_s_fcat,
          ls_layout      TYPE lvc_s_layo,
          ls_variant     TYPE disvariant,
          lv_seltext     TYPE char40,
          ls_header_data TYPE zcl_iv_hmc_automatic_checks=>ty_header,
          lt_sort        TYPE lvc_t_sort,
          ls_sort        TYPE lvc_s_sort.

    ls_fcat-fieldname = 'COMBO_KEY'.
    ls_fcat-no_out    = abap_true.
    ls_fcat-key       = abap_true.
    ls_fcat-icon      = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZHMC_SPEC_CODE'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZHMC_COLOR'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZHMC_TYRES'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'SOURCE'.
    ls_fcat-reptext   = 'Source'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname  = 'ZQUANTITY'.
    ls_fcat-coltext    = 'Quantity'.
    ls_fcat-decimals_o = '0'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZBASIC'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZOPT'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZFOB'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZFREIGHT'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZINS'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-coltext   = 'Insurance'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZUNITPRICE'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZSUM'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'ZCOLOR'.
    ls_fcat-ref_table = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname  = 'ZTOTAL'.
    ls_fcat-ref_table  = 'ZIV_HMC_PROFORMA'.
    ls_fcat-cfieldname = 'ZCURR'.
    ls_fcat-do_sum     = abap_true.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.


    READ TABLE mt_header_data INTO ls_header_data INDEX ms_technical-last_row.

    CONCATENATE 'Price check for Pro Forma nr.'(018) ls_header_data-zproforma
           INTO ls_layout-grid_title SEPARATED BY space.


    ls_layout-sel_mode    = 'D'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-cwidth_opt  = abap_true.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.

    ls_sort-subtot    = abap_true.
    ls_sort-comp      = abap_true.
*    ls_sort-up        = abap_true.
    ls_sort-expa      = abap_true.
*    ls_sort-spos      = 1.
    ls_sort-fieldname = 'COMBO_KEY'.
    APPEND ls_sort TO lt_sort.
    CLEAR ls_sort.

    ls_sort-spos      = 1.
    ls_sort-fieldname = 'ZHMC_SPEC_CODE'.
    APPEND ls_sort TO lt_sort.
    ls_sort-spos      = 2.
    ls_sort-fieldname = 'ZHMC_COLOR'.
    APPEND ls_sort TO lt_sort.
    ls_sort-spos      = 3.
    ls_sort-fieldname = 'ZHMC_TYRES'.
    APPEND ls_sort TO lt_sort.

    SET HANDLER: handle_grid_proforma_toolbar
                 handle_proforma_ucomm FOR mo_proforma_grid.

*    sort  ct_zprice_tab by combo_key+5(195) ASCENDING.

    mo_proforma_grid->set_table_for_first_display(
    EXPORTING
      i_save             = 'X'
      is_layout          = ls_layout
      is_variant         = ls_variant
    CHANGING
      it_fieldcatalog    = lt_fcat
      it_outtab          = ct_zprice_tab
      it_sort            = lt_sort
      ).

  ENDMETHOD.                    "display_price_tab


  METHOD display_quantity_tab.

    DATA: lt_fcat        TYPE lvc_t_fcat,
          ls_fcat        TYPE lvc_s_fcat,
          ls_layout      TYPE lvc_s_layo,
          ls_variant     TYPE disvariant,
          lv_seltext     TYPE char40,
          ls_header_data TYPE zcl_iv_hmc_automatic_checks=>ty_header,
          lt_sort        TYPE lvc_t_sort,
          ls_sort        TYPE lvc_s_sort.

    lt_fcat = get_fcat_config( ).

    ls_fcat-fieldname = 'IV_QUANT'.
    ls_fcat-reptext   = 'Batch Quantity'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'HM_QUANT'.
    ls_fcat-reptext   = 'Pro forma Quantity'.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'DELTA'.
    ls_fcat-reptext   = 'DELTA'.
    ls_fcat-icon      = abap_true.
    ls_fcat-style     = 1.
    APPEND ls_fcat TO lt_fcat.
    CLEAR ls_fcat.


    READ TABLE mt_header_data INTO ls_header_data INDEX ms_technical-last_row.

    CONCATENATE 'Quantity check for Pro Forma nr.'(019) ls_header_data-zproforma
           INTO ls_layout-grid_title SEPARATED BY space.


    ls_layout-sel_mode    = 'D'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-cwidth_opt  = abap_true.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.


    CLEAR: ls_sort , lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 1.
    ls_sort-fieldname = 'SPEC_CODE'.
    APPEND ls_sort TO lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 2.
    ls_sort-fieldname = 'COLOR'.
    APPEND ls_sort TO lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 3.
    ls_sort-fieldname = 'TYRES'.
    APPEND ls_sort TO lt_sort.

    SET HANDLER: handle_grid_proforma_toolbar
                 handle_proforma_ucomm FOR mo_proforma_grid.

    mo_proforma_grid->set_table_for_first_display(
    EXPORTING
      i_save             = 'X'
      is_layout          = ls_layout
      is_variant         = ls_variant
    CHANGING
      it_fieldcatalog    = lt_fcat
      it_outtab          = ct_zquantity_tab
      it_sort            = lt_sort ).
  ENDMETHOD.                    "display_quantity_tab

  METHOD get_fcat_config.
    DATA ls_fcat        TYPE lvc_s_fcat.

    ls_fcat-fieldname = 'SPEC_CODE'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    ls_fcat-ref_field = 'ZHMC_SPEC_CODE'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO rt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'COLOR'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    ls_fcat-ref_field = 'ZHMC_COLOR'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO rt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'TYRES'.
    ls_fcat-ref_table = 'ZIV_HMC_BATCH_IT'.
    ls_fcat-ref_field = 'ZHMC_TYRES'.
    ls_fcat-key       = abap_true.
    APPEND ls_fcat TO rt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname = 'IVECO_FOUND'.
    ls_fcat-reptext    = 'IVECO'.
    ls_fcat-icon       = abap_true.
    APPEND ls_fcat TO rt_fcat.
    CLEAR ls_fcat.

    ls_fcat-fieldname  = 'HUND_FOUND'.
    ls_fcat-reptext    = 'HYUNDAI'.
    ls_fcat-icon       = abap_true.
    APPEND ls_fcat TO rt_fcat.
    CLEAR ls_fcat.

  ENDMETHOD.                    "get_fcat_config
  METHOD display_config_tab.

    DATA: lt_fcat        TYPE lvc_t_fcat,
          ls_layout      TYPE lvc_s_layo,
          ls_variant     TYPE disvariant,
          ls_header_data TYPE zcl_iv_hmc_automatic_checks=>ty_header,
          lt_sort        TYPE lvc_t_sort,
          ls_sort        TYPE lvc_s_sort.

    lt_fcat = get_fcat_config( ).

    READ TABLE mt_header_data INTO ls_header_data INDEX ms_technical-last_row.

    CONCATENATE 'Configuration check for Pro Forma nr.'(020) ls_header_data-zproforma
           INTO ls_layout-grid_title SEPARATED BY space.

    ls_layout-sel_mode    = 'D'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-cwidth_opt  = abap_true.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.

    SET HANDLER: handle_grid_proforma_toolbar
                 handle_proforma_ucomm FOR mo_proforma_grid.

    CLEAR: ls_sort , lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 1.
    ls_sort-fieldname = 'SPEC_CODE'.
    APPEND ls_sort TO lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 2.
    ls_sort-fieldname = 'COLOR'.
    APPEND ls_sort TO lt_sort.

    ls_sort-up        = abap_true.
    ls_sort-spos      = 3.
    ls_sort-fieldname = 'TYRES'.
    APPEND ls_sort TO lt_sort.

    mo_proforma_grid->set_table_for_first_display(
    EXPORTING
      i_save             = 'X'
      is_layout          = ls_layout
      is_variant         = ls_variant
    CHANGING
      it_fieldcatalog    = lt_fcat
      it_outtab          = ct_zconfig_tab
      it_sort            = lt_sort ).

  ENDMETHOD.                    "display_config_tab


ENDCLASS.                    "lcl_approval IMPLEMENTATION
