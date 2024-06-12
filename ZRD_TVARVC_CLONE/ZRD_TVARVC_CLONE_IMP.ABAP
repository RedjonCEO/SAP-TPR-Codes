*&---------------------------------------------------------------------*
*&  Include           ZRD_TVARVC_CLONE_IMP
*&---------------------------------------------------------------------*

CLASS lcl_tvarvc_clone IMPLEMENTATION.

  METHOD execute.
    create_fieldcatalog( ).
    retrieve_data( ).
    CALL SCREEN 1.
  ENDMETHOD.


  METHOD retrieve_data.

    SELECT name low AS value
    FROM tvarvc
    INTO TABLE mt_param_table
    WHERE type = 'P'
    AND name LIKE 'ZFT%'.

    SELECT name opti AS option low AS low_limit high AS upp_limit sign
    FROM tvarvc
    INTO CORRESPONDING FIELDS OF TABLE mt_sel_table
    WHERE type = 'S'
    AND name LIKE 'ZFT%'.

    DELETE mt_sel_table WHERE name IS INITIAL.
    set_icons( ).

  ENDMETHOD.


  METHOD set_icons.

    DATA: lv_multi     TYPE icons-text,
          lv_option    TYPE icons-text,
          lv_icon_name TYPE char255.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = 'ICON_ENTER_MORE'
      IMPORTING
        result                = lv_multi
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.

    LOOP AT mt_sel_table ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-multi_sel = lv_multi.

      set_option_icon(
      EXPORTING
        iv_opti = CONV char2( <fs>-option )
        iv_inex = <fs>-sign
      CHANGING
        cv_result = lv_option ).

      IF lv_option IS INITIAL.
        CONTINUE.
      ENDIF.

      <fs>-option = lv_option.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_fieldcatalog.

    mt_fcat_p = VALUE #( ( fieldname = 'NAME'       scrtext_m = 'Name' outputlen = 38 )
                         ( fieldname = 'VALUE'      scrtext_m = 'Value' outputlen = 47 )
                         ( fieldname = 'CASE_SENS'  scrtext_m = 'Case-Sensitive' outputlen = 2 checkbox = 'X'  ) ).

    mt_fcat_s = VALUE #( ( fieldname = 'NAME'       scrtext_m = 'Name' outputlen = 32 )
                         ( fieldname = 'OPTION'     scrtext_m = 'Option'  hotspot = 'X' outputlen = 2 icon = 'X' style = 1  )
                         ( fieldname = 'LOW_LIMIT'  scrtext_m = 'Lower limit' outputlen = 21 )
                         ( fieldname = 'UPP_LIMIT'  scrtext_m = 'Upper limit' outputlen = 21 )
                         ( fieldname = 'MULTI_SEL'  scrtext_m = 'Multi selection' hotspot = 'X' outputlen = 3 icon = 'X' style = 1 )
                         ( fieldname = 'CASE_SENS'  scrtext_m = 'Case-Sensitive' outputlen = 2 checkbox = 'X' )
                         ( fieldname = 'INTERVAL'   scrtext_m = 'Interval check' outputlen = 2 checkbox = 'X' ) ).
  ENDMETHOD.


  METHOD save_edits.





    DATA lt_temp_param TYPE tvarvc.

    IF gv_tabstrip = 'S'.

      LOOP AT mt_sel_change ASSIGNING FIELD-SYMBOL(<fs_check_names>).
        IF <fs_check_names>-name(3) <> 'ZFT' .
          MESSAGE | You can only insert values that start with 'ZFT' | TYPE 'I'.
          CLEAR <fs_check_names>-option.
          RETURN.
        ENDIF.
      ENDLOOP.


*      SELECT  * FROM tvarvc
*      INTO TABLE @DATA(lt_check_record)
*      FOR ALL ENTRIES IN @mt_sel_change
*      WHERE name = @mt_sel_change-name.





    ENDIF.
  ENDMETHOD.


  METHOD on_param_data_changed.

    FIELD-SYMBOLS: <lt_data> TYPE tt_parameters.
    ASSIGN er_data_changed->mp_mod_rows->* TO <lt_data>.
    mt_param_change = <lt_data>.

  ENDMETHOD.


  METHOD on_sel_data_changed.

    FIELD-SYMBOLS: <lt_data_sel> TYPE tt_sel_options.
    ASSIGN er_data_changed->mp_mod_rows->* TO <lt_data_sel>.
    mt_sel_change = <lt_data_sel>.

  ENDMETHOD.


  METHOD handle_grid_toolbar.

    DELETE e_object->mt_toolbar WHERE butn_type <> 0.
    DELETE e_object->mt_toolbar WHERE icon = icon_create.

    APPEND VALUE #( function  = 'CREATE' icon      = icon_create
                     quickinfo = 'Create' ) TO e_object->mt_toolbar .

    IF gv_edit_mode = abap_false.
      CLEAR e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.


  METHOD handle_grid_user_command.
    DATA: lv_rows      TYPE lvc_t_row,
          lv_stable    TYPE lvc_s_stbl,
          lv_result    TYPE icons-text,
          ls_sel_table LIKE LINE OF mt_sel_table.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = 'ICON_ENTER_MORE'
      IMPORTING
        result                = lv_result
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.

    ls_sel_table-multi_sel = lv_result.

    CASE e_ucomm.
      WHEN 'CREATE'.
        IF gv_tabstrip = 'P'.
          APPEND INITIAL LINE TO mt_param_table.
          mo_grid_param->refresh_table_display( is_stable = lv_stable ).
        ELSE.
          APPEND ls_sel_table TO mt_sel_table.
          mo_grid_selop->refresh_table_display( is_stable = lv_stable ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD on_hotspot_click.

    DATA sel_val TYPE RANGE OF tvarvc-low.

    DATA: lv_optionlist TYPE rsoptions,
          lv_tabix      LIKE sy-tabix,
          lv_title      LIKE sy-title,
          lt_tvarvc     TYPE tvarvc,
          lv_icon_name  TYPE char255,
          lv_result     TYPE icons-text,
          lv_result2    TYPE icons-text,
          lt_range_tab  TYPE range,
          lv_stable     TYPE lvc_s_stbl VALUE 'X'.

    CASE e_column_id.
      WHEN 'OPTION'.

        lv_optionlist = 'XXXXXXXXXX'.
        CALL FUNCTION 'SELECT_OPTION_OPTIONS'
          EXPORTING
            selctext     = lv_title
            option_list  = lv_optionlist
          IMPORTING
            sign         = lt_tvarvc-sign
            option       = lt_tvarvc-opti
          EXCEPTIONS
            not_executed = 1
            delete_line  = 3
            OTHERS       = 2.

        set_option_icon(
        EXPORTING
          iv_opti = lt_tvarvc-opti
          iv_inex = lt_tvarvc-sign
        CHANGING
          cv_result = lv_result ).

        IF gv_edit_mode = abap_true.
          READ TABLE mt_sel_table INDEX e_row_id ASSIGNING FIELD-SYMBOL(<fs_operator_row>).
          <fs_operator_row>-option = lv_result.
        ENDIF.

      WHEN 'MULTI_SEL'.

        CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
          EXPORTING
            title             = lv_title
            no_interval_check = 'X'
          TABLES
            range             = sel_val
          EXCEPTIONS
            cancelled         = 1
            OTHERS            = 2.

        IF sel_val IS NOT INITIAL.

          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              name                  = 'ICON_DISPLAY_MORE'
            IMPORTING
              result                = lv_result2
            EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.

          IF gv_edit_mode = abap_true.
            READ TABLE mt_sel_table INDEX e_row_id ASSIGNING <fs_operator_row>.
            <fs_operator_row>-multi_sel = lv_result2.
          ENDIF.
        ENDIF.

    ENDCASE.
    mo_grid_selop->refresh_table_display( is_stable = lv_stable ).
  ENDMETHOD.


  METHOD set_option_icon.

    DATA lv_icon_name TYPE char255.

    IF iv_inex = 'I'.
      DATA(lv_color) = 'GREEN'.
    ELSEIF iv_inex = 'E'.
      lv_color = 'RED'.
    ENDIF.

    CASE iv_opti.
      WHEN 'EQ'.
        CONCATENATE 'ICON_EQUAL_' lv_color INTO lv_icon_name.
      WHEN 'NE'.
        CONCATENATE 'ICON_NOT_EQUAL_' lv_color INTO lv_icon_name.
      WHEN 'GT'.
        CONCATENATE 'ICON_GREATER_' lv_color INTO lv_icon_name.
      WHEN 'LT'.
        CONCATENATE 'ICON_LESS_' lv_color INTO lv_icon_name.
      WHEN 'GE'.
        CONCATENATE 'ICON_GREATER_EQUAL_' lv_color INTO lv_icon_name.
      WHEN 'LE'.
        CONCATENATE 'ICON_LESS_EQUAL_' lv_color INTO lv_icon_name.
      WHEN 'BT'.
        CONCATENATE 'ICON_INTERVAL_INCLUDE_' lv_color INTO lv_icon_name.
      WHEN 'NB'.
        CONCATENATE 'ICON_INTERVAL_EXCLUDE_' lv_color INTO lv_icon_name.
      WHEN 'CP'.
        CONCATENATE 'ICON_PATTERN_INCLUDE_' lv_color INTO lv_icon_name.
      WHEN 'NP'.
        CONCATENATE 'ICON_PATTERN_EXCLUDE_' lv_color INTO lv_icon_name.
    ENDCASE.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = lv_icon_name
      IMPORTING
        result                = cv_result
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
  ENDMETHOD.

ENDCLASS.
