  METHOD handle_user_command.

    DATA: lt_selected_rows TYPE lvc_t_row.

    CASE e_ucomm.

      WHEN 'FC_ADD_ROW'.

        APPEND INITIAL LINE TO mt_weight.

      WHEN 'FC_DELETE_ROW'.

        mo_grid->get_selected_rows(
          IMPORTING
            et_index_rows =   lt_selected_rows  ).

        LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected_row>).

          READ TABLE mt_weight ASSIGNING FIELD-SYMBOL(<ls_weight>) INDEX <ls_selected_row>-index.
          IF sy-subrc = 0.
            <ls_weight>-selected = abap_true.
          ENDIF.

        ENDLOOP.

        DELETE mt_weight WHERE selected = abap_true.

      WHEN 'FC_DELETE_ALL'.

        CLEAR mt_weight.

    ENDCASE.

    mo_grid->refresh_table_display( ).


  ENDMETHOD.

  METHOD handle_toolbar.
    DATA: ls_toolbar TYPE stb_button.

    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
        ( function = 'FC_ADD_ROW'       icon = icon_insert_row   quickinfo = 'Insert new row' )
        ( function = 'FC_DELETE_ROW'    icon = icon_delete_row   quickinfo = 'Delete selected rows' )
        ( function = 'FC_DELETE_ALL'    icon = icon_delete       quickinfo = 'Delete all rows' ) ).

  ENDMETHOD.    "handle_toolbar
