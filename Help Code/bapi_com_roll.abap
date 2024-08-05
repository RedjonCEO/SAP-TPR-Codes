      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header = ls_gm_header
          goodsmvt_code   = ls_goodsmvt_code
        TABLES
          goodsmvt_item   = lt_gm_items
          return          = lt_gm_messages.

      CLEAR lt_gm_items.

      LOOP AT lt_gm_messages INTO ls_gm_messages.

        ls_po_messages-type       = ls_gm_messages-type.
        ls_po_messages-code       = ls_gm_messages-id.
        ls_po_messages-log_msg_no = ls_gm_messages-log_msg_no.
        ls_po_messages-message    = ls_gm_messages-message.
        ls_po_messages-log_no     = ls_gm_messages-log_no.

        APPEND ls_po_messages TO lt_po_messages.
        CLEAR ls_po_messages.
      ENDLOOP.

      lv_error = append_logs(
                   it_logs  = lt_po_messages
                   iv_vhcle = ls_vehicles-vhcle
                   iv_ebeln = ls_vehicles-ebeln ).

      CLEAR: lt_po_messages, lt_gm_messages.

      IF lv_error = abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        APPEND  ls_vehicles TO lt_vhcle.

        change_status( iv_vhcle = ls_vehicles-vhcle
                       iv_ebeln = ls_vehicles-ebeln ).

      ENDIF.
