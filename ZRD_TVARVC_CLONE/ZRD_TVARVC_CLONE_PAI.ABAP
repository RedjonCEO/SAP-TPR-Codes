*&---------------------------------------------------------------------*
*&  Include           ZRD_TVARVC_CLONE_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA: BEGIN OF gs_screen01,
          ok_code TYPE sy-ucomm,
        END OF gs_screen01.

  CASE gs_screen01-ok_code.
    WHEN 'SAVE'.
      go_tvarvc_clone->save_edits( ).
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'MODE'.
      IF gv_edit_mode = abap_false.

        CALL FUNCTION 'ENQUEUE_E_TABLE'
          EXPORTING
            mode_rstable   = 'E'
            tabname        = 'TVARVC'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0.
          gv_edit_mode = abap_true.
        ELSE.
          MESSAGE 'Table entry locked at present' TYPE 'I'.
        ENDIF.

      ELSE.

        CALL FUNCTION 'DEQUEUE_E_TABLE'
          EXPORTING
            mode_rstable = 'E'
            tabname      = 'TVARVC'.

        gv_edit_mode = abap_false.

      ENDIF.
    WHEN 'ZTABSTRIP_FC1'.
      gv_tabstrip = 'P'.
    WHEN 'ZTABSTRIP_FC2'.
      gv_tabstrip = 'S'.
  ENDCASE.

ENDMODULE.
