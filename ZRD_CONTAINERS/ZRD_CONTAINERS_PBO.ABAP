*&---------------------------------------------------------------------*
*&  Include           ZRD_CONTAINERS_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  gv_exclude-fcode = 'FC_DISPLAY'.
  APPEND gv_exclude.
  gv_exclude-fcode = 'FC_SAVE'.
  APPEND gv_exclude.

  CASE gv_edit.
    WHEN abap_false.
      SET PF-STATUS 'PF' EXCLUDING gv_exclude.
      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'GS_SCREEN01-ADDBTN'.
            screen-active = 0.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
    WHEN abap_true.
      SET PF-STATUS 'PF' EXCLUDING 'FC_EDIT'.
  ENDCASE.
  SET TITLEBAR 'TB'.
  PERFORM edit_table.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'ZRD_TAB_CTRL'. DO NOT CHANGE THIS LINE
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE zrd_tab_ctrl_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_inoutab LINES zrd_tab_ctrl-lines.
ENDMODULE.
