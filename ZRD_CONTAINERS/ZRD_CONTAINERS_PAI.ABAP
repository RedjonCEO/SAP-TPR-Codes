*&---------------------------------------------------------------------*
*&  Include           ZRD_CONTAINERS_PAI
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TC 'ZRD_TAB_CTRL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE zrd_tab_ctrl_modify INPUT.
  MODIFY gt_inoutab
    FROM gs_inoutab
    INDEX zrd_tab_ctrl-current_line.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  CASE gs_screen01-ok_code.
    WHEN 'FC_ENTER'.
      PERFORM check_cn.
    WHEN 'FC_EDIT'.
      PERFORM check_cn.
    WHEN 'FC_ADD'.
      APPEND INITIAL LINE TO gt_inoutab.
    WHEN 'FC_DISPLAY'.
      DELETE gt_inoutab
      WHERE matnr IS INITIAL.
      PERFORM update_display_table.
      gv_edit = abap_false.
    WHEN 'FC_SAVE'.
      PERFORM fill_database.
    WHEN 'FC_ULOAD'.
      IF gt_inoutab IS INITIAL.
        MESSAGE 'You can not upload an empty table.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM directory_upload.
      ENDIF.
    WHEN 'FC_DLOAD'.
      IF gt_inoutab IS INITIAL.
        MESSAGE 'You can not download an empty table.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM download_file.
      ENDIF.
    WHEN 'FC_EMAIL'.
      IF gt_inoutab IS INITIAL.
        MESSAGE 'You can not atach an empty table.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM atach_email.
      ENDIF.
    WHEN 'FC_CHB'.
      PERFORM checkbox_action.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE gs_screen01-ok_code.
    WHEN 'FC_BACK'.
      IF gv_edit = abap_false.
        LEAVE TO SCREEN 0.
      ELSE.
        gv_edit = abap_false.
        CLEAR gt_inoutab.
      ENDIF.
  ENDCASE.
ENDMODULE.
