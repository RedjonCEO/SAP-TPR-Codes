*&---------------------------------------------------------------------*
*&  Include           ZRD_CONTAINERS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  EDIT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_table .
  CASE gv_edit.

    WHEN abap_true.
      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'GS_INOUTAB-MATNR'
          OR   'GS_INOUTAB-QUANT'
          OR   'GS_INOUTAB-DEL'
          OR   'GS_INOUTAB-SHIFT'.
            screen-input = 1.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_DATABASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_database .
  TYPES:BEGIN OF lty_tabs,
          matnr TYPE matnr,
          count TYPE i,
        END OF lty_tabs.

  DATA:ls_inoutab     TYPE zrd_finaldt,
       ls_temptab     TYPE zrd_finaldt,
       lt_inoutab     TYPE TABLE OF zrd_finaldt,
       lt_temptab     TYPE TABLE OF zrd_finaldt,
       lv_maktx       TYPE maktx,
       lv_continue    TYPE abap_bool VALUE abap_true,
       lv_matnr       TYPE matnr,
       lv_matnr2      TYPE matnr,
       lv_matnr3      TYPE matnr,
       lv_answ(1)     TYPE c,
       lv_count       TYPE i,
       lv_cn          TYPE zrd_cn,
       lt_tabs        TYPE STANDARD TABLE OF lty_tabs,
       lt_tabs2       TYPE STANDARD TABLE OF lty_tabs,
       ls_tabs2       TYPE lty_tabs,
       ls_tabs        TYPE lty_tabs,
       lv_count_matnr TYPE i,
       lv_matnr_code  TYPE c LENGTH 25,
       lv_repeat      TYPE abap_bool VALUE abap_false.


  LOOP AT gt_inoutab INTO gs_inoutab .
    lv_matnr2 = gs_inoutab-matnr.
    lv_matnr = |{ gs_inoutab-matnr ALPHA = IN }|.
    SELECT SINGLE matnr FROM makt
    INTO lv_matnr
    WHERE  matnr = lv_matnr.
    IF sy-subrc <> 0.
      lv_continue = abap_false.
      MESSAGE |Insertion Failed. We can not find material with code number: { lv_matnr2 }| TYPE 'I'.
    ENDIF.
  ENDLOOP.

  IF lv_continue = abap_true.
    LOOP AT gt_inoutab INTO gs_inoutab.
      IF gs_inoutab-matnr IS NOT INITIAL.

        IF gs_inoutab-dats is INITIAL.
          ls_inoutab-dats = sy-datum.
        ELSE.
          ls_inoutab-dats = gs_inoutab-dats.
        ENDIF.

        lv_matnr = |{ gs_inoutab-matnr ALPHA = IN }|.
        ls_inoutab-cn = gs_screen01-cn.
        ls_inoutab-mandt = sy-mandt.
        ls_inoutab-matnr = gs_inoutab-matnr.
        ls_inoutab-shift = gs_inoutab-shift.
        ls_inoutab-quant = gs_inoutab-quant.
        ls_inoutab-del = gs_inoutab-del.

        ls_inoutab-usern = sy-uname.

        SELECT SINGLE maktx FROM makt
        INTO lv_maktx
        WHERE matnr = lv_matnr
        AND spras = sy-langu.
        ls_inoutab-maktx = lv_maktx.

        APPEND ls_inoutab TO lt_inoutab.
      ENDIF.
    ENDLOOP.
    "===========================================================================================

    SORT lt_inoutab BY matnr ASCENDING.
    APPEND LINES OF lt_inoutab TO lt_temptab.

    LOOP AT lt_temptab INTO ls_temptab.
      AT NEW matnr.
        lv_matnr3 = ls_temptab-matnr.
        LOOP AT lt_inoutab INTO ls_inoutab WHERE matnr = lv_matnr3.
          lv_count = lv_count + 1.
        ENDLOOP.
        APPEND VALUE #( matnr = lv_matnr3 count = lv_count ) TO lt_tabs.
        lv_count = 0.
      ENDAT.
    ENDLOOP.

    LOOP AT lt_tabs INTO ls_tabs.
      IF ls_tabs-count > 1.
        APPEND VALUE #( matnr = ls_tabs-matnr count = ls_tabs-count ) TO lt_tabs2.
        lv_repeat = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_repeat = abap_true.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'INFORMATION'
          text_question         = 'One or more material numbers entered are repeated. Choose your action.'
          text_button_1         = 'Update'
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = 'Keep old'
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = ' '
          popup_type            = 'ICON_MESSAGE_INFORMATION'
        IMPORTING
          answer                = lv_answ
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      LOOP AT lt_tabs2 INTO ls_tabs2.
        IF lv_answ = 1.
          LOOP AT lt_temptab INTO ls_temptab.
            IF ls_temptab-matnr = ls_tabs2-matnr.
              lv_count_matnr = lv_count_matnr + 1.
              IF lv_count_matnr <> ls_tabs2-count.
                DELETE lt_temptab INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          LOOP AT lt_temptab INTO ls_temptab.
            IF ls_temptab-matnr = ls_tabs2-matnr.
              lv_count_matnr = lv_count_matnr + 1.
              IF lv_count_matnr <> 1.
                DELETE lt_temptab INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        CLEAR lv_count_matnr.
      ENDLOOP.
      CLEAR lv_answ.
      lv_repeat = abap_false.
    ENDIF.



    "===========================================================================================
    MODIFY zrd_finaldt FROM TABLE lt_temptab.
    SELECT * FROM zrd_finaldt
    INTO TABLE gt_inoutab
    WHERE cn = gs_screen01-cn
    AND del = abap_false.
    MESSAGE 'Data modified successfully.' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ANSW  text
*----------------------------------------------------------------------*
FORM check_cn.
  DATA: lt_table   TYPE TABLE OF zrd_finaldt,
        lv_answ(1) TYPE c,
        lv_cn TYPE zrd_cn.
  IF gs_screen01-cn < 1.
    MESSAGE 'Container number can not be 0 (zero) or negative.' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
  SELECT * FROM zrd_finaldt
  INTO CORRESPONDING FIELDS OF TABLE lt_table
  WHERE cn = gs_screen01-cn
  AND del = abap_false.

  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'INFORMATION'
        text_question         = 'There is no data for the container. Do you want to create entries?'
        text_button_1         = 'Yes'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'No'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
        popup_type            = 'ICON_MESSAGE_INFORMATION'
      IMPORTING
        answer                = lv_answ
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF lv_answ = 1.
      gv_edit = abap_true.
      APPEND INITIAL LINE TO gt_inoutab.
    ENDIF.

  ELSE.

    SELECT SINGLE cn
    FROM zrd_finaldt
    INTO lv_cn
    WHERE cn = gs_screen01-cn.
    IF lv_cn IS NOT INITIAL.
      CASE gs_screen01-ok_code.
        WHEN 'FC_EDIT'.
          gv_edit = abap_true.
        WHEN 'FC_ENTER'.
          gv_edit = abap_false.
      ENDCASE.
    LOOP AT SCREEN.
      IF p_chb = 'X'.
        SELECT * FROM zrd_finaldt
        INTO TABLE gt_inoutab
        WHERE cn = gs_screen01-cn.
      ELSE.
        SELECT * FROM zrd_finaldt
        INTO TABLE gt_inoutab
        WHERE cn = gs_screen01-cn
        AND del = abap_false.
      ENDIF.
    ENDLOOP.
    ENDIF.
  ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DISPLAY_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_display_table .
  LOOP AT SCREEN.
    IF p_chb = 'X'.
      SELECT * FROM zrd_finaldt
      INTO TABLE gt_inoutab
      WHERE cn = gs_screen01-cn.
    ELSE.
      SELECT * FROM zrd_finaldt
      INTO TABLE gt_inoutab
      WHERE cn = gs_screen01-cn
      AND del = abap_false.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_file.

  TYPES: BEGIN OF lty_structure,
           cn    TYPE zrd_finaldt-cn,
           matnr TYPE zrd_finaldt-matnr,
           maktx TYPE zrd_finaldt-maktx,
           quant TYPE zrd_finaldt-quant,
           shift TYPE c LENGTH 10,
           dats  TYPE zrd_finaldt-dats,
           usern TYPE zrd_finaldt-usern,
           del   TYPE zrd_finaldt-del,
         END OF lty_structure.

  DATA: lt_inoutab TYPE STANDARD TABLE OF lty_structure,
        ls_inoutab TYPE lty_structure.
  MOVE-CORRESPONDING gt_inoutab TO lt_inoutab.

  LOOP AT lt_inoutab INTO ls_inoutab.
    CASE ls_inoutab-shift.
      WHEN '1'.
        ls_inoutab-shift = 'Morning'.
      WHEN '2'.
        ls_inoutab-shift = 'Afternoon'.
      WHEN '3'.
        ls_inoutab-shift = 'Evening'.
    ENDCASE.
    MODIFY lt_inoutab FROM ls_inoutab.
  ENDLOOP.

  APPEND 'Container Number' TO ls_header.
  APPEND 'Material Number' TO ls_header.
  APPEND 'Material Description' TO ls_header.
  APPEND 'Quantity' TO ls_header.
  APPEND 'Shift' TO ls_header.
  APPEND 'Date' TO ls_header.
  APPEND 'User Name' TO ls_header.
  APPEND 'Deleted' TO ls_header.

  DATA: lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fullpath TYPE string,
        lv_result   TYPE i,
        lv_default  TYPE string,
        lv_fname    TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'File Directory'
      default_extension = 'XLS'
      initial_directory = 'C:\Users\Redjon\Desktop\'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath
      user_action       = lv_result.

  lv_fname = lv_fullpath.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize            = ''
      filename                = lv_fname
      filetype                = 'DAT'
    TABLES
      data_tab                = lt_inoutab
      fieldnames              = ls_header
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DIRECTORY_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM directory_upload .

  DATA: lv_filename TYPE string VALUE './ZRD_Container_Temp.csv',
        lv_string   TYPE string,
        lv_quant    TYPE string,
        lv_shift    TYPE string,
        lv_date     TYPE string.


  OPEN DATASET lv_filename
  FOR OUTPUT IN TEXT MODE
  ENCODING DEFAULT.

  CONCATENATE   'Container Number' 'Material Number' 'Material Description'
 'Qunatity' 'Shift' 'Date' 'Username' 'Deleted'
  INTO lv_string SEPARATED BY ','.
  TRANSFER lv_string TO lv_filename.

  LOOP AT gt_inoutab INTO DATA(gs_inoutab2).
    CONCATENATE gs_inoutab2-dats+6(2) gs_inoutab2-dats+4(2)
    gs_inoutab2-dats+0(4) INTO lv_date SEPARATED BY '.'.
    MOVE gs_inoutab2-quant TO lv_quant.
    CASE gs_inoutab2-shift.
      WHEN '1'.
        lv_shift = 'Morning'.
      WHEN '2'.
        lv_shift = 'Afternoon'.
      WHEN '3'.
        lv_shift = 'Evening'.
    ENDCASE.
    CONCATENATE
        gs_inoutab2-cn  gs_inoutab2-matnr
       gs_inoutab2-maktx  lv_quant lv_shift
       lv_date  gs_inoutab2-usern  gs_inoutab2-del
    INTO lv_string
    SEPARATED BY ','.
    TRANSFER lv_string TO lv_filename.
  ENDLOOP.
  CLOSE DATASET lv_filename.
  MESSAGE 'Data has been uploaded successfully' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECKBOX_ACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM checkbox_action .
  LOOP AT SCREEN.
    IF p_chb = 'X' AND gs_screen01-cn IS NOT INITIAL.
      SELECT * FROM zrd_finaldt
      INTO TABLE gt_inoutab
      WHERE cn = gs_screen01-cn.
      MESSAGE 'Deleted data visible.' TYPE 'S'.
    ELSEIF p_chb = '' AND gs_screen01-cn IS NOT INITIAL.
      SELECT * FROM zrd_finaldt
      INTO TABLE gt_inoutab
      WHERE cn = gs_screen01-cn
      AND del = abap_false.
      MESSAGE 'Deleted data unvisible.' TYPE 'S'.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATACH_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atach_email .


  DATA: lv_string       TYPE string,
        lv_string2      TYPE string,
        lv_data_string  TYPE string,
        lv_xstring      TYPE xstring,
        lt_bin          TYPE solix_tab,
        lt_body         TYPE soli_tab,
        ls_body         TYPE soli,
        lv_quant        TYPE string,
        lv_shift        TYPE string,
        lv_count        TYPE i,
        lv_titulli      TYPE so_obj_des,
        lv_file_title   TYPE so_obj_des,
        lv_container    TYPE string,
        lv_count_string TYPE string,
        lv_username     TYPE string,
        lv_date         TYPE string.

  CONCATENATE  'Container Number' 'Material Number' 'Material Description'
  'Quantity' 'Shift' 'Date' 'Username' 'Deleted'
  INTO lv_string2 SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

  LOOP AT gt_inoutab ASSIGNING FIELD-SYMBOL(<gs_tab>).
    CONCATENATE <gs_tab>-dats+6(2) <gs_tab>-dats+4(2)
    <gs_tab>-dats+0(4) INTO lv_date SEPARATED BY '.'.
    lv_count = lv_count + 1.
    MOVE <gs_tab>-quant TO lv_quant.
    CASE <gs_tab>-shift.
      WHEN '1'.
        lv_shift = 'Morning'.
      WHEN '2'.
        lv_shift = 'Afternoon'.
      WHEN '3'.
        lv_shift = 'Evening'.
    ENDCASE.
    CONCATENATE  <gs_tab>-cn <gs_tab>-matnr <gs_tab>-maktx lv_quant
    lv_shift lv_date <gs_tab>-usern <gs_tab>-del
    INTO lv_string SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    CONCATENATE lv_string lv_data_string INTO lv_data_string
    SEPARATED BY cl_abap_char_utilities=>newline.
  ENDLOOP.
  CONCATENATE lv_string2 lv_data_string INTO lv_data_string
  SEPARATED BY cl_abap_char_utilities=>newline.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_data_string
    IMPORTING
      buffer = lv_xstring.


  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lv_xstring
    TABLES
      binary_tab = lt_bin.

  CONCATENATE sy-uname ',' INTO lv_username.
  MOVE gs_screen01-cn TO lv_container.
  CONCATENATE 'Container' lv_container INTO lv_file_title SEPARATED BY ' '.
  CONCATENATE 'Hello' lv_username INTO ls_body-line SEPARATED BY ' '.
  APPEND ls_body TO lt_body.
  CLEAR ls_body.
  APPEND INITIAL LINE TO lt_body.
  CONCATENATE 'Please find attached all the materials for' lv_container 'container'
  INTO ls_body-line SEPARATED BY ' '.
  APPEND ls_body TO lt_body.
  APPEND INITIAL LINE TO lt_body.
  ls_body-line = 'Best Regards,'.
  APPEND ls_body TO lt_body.
  ls_body-line = 'SAP Team "'.
  APPEND ls_body TO lt_body.


  MOVE lv_count TO lv_count_string.
  CONCATENATE lv_count_string 'Material Number' INTO lv_titulli
  SEPARATED BY ' '.

  TRY.
      CALL METHOD cl_document_bcs=>create_document
        EXPORTING
          i_type    = 'RAW'
          i_subject = lv_titulli
          i_text    = lt_body
        RECEIVING
          result    = DATA(lo_doc).
    CATCH cx_document_bcs.
  ENDTRY.


  TRY.
      CALL METHOD lo_doc->add_attachment
        EXPORTING
          i_attachment_type    = 'xls'
          i_attachment_subject = lv_file_title
          i_att_content_hex    = lt_bin.
    CATCH cx_document_bcs.
  ENDTRY.


  TRY.
      CALL METHOD cl_bcs=>create_persistent
        RECEIVING
          result = DATA(lo_send_request).
    CATCH cx_send_req_bcs.
  ENDTRY.

  TRY.
      CALL METHOD lo_send_request->set_document
        EXPORTING
          i_document = lo_doc.
    CATCH cx_send_req_bcs.
  ENDTRY.


  TRY.
      CALL METHOD cl_cam_address_bcs=>create_internet_address
        EXPORTING
          i_address_string = 'STUDENT STUDENT'
        RECEIVING
          result           = DATA(lo_sender).
    CATCH cx_address_bcs.
  ENDTRY.

  TRY.
      CALL METHOD cl_cam_address_bcs=>create_internet_address
        EXPORTING
          i_address_string = 'redjon.dedej@outlook.com'
        RECEIVING
          result           = DATA(lo_recipient).
    CATCH cx_address_bcs.
  ENDTRY.

  TRY.
      CALL METHOD lo_send_request->set_sender
        EXPORTING
          i_sender = lo_sender.
    CATCH cx_send_req_bcs.
  ENDTRY.

  TRY.
      CALL METHOD lo_send_request->add_recipient
        EXPORTING
          i_recipient = lo_recipient
          i_express   = abap_true.
    CATCH cx_send_req_bcs.
  ENDTRY.

  TRY.
      CALL METHOD lo_send_request->send
        RECEIVING
          result = DATA(lv_status).
    CATCH cx_send_req_bcs.
  ENDTRY.

  COMMIT WORK.


  MESSAGE 'Email sent successfully.' TYPE 'S'.



ENDFORM.
