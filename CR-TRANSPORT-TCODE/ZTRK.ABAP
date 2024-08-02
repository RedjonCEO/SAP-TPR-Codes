*&---------------------------------------------------------------------*
*& Report  ztrk_cr_syst
*&---------------------------------------------------------------------*
REPORT  ztrk_cr_syst MESSAGE-ID zrk.

TABLES: usr21, adrp.

TYPES: BEGIN OF ty_fieldname_r_s,
          sign TYPE raldb_sign,
          option TYPE raldb_opti,
          low  TYPE fieldname,
          high TYPE fieldname,
         END OF ty_fieldname_r_s,
         ty_fieldname_r_t TYPE TABLE OF ty_fieldname_r_s.

TYPES: BEGIN OF ty_zdzfieldx_r_s,
          sign   TYPE raldb_sign,
          option TYPE raldb_opti,
          low    TYPE zdzfield1,
          high   TYPE zdzfield1,
         END OF ty_zdzfieldx_r_s,
         ty_zdzfieldx_r_t TYPE TABLE OF ty_zdzfieldx_r_s.
TYPES: BEGIN OF ty_zdzcts_r_s,
          sign   TYPE raldb_sign,
          option TYPE raldb_opti,
          low    TYPE zdzbccts,
          high   TYPE zdzbccts,
         END OF ty_zdzcts_r_s,
         ty_zdzcts_r_t TYPE TABLE OF ty_zdzcts_r_s.

TYPES: BEGIN OF ty_trkorr_r_s,
          sign TYPE raldb_sign,
          option TYPE raldb_opti,
          low  TYPE trkorr,
          high TYPE trkorr,
         END OF ty_trkorr_r_s,
         ty_trkorr_r_t TYPE TABLE OF ty_trkorr_r_s.

TYPES: BEGIN OF ty_fld_4_selop_s,
          reftab      TYPE tabname,
          returnfield	TYPE fieldname,
          text        TYPE txt25,
          matchcode   TYPE shlpname,
         END OF ty_fld_4_selop_s,
         ty_fld_4_selop_t TYPE TABLE OF ty_fld_4_selop_s.

TYPES: BEGIN OF g_type_s_test,
         amount  TYPE i,
         repid   TYPE syrepid,
         display TYPE i,
         dynamic TYPE sap_bool,
       END OF g_type_s_test.

TYPES:   BEGIN OF g_outtab_line.
        INCLUDE TYPE zstrklayalv.
TYPES:  icon.
TYPES:  loekz.
TYPES:  korrdev(4)  TYPE c.
TYPES:  END OF g_outtab_line,
        gt_outtab_line TYPE TABLE OF g_outtab_line.

TYPES: BEGIN OF ty_outtab_in_alv_s.
        INCLUDE TYPE g_outtab_line.
TYPES: checked_item TYPE xfeld,
       END OF ty_outtab_in_alv_s,
       ty_outtab_in_alv_t TYPE TABLE OF ty_outtab_in_alv_s.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    DATA: a_tab_outtab_int TYPE ty_outtab_in_alv_t,
          a_obj_tree_event TYPE REF TO cl_salv_events_tree,
          a_str_index_show TYPE ztrksystalv_show.

    METHODS:
      constructor IMPORTING !pt_outtab_im TYPE gt_outtab_line
                            !po_tree_events_im TYPE REF TO cl_salv_events_tree
                            !ps_index_show_im TYPE ztrksystalv_show,
      on_double_click FOR EVENT if_salv_events_tree~double_click OF cl_salv_events_tree
                      IMPORTING node_key columnname,
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function,
      on_raise_checkbox_change FOR EVENT if_salv_events_tree~checkbox_change OF cl_salv_events_tree
                               IMPORTING node_key	columnname checked.
  PRIVATE SECTION.
    METHODS: register_events,
             show_popup_select_opt IMPORTING !ps_index_show_im TYPE ztrksystalv_show
                                   EXPORTING pt_sel_opt_val_ch TYPE ztrk_dyn_sel_opt_values_t.
ENDCLASS.                    "lcl_handle_events DEFINITION

* Local Field Symbols
FIELD-SYMBOLS: <data_to_show_s> TYPE any,
               <data_to_show_t> TYPE STANDARD TABLE.

DATA: BEGIN OF tb_e070 OCCURS 0.
        INCLUDE STRUCTURE e070.
DATA:   zstatcr LIKE zstrklayalv-zstatcr,
        as4text LIKE e07t-as4text,
        zsystem LIKE zstrklayalv-zsystem,
        loekz   LIKE ztzbctrkcr_next-zloekz,
        zindidng LIKE zstrklayalv-zindidng,
        ctsproject LIKE ztzbctrkcr_st-ctsproject,
        zaction LIKE ztzbctrkcr_st-zaction,
        tarclient LIKE ztzbctrkcr_st-tarclient,
        srccli    TYPE trclient,
        comsys    TYPE tmssysnam,
        bypass    TYPE xfeld,
        rejected  TYPE xfeld,
        sysid_bypassed TYPE sy-sysid,
        zweekly_tran   TYPE ztzbctrkcr_st-zweekly_tran,
        zinfo_nrt TYPE ztzbctrkcr_st-zinfo_nrt,
        zfield1   TYPE ztzbctrkcr_st-zfield1,
        zfield2   TYPE ztzbctrkcr_st-zfield2,
        zfield3   TYPE ztzbctrkcr_st-zfield3,
        zfield4   TYPE ztzbctrkcr_st-zfield4,
        zfield5   TYPE ztzbctrkcr_st-zfield5.
DATA: END OF tb_e070.

DATA: xsystems  TYPE ztzbctrksys OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF tb_crtrasp OCCURS 1000.
        INCLUDE STRUCTURE zstrklayalv.
DATA: END OF tb_crtrasp.

DATA: BEGIN OF tb_crs1 OCCURS 0.
        INCLUDE STRUCTURE ztzbctrkcr_st.
DATA: END OF tb_crs1.
FIELD-SYMBOLS: <fs_crs1> LIKE LINE OF tb_crs1.

DATA: BEGIN OF tb_next OCCURS 0.
        INCLUDE STRUCTURE ztzbctrkcr_st.
DATA: END OF tb_next.
FIELD-SYMBOLS: <fs_next> LIKE LINE OF tb_next.

DATA: va_zrfc   LIKE ztzbctrkmainc-zrfc,
      va_zrfc1  LIKE ztzbctrkmainc-zrfc,
      lv_system_mainc  TYPE zsystem,
*      va_as4text LIKE e07t-as4text,
      fl_errzrfc,
      lv_ctsproject LIKE zstrklayalv-ctsproject,
      lv_error,
      lv_seqchange,
      va_errtxt LIKE zstrklayalv-zstconn.

DATA: lt_options             TYPE TABLE OF rfc_db_opt,
      ls_options             TYPE string,
      lt_data                TYPE TABLE OF tab512,
      ls_data                LIKE LINE OF lt_data,
      lt_fields              TYPE TABLE OF rfc_db_fld,
      ls_fields              LIKE LINE OF lt_fields.
*      lv_weeklydate(50),
*      lv_weeklydate_date     LIKE sy-datum.

TYPE-POOLS: icon, triwb.

CONSTANTS: gc_true  TYPE sap_bool VALUE 'X',
           ca_tabname_weekly  TYPE tabname VALUE 'ZTZBCTRK_WEEKLY',
           ca_tabname_e070    TYPE tabname VALUE 'E070',

           BEGIN OF gc_s_display,
             tree       TYPE i VALUE 1,
             fullscreen TYPE i VALUE 2,
           END   OF gc_s_display.

DATA: BEGIN OF ls_zstrklayalv.
        INCLUDE STRUCTURE zstrklayalv.
DATA: icon.
DATA: END OF ls_zstrklayalv.

TYPES: BEGIN OF ty_zstrklayalv_s.
        INCLUDE STRUCTURE zstrklayalv.
TYPES: icon,
END OF ty_zstrklayalv_s.

DATA: gs_test TYPE g_type_s_test.

DATA: gt_outtab     TYPE gt_outtab_line.
DATA: gs_outtab     TYPE g_outtab_line,
      gs_indez_show TYPE ztrksystalv_show,
      gt_select_values TYPE ztrk_dyn_sel_opt_values_t,
      gs_select_values TYPE ztrk_dyn_sel_opt_values_s.

DATA: gr_tree   TYPE REF TO cl_salv_tree,
      gr_container TYPE REF TO cl_gui_custom_container,
      gr_events TYPE REF TO lcl_handle_events.

DATA: lv_system   TYPE tmssysnam,
      lv_client   TYPE trclient,
      lv_ucomm    LIKE sy-ucomm,
      lv_trkorr   TYPE trkorr.
data: w_text type NATXT. "12-06-2023 NEW ENHANCEMENTS
DATA: ok_code TYPE syucomm.

DATA: fl_ko.   "ins RM 02.11.2020 - Check Append

CONSTANTS:  c_default_user      TYPE syuname VALUE 'DEFAULT',
            c_struct_index_show TYPE tabname VALUE 'ZTRKSYSTALV_SHOW'.

TYPE-POOLS: icon.
*MC- TB - 23.04.14 - Aggiunto Ranges per SET user
RANGES: r_user FOR syst-uname,
        r_user_ne FOR syst-uname.

*----------------------------------------------------------------------*
* SELECTION-SCREEN - for demonstration purposes only                   *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK dsp WITH FRAME.
PARAMETERS: p_full DEFAULT 'X' NO-DISPLAY.
*p_tree   RADIOBUTTON GROUP dsp.
SELECTION-SCREEN END OF BLOCK dsp.
*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
*** MC - TB 23.07.14 - Aggiunto controllo SET per user
  PERFORM range_user.
*  gs_test-amount = p_amount.
  gs_test-repid = sy-repid.
  CASE gc_true.
*    WHEN p_tree.
*      gs_test-display = gc_s_display-tree.
    WHEN p_full.
      gs_test-display = gc_s_display-fullscreen.
  ENDCASE.
*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  SELECT SINGLE zsystem INTO lv_system_mainc  FROM ztzbctrkmainc.
  RANGES r_user1    FOR ztzbctrkcr_log-as4user.
  FREE r_user1.
  TABLES setleaf.
  SELECT * FROM setleaf WHERE setname = 'ZTR_USER'.
    MOVE: setleaf-valsign   TO r_user1-sign,
          setleaf-valoption TO r_user1-option,
          setleaf-valfrom   TO r_user1-low,
          setleaf-valto     TO r_user1-high.
    APPEND r_user1.
  ENDSELECT.

  CASE gs_test-display.
    WHEN gc_s_display-fullscreen.
      PERFORM display_fullscreen.
    WHEN gc_s_display-tree.
      PERFORM display_tree.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
FORM select_data.

*... §2.2 select the data from the data table into a local internal table
  DATA: lt_outtab LIKE gt_outtab.
  DATA: lv_zid(10).

  DATA: lt_outtab_t LIKE gt_outtab,
        lt_outtab_g LIKE gt_outtab,
        ls_outtab   TYPE g_outtab_line,
        ls_outtab_s TYPE g_outtab_line.

  "DATA: BEGIN OF ls_outtab.
  "        INCLUDE STRUCTURE zstrklayalv.
  "DATA  korrdev(4)   TYPE n.
  "DATA: END OF ls_outtab.


  FIELD-SYMBOLS: <fs_outtab> LIKE ls_outtab,
                 <fs_outtab_t> LIKE ls_outtab,
                <fs_e070> LIKE tb_e070.

  DATA: t_outtab LIKE ls_outtab OCCURS 1000 WITH HEADER LINE.

* SELECT * FROM ztzbctrksys INTO CORRESPONDING FIELDS OF TABLE lt_outtab_t.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.
  IF sy-subrc = 0.

    CALL FUNCTION 'ZTZBCTRK_READ_SYST'
      DESTINATION va_zrfc
      EXPORTING
        no_bypass             = 'X'
      TABLES
        t_outtab              = t_outtab
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e11 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
    UNASSIGN <fs_outtab>.
    LOOP AT t_outtab ASSIGNING <fs_outtab>.
      MOVE-CORRESPONDING <fs_outtab> TO ls_outtab.
      MOVE-CORRESPONDING <fs_outtab> TO xsystems.
      APPEND <fs_outtab> TO lt_outtab_g.
      APPEND xsystems.
    ENDLOOP.

    FREE t_outtab.

    CALL FUNCTION 'ZTZBCTRK_READ_SYST'
      DESTINATION va_zrfc
      TABLES
        t_outtab              = t_outtab
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e11 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
    UNASSIGN <fs_outtab>.
    LOOP AT t_outtab ASSIGNING <fs_outtab>.
      MOVE-CORRESPONDING <fs_outtab> TO ls_outtab.
      MOVE-CORRESPONDING <fs_outtab> TO xsystems.
      APPEND <fs_outtab> TO lt_outtab_t.
      APPEND xsystems.
    ENDLOOP.


    LOOP AT lt_outtab_t ASSIGNING <fs_outtab_t>.
      lv_zid = <fs_outtab_t>-zid.
      CONCATENATE <fs_outtab_t>-zid '->' " ls_outtab-zsystem
       INTO <fs_outtab_t>-znode SEPARATED BY space.

      PERFORM retrive_korr USING <fs_outtab_t>-zsystem
                                 <fs_outtab_t>-zclient.

      UNASSIGN <fs_e070>.
      LOOP AT tb_e070 ASSIGNING <fs_e070>.
        ls_outtab-zsystem = <fs_outtab_t>-zsystem.
        ls_outtab-zclient = <fs_outtab_t>-zclient.
        ls_outtab-zid = lv_zid.
        ls_outtab-trkorr = <fs_e070>-trkorr.
        ls_outtab-as4user = <fs_e070>-as4user.
        ls_outtab-zstatcr = <fs_e070>-zstatcr.
        ls_outtab-tarclient  = <fs_e070>-tarclient.
        CONCATENATE ls_outtab-zid '->' ls_outtab-zclient '->' ls_outtab-zstatcr
               INTO ls_outtab-znode SEPARATED BY space.
        ls_outtab-ctsproject = <fs_e070>-ctsproject.
        ls_outtab-as4text = <fs_e070>-as4text.
        ls_outtab-trlayer = <fs_e070>-tarsystem.
        ls_outtab-comsys  = <fs_e070>-comsys.
        ls_outtab-srccli  = <fs_e070>-srccli.
        ls_outtab-zfield1 = <fs_e070>-zfield1.
        ls_outtab-zfield2 = <fs_e070>-zfield2.
        ls_outtab-zfield3 = <fs_e070>-zfield3.
        ls_outtab-zfield4 = <fs_e070>-zfield4.
        ls_outtab-zfield5 = <fs_e070>-zfield5.
        APPEND ls_outtab TO lt_outtab.
      ENDLOOP.
    ENDLOOP.

    PERFORM get_crs.

    UNASSIGN <fs_e070>.
    LOOP AT tb_e070 ASSIGNING <fs_e070>.
      CLEAR: ls_outtab.
      READ TABLE lt_outtab_g INTO ls_outtab_s WITH KEY zsystem = <fs_e070>-zsystem.
      ls_outtab-trkorr          = <fs_e070>-trkorr.
      ls_outtab-zid             = ls_outtab_s-zid.
      ls_outtab-as4user         = <fs_e070>-as4user.
      ls_outtab-zstatcr         = <fs_e070>-zstatcr.
      ls_outtab-zsystem         = <fs_e070>-zsystem.
      ls_outtab-zloekz          = <fs_e070>-loekz.
      ls_outtab-zindidng        = <fs_e070>-zindidng.
      ls_outtab-rejected        = <fs_e070>-rejected.
      ls_outtab-ctsproject      = <fs_e070>-ctsproject.
      ls_outtab-tarclient       = <fs_e070>-tarclient.
      ls_outtab-zaction         = <fs_e070>-zaction .
      ls_outtab-korrdev         = <fs_e070>-korrdev.
      ls_outtab-zsyst_bypassed  = <fs_e070>-sysid_bypassed.
      ls_outtab-comsys          = <fs_e070>-comsys.
      ls_outtab-srccli          = <fs_e070>-srccli.
      ls_outtab-zweekly_tran    = <fs_e070>-zweekly_tran.
      ls_outtab-zinfo_nrt       = <fs_e070>-zinfo_nrt.
      ls_outtab-zfield1         = <fs_e070>-zfield1.
      ls_outtab-zfield2         = <fs_e070>-zfield2.
      ls_outtab-zfield3         = <fs_e070>-zfield3.
      ls_outtab-zfield4         = <fs_e070>-zfield4.
      ls_outtab-zfield5         = <fs_e070>-zfield5.
      CLEAR: ls_outtab_s.
      READ TABLE lt_outtab_g INTO ls_outtab_s WITH KEY zsystem = <fs_e070>-zsystem.
      CONCATENATE ls_outtab_s-zid '->' ls_outtab-tarclient '->' ls_outtab-zstatcr
             INTO ls_outtab-znode SEPARATED BY space.

*      CLEAR va_as4text.
*      SELECT SINGLE as4text INTO va_as4text FROM e07t
*             WHERE trkorr = ls_outtab-trkorr AND
*                   langu = sy-langu.
*      IF sy-subrc >< 0.
*        SELECT SINGLE as4text INTO va_as4text FROM e07t
*                   WHERE trkorr = ls_outtab-trkorr.
*      ENDIF.

      ls_outtab-as4text = <fs_e070>-as4text.
      APPEND ls_outtab TO lt_outtab.
    ENDLOOP.

*   SORT lt_outtab BY zid zstatcr znode ctsproject zaction korrdev. "P.A.
    SORT lt_outtab BY zstatcr znode ctsproject zaction korrdev. "P.A.

*******************************************
*SORT zid 'YAD' 'URA' 'UNA' 'ENA'. "P.A.
*******************************************

    PERFORM supply_data USING lt_outtab.
  ENDIF.
ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  display_fullscreen
*&---------------------------------------------------------------------*
FORM display_fullscreen .

*... §1 create an ALV tree
  TRY.
      cl_salv_tree=>factory(
        IMPORTING
          r_salv_tree = gr_tree
        CHANGING
          t_table      = gt_outtab ).

    CATCH cx_salv_no_new_data_allowed cx_salv_error.
      EXIT.
  ENDTRY.

  PERFORM create_tree.

  AUTHORITY-CHECK OBJECT 'ZTRK_ADM'
           ID 'ACTVT' FIELD '06'.
  IF sy-subrc NE 0.
    gr_tree->set_screen_status(
      pfstatus      =  'SALV_STANDARD_R'
      report        =  gs_test-repid
      set_functions =  gr_tree->c_functions_all ).
  ELSE.
    gr_tree->set_screen_status(
      pfstatus      =  'SALV_STANDARD'
      report        =  gs_test-repid
      set_functions =  gr_tree->c_functions_all ).
  ENDIF.
  DATA: lr_functions TYPE REF TO cl_salv_functions_tree.
  lr_functions = gr_tree->get_functions( ).
  lr_functions->set_group_print( abap_false ).

**   IF SY-UNAME = 'GF10030' or sy-uname = 'TO4GM' or "M.O. 22-09-2022 TOGLIERE
**      SY-UNAME = 'F31236A' OR SY-UNAME = 'F39147A'. "M.O. 22-09-2022 TOGLIERE
  TRY. "M.O. 22-09-2022
     CALL METHOD lr_functions->set_function "M.O. 22-09-2022
          EXPORTING                        "M.O. 22-09-2022
            name    = '&REJECTTRP'         "M.O. 22-09-2022
            boolean = space.               "M.O. 22-09-2022
      CATCH cx_salv_not_found .            "M.O. 22-09-2022
      CATCH cx_salv_wrong_call .           "M.O. 22-09-2022
    ENDTRY.                                "M.O. 22-09-2022
     TRY. "M.O. 22-09-2022
     CALL METHOD lr_functions->set_function "M.O. 22-09-2022
          EXPORTING                        "M.O. 22-09-2022
            name    = '&SHOW'             "M.O. 22-09-2022
            boolean = space.               "M.O. 22-09-2022
      CATCH cx_salv_not_found .            "M.O. 22-09-2022
      CATCH cx_salv_wrong_call .           "M.O. 22-09-2022
    ENDTRY.                                "M.O. 22-09-2022

     TRY. "M.O. 22-09-2022
     CALL METHOD lr_functions->set_function "M.O. 22-09-2022
          EXPORTING                        "M.O. 22-09-2022
            name    = '&DANGER'             "M.O. 22-09-2022
            boolean = space.               "M.O. 22-09-2022
      CATCH cx_salv_not_found .            "M.O. 22-09-2022
      CATCH cx_salv_wrong_call .           "M.O. 22-09-2022
    ENDTRY.                                "M.O. 22-09-2022

     TRY. "M.O. 22-09-2022
     CALL METHOD lr_functions->set_function "M.O. 22-09-2022
          EXPORTING                        "M.O. 22-09-2022
            name    = '&ADDTOBUFF'         "M.O. 22-09-2022
            boolean = space.               "M.O. 22-09-2022
      CATCH cx_salv_not_found .            "M.O. 22-09-2022
      CATCH cx_salv_wrong_call .           "M.O. 22-09-2022
    ENDTRY.                                "M.O. 22-09-2022

     TRY. "M.O. 22-09-2022
     CALL METHOD lr_functions->set_function "M.O. 22-09-2022
          EXPORTING                        "M.O. 22-09-2022
            name    = '&DELCR'             "M.O. 22-09-2022
            boolean = space.               "M.O. 22-09-2022
      CATCH cx_salv_not_found .            "M.O. 22-09-2022
      CATCH cx_salv_wrong_call .           "M.O. 22-09-2022
    ENDTRY.                                "M.O. 22-09-2022
**  ENDIF.                                  "M.O. 22-09-2022 TOGLIERE

  PERFORM set_columns_technical CHANGING gs_indez_show.

  PERFORM application_action USING gs_indez_show.

*IF ( ( go_salv_events_tree IS BOUND )
* AND ( gr_tree IS BOUND ) ) .
*
*    CREATE OBJECT go_alv_mouse_click
*      EXPORTING
*        pt_outtab_im = gt_outtab.
*    SET HANDLER go_alv_mouse_click->on_double_click FOR go_salv_events_tree.
**          item = leaf-node->get_hierarchy_item( ).
**          lo_parent_node_3 = leaf-node->get_parent( ).
*  ENDIF.
  gr_tree->display( ).

ENDFORM.                    " display_fullscreen
*&--------------------------------------------------------------------*
*&      Form  display_grid
*&--------------------------------------------------------------------*---*
FORM display_tree.

  CALL SCREEN 100.

ENDFORM.                    "display_grid
*&---------------------------------------------------------------------*
*&      Module  d0100_pbo  OUTPUT
*&---------------------------------------------------------------------*
MODULE d0100_pbo OUTPUT.
  PERFORM d0100_pbo.
ENDMODULE.                 " d0100_pbo  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  d0100_pai  INPUT
*&---------------------------------------------------------------------**
MODULE d0100_pai INPUT.
  PERFORM d0100_pai.
ENDMODULE.                 " d0100_pai  INPUT
*&---------------------------------------------------------------------*
*&      Form  d0100_pbo
*&---------------------------------------------------------------------*
FORM d0100_pbo .

  DATA lo_layout TYPE REF TO cl_salv_layout. .

  SET PF-STATUS '100'. " excluding '&DELE'.

  IF gr_container IS NOT BOUND.
    IF cl_salv_tree=>is_offline( ) EQ if_salv_c_bool_sap=>false.
      CREATE OBJECT gr_container
        EXPORTING
          container_name = 'CONTAINER'.
    ENDIF.

*... §1 create an ALV table
    TRY.
        cl_salv_tree=>factory(
          EXPORTING
            r_container  = gr_container
          IMPORTING
            r_salv_tree = gr_tree
          CHANGING
            t_table      = gt_outtab ).
      CATCH cx_salv_no_new_data_allowed cx_salv_error.
        EXIT.
    ENDTRY.
    IF gr_tree IS BOUND.
      lo_layout = gr_tree->get_layout( ).

    ENDIF.
    PERFORM create_tree.

*... §3 Functions
*... §3.1 activate ALV some Functions
*... §3.2 add your own button at the left
    DATA: lr_functions TYPE REF TO cl_salv_functions_tree.

    lr_functions = gr_tree->get_functions( ).
    lr_functions->set_all( gc_true ).
    lr_functions->set_group_print( abap_false ).

    DATA: l_text1 TYPE string,
          l_text2 TYPE string.

    TRY.
        l_text1 = text-b01.
        l_text2 = text-b02.

        lr_functions->add_function(
          name     = 'MYFUNCTION'
          icon     = '@DF@'
          text     = l_text1
          tooltip  = l_text2
          position = if_salv_c_function_position=>right_of_salv_functions ).


        l_text1 = text-b03.
        l_text2 = text-b03.

        lr_functions->add_function(
          name     = 'DMSDOC'
          icon     = '@0O@'
          text     = l_text1
          tooltip  = l_text2
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_wrong_call cx_salv_existing.
    ENDTRY.

    PERFORM set_columns_technical CHANGING gs_indez_show.
    PERFORM application_action USING gs_indez_show.

*... §6 display the table
    gr_tree->display( ).
  ENDIF.

ENDFORM.                                                    " d0100_pbo
*&---------------------------------------------------------------------*
*&      Form  d0100_pai
*&---------------------------------------------------------------------*
FORM d0100_pai .

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'QUIT'.
      CLEAR ok_code.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.                                                    " d0100_pai
*&---------------------------------------------------------------------*
*&      Form  set_columns_technical
*&---------------------------------------------------------------------*
FORM set_columns_technical CHANGING ps_index_show_ch TYPE ztrksystalv_show.

  FIELD-SYMBOLS: <fs1struct> TYPE abap_compdescr,
                 <fs1>       TYPE any.

  DATA: lr_columns TYPE REF TO cl_salv_columns,
        lr_column  TYPE REF TO cl_salv_column,
        lr_layout  TYPE REF TO cl_salv_layout,
        l_idx      TYPE i,
*        lt_show_index TYPE TABLE OF ztrksystalv_show,
*        ls_show_index TYPE ztrksystalv_show,
        lo_descr_ref TYPE REF TO cl_abap_structdescr,
        l_column_name TYPE lvc_fname.

  CLEAR: ps_index_show_ch, l_idx, lo_descr_ref." lt_show_index[],

  lr_columns = gr_tree->get_columns( ).

  lr_columns->set_optimize( abap_true ).

  lo_descr_ref ?= cl_abap_typedescr=>describe_by_name( c_struct_index_show ).

  SELECT SINGLE * FROM ztrksystalv_show INTO ps_index_show_ch WHERE zuser = sy-uname.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM ztrksystalv_show INTO ps_index_show_ch WHERE zuser = c_default_user.
  ENDIF.


  IF ps_index_show_ch IS NOT INITIAL.

    LOOP AT lo_descr_ref->components ASSIGNING <fs1struct>.
      CLEAR: l_column_name. l_column_name = <fs1struct>-name.
      IF l_column_name NE 'ZUSER'.
        UNASSIGN <fs1>.
        ASSIGN COMPONENT l_column_name OF STRUCTURE ps_index_show_ch TO <fs1>.
        IF <fs1> IS NOT INITIAL.
          CLEAR: l_idx. l_idx = <fs1>.
          lr_columns->set_column_position( columnname = l_column_name
                                 position   = l_idx ).
          IF l_column_name = 'ZDMSICON'.
            TRY.
                lr_column ?= lr_columns->get_column( l_column_name ).
                lr_column->set_long_text( text-hi1 ).
              CATCH cx_salv_not_found.                  "#EC NO_HANDLER
            ENDTRY.
          ENDIF.
        ELSE.
          DATA v_lenght TYPE ddleng.
          lr_column = lr_columns->get_column( l_column_name ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
*          v_lenght = lr_column->GET_DDIC_OUTPUTLEN( ). "P.A.
*          lr_column->set_output_length( v_lenght ).
        ENDIF.
*          IF l_column_name = 'ZSYSTEM'
*          OR l_column_name = 'ZCLIENT'
*          OR l_column_name = 'CTSPROJECT'
*          OR l_column_name = 'TRKORR'.
*            lr_column = lr_columns->get_column( l_column_name ).
*            lr_column->set_optimized( 'X' ).
*          ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    CLEAR: l_idx.
    LOOP AT lo_descr_ref->components ASSIGNING <fs1struct>.
      ADD 1 TO l_idx.
      CLEAR: l_column_name. l_column_name = <fs1struct>-name.
      lr_columns->set_column_position( columnname = l_column_name
             position   = l_idx ).
      IF l_column_name = 'ZDMSICON'.
        TRY.
            lr_column ?= lr_columns->get_column( l_column_name ).
            lr_column->set_long_text( text-hi1 ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.
      ENDIF.
    ENDLOOP.
  ENDIF.
* ENDIF.
ENDFORM.                    " set_columns_technical(
*&---------------------------------------------------------------------*
*&      Form  create_tree
*&---------------------------------------------------------------------*
FORM create_tree .

  PERFORM build_header.

  PERFORM select_data.

ENDFORM.                    " build_tree
*&---------------------------------------------------------------------*
*&      Form  build_header
*&---------------------------------------------------------------------*
FORM build_header .

*... § 2.1 build the hierarchy header
  DATA: settings TYPE REF TO cl_salv_tree_settings.

  settings = gr_tree->get_tree_settings( ).
  settings->set_hierarchy_header( text-hd1 ).
  settings->set_hierarchy_tooltip( text-ht1 ).
  settings->set_hierarchy_size( 30 ).

  DATA: title TYPE salv_de_tree_text.
  title = sy-title.
  settings->set_header( title ).

* set info for styles
  DATA: tooltips TYPE REF TO cl_salv_tooltips.

  tooltips = settings->get_tooltips( ).
  DATA: value TYPE lvc_value.

*  move IF_SALV_C_TREE_STYLE=>EMPHASIZED_A to value.
  value = if_salv_c_tree_style=>emphasized_a.
  TRY.
      CALL METHOD tooltips->add_tooltip
        EXPORTING
          type      = cl_salv_tooltip=>c_type_tree_style
          value     = value
          tooltip   = 'TEST'
*    receiving
*      r_tooltip =
          .
    CATCH cx_salv_existing .
  ENDTRY.


ENDFORM.                    " build_header
*&---------------------------------------------------------------------*
*&      Form  supply_data
*&---------------------------------------------------------------------*
FORM supply_data  USING lt_outtab TYPE gt_outtab_line.

*... §2.3 supply the data to ALV, building the hierarchy

  DATA: ls_data TYPE g_outtab_line,
        ls_data2 TYPE g_outtab_line.

  DATA: l_system_key TYPE lvc_nkey,
        l_transport_key TYPE lvc_nkey,
        l_ctsprj_key TYPE lvc_nkey,
        l_last_key TYPE lvc_nkey,
        l_idx TYPE sy-tabix,
        l_idx1 TYPE sy-tabix.


  DATA: nodes TYPE REF TO cl_salv_nodes.

  CLEAR: ls_data, l_idx, l_idx1.
  LOOP AT lt_outtab INTO ls_data.
    l_idx = sy-tabix.

    AT NEW zsystem.
      READ TABLE lt_outtab INTO ls_data2 INDEX l_idx.
      CLEAR: ls_data2-zsyst_bypassed, ls_data2-ctsproject," ls_data2-zsystem,
              ls_data2-tarclient, ls_data2-zaction, ls_data2-zweekly_tran, ls_data2-zinfo_nrt. ", ls_data2-znode
      PERFORM add_system_line USING  ls_data2 ''
                            CHANGING l_system_key
                                     l_idx1.
    ENDAT.

    AT NEW znode.
      READ TABLE lt_outtab INTO ls_data2 INDEX l_idx.
      CLEAR: ls_data2-zsyst_bypassed, ls_data2-ctsproject," ls_data2-zsystem,
             ls_data2-tarclient, ls_data2-zaction, ls_data2-zweekly_tran, ls_data2-zinfo_nrt." , ls_data2-znode.
      PERFORM add_transport_line USING ls_data2 l_system_key
                              CHANGING l_transport_key
                                       l_idx1.

    ENDAT.
    AT NEW ctsproject.
      READ TABLE lt_outtab INTO ls_data2 INDEX l_idx.
      CLEAR: ls_data2-zsyst_bypassed, "ls_data2-ctsproject," ls_data2-zsystem,
             ls_data2-tarclient, ls_data2-zaction, ls_data2-zweekly_tran, ls_data2-zinfo_nrt." , ls_data2-znode.
      PERFORM add_cts_line USING ls_data2 l_transport_key
                              CHANGING l_ctsprj_key
                                       l_idx1.
    ENDAT.
    PERFORM add_crlist_line   USING ls_data l_ctsprj_key
                           CHANGING l_last_key
                                    l_idx1.
    MOVE l_idx1 TO ls_data-position.
    MODIFY lt_outtab FROM ls_data INDEX l_idx.

  ENDLOOP.

  IF sy-uname = 'TO4IJ'.
    nodes = gr_tree->get_nodes( ).
    nodes->expand_all( ).
  ENDIF.

ENDFORM.                    " supply_data
*&---------------------------------------------------------------------*
*&      Form  add_system_line
*&---------------------------------------------------------------------*
FORM add_system_line  USING    p_ls_data TYPE g_outtab_line
                               p_key
                      CHANGING p_l_system_key
                               p_idx.

  DATA: nodes TYPE REF TO cl_salv_nodes,
          node TYPE REF TO cl_salv_node,
          text TYPE lvc_value,
          item TYPE REF TO cl_salv_item.

  DATA: wa_data LIKE p_ls_data.

*... §0 working with nodes
  nodes = gr_tree->get_nodes( ).

  TRY.

*  ... §0.1 add a new node
      node = nodes->add_node( related_node = p_key
                              relationship = cl_gui_column_tree=>relat_last_child ).

*  ... §0.2 if information should be displayed at
*    the hierarchy column set the system as text for this node
      text = p_ls_data-zid.
      WRITE ' ' TO text+30(1).
      node->set_text( text ).

      wa_data = p_ls_data.                         """""""""""""
      PERFORM clear_data USING wa_data.             """"""""""""
*  ... §0.3 set the data for the nes node
*      node->set_data_row( p_ls_data ).             """"""""""""
      node->set_data_row( wa_data ).                """"""""""""
      item = node->get_hierarchy_item( ).
*item->set_style( IF_SALV_C_TREE_STYLE=>EMPHASIZED_B ).
      item->set_icon( '@1E@' ).
*      item->set_type( if_salv_c_item_type=>link ).

      p_l_system_key = node->get_key( ).

*      IF sy-uname = 'TO4IJ'.
*        node->expand( ).
*      ENDIF.

    CATCH cx_salv_msg.
  ENDTRY.
  ADD 1 TO p_idx.

ENDFORM.                    " add_system_line
*&---------------------------------------------------------------------*
*&      Form  add_transport_line
*&---------------------------------------------------------------------*
FORM add_cts_line USING p_ls_data TYPE g_outtab_line
                        p_l_system_key
               CHANGING p_l_transport_key
                               p_idx.

  DATA: nodes TYPE REF TO cl_salv_nodes,
        node TYPE REF TO cl_salv_node,
        text TYPE lvc_value,
        item TYPE REF TO cl_salv_item.

  DATA: wa_data LIKE p_ls_data.


  nodes = gr_tree->get_nodes( ).

  TRY.

*    PERFORM check-system USING p_ls_data.
*
*      p_ls_data-zstconn =  va_errtxt.
*
*      node = nodes->add_node( related_node = p_l_system_key
*                              relationship = cl_gui_column_tree=>relat_last_child ).
*
**   set the transport as text for this node
*      text = p_ls_data-znode.
*      node->set_text( text ).
**      node->set_icon( '@OE@' ).
*
*      node->set_data_row( p_ls_data ).
*
*      p_l_transport_key = node->get_key( ).
*    CATCH cx_salv_msg.
*
      PERFORM check-system USING p_ls_data-zsystem
                                 p_ls_data-zclient.

      p_ls_data-zstconn =  va_errtxt.

      text = p_ls_data-ctsproject.


      node = nodes->add_node( related_node = p_l_system_key
                              data_row     = p_ls_data
                              text         = text
                              relationship = cl_gui_column_tree=>relat_last_child ).


      wa_data = p_ls_data.
      PERFORM clear_data USING wa_data.             """""""""""""
*      node->set_data_row( p_ls_data ).             """""""""""""
      node->set_data_row( wa_data ).                """""""""""""
      item = node->get_hierarchy_item( ).
      item->set_icon( '@OE@' ).
*      item->set_type( if_salv_c_item_type=>link ).

      p_l_transport_key = node->get_key( ).

*      IF sy-uname = 'TO4IJ'.
*        node->expand( ).
*      ENDIF.

    CATCH cx_salv_msg.
  ENDTRY.
  ADD 1 TO p_idx.

ENDFORM.                    " add_transport_line
*&---------------------------------------------------------------------*
*&      Form  add_transport_line
*&---------------------------------------------------------------------*
FORM add_transport_line USING  p_ls_data TYPE g_outtab_line
                               p_l_system_key
                      CHANGING p_l_transport_key
                               p_idx.

  DATA: nodes TYPE REF TO cl_salv_nodes,
        node TYPE REF TO cl_salv_node,
        text TYPE lvc_value,
        item TYPE REF TO cl_salv_item.

  DATA: wa_data LIKE p_ls_data.


  nodes = gr_tree->get_nodes( ).

  TRY.

*    PERFORM check-system USING p_ls_data.
*
*      p_ls_data-zstconn =  va_errtxt.
*
*      node = nodes->add_node( related_node = p_l_system_key
*                              relationship = cl_gui_column_tree=>relat_last_child ).
*
**   set the transport as text for this node
*      text = p_ls_data-znode.
*      node->set_text( text ).
**      node->set_icon( '@OE@' ).
*
*      node->set_data_row( p_ls_data ).
*
*      p_l_transport_key = node->get_key( ).
*    CATCH cx_salv_msg.
*
      PERFORM check-system USING p_ls_data-zsystem
                                 p_ls_data-zclient.

      p_ls_data-zstconn =  va_errtxt.

      text = p_ls_data-znode.

*      node = nodes->add_node( related_node = p_l_system_key
*                              data_row     = p_ls_data
*                              text         = text
*                              relationship = cl_gui_column_tree=>relat_last_child ).
*
*

      wa_data = p_ls_data.
      PERFORM clear_data USING wa_data.             """""""""""""
      node = nodes->add_node( related_node = p_l_system_key
                              data_row     = wa_data
                              text         = text
                              relationship = cl_gui_column_tree=>relat_last_child ).


*      node->set_data_row( p_ls_data ).             """""""""""""
      node->set_data_row( wa_data ).                """""""""""""
      item = node->get_hierarchy_item( ).
      item->set_icon( '@OE@' ).
*      item->set_type( if_salv_c_item_type=>link ).

      p_l_transport_key = node->get_key( ).

*      IF sy-uname = 'TO4IJ'.
*        node->expand( ).
*      ENDIF.

    CATCH cx_salv_msg.
  ENDTRY.
  ADD 1 TO p_idx.

ENDFORM.                    " add_transport_line
*&---------------------------------------------------------------------*
*&      Form  add_crlist_line
*&---------------------------------------------------------------------*
FORM add_crlist_line  USING p_ls_data TYPE g_outtab_line
                            p_l_transport_key
                   CHANGING p_l_last_key
                            p_idx.

  DATA: nodes TYPE REF TO cl_salv_nodes,
        node TYPE REF TO cl_salv_node,
        text TYPE lvc_value,
        ls_data3 TYPE g_outtab_line,
        item TYPE REF TO cl_salv_item,
        fl_ret.

  nodes = gr_tree->get_nodes( ).

  TRY.
      ls_data3 = p_ls_data.
* clear: ls_data3-zweekly, ls_data3-zinfo_nrt .
      node = nodes->add_node( related_node = p_l_transport_key
                              data_row     = ls_data3
                              relationship = cl_gui_column_tree=>relat_last_child ).

*... §5.1 add a checkbox to this node in the hierarchy column
      item = node->get_hierarchy_item( ).
      item->set_type( if_salv_c_item_type=>checkbox ).
      item->set_editable( abap_true ).

      text = p_ls_data-trkorr.


* set the icon in the icon column

      PERFORM check_dms USING p_ls_data-trkorr
                        CHANGING fl_ret.

      IF p_ls_data-zsyst_bypassed IS NOT INITIAL.
        item->set_style( if_salv_c_tree_style=>emphasized_negative ).
      ENDIF.

      IF fl_ret >< space.
        item = node->get_item( 'ZDMSICON' ).
        item->set_icon( '@6X@').
      ENDIF.

      CASE p_ls_data-zinfo_nrt.
        WHEN 'X'.
          item = node->get_item( 'ZNRTICON' ).
          item->set_icon( '@1V@').
        WHEN 'R'.
          item = node->get_item( 'ZNRTICON' ).
          item->set_icon( '@0W@').
        WHEN 'A'.
          item = node->get_item( 'ZNRTICON' ).
          item->set_icon( '@0V@').
      ENDCASE.

      READ TABLE xsystems WITH KEY zsystem = p_ls_data-zsystem
                                   zclient = p_ls_data-tarclient.

      IF p_ls_data-zweekly_tran = 'X' AND
         xsystems-zweekly_tran = 'X'.
        IF p_ls_data-zaction NE '01'
                                AND p_ls_data-zaction NE ' '.
          item = node->get_item( 'ZWEEKICON' ).
          item->set_icon( '@6Q@').
        ENDIF.
      ENDIF.

      IF p_ls_data-zindidng = 'X'.
        item = node->get_item( 'ZDNGICON' ).
        item->set_icon( '@OJ@').
      ENDIF.

      IF p_ls_data-rejected = 'X'.
        item = node->get_item( 'ZREJICON' ).
        item->set_icon( '@8Y@').
      ENDIF.
      node->set_text( text ).

      p_l_last_key = node->get_key( ).
    CATCH cx_salv_msg.
  ENDTRY.
  ADD 1 TO p_idx.

ENDFORM.                    " add_complete_line
*&---------------------------------------------------------------------*
*&      Form  set_metadata
*&---------------------------------------------------------------------*
FORM application_action USING ps_index_show_im TYPE ztrksystalv_show.

  PERFORM register_events USING ps_index_show_im.

ENDFORM.                    " set_metadata
*&---------------------------------------------------------------------*
*&      Form  top_end_of_page
*&---------------------------------------------------------------------*
FORM top_end_of_page .

  DATA: lr_content TYPE REF TO cl_salv_form_header_info,
        l_text     TYPE string.

*... create header information
  CONCATENATE 'TOP_OF_LIST' text-h01 INTO l_text SEPARATED BY space.

  CREATE OBJECT lr_content
    EXPORTING
      text    = l_text
      tooltip = l_text.

  gr_tree->set_top_of_list( lr_content ).

  CONCATENATE 'END_OF_LIST' text-h02 INTO l_text SEPARATED BY space.

  CREATE OBJECT lr_content
    EXPORTING
      text    = l_text
      tooltip = l_text.

  gr_tree->set_end_of_list( lr_content ).

ENDFORM.                    " top_end_of_page
*&---------------------------------------------------------------------*
*&      Form  show_function_info
*&---------------------------------------------------------------------*
FORM show_function_info USING i_function TYPE salv_de_function
                              i_text     TYPE string.

  DATA: l_string TYPE string.

  CONCATENATE i_text i_function INTO l_string SEPARATED BY space.

  MESSAGE i000(0k) WITH l_string.

ENDFORM.                    " show_function_info
*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
FORM register_events USING ps_index_show_im TYPE ztrksystalv_show.

  DATA: lr_events TYPE REF TO cl_salv_events_tree.

  lr_events = gr_tree->get_event( ).

  CREATE OBJECT gr_events
    EXPORTING
      pt_outtab_im      = gt_outtab[]
      po_tree_events_im = lr_events
      ps_index_show_im  = ps_index_show_im.

ENDFORM.                    " register_events
*&---------------------------------------------------------------------*
*&      Form  user_action
*&---------------------------------------------------------------------*
FORM user_action..

  FIELD-SYMBOLS: <fs> TYPE any.

  DATA: nodes TYPE REF TO cl_salv_nodes,
         node TYPE REF TO cl_salv_node,
         l_last_node TYPE REF TO cl_salv_node,
         item TYPE REF TO cl_salv_item,
         tree TYPE salv_t_nodes,
         leaf TYPE salv_s_nodes,
         l_fieldname TYPE lvc_fname,
         l_text_liv_2_from_par TYPE lvc_value,
         l_text_liv_2 TYPE lvc_value,
         l_text_liv_3 TYPE lvc_value,
         l_text_liv_4 TYPE lvc_value.

  DATA: lt_salv_node TYPE salv_t_nodes,
        ls_salv_node TYPE salv_s_nodes,
        lo_parent_node_2 TYPE REF TO cl_salv_node,
        lo_parent_node_3 TYPE REF TO cl_salv_node,
        l_find_parent_flag TYPE xfeld.

  CLEAR ls_zstrklayalv.

  DATA: lv_tree TYPE REF TO data,
        l_count TYPE i,
        l_idx TYPE sy-tabix,
        l_idx_2 TYPE sy-tabix,
        l_flag_checks_for_delcr TYPE xfeld,
        lo_layout TYPE REF TO cl_salv_layout.

  DATA: ls_mass_sel_param TYPE ztrkrelease_s,
        l_subrc           TYPE sy-subrc.

  CLEAR tb_crtrasp.
  REFRESH tb_crtrasp.

* get all existing nodes
  TRY.
      nodes = gr_tree->get_nodes( ).
      tree = nodes->get_all_nodes( ).
    CATCH cx_salv_msg.
      EXIT.
  ENDTRY.

  IF sy-ucomm = '&ADDTOBUFF'.
    AUTHORITY-CHECK OBJECT 'ZTRK_ADM'
             ID 'ACTVT' FIELD '02'.
    IF sy-subrc = 0.

      PERFORM add_to_buffer.
      PERFORM refresh.
    ENDIF.
  ELSEIF sy-ucomm = '&HISTOR'
  OR sy-ucomm = '&DMSDOC'
  OR sy-ucomm = '&LOGCR'
  OR sy-ucomm = '&INSPCODE'
  OR sy-ucomm = '&SHOW'
  OR sy-ucomm = '&OBJCR'.

    CLEAR: l_count, l_idx, l_idx_2.

    LOOP AT tree INTO leaf.
      l_idx = sy-tabix.
      item = leaf-node->get_hierarchy_item( ).
      IF item->is_checked( ) EQ abap_true.
        ADD 1 TO l_count.
        l_idx_2 = l_idx.
      ENDIF.
    ENDLOOP.
    IF l_count = 1.
      CLEAR: leaf. READ TABLE tree INTO leaf INDEX l_idx_2.
      IF sy-subrc = 0.
        lv_tree = leaf-node->get_data_row( ).
        UNASSIGN <fs>.
        ASSIGN lv_tree->* TO <fs>.
        ls_zstrklayalv = <fs>.
        CASE sy-ucomm.
          WHEN '&HISTOR'.
            PERFORM histor_cr USING ls_zstrklayalv-trkorr.
          WHEN '&DMSDOC'.
            PERFORM dms_doc USING ls_zstrklayalv-zsystem
                                  ls_zstrklayalv-trkorr.
          WHEN '&LOGCR'.
            IF ls_zstrklayalv-zstatcr = 'RLS'.
              PERFORM log_cr USING ls_zstrklayalv-trkorr
                                   ls_zstrklayalv-zsystem
                                   ls_zstrklayalv-zclient
                                   ls_zstrklayalv-comsys
                                   ls_zstrklayalv-srccli.
            ELSE.
              MESSAGE i000(db) WITH text-e05.
            ENDIF.
          WHEN '&INSPCODE'.
            PERFORM inspection_code USING ls_zstrklayalv-trkorr
                                          ls_zstrklayalv-zsystem
                                          ls_zstrklayalv-zclient
                                          ls_zstrklayalv-comsys
                                          ls_zstrklayalv-srccli.
          WHEN '&OBJCR'.
            PERFORM display_cr USING ls_zstrklayalv-trkorr
                                     ls_zstrklayalv-zsystem
                                     ls_zstrklayalv-tarclient
                                     ls_zstrklayalv-comsys
                                     ls_zstrklayalv-srccli.
          WHEN '&SHOW'.
            PERFORM show_cr USING ls_zstrklayalv-trkorr
                                     ls_zstrklayalv-zsystem
                                     ls_zstrklayalv-tarclient.

        ENDCASE.
      ENDIF.
    ELSEIF l_count > 1.
      MESSAGE i000(db) WITH text-e07.
    ENDIF.
  ELSE.
    CLEAR: l_flag_checks_for_delcr.
    LOOP AT tree INTO leaf.
      item = leaf-node->get_hierarchy_item( ).
      IF item->is_checked( ) EQ abap_true.
        l_flag_checks_for_delcr = 'X'.
        lv_tree = leaf-node->get_data_row( ).
        UNASSIGN <fs>.
        ASSIGN lv_tree->* TO <fs>.
        ls_zstrklayalv = <fs> .

        CASE sy-ucomm.

          WHEN '&DELCR'.
            AUTHORITY-CHECK OBJECT 'ZTRK_ADM'
                     ID 'ACTVT' FIELD '06'.
            IF sy-subrc = 0.
              PERFORM delete_cr USING ls_zstrklayalv-trkorr
                                      ls_zstrklayalv-zsystem
                                      ls_zstrklayalv-tarclient.
            ELSE.
              MESSAGE i314(1r).

            ENDIF.

          WHEN '&DANGER'.
            AUTHORITY-CHECK OBJECT 'ZTRK_ADM'
                     ID 'ACTVT' FIELD '01'.
            IF sy-subrc = 0.
              IF ls_zstrklayalv-zstatcr = 'RLS'.
                PERFORM set_cr_danger USING ls_zstrklayalv.
              ELSE.
                MESSAGE e000(db) WITH text-e04.
              ENDIF.
            ENDIF.
          WHEN '&TRANSP'.
            PERFORM transport_cr USING ls_zstrklayalv.
          WHEN '&REJECTTRP'.
            PERFORM reject_transport_cr USING ls_zstrklayalv.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF sy-ucomm = '&REFRESH' OR sy-ucomm = '&DELCR'.
    IF sy-ucomm = '&DELCR' AND  l_flag_checks_for_delcr IS INITIAL.
      CLEAR: l_flag_checks_for_delcr.
      AUTHORITY-CHECK OBJECT 'ZTRK_ADM'
               ID 'ACTVT' FIELD '06'.
      IF sy-subrc = 0.
        PERFORM delete_cr_from_popup.
        PERFORM refresh.
      ELSE.
        MESSAGE i314(1r).
      ENDIF.
    ELSEIF sy-ucomm = '&REFRESH'.
      PERFORM refresh.
    ENDIF.
  ENDIF.

  IF ( sy-ucomm = '&TRANSP' )  AND
     ( ls_zstrklayalv-zstatcr = 'RLS' OR ls_zstrklayalv-zstatcr = 'OPN' ).
* start insert RM 02.11.2020 - Check Append
*    IF sy-uname = 'TO4GM' OR sy-uname = 'F39147A'.
      IF ls_zstrklayalv-zsystem = 'IF3'.
        PERFORM check_append USING ls_zstrklayalv.
      ENDIF.
      IF fl_ko = 'X'.
        MESSAGE i000(db) WITH text-i11.
        PERFORM refresh.
        EXIT.
      ENDIF.
*    ENDIF.
* end   insert RM 02.11.2020 - Check Append
    PERFORM popup_sequence_cr USING ls_zstrklayalv.
    PERFORM refresh.

  ELSEIF ( sy-ucomm = '&REJECTTRP' )  AND
     ( ls_zstrklayalv-zstatcr = 'RLS' OR ls_zstrklayalv-zstatcr = 'OPN' ).

    IF NOT ls_zstrklayalv-zaction = '01'.
      PERFORM reject_crs.
      PERFORM refresh.
    ENDIF.
  ENDIF.

  CLEAR: sy-ucomm.
ENDFORM.                    " user_action
*&---------------------------------------------------------------------*
*&      Form  RETRIVE_KORR
*&---------------------------------------------------------------------*
FORM retrive_korr USING    p_ls_data-zsystem
                           p_ls_data-zclient.
  " zstrklayalv.

  CLEAR va_zrfc.

  CLEAR tb_crs1.
  REFRESH tb_crs1.

*  clear tb_next.
*  refresh tb_next.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.

    CALL FUNCTION 'ZTZBCTRK_RFC_PING_PREPARE'
      DESTINATION va_zrfc
      EXPORTING
        i_system              = p_ls_data-zsystem
        i_tarcli              = p_ls_data-zclient
      EXCEPTIONS
        system_not_found      = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e12 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
    IF sy-subrc = 0.

      CALL FUNCTION 'ZTZBCTRK_OPENED_CR_PREPARE'
        DESTINATION va_zrfc
        EXPORTING
          i_system              = p_ls_data-zsystem
          i_client              = p_ls_data-zclient
          i_uname               = sy-uname
        TABLES
          t_crs                 = tb_crs1
        EXCEPTIONS
          not_found             = 1
          system_failure        = 2
          communication_failure = 3
          OTHERS                = 4.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e13 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
*      IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.

*      call function 'ZTZBCTRK_GET_CRS'
*        destination va_zrfc
*        exporting
*          i_uname   = sy-uname
*        tables
*          t_crs     = tb_next
*        exceptions
*          not_found = 1
*          others    = 2.
*      if sy-subrc <> 0.
*** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      endif.


    ENDIF.

    CLEAR tb_e070.
    REFRESH tb_e070.

    LOOP AT tb_crs1 ASSIGNING <fs_crs1>.
      tb_e070-trkorr     = <fs_crs1>-trkorr.
      tb_e070-as4user    = <fs_crs1>-as4user.
      tb_e070-zstatcr    = 'OPN'.
      tb_e070-as4text    = <fs_crs1>-as4text.
      tb_e070-ctsproject = <fs_crs1>-ctsproject.
      tb_e070-tarclient  = <fs_crs1>-client.
      tb_e070-tarsystem  = <fs_crs1>-tarsystem.
      tb_e070-comsys     = p_ls_data-zsystem.
      tb_e070-srccli     = p_ls_data-zclient.
      tb_e070-zfield1    = <fs_crs1>-zfield1.
      tb_e070-zfield2    = <fs_crs1>-zfield2.
      tb_e070-zfield3    = <fs_crs1>-zfield3.
      tb_e070-zfield4    = <fs_crs1>-zfield4.
      tb_e070-zfield5    = <fs_crs1>-zfield5.
      APPEND tb_e070.
    ENDLOOP.

*    loop at tb_next.
*      tb_e070-trkorr = tb_next-trkorr.
*      tb_e070-as4user = tb_next-as4user.
*      tb_e070-zstatcr = 'RLS'.
*      tb_e070-as4text = tb_next-as4text.
*      append tb_e070.
*    endloop.
  ENDIF.

ENDFORM.                    " RETRIVE_KORR
*&---------------------------------------------------------------------*
*&      Form  CHECK-SYSTEM
*&---------------------------------------------------------------------**
FORM check-system USING p_ls_data-zsystem
                        p_ls_data-zclient.
  "zstrklayalv.

  CLEAR fl_errzrfc.
  CLEAR va_zrfc.
  CLEAR va_errtxt.



  CALL FUNCTION 'ZTZBCTRK_CHECK_SYSTEM'
    DESTINATION va_zrfc
    EXPORTING
      in_system             = p_ls_data-zsystem
      in_client             = p_ls_data-zclient
    IMPORTING
      out_message           = va_errtxt
      out_errzrfc           = fl_errzrfc
    EXCEPTIONS
      system_failure        = 1
      communication_failure = 2
      OTHERS                = 3.

*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e14 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS

ENDFORM.                    " CHECK-SYSTEM
*&---------------------------------------------------------------------*
*&      Form  DMS_DOC
*&---------------------------------------------------------------------*
FORM dms_doc USING p_ls_zstrklayalv-zsystem
                   p_ls_zstrklayalv-trkorr.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.

    CALL FUNCTION 'ZTZBCTRK_ATTACHMENT'
      DESTINATION va_zrfc
      EXPORTING
        i_trkorr              = p_ls_zstrklayalv-trkorr
        i_type                = 'E070'
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e16 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
    PERFORM set_icon_dms USING p_ls_zstrklayalv-zsystem
                               p_ls_zstrklayalv-trkorr.

  ENDIF.

ENDFORM.                    " DMS_DOC
*&---------------------------------------------------------------------*
*&      Form  TRANSPORT_CR
*&---------------------------------------------------------------------*
FORM transport_cr USING    p_ls_zstrklayalv.

  MOVE-CORRESPONDING p_ls_zstrklayalv TO tb_crtrasp.

  APPEND tb_crtrasp .

*  MESSAGE i000 WITH text-s01.

ENDFORM.                    " TRANSPORT_CR
*&---------------------------------------------------------------------*
*&      Form  NOTE_CR
*&---------------------------------------------------------------------*
*FORM note_cr  USING    p_ls_zstrklayalv_trkorr.

*  DATA: va_langu LIKE e07t-langu.
*  DATA: wa_thead TYPE thead.
*
*
*  DATA: BEGIN OF lines OCCURS 10.
*          INCLUDE STRUCTURE tline.
*  DATA: END OF lines.
*
*  SELECT SINGLE langu INTO va_langu FROM e07t
*                   WHERE trkorr = p_ls_zstrklayalv_trkorr.
*  IF sy-subrc >< 0.
*    va_langu = 'EN'.
*  ENDIF.
*
*  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.
*
*  IF sy-subrc = 0.
*
*    CALL FUNCTION 'ZTZBCTRK_READ_TEXT'
*      DESTINATION va_zrfc
*      EXPORTING
*        id       = 'ZTRK'
*        language = va_langu
*        name     = p_ls_zstrklayalv_trkorr
*        object   = 'ZTRK'
*      TABLES
*        lines    = lines
*      EXCEPTIONS
*        OTHERS   = 8.
*
*
*
*    wa_thead-tdobject    =  'ZTRK'.
*    wa_thead-tdname      =  p_ls_zstrklayalv_trkorr.
*    wa_thead-tdid        =  'ZTRK'.
*    wa_thead-tdspras     =  va_langu.
*    wa_thead-tdtitle     =  p_ls_zstrklayalv_trkorr.
*    wa_thead-tdlinesize  = '72'.
*    CALL FUNCTION 'ZTZBCTRK_EDIT_TEXT'
*    DESTINATION va_zrfc
*      EXPORTING
**       DISPLAY             = ' '
**       EDITOR_TITLE        = ' '
*        header              = wa_thead
**       PAGE                = ' '
**       WINDOW              = ' '
*        save                = 'X'
**       LINE_EDITOR         = ' '
**       CONTROL             = ' '
**       PROGRAM             = ' '
**       LOCAL_CAT           = ' '
**     IMPORTING
**       FUNCTION            =
**       NEWHEADER           =
**       RESULT              =
*      TABLES
*        lines               = lines
*     EXCEPTIONS
*       id                  = 1
*       language            = 2
*       linesize            = 3
*       name                = 4
*       object              = 5
*       textformat          = 6
*       communication       = 7
*      OTHERS               = 8.
*
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*  ENDIF.

*ENDFORM.                    " NOTE_CR
*&---------------------------------------------------------------------*
*&      Form  POPUP_SEQUENCE_CR
*&---------------------------------------------------------------------*
FORM popup_sequence_cr USING p_ls_zstrklayalv STRUCTURE ls_zstrklayalv.
data lv_zrefresh type ZEZBCREFRESH. "12-06-2023 NEW ENHANCEMENTS
  FIELD-SYMBOLS: <crtrasp> TYPE zstrklayalv.
  DATA: ls_layout  TYPE slis_layout_alv,
       wa_fieldcat TYPE slis_fieldcat_alv,
       tb_fieldcat TYPE slis_t_fieldcat_alv,
       ls_selfield TYPE slis_selfield,
       ls_grid     LIKE lvc_s_glay ,
       ls_return   TYPE slis_exit_by_user,
       fl_errseq,
       ls_trksys   LIKE ztzbctrksys,
       lv_answer,
       ls_tmscsys  TYPE tmscsys,
       lv_rich,
       lv_tosystem TYPE tr_target,
       lv_client   TYPE sy-mandt,
       lv_ok,
       lv_vers     LIKE  tcevers-version,
       lt_deliver  TYPE STANDARD TABLE OF triwb_s_deliver,
       lt_target   TYPE STANDARD TABLE OF triwb_s_target,
       ls_deliver  TYPE triwb_s_deliver,
       ls_target   TYPE triwb_s_target,
       lv_moresystem,
       lv_objnr(16) TYPE n,
       fl_errnode.
  DATA: ls_e070    LIKE e070.

  DATA: ls_text  LIKE ibiptextln,
        lt_text  LIKE ibiptextln OCCURS 0.

  DATA: lt_line      TYPE STANDARD TABLE OF  tline.
  DATA: ls_line      TYPE tline.

  DATA: l_count TYPE zezbcseq.
  DATA: l_warng_elab_4_next_step_flag TYPE xfeld.

* Evolutive 05.02.2014 Start
  DATA: ls_syst     TYPE ztzbctrksys,
        ls_approver TYPE ztzbctrkapprover.

  DATA: l_phone TYPE char16,
        l_subrc TYPE sy-subrc.
* Evolutive 05.02.2014 End

  DATA: vn_lines(3) TYPE n.

  DATA: BEGIN OF tb_ztzbctrkcr OCCURS 1000.
          INCLUDE STRUCTURE ztzbctrkcr_st.
  DATA: END OF tb_ztzbctrkcr.

  CLEAR tb_fieldcat.
  REFRESH tb_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = 'ZSEQUENCE'.
  wa_fieldcat-tabname       = 'TB_CRTRASP'.
  wa_fieldcat-ref_fieldname = 'ZSEQUENCE'.
  wa_fieldcat-ref_tabname   = 'ZSTRKLAYALV'.
* wa_fieldcat-input = 'X'.
*  wa_fieldcat-edit = 'X'.

  APPEND wa_fieldcat TO tb_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = 'CTSPROJECT'.
  wa_fieldcat-tabname       = 'TB_CRTRASP'.
  wa_fieldcat-ref_fieldname = 'CTSPROJECT'.
  wa_fieldcat-ref_tabname   = 'ZSTRKLAYALV'.

  APPEND wa_fieldcat TO tb_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = 'TRKORR'.
  wa_fieldcat-tabname       = 'TB_CRTRASP'.
  wa_fieldcat-ref_fieldname = 'TRKORR'.
  wa_fieldcat-ref_tabname   = 'ZSTRKLAYALV'.
  APPEND wa_fieldcat TO tb_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = 'AS4TEXT'.
  wa_fieldcat-tabname       = 'TB_CRTRASP'.
  wa_fieldcat-ref_fieldname = 'AS4TEXT'.
  wa_fieldcat-ref_tabname   = 'ZSTRKLAYALV'.
  APPEND wa_fieldcat TO tb_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = 'AS4USER'.
  wa_fieldcat-tabname       = 'TB_CRTRASP'.
  wa_fieldcat-ref_fieldname = 'AS4USER'.
  wa_fieldcat-ref_tabname   = 'ZSTRKLAYALV'.
  APPEND wa_fieldcat TO tb_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = 'ZNODE'.
  wa_fieldcat-tabname       = 'TB_CRTRASP'.
  wa_fieldcat-ref_fieldname = 'ZNODE'.
  wa_fieldcat-ref_tabname   = 'ZSTRKLAYALV'.
  APPEND wa_fieldcat TO tb_fieldcat.

  ls_layout-zebra  = 'X'.
  ls_grid-edt_cll_cb = 'X'.

  lv_ok = 'X'.
  DESCRIBE TABLE tb_crtrasp LINES vn_lines.
  UNASSIGN: <crtrasp>.
  CLEAR: l_count, lv_error.
  CLEAR lv_ctsproject.
  LOOP AT tb_crtrasp ASSIGNING <crtrasp>.
    READ TABLE tb_e070 WITH KEY trkorr = <crtrasp>-trkorr
                               zsystem = <crtrasp>-zsystem
                             tarclient = <crtrasp>-tarclient.
    <crtrasp>-zsequence = tb_e070-strkorr(6).
  ENDLOOP.
  SORT tb_crtrasp BY zsequence.
  READ TABLE tb_crtrasp ASSIGNING <crtrasp> INDEX 1.

  SELECT SINGLE * FROM ztzbctrksys INTO ls_trksys WHERE  zsystem = <crtrasp>-zsystem
                                                    AND  zclient = <crtrasp>-tarclient.

  CLEAR       lv_moresystem.
  IF ls_trksys-zapprovsequ = ''.
    SELECT SINGLE * FROM tmscsys INTO ls_tmscsys
                                  WHERE sysnam = <crtrasp>-zsystem.
    IF sy-subrc = 0 AND ls_tmscsys-systyp = 'V'.
      lv_system = <crtrasp>-zsystem.
      SELECT SINGLE zrfc INTO va_zrfc1 FROM ztzbctrkrfc
                      WHERE zsystem = lv_system
                        AND zclient = <crtrasp>-tarclient
                           AND type = 0.

    ELSE.

      SELECT MAX( version ) INTO lv_vers FROM tcevers.

      CALL FUNCTION 'TMS_WBO_CONFIG_READ'
        EXPORTING
          iv_version              = lv_vers
        IMPORTING
          et_deliver              = lt_deliver
          et_target               = lt_target
        EXCEPTIONS
          configuration_not_found = 1
          OTHERS                  = 2.

      CONCATENATE <crtrasp>-zsystem <crtrasp>-tarclient INTO lv_tosystem SEPARATED BY '.'.
      READ TABLE lt_deliver INTO ls_deliver WITH KEY tosystem = lv_tosystem.
      IF sy-subrc = 0.

        lv_system = ls_deliver-fromsystem.
        lv_client = ls_deliver-fromclient.
        CLEAR LV_ZREFRESH. "12-06-2023 NEW ENHANCEMENTS
        select SINGLE zrefresh from ZTZBCTRKSYS into lv_zrefresh "12-06-2023 NEW ENHANCEMENTS
           where zsystem = lv_system "12-06-2023 NEW ENHANCEMENTS
           and   zclient = lv_client.  "12-06-2023 NEW ENHANCEMENTS
        if sy-subrc = 0. "19-07-2022 FIX NEW ENHANCEMENTS
          IF LV_ZREFRESH IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
             SELECT SINGLE ZSYSTEM INTO LV_SYSTEM FROM ZTZBCTRKMAINC. "12-06-2023 NEW ENHANCEMENTS
             SELECT SINGLE ZCLIENT INTO LV_CLIENT FROM ZTZBCTRKSYS "12-06-2023 NEW ENHANCEMENTS
               WHERE ZSYSTEM = LV_SYSTEM. "12-06-2023 NEW ENHANCEMENTS
          ENDIF.  "12-06-2023 NEW ENHANCEMENTS
        else. "19-07-2022 FIX NEW ENHANCEMENTS
              SELECT SINGLE ZSYSTEM INTO LV_SYSTEM FROM ZTZBCTRKMAINC.  "19-07-2022 FIX NEW ENHANCEMENTS
              SELECT SINGLE ZCLIENT INTO LV_CLIENT FROM ZTZBCTRKSYS  "19-07-2022 FIX NEW ENHANCEMENTS
               WHERE ZSYSTEM = LV_SYSTEM.  "19-07-2022 FIX NEW ENHANCEMENTS
        endif. "19-07-2022 FIX NEW ENHANCEMENTS

        SELECT SINGLE zrfc INTO va_zrfc1 FROM ztzbctrkrfc
                      WHERE zsystem = lv_system
                        AND zclient = lv_client
                           AND type = 0.

      ENDIF.
    ENDIF.
    LOOP AT tb_crtrasp ASSIGNING <crtrasp>.

      CLEAR: lt_fields, lt_data, lt_options.
      FREE : lt_fields, lt_data, lt_options.
      CONCATENATE '''' <crtrasp>-trkorr '''' INTO ls_options.
      CONCATENATE 'TRKORR =' ls_options INTO ls_options SEPARATED BY space.
      APPEND ls_options TO lt_options.


      CALL FUNCTION 'ZTRK_RFC_READ_TABLE'
        DESTINATION va_zrfc1
        EXPORTING
          query_table           = ca_tabname_e070
          i_system              = <crtrasp>-zsystem
        TABLES
          fields                = lt_fields
          options               = lt_options
          data                  = lt_data
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e15 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
      READ TABLE lt_data INTO ls_data INDEX 1.
      IF sy-subrc = 0.
        ls_e070 = ls_data.
        MOVE: ls_e070-as4date TO <crtrasp>-as4date,
              ls_e070-as4time TO <crtrasp>-as4time.
      ENDIF.
    ENDLOOP.
    SORT tb_crtrasp BY as4date as4time.
  ELSE.
    SORT tb_crtrasp BY zsequence.
  ENDIF.

  READ TABLE tb_crtrasp ASSIGNING <crtrasp> INDEX 1.
  lv_system = <crtrasp>-zsystem.
  LOOP AT tb_crtrasp ASSIGNING <crtrasp>.
    ADD 1 TO l_count.
    <crtrasp>-zsequence = l_count.
    IF lv_ctsproject IS INITIAL.
      lv_ctsproject = <crtrasp>-ctsproject.
    ELSE.
*      IF lv_ctsproject NE <crtrasp>-ctsproject.
*        IF sy-uname NE 'TO4IJ'.
*          lv_error = 'X'.
*          MESSAGE i000(db) WITH text-w09 text-w10.
*        ENDIF.
*        EXIT.
*      ENDIF.
    ENDIF.
    IF lv_system NE <crtrasp>-zsystem.
      lv_moresystem = 'X'.
    ENDIF.
    IF <crtrasp>-zinfo_nrt = 'X' OR <crtrasp>-zinfo_nrt = 'R'.
      CLEAR lv_ok.
    ENDIF.
  ENDLOOP.


  IF lv_ok = ''.
    MESSAGE i000(ztrk) WITH text-552 text-553 text-554 text-555.
  ELSE.
    IF lv_moresystem = 'X'.
      MESSAGE i000(ztrk) WITH text-556 text-557 text-554 text-555.
    ELSE.
      READ TABLE tb_crtrasp WITH KEY zaction = '01'.
      IF sy-subrc = 0.
        LOOP AT tb_crtrasp WHERE zaction NE '01'.
          lv_error = 'X'.
        ENDLOOP.
        IF lv_error = 'X'.
          MESSAGE i000(db) WITH text-w11.
        ELSE.
          lv_rich = 'X'.
        ENDIF.
      ELSE.
        CLEAR lv_rich.
      ENDIF.

      IF lv_error = ''.
        IF vn_lines > 1 AND lv_rich = 'X'..
          CLEAR lv_seqchange.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_callback_program       = sy-repid
              i_callback_pf_status_set = 'PF_STATUS_SET'
              i_callback_user_command  = 'USER_COMMAND'
              is_layout                = ls_layout
              it_fieldcat              = tb_fieldcat
              i_default                = 'X'
              i_grid_settings          = ls_grid
              i_save                   = 'A'
              i_screen_start_column    = 25
              i_screen_start_line      = 3
              i_screen_end_column      = 180
              i_screen_end_line        = 20
            IMPORTING
              es_exit_caused_by_user   = ls_return
            TABLES
              t_outtab                 = tb_crtrasp
            EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.

        ENDIF.

        CLEAR fl_errseq.

        IF ls_return-back = space AND
           ls_return-cancel = space.     "ins RM 16.10.2018

          PERFORM check_sequence CHANGING fl_errseq
                                          fl_errnode.

          IF fl_errnode = space.

            IF fl_errseq = space.

              SORT tb_crtrasp BY zsequence.

              CLEAR tb_ztzbctrkcr.
              REFRESH tb_ztzbctrkcr.

              LOOP AT tb_crtrasp.
                MOVE-CORRESPONDING tb_crtrasp TO tb_ztzbctrkcr.
*           tb_ztzbctrkcr-trkorr = tb_crtrasp-trkorr.
*           tb_ztzbctrkcr-zaction = p_ls_zstrklayalv-zaction.
*           tb_ztzbctrkcr-tarclient = p_ls_zstrklayalv-tarclient.
*           tb_ztzbctrkcr-ctsproject = p_ls_zstrklayalv-ctsproject.
                tb_ztzbctrkcr-tarsystem = p_ls_zstrklayalv-zsystem.
*           tb_ztzbctrkcr-trlayer = p_ls_zstrklayalv-trlayer.
*           tb_ztzbctrkcr-zindidng = p_ls_zstrklayalv-zindidng.
                tb_ztzbctrkcr-as4time = tb_crtrasp-zsequence.
                APPEND tb_ztzbctrkcr.
              ENDLOOP.

              SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

              IF sy-subrc = 0.

                IF p_ls_zstrklayalv-zstatcr = 'RLS'.

                  CLEAR: l_subrc, l_phone.

*** MC - TB 23.07.14 - Aggiunto controllo SET per user
*              IF sy-uname = 'GF99841'
*              OR sy-uname = 'DALESSIO'
*              OR sy-uname = 'TO4IJ'
*              OR sy-uname = 'CASTELLO'.
*              IF sy-uname IN r_user.
*** MC - TB 23.07.14 - Fine modifica
                  CALL FUNCTION 'ZTZBCTRK_GET_SYST_INFO'
                    DESTINATION va_zrfc
                    EXPORTING
                      p_system_im           = p_ls_zstrklayalv-zsystem
                      p_client_im           = p_ls_zstrklayalv-tarclient
                    IMPORTING
                      ps_syst_info_ex       = ls_syst
                    EXCEPTIONS
                      data_not_found        = 1
                      system_failure        = 2
                      communication_failure = 3
                      OTHERS                = 4.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e17 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
                  IF sy-subrc = 0.
                    IF ls_syst-zproductive = 'X'.
                      READ TABLE tb_crtrasp WITH KEY zaction = '01'.
                      IF sy-subrc = 0.
                        FREE lt_line.
                        CLEAR lt_line.
                        CONCATENATE p_ls_zstrklayalv-zsystem
                                    p_ls_zstrklayalv-trkorr
                                       INTO lv_objnr.
                        DO.
                          CALL FUNCTION 'ZTZBCTRK_NOTE'
                            EXPORTING
                              i_type  = 'P'
                              i_objnr = lv_objnr
                            TABLES
                              tb_line = lt_line.
                          IF lt_line[] IS INITIAL.
                            MESSAGE e000(ztrk) WITH text-501.
                          ELSE.
                            LOOP AT tb_ztzbctrkcr.
                              CONCATENATE tb_ztzbctrkcr-tarsystem
                                          tb_ztzbctrkcr-trkorr
                                             INTO lv_objnr.
                              LOOP AT lt_line INTO ls_line.
                                MOVE-CORRESPONDING ls_line TO ls_text.
                                ls_text-tdobject = 'ZTRK'.
                                ls_text-tdname = lv_objnr.
                                ls_text-tdid = 'ZTRK'.
                                ls_text-tdspras = 'EN'.

                                APPEND ls_text TO lt_text.
                              ENDLOOP.
                              CALL FUNCTION 'RFC_SAVE_TEXT'
                                DESTINATION va_zrfc
                                TABLES
                                  text_lines = lt_text.
                              FREE lt_text.
                            ENDLOOP.
                            EXIT.

                          ENDIF.
                        ENDDO.
                      ENDIF.
                    ENDIF.

                    IF ls_syst-zimportdiff = 'X'.
                      READ TABLE tb_crtrasp WITH KEY zaction = '01'.
                      IF sy-subrc = 0.
                        CALL FUNCTION 'ZTZBCTRK_GET_APPRV_LEVEL_INFO'
                          DESTINATION va_zrfc
                          EXPORTING
                            p_system_im           = p_ls_zstrklayalv-zsystem
                            p_client_im           = p_ls_zstrklayalv-tarclient
                            p_cts_im              = p_ls_zstrklayalv-ctsproject
                            p_step_im             = '01'
                            p_level_im            = '1'
                            p_uname_im            = sy-uname
                          IMPORTING
                            ps_apprv_info_ex      = ls_approver
                          EXCEPTIONS
                            data_not_found        = 1
                            system_failure        = 2
                            communication_failure = 3
                            OTHERS                = 4.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e18 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
                        IF sy-subrc = 0.
**                        IF SY-UNAME = 'GF10030' or sy-uname = 'TO4GM' or "M.O. 22-09-2022 TOGLIERE
**                           SY-UNAME = 'F31236A' OR SY-UNAME = 'F39147A'. "M.O. 22-09-2022 TOGLIERE
**                          ELSE. "M.O. 22-09-2022 - TOGLIERE
*                          PERFORM pop_up_for_values CHANGING l_phone l_subrc. "M.O. 22-09-2022 del
*                          IF ( ( l_subrc = 0 ) AND ( l_phone IS NOT INITIAL ) ). "M.O. 22-09-2022 del
*                            CALL FUNCTION 'ZTZBCTRK_SET_REF_PHNNUM_CRS' "M.O. 22-09-2022 del
*                              DESTINATION va_zrfc                            "M.O. 22-09-2022 del
*                              EXPORTING                                        "M.O. 22-09-2022 del
*                                p_ref_phone_numb_im         = l_phone            "M.O. 22-09-2022 del
*                              TABLES                                         "M.O. 22-09-2022 del
*                                pt_crs_im                   = tb_ztzbctrkcr  "M.O. 22-09-2022 del
*                              EXCEPTIONS                                     "M.O. 22-09-2022 del
**                               errors_in_update_elabration = 1              "M.O. 22-09-2022 del
*                                data_not_found              = 2              "M.O. 22-09-2022 del
*                                system_failure              = 3              "M.O. 22-09-2022 del
*                                communication_failure       = 4              "M.O. 22-09-2022 del
*                                OTHERS                      = 5.           "M.O. 22-09-2022 del
*                            IF sy-subrc NE 0.                              "M.O. 22-09-2022 del
*                              l_subrc = sy-subrc.                          "M.O. 22-09-2022 del
*                            ENDIF.                                        "M.O. 22-09-2022 del
*                          ELSEIF l_subrc <> 0. "P.A.                      ""M.O. 22-09-2022 del
*                            MESSAGE e000(db) WITH text-e09. "P.A.        "M.O. 22-09-2022 del
*                          ENDIF.                                         "M.O. 22-09-2022 del
**                         ENDIF. "M.O. 22-09-2022 - TOGLIERE
                        ENDIF.
                      ENDIF.
                    ENDIF.
                    IF ls_syst-zinfo_nrt = 'X'.
                      READ TABLE tb_crtrasp WITH KEY zaction = '01'.
                      IF sy-subrc = 0.
                        CLEAR lv_answer.
                        CALL FUNCTION 'POPUP_TO_CONFIRM'
                          EXPORTING
                            titlebar              = text-535
                            text_question         = text-536
                            text_button_1         = text-537
                            text_button_2         = text-538
                            display_cancel_button = 'X'
                          IMPORTING
                            answer                = lv_answer.
                        IF lv_answer = '1'.
                          IF sy-uname IN r_user1.
                            CALL FUNCTION 'ZTZBCTRK_NRT_POPUP'
                              EXPORTING
                                i_system  = lv_system_mainc
                                i_history = ''
                                i_display = ''
                                i_nrtinfo = ''
                              IMPORTING
                                e_nrtinfo = lv_ok
                              TABLES
                                i_crs     = tb_ztzbctrkcr.
                            IF lv_ok = 'E'.
                              MESSAGE e134(ztrk).
                            ENDIF.
                          ELSE.
                            LOOP AT tb_ztzbctrkcr.
                              MOVE 'X' TO tb_ztzbctrkcr-zinfo_nrt.
                              MODIFY tb_ztzbctrkcr INDEX sy-tabix.
                            ENDLOOP.
                          ENDIF.
                        ELSEIF lv_answer = 'A'. "P.A.
                          MESSAGE e398(00) WITH text-w12. "P.A.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                    IF ls_syst-zweekly_tran = 'X'.
                      READ TABLE tb_crtrasp WITH KEY zaction = '01'.
                      IF sy-subrc = 0.

                        CLEAR: lt_fields, lt_data, lt_options.
                        FREE : lt_fields, lt_data, lt_options.
                        MOVE 'WEEKLY_DATE' TO ls_fields.
                        APPEND ls_fields TO lt_fields.

                        CONCATENATE '''' ls_syst-zsystem '''' INTO ls_options.
                        CONCATENATE 'ZSYSTEM =' ls_options INTO ls_options SEPARATED BY space.
                        APPEND ls_options TO lt_options.
                        CONCATENATE '''' ls_syst-zclient '''' INTO ls_options.
                        CONCATENATE 'AND ZCLIENT =' ls_options INTO ls_options SEPARATED BY space.
                        APPEND ls_options TO lt_options.

                        CALL FUNCTION 'RFC_READ_TABLE'
                          DESTINATION va_zrfc
                          EXPORTING
                            query_table           = ca_tabname_weekly
                          TABLES
                            fields                = lt_fields
                            options               = lt_options
                            data                  = lt_data
                          EXCEPTIONS
                            system_failure        = 1
                            communication_failure = 2
                            OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e19 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
                        DATA: lv_weeklydate(12).

                        READ TABLE lt_data INTO ls_data INDEX 1.
*                        IF NOT ls_data IS INITIAL.
                        CONCATENATE text-543 ls_data INTO lv_weeklydate SEPARATED BY space.
*                          CONCATENATE lv_weeklydate_date text-543 TO lv_weeklydate_date
*                          IF lv_weeklydate_date >= SY-DATUM.
*                          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
*                            EXPORTING
*                              date_internal            = lv_weeklydate_date
*                            IMPORTING
*                              date_external            = lv_weeklydate
*                            EXCEPTIONS
*                              date_internal_is_invalid = 1
*                              OTHERS                   = 2.
*                          IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*                          ENDIF.
*                          CONCATENATE text-598 lv_weeklydate
*                                  INTO lv_weeklydate SEPARATED BY space.
*                          CONCATENATE lv_weeklydate '.'
*                                  INTO lv_weeklydate.
*                          CONCATENATE lv_weeklydate text-541
*                                  INTO lv_weeklydate SEPARATED BY space.
*                          ELSE.
*                            MOVE text-541 TO lv_weeklydate.
*                          ENDIF.
*                        ELSE.
*                          MOVE text-541 TO lv_weeklydate.
*                        ENDIF.
                        CLEAR lv_answer.
                        CALL FUNCTION 'POPUP_TO_CONFIRM'
                          EXPORTING
                            titlebar              = text-540
                            text_question         = text-541
                            text_button_1         = text-542
                            text_button_2         = lv_weeklydate
                            display_cancel_button = 'X' "P.A.
                          IMPORTING
                            answer                = lv_answer.
                        IF lv_answer = '2'.
                          LOOP AT tb_ztzbctrkcr.
                            MOVE 'X' TO tb_ztzbctrkcr-zweekly_tran.
                            MODIFY tb_ztzbctrkcr INDEX sy-tabix.
                          ENDLOOP.
                        ELSEIF lv_answer = 'A'. "P.A.
                          MESSAGE e398(00) WITH text-w12. "P.A.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                    IF NOT ls_syst-zrelsystem IS INITIAL.
                      MESSAGE i000(0h) WITH text-499 ls_syst-zrelsystem text-498.
                    ENDIF.
                  ENDIF.
*              ENDIF.

                  IF l_subrc IS INITIAL.
                    CLEAR: l_warng_elab_4_next_step_flag.
                    CALL FUNCTION 'ZTZBCTRK_SAVE_CRS'
                      DESTINATION va_zrfc
                      EXPORTING
                        i_uname               = sy-uname
                        i_action              = 'A'
                        i_system              = p_ls_zstrklayalv-zsystem
                        i_name                = adrp-name_text
                        i_seq_changed         = lv_seqchange
                      IMPORTING
                        elab_warning_flag_ch  = l_warng_elab_4_next_step_flag
                      TABLES
                        tb_cr                 = tb_ztzbctrkcr
                      EXCEPTIONS
                        system_failure        = 1
                        communication_failure = 2
                        OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e20 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
                    IF l_warng_elab_4_next_step_flag IS NOT INITIAL.
                      MESSAGE i000(db) WITH text-w01.
                    ELSE.
                      MESSAGE s000(db) WITH text-s03.
                    ENDIF.
*            ELSE. "P.A.
*                MESSAGE e000(db) WITH text-e09. "P.A.
                  ENDIF.

                ELSE.

                  CALL FUNCTION 'ZTZBCTRK_RELEASE_CR_PREPARE'
                    DESTINATION va_zrfc
                    EXPORTING
                      i_system                   = p_ls_zstrklayalv-zsystem
                      i_uname                    = sy-uname
                    TABLES
                      tb_cr                      = tb_ztzbctrkcr
                    EXCEPTIONS
                      cts_initialization_failure = 1
                      enqueue_failed             = 2
                      no_authorization           = 3
                      invalid_request            = 4
                      request_already_released   = 5
                      repeat_too_early           = 6
                      error_in_export_methods    = 7
                      object_check_error         = 8
                      docu_missing               = 9
                      db_access_error            = 10
                      action_aborted_by_user     = 11
                      export_failed              = 12
                      system_failure             = 13
                      communication_failure      = 14
                      OTHERS                     = 15.

                  IF sy-subrc <> 0.
                    MESSAGE e000(db) WITH text-e06.
                  ELSE.
                    MESSAGE s000(db) WITH text-s01.
                  ENDIF.

                ENDIF.
              ENDIF.
            ELSE.
              MESSAGE e000(db) WITH text-e02.
            ENDIF.
          ELSE.
            MESSAGE e000(db) WITH text-e03.
          ENDIF.
        ELSE.
          MESSAGE w000(db) WITH text-e01 sy-uname.
        ENDIF.
        PERFORM refresh.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " POPUP_SEQUENCE_CR
*&---------------------------------------------------------------------*
*&      Form  CHECK_SEQUENCE
*&---------------------------------------------------------------------*
FORM check_sequence CHANGING p_fl_errseq
                             p_fl_errnode.

  DATA: vn_sequence LIKE zstrklayalv-zsequence.
  DATA: va_znode LIKE zstrklayalv-znode.
  DATA: l_ctsproject TYPE zstrklayalv-ctsproject.
  SORT tb_crtrasp BY zsequence.

  DATA: vn_lines(3) TYPE n.

  DESCRIBE TABLE tb_crtrasp LINES vn_lines.

  IF vn_lines > 1.

    LOOP AT tb_crtrasp.
      ADD 1 TO vn_sequence.

      IF va_znode >< space AND
           tb_crtrasp-znode >< va_znode.
        p_fl_errnode = 'X'.
        EXIT.
      ENDIF.

*      IF sy-uname NE 'TO4IJ'.
*        IF l_ctsproject >< space AND
*             tb_crtrasp-ctsproject >< l_ctsproject.
*          p_fl_errnode = 'X'.
*          EXIT.
*        ENDIF.
*      ENDIF.
      IF tb_crtrasp-zsequence >< vn_sequence OR
         tb_crtrasp-zsequence IS INITIAL.
        p_fl_errseq = 'X'.
        EXIT.
      ENDIF.

      va_znode = tb_crtrasp-znode.
      l_ctsproject = tb_crtrasp-ctsproject.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " CHECK_SEQUENCE
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data USING    p_wa_data TYPE g_outtab_line.


  CLEAR p_wa_data-zactiv.
  CLEAR p_wa_data-zimpautom.
  CLEAR p_wa_data-zadremail.
  CLEAR p_wa_data-trkorr.
  CLEAR p_wa_data-as4text.
  CLEAR p_wa_data-as4user.
  CLEAR p_wa_data-zrstprec.
  CLEAR p_wa_data-zstconn.
  CLEAR p_wa_data-zdestsys.
  CLEAR p_wa_data-zdmsicon.
*  CLEAR p_wa_data-zbutselcr.
*  CLEAR p_wa_data-znode.
  CLEAR p_wa_data-zsequence.
  CLEAR p_wa_data-zstatcr.
  CLEAR p_wa_data-zfield1.
  CLEAR p_wa_data-zfield2.
  CLEAR p_wa_data-zfield3.
  CLEAR p_wa_data-zfield4.
  CLEAR p_wa_data-zfield5.


ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_DMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LS_DATA_TRKORR  text
*      <--P_FL_RET  text
*----------------------------------------------------------------------*
FORM check_dms  USING    p_p_ls_data_trkorr
                CHANGING p_fl_ret.


  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  CALL FUNCTION 'ZTZBCTRK_ATTACHMENT_CHECK'
    DESTINATION va_zrfc
    EXPORTING
      i_trkorr              = p_p_ls_data_trkorr
      i_type                = 'E070'
    IMPORTING
      e_doc                 = p_fl_ret
    EXCEPTIONS
      system_failure        = 1
      communication_failure = 2
      OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e21 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
ENDFORM.                    " CHECK_DMS
*&---------------------------------------------------------------------*
*&      Form  HISTOR_CR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ZSTRKLAYALV_TRKORR  text
*----------------------------------------------------------------------*
FORM histor_cr  USING    p_ls_zstrklayalv_trkorr.

* Local Reference Variables.
  DATA: l_dref1 TYPE REF TO data,
        l_dref2 TYPE REF TO data.

* Local Structures
  DATA: ls_variant TYPE disvariant,
        ls_lvc_layo  TYPE lvc_s_layo,
        ls_layout     TYPE slis_layout_alv,
        lt_lvc_fcat TYPE lvc_t_fcat,
        ls_lvc_fcat TYPE lvc_s_fcat,
        ls_fieldcat TYPE slis_fieldcat_alv,
        lt_fieldcat TYPE slis_t_fieldcat_alv,
        lt_generic   TYPE ztrk_generic_t.

* Local Variables
  DATA: l_save      TYPE char1,
        l_default   TYPE char1,
        l_scr_start_col_c TYPE char1,
        l_scr_start_line_c TYPE char1,
        l_scr_end_col_c TYPE char1,
        l_scr_end_line_c TYPE char1,
        l_scr_start_col_i TYPE i,
        l_scr_start_line_i TYPE i,
        l_scr_end_col_i TYPE i,
        l_scr_end_line_i TYPE i.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.
    CLEAR: ls_variant, ls_layout, ls_lvc_layo,  lt_lvc_fcat[], lt_generic, l_save,
           l_scr_start_col_c, l_scr_start_line_c, l_scr_end_col_c, l_scr_end_line_c,
           l_scr_start_col_i, l_scr_start_line_i, l_scr_end_col_i, l_scr_end_line_i,
           ls_fieldcat, lt_fieldcat[], l_dref1, l_dref2.

    CALL FUNCTION 'ZTZBCTRK_CR_HISTOR'
      DESTINATION va_zrfc
      EXPORTING
        i_trkorr              = p_ls_zstrklayalv_trkorr
      IMPORTING
        ex_fieldcat_tab       = lt_lvc_fcat[]
        ex_data_to_show_tab   = lt_generic[]
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e22 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS

    IF ( ( NOT lt_lvc_fcat IS INITIAL ) AND
      ( NOT lt_generic[] IS NOT INITIAL ) ).
      LOOP AT lt_lvc_fcat INTO ls_lvc_fcat.
        MOVE-CORRESPONDING ls_lvc_fcat TO ls_fieldcat.
        ls_fieldcat-ref_tabname   = ls_lvc_fcat-ref_table.
        ls_fieldcat-ref_fieldname = ls_lvc_fcat-ref_field.
        ls_fieldcat-seltext_l     = ls_lvc_fcat-scrtext_l.
        ls_fieldcat-seltext_m     = ls_lvc_fcat-scrtext_m.
        ls_fieldcat-seltext_s     = ls_lvc_fcat-scrtext_s.
        APPEND  ls_fieldcat TO lt_fieldcat[].
      ENDLOOP.


      CALL METHOD cl_alv_table_create=>create_dynamic_table
        EXPORTING
          it_fieldcatalog = lt_lvc_fcat[]
        IMPORTING
          ep_table        = l_dref1.

      UNASSIGN <data_to_show_t>.
      ASSIGN l_dref1->* TO <data_to_show_t>.
      IF sy-subrc = 0.
        <data_to_show_t>[] = lt_generic[].
      ENDIF.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program       = sy-repid
          i_callback_pf_status_set = 'PF_STATUS_SET_2'
          i_callback_user_command  = 'USER_COMMAND_2'
          is_layout                = ls_layout
          it_fieldcat              = lt_fieldcat[]
          i_default                = 'X'
          i_save                   = 'A'
          is_variant               = ls_variant
          i_screen_start_column    = 30
          i_screen_start_line      = 2
          i_screen_end_column      = 170
          i_screen_end_line        = 15
        TABLES
          t_outtab                 = <data_to_show_t>
        EXCEPTIONS
          program_error            = 1
          OTHERS                   = 2.
    ENDIF.
  ENDIF.
ENDFORM.                    " HISTOR_CR

*&---------------------------------------------------------------------*
*&      Form  LOG_CR
*&---------------------------------------------------------------------*
FORM log_cr  USING    p_ls_zstrklayalv_trkorr
                      p_ls_zstrklayalv_zsystem
                      p_ls_zstrklayalv_zclient
                      p_ls_zstrklayalv_comsys
                      p_ls_zstrklayalv_srccli.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.

    CALL FUNCTION 'ZTZBCTRK_CR_MANAGE_PREPARE'
      DESTINATION va_zrfc
      EXPORTING
        i_trkorr              = p_ls_zstrklayalv_trkorr
        i_command             = 'TAST'
        i_system              = p_ls_zstrklayalv_comsys
        i_client              = p_ls_zstrklayalv_srccli
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e23 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
  ENDIF.

ENDFORM.                    " LOG_CR
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ZSTRKLAYALV_TRKORR  text
*----------------------------------------------------------------------*
FORM display_cr  USING    p_ls_zstrklayalv_trkorr
                          p_ls_zstrklayalv_zsystem
                          p_ls_zstrklayalv_tarclient
                          p_ls_zstrklayalv_comsys
                          p_ls_zstrklayalv_srccli.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.

    CALL FUNCTION 'ZTZBCTRK_CR_MANAGE_PREPARE'
      DESTINATION va_zrfc
      EXPORTING
        i_trkorr              = p_ls_zstrklayalv_trkorr
        i_command             = 'REQUEST_SHOW'
        i_system              = p_ls_zstrklayalv_comsys
        i_client              = p_ls_zstrklayalv_srccli
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTSS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e23 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
  ENDIF.


ENDFORM.                    " DISPLAY_CR
*&---------------------------------------------------------------------*
*&      Form  INSPECTION_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ZSTRKLAYALV_TRKORR  text
*----------------------------------------------------------------------*
FORM inspection_code  USING    p_ls_zstrklayalv_trkorr
                               p_ls_zstrklayalv_zsystem
                               p_ls_zstrklayalv_zclient
                               p_ls_zstrklayalv_comsys
                               p_ls_zstrklayalv_srccli.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.

    CALL FUNCTION 'ZTZBCTRK_CR_MANAGE_PREPARE'
      DESTINATION va_zrfc
      EXPORTING
        i_trkorr              = p_ls_zstrklayalv_trkorr
        i_command             = 'CHAO'
        i_system              = p_ls_zstrklayalv_comsys
        i_client              = p_ls_zstrklayalv_srccli
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e23 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
  ENDIF.


ENDFORM.                    " INSPECTION_CODE
*&---------------------------------------------------------------------*
*&      Form  GET_CRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_crs .

  CLEAR tb_e070.
  REFRESH tb_e070.

  CLEAR va_zrfc.

  CLEAR tb_next.
  REFRESH tb_next.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.

    CALL FUNCTION 'ZTZBCTRK_GET_CRS'
      DESTINATION va_zrfc
      EXPORTING
        i_uname               = sy-uname
      TABLES
        t_next                = tb_next
      EXCEPTIONS
        system_with_alertid   = 1
        not_found             = 2
        system_failure        = 3
        communication_failure = 4
        OTHERS                = 5.

*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e24 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS

*    IF lv_syserror NE ''.
*      MESSAGE i000(0h) WITH text-890 lv_syserror.
*    ENDIF.

    LOOP AT tb_next ASSIGNING <fs_next>.
      tb_e070-trkorr          = <fs_next>-trkorr.
      tb_e070-as4user         = <fs_next>-as4user.
      tb_e070-zstatcr         = 'RLS'.
      tb_e070-as4text         = <fs_next>-as4text.
      tb_e070-zsystem         = <fs_next>-tarsystem.
      tb_e070-loekz           = <fs_next>-zloekz.
      tb_e070-zindidng        = <fs_next>-zindidng.
      tb_e070-ctsproject      = <fs_next>-externalid.
      tb_e070-tarclient       = <fs_next>-client.
      tb_e070-strkorr         = <fs_next>-strkorr.
      tb_e070-zaction         = <fs_next>-zaction .
      tb_e070-bypass          = <fs_next>-bypass.
      tb_e070-comsys          = <fs_next>-comsys .
      tb_e070-srccli          = <fs_next>-srccli.
      tb_e070-korrdev         = <fs_next>-korrdev.
      tb_e070-sysid_bypassed  = <fs_next>-zsyst_bypassed.
      tb_e070-rejected        = <fs_next>-rejected.
      tb_e070-zinfo_nrt       = <fs_next>-zinfo_nrt.
      tb_e070-zweekly_tran    = <fs_next>-zweekly_tran.
      tb_e070-zfield1         = <fs_next>-zfield1.
      tb_e070-zfield2         = <fs_next>-zfield2.
      tb_e070-zfield3         = <fs_next>-zfield3.
      tb_e070-zfield4         = <fs_next>-zfield4.
      tb_e070-zfield5         = <fs_next>-zfield5.
      APPEND tb_e070.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_CRS
*&---------------------------------------------------------------------*
*&      Form  SET_CR_DANGER
*&---------------------------------------------------------------------*
FORM set_cr_danger  USING p_ls_zstrklayalv STRUCTURE zstrklayalv.


  DATA: sv_outtab TYPE STANDARD TABLE OF g_outtab_line.

  DATA: ls_outtab LIKE zstrklayalv.
  DATA: ls_data TYPE REF TO data.

  DATA: nodes TYPE REF TO cl_salv_nodes.
  DATA: node TYPE REF TO cl_salv_node.

  DATA: BEGIN OF tb_ztzbctrkcr OCCURS 1000.
          INCLUDE STRUCTURE ztzbctrkcr_st.
  DATA: END OF tb_ztzbctrkcr.

  TRY.

      nodes = gr_tree->get_nodes( ).

      sv_outtab[] = gt_outtab[].

      nodes->delete_all( ).

      DELETE sv_outtab  WHERE trkorr = space.

      READ TABLE sv_outtab INTO ls_outtab WITH KEY zsystem = p_ls_zstrklayalv-zsystem
                                                   trkorr =  p_ls_zstrklayalv-trkorr.
      IF sy-subrc = 0.
        ls_outtab-zindidng = 'X'.
        MODIFY sv_outtab INDEX sy-tabix FROM ls_outtab.

        FREE tb_ztzbctrkcr.
        MOVE-CORRESPONDING p_ls_zstrklayalv TO tb_ztzbctrkcr.
        tb_ztzbctrkcr-externalid = p_ls_zstrklayalv-ctsproject.
        tb_ztzbctrkcr-as4date    = sy-datum.
        tb_ztzbctrkcr-as4time    = sy-uzeit.
        tb_ztzbctrkcr-tarsystem = p_ls_zstrklayalv-zsystem.
        tb_ztzbctrkcr-zindidng  = 'X'.
        APPEND tb_ztzbctrkcr.

        SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

        IF sy-subrc = 0.

          CALL FUNCTION 'ZTZBCTRK_SAVE_CRS'
            DESTINATION va_zrfc
            EXPORTING
              i_uname               = sy-uname
              i_action              = 'D'
              i_system              = p_ls_zstrklayalv-zsystem
            TABLES
              tb_cr                 = tb_ztzbctrkcr
            EXCEPTIONS
              system_failure        = 1
              communication_failure = 2
              OTHERS                = 3.


*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e25 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS

        ENDIF.
      ENDIF.

      PERFORM supply_data USING sv_outtab.

      nodes->expand_all( ).

    CATCH cx_salv_error.
  ENDTRY.

  gr_tree->display( ).

ENDFORM.                    " SET_CR_DANGER
*&---------------------------------------------------------------------*
*&      Form  SET_ICON_DMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LS_ZSTRKLAYALV_ZSYSTEM  text
*      -->P_P_LS_ZSTRKLAYALV_TRKORR  text
*----------------------------------------------------------------------*
FORM set_icon_dms  USING    p_ls_zstrklayalv-zsystem
                            p_ls_zstrklayalv-trkorr.

  DATA: sv_outtab TYPE STANDARD TABLE OF g_outtab_line.

  DATA: ls_outtab LIKE zstrklayalv.
  DATA: ls_data TYPE REF TO data.

  DATA: nodes TYPE REF TO cl_salv_nodes.
  DATA: node TYPE REF TO cl_salv_node.

  TRY.

      nodes = gr_tree->get_nodes( ).

      sv_outtab[] = gt_outtab[].
*      sv_outtab[] = lt_outtab[].

      nodes->delete_all( ).

      DELETE sv_outtab  WHERE trkorr = space.

      READ TABLE sv_outtab INTO ls_outtab WITH KEY zsystem = p_ls_zstrklayalv-zsystem
                                                   trkorr =  p_ls_zstrklayalv-trkorr.
      IF sy-subrc = 0.
        ls_outtab-zindidms = 'X'.
        MODIFY sv_outtab INDEX sy-tabix FROM ls_outtab.
      ENDIF.

      PERFORM supply_data USING sv_outtab.


*      nodes->expand_all( ).

    CATCH cx_salv_error.
  ENDTRY.

  gr_tree->display( ).

ENDFORM.                    " SET_ICON_DMS
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh .

*  if sy-uname = 'TO4IJ'.
  LEAVE TO TRANSACTION 'ZTRKSYST'.
*  else.
*  DATA: nodes TYPE REF TO cl_salv_nodes.
*
*  nodes = gr_tree->get_nodes( ).
*
*  nodes->delete_all( ).
*
*  CLEAR gt_outtab.
*  REFRESH gt_outtab.
*
*  PERFORM create_tree.
*
**... §3 Functions
**... §3.1 activate ALV generic Functions
*  gr_tree->set_screen_status(
*    pfstatus      =  'SALV_STANDARD'
*    report        =  gs_test-repid
*    set_functions =  gr_tree->c_functions_all ).
*
*  DATA: lr_functions TYPE REF TO cl_salv_functions_tree.
*  lr_functions = gr_tree->get_functions( ).
*  lr_functions->set_group_print( abap_false ).
*
*  PERFORM set_columns_technical CHANGING gs_indez_show.
*
*  PERFORM application_action USING gs_indez_show.
*
**... §6 display the table
*  gr_tree->display( ).
*
*  CLEAR sy-ucomm.
*  endif.
ENDFORM.                    " REFRESH
*&---------------------------------------------------------------------*
*&      Form  DELETE_CR_FROM_POPUP
*&---------------------------------------------------------------------*

FORM delete_cr_from_popup .
  DATA: lv_subrc,
        ls_field     TYPE sval,
        lt_field     TYPE STANDARD TABLE OF sval.

  MOVE : 'ZTZBCTRKSYS'  TO ls_field-tabname,
         'ZSYSTEM'      TO ls_field-fieldname.
  APPEND ls_field TO lt_field.
  MOVE : 'ZTZBCTRKSYS'  TO ls_field-tabname,
         'ZCLIENT'      TO ls_field-fieldname.
  APPEND ls_field TO lt_field.
  MOVE : 'E070'         TO ls_field-tabname,
         'TRKORR'       TO ls_field-fieldname.
  APPEND ls_field TO lt_field.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*     NO_VALUE_CHECK  = ' '
      popup_title     = text-521
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = lv_subrc
    TABLES
      fields          = lt_field
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc = 0 AND lv_subrc IS INITIAL.
    READ TABLE lt_field INTO ls_field INDEX 1.
    MOVE ls_field-value TO ls_zstrklayalv-zsystem.
    IF NOT ls_field-value IS INITIAL.
      READ TABLE lt_field INTO ls_field INDEX 2.
      MOVE ls_field-value TO ls_zstrklayalv-tarclient.
      IF NOT ls_field-value IS INITIAL.
        READ TABLE lt_field INTO ls_field INDEX 3.
        FREE tb_crs1.
        IF NOT ls_field-value IS INITIAL.
          MOVE: ls_field-value          TO tb_crs1-trkorr,
                ls_zstrklayalv-zsystem  TO tb_crs1-tarsystem.
          APPEND tb_crs1.
          SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.
          IF sy-subrc = 0.

            CALL FUNCTION 'ZTZBCTRK_DELBUF_CR_PREPARE'
              DESTINATION va_zrfc
              EXPORTING
                i_system              = ls_zstrklayalv-zsystem
                i_client              = ls_zstrklayalv-tarclient
              TABLES
                i_trkorr              = tb_crs1
              EXCEPTIONS
                not_found             = 1
                system_failure        = 2
                communication_failure = 3
                OTHERS                = 4.

*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e26 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " DELETE_CR_FROM_POPUP
*&---------------------------------------------------------------------*
*&      Form  DELETE_CR
*&---------------------------------------------------------------------*

FORM delete_cr  USING    p_ls_zstrklayalv_trkorr
                         p_ls_zstrklayalv_zsystem
                         p_ls_zstrklayalv_tarclient.

  DATA: sv_outtab TYPE STANDARD TABLE OF g_outtab_line.

  DATA: ls_outtab LIKE zstrklayalv.
  DATA: ls_data TYPE REF TO data.

  DATA: nodes TYPE REF TO cl_salv_nodes.
  DATA: node TYPE REF TO cl_salv_node.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.
    IF ls_zstrklayalv-zstatcr = 'RLS'.
      CLEAR tb_crs1.
      FREE tb_crs1.
      MOVE p_ls_zstrklayalv_trkorr TO tb_crs1-trkorr.
      APPEND tb_crs1.

      CALL FUNCTION 'ZTZBCTRK_DELBUF_CR_PREPARE'
        DESTINATION va_zrfc
        EXPORTING
          i_system              = p_ls_zstrklayalv_zsystem
          i_client              = p_ls_zstrklayalv_tarclient
        TABLES
          i_trkorr              = tb_crs1
        EXCEPTIONS
          not_found             = 1
          system_failure        = 2
          communication_failure = 3
          OTHERS                = 4.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e26 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
    ELSE.
      CALL FUNCTION 'ZTZBCTRK_CR_MANAGE_PREPARE'
        DESTINATION va_zrfc
        EXPORTING
          i_trkorr              = p_ls_zstrklayalv_trkorr
          i_command             = 'DELE'
          i_system              = p_ls_zstrklayalv_zsystem
          i_client              = p_ls_zstrklayalv_tarclient
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e27 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
    ENDIF.
  ENDIF.

  IF sy-subrc = 0.

    TRY.

        nodes = gr_tree->get_nodes( ).

        sv_outtab[] = gt_outtab[].

        nodes->delete_all( ).

        DELETE sv_outtab  WHERE trkorr = space.
        DELETE sv_outtab  WHERE trkorr = p_ls_zstrklayalv_trkorr.

        PERFORM supply_data USING sv_outtab.


        nodes->expand_all( ).

      CATCH cx_salv_error.
    ENDTRY.

    gr_tree->display( ).

  ENDIF.


ENDFORM.                    " DELETE_CR
*&---------------------------------------------------------------------*
*&      Form  REJECT_TRANSPORT_CR
*&---------------------------------------------------------------------*
FORM reject_transport_cr  USING  p_ls_zstrklayalv TYPE ty_zstrklayalv_s.

  MOVE-CORRESPONDING p_ls_zstrklayalv TO tb_crtrasp.

  APPEND tb_crtrasp .

ENDFORM.                    " REJECT_TRANSPORT_CR


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

* Field Symbols
  FIELD-SYMBOLS: <sequence> TYPE zstrklayalv.

* Local Structures
  DATA: ls_sequence_1 TYPE zstrklayalv,
        ls_sequence_2 TYPE zstrklayalv,
        ls_tmscsys    TYPE tmscsys.
  DATA lv_filename LIKE rlgrap-filename.
  DATA: lt_trkorr   TYPE TABLE OF ztzbctrkcr_st,
     ls_trkorr   TYPE ztzbctrkcr_st.

* Local Variables
  DATA: l_filename         TYPE string,
        l_idx              TYPE i,
        lv_rc              LIKE sy-subrc,
        lv_nullseq,
        lv_tabix           LIKE sy-tabix,
        l_lines            TYPE i.

  DATA: tb_int LIKE zalsmex_tabline OCCURS 0 WITH HEADER LINE.

* B.o.P
  CASE r_ucomm.
    WHEN 'UP'.
      lv_seqchange = 'X'.
      CLEAR: ls_sequence_1, ls_sequence_2, l_idx.
      IF rs_selfield-tabindex > 1.
        READ TABLE tb_crtrasp[] INTO ls_sequence_1 INDEX rs_selfield-tabindex.
        IF sy-subrc = 0.
          l_idx = sy-tabix.
          ls_sequence_2 = ls_sequence_1.
          DELETE tb_crtrasp[] INDEX l_idx .
          l_idx = rs_selfield-tabindex - 1.
          INSERT ls_sequence_2 INTO tb_crtrasp[] INDEX l_idx.

          UNASSIGN: <sequence>.
          LOOP AT tb_crtrasp ASSIGNING <sequence>.
            <sequence>-zsequence = sy-tabix.
          ENDLOOP.
        ENDIF.
        rs_selfield-refresh = 'X'.
        r_ucomm = '&OPT'.
      ENDIF.

    WHEN 'DOWN'.
      lv_seqchange = 'X'.
      CLEAR: ls_sequence_1, ls_sequence_2, l_idx, l_lines.
      LOOP AT tb_crtrasp INTO ls_sequence_1.
        l_lines = l_lines + 1.
      ENDLOOP.

      IF rs_selfield-tabindex < l_lines.
        READ TABLE tb_crtrasp[] INTO ls_sequence_1 INDEX rs_selfield-tabindex.
        IF sy-subrc = 0.
          l_idx = sy-tabix.
          ls_sequence_2 = ls_sequence_1.
          DELETE tb_crtrasp INDEX l_idx .
          l_idx = rs_selfield-tabindex + 1.
          INSERT ls_sequence_2 INTO tb_crtrasp INDEX l_idx.

          UNASSIGN: <sequence>.
          LOOP AT tb_crtrasp ASSIGNING <sequence>.
            <sequence>-zsequence = sy-tabix.
          ENDLOOP.
        ENDIF.

        rs_selfield-refresh = 'X'.
        r_ucomm = '&OPT'.

      ENDIF.

    WHEN 'UPLOAD'.

      IF sy-uname = 'TO4IJ' OR sy-uname = 'F39147A'.
        CALL FUNCTION 'WS_FILENAME_GET'
          EXPORTING
            def_filename     = ' '
            def_path         = 'c:\'
            mask             = ',*.*.'
            mode             = 'O'
            title            = text-302
          IMPORTING
            filename         = lv_filename
          EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.

        CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
          EXPORTING
            filename    = lv_filename
            i_begin_col = 1
            i_begin_row = 1
            i_end_col   = 1
            i_end_row   = 5000
          TABLES
            intern      = tb_int.

        LOOP AT tb_int.
          READ TABLE  tb_crtrasp ASSIGNING <sequence>.
          IF sy-subrc = 0.
            <sequence>-zsequence = tb_int-row.
          ENDIF.
        ENDLOOP.

        DESCRIBE TABLE tb_crtrasp LINES lv_tabix.
        IF lv_tabix < tb_int-row.
          lv_tabix = tb_int-row.
        ENDIF.
        CLEAR lv_nullseq.
        LOOP AT tb_crtrasp ASSIGNING <sequence>.
          IF <sequence>-zsequence IS INITIAL.
            lv_nullseq = 'X'.
            ADD 1 TO lv_tabix.
            <sequence>-zsequence = lv_tabix.
          ENDIF.
        ENDLOOP.

        IF lv_nullseq = 'X'.
          MESSAGE i000(0h) WITH text-529 text-530 text-531.
        ENDIF.
      ENDIF.
    WHEN 'OK'.
      "      l_idx = 1.
      "      LOOP AT tb_crtrasp[] INTO ls_sequence_1.
      "        IF l_idx NE ls_sequence_1-zsequence.
      "          l_idx = 0.
      "          EXIT.
      "        ENDIF.
      "        ADD 1 TO l_idx.
      "      ENDLOOP.
      "      IF l_idx = 0.
      SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.
      FREE lt_trkorr.
      LOOP AT tb_crtrasp[] INTO ls_sequence_1.
        MOVE ls_sequence_1-trkorr  TO ls_trkorr-trkorr.
        MOVE sy-tabix TO ls_trkorr-as4time.
        APPEND ls_trkorr TO lt_trkorr.
      ENDLOOP.

*          CALL FUNCTION 'ZTZBCTRK_DELBUF_CR_PREPARE'
*            DESTINATION va_zrfc
*            EXPORTING
*              i_system              = ls_sequence_1-zsystem
*              i_client              = ls_sequence_1-zclient
*            TABLES
*              i_trkorr              = lt_trkorr
*            EXCEPTIONS
*              not_found             = 1
*              system_not_found      = 2
*              system_failure        = 3
*              communication_failure = 4
*              OTHERS                = 5.


      CALL FUNCTION 'ZTZBCTRK_IS_QA_SYSTEM'
        EXPORTING
          i_system = ls_sequence_1-zsystem
        IMPORTING
          e_rc     = lv_rc
          e_comsys = ls_tmscsys-comsys
          e_domnam = ls_tmscsys-domnam
          e_sysnam = ls_tmscsys-sysnam.
      IF lv_rc = 0.

*      SELECT SINGLE * FROM tmscsys INTO ls_tmscsys
*                                          WHERE sysnam = ls_sequence_1-zsystem
*                                            AND systyp = 'V'.
*      IF sy-subrc = 0.
        CALL FUNCTION 'ZTZBCTRK_ADD_CR_PREPARE'
          DESTINATION va_zrfc
          EXPORTING
            i_system              = ls_sequence_1-zsystem
            i_tarcli              = ls_sequence_1-tarclient
            i_seq_changed         = lv_seqchange
          TABLES
            i_trkorr              = lt_trkorr
          EXCEPTIONS
            system_not_found      = 1
            system_failure        = 2
            communication_failure = 3
            OTHERS                = 4.
*   IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIEER
      IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
         IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
         ELSE."12-06-2023 NEW ENHANCEMENTS
            CASE SY-SUBRC."12-06-2023 NEW ENHANCEMENTS
                WHEN 1."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 ' system_not_found'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 2."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 'system_failure'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 3."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 'communication_failure'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 4."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28. "12-06-2023 NEW ENHANCEMENTS.
               ENDCASE.
          ENDIF. "12-06-2023 NEW ENHANCEMENTS.
       ENDIF."12-06-2023 NEW ENHANCEMENTS
*    ENDIF. "12-06-2023 NEW ENHANCEMENTS
      ELSE.
        IF lv_seqchange = 'X'.
          CALL FUNCTION 'ZTZBCTRK_ADD_CR_PREPARE'
            DESTINATION va_zrfc
            EXPORTING
              i_system              = ls_sequence_1-zsystem
              i_tarcli              = ls_sequence_1-tarclient
              i_seq_changed         = lv_seqchange
            TABLES
              i_trkorr              = lt_trkorr
            EXCEPTIONS
              system_not_found      = 1
              system_failure        = 2
              communication_failure = 3
              OTHERS                = 4.
*   IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIEER
      IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
         IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
         ELSE."12-06-2023 NEW ENHANCEMENTS
            CASE SY-SUBRC."12-06-2023 NEW ENHANCEMENTS
                WHEN 1."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 ' system_not_found'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 2."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 'system_failure'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 3."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 'communication_failure'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 4."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28. "12-06-2023 NEW ENHANCEMENTS.
               ENDCASE.
          ENDIF. "12-06-2023 NEW ENHANCEMENTS.
       ENDIF."12-06-2023 NEW ENHANCEMENTS
*    ENDIF. "12-06-2023 NEW ENHANCEMENTS
        ENDIF.

      ENDIF.
*      r_ucomm = '&F12'.  " del RM 17.10.2018
      r_ucomm = '&F15'.   "ins RM 17.10.2018

*
*    when 'BACK2'.
*      clear: it_fieldcat[].
*      perform initialize_fieldcat using g_repid changing it_fieldcat[].
*
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  user_command_2
*&---------------------------------------------------------------------*
FORM user_command_2 USING r_ucomm LIKE sy-ucomm
                               rs TYPE slis_selfield.

  FIELD-SYMBOLS: <trkorr>.
  DATA: l_fieldname(40).

  CASE r_ucomm.
    WHEN '&IC1' OR 'ATTACHMENT'.
      IF r_ucomm = 'ATTACHMENT'.
        rs-sel_tab_field = '1-DMS'.
      ENDIF.

      UNASSIGN <data_to_show_s>.
      READ TABLE <data_to_show_t> ASSIGNING <data_to_show_s> INDEX rs-tabindex.

      IF rs-sel_tab_field = '1-DMS'.
        MOVE '<data_to_show>-TRKORR' TO l_fieldname.
        ASSIGN (l_fieldname) TO <trkorr>.
        CALL FUNCTION 'ZTZBCTRK_ATTACHMENT'
          DESTINATION va_zrfc
          EXPORTING
            i_trkorr              = <trkorr>
            i_type                = 'E070'
          EXCEPTIONS
            system_failure        = 1
            communication_failure = 2
            OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e16 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
      ENDIF.
    WHEN 'OK'.
      r_ucomm = '&F12'.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  READ TABLE tb_crtrasp INDEX 1.
  READ TABLE xsystems WITH KEY zsystem = tb_crtrasp-zsystem.
  IF sy-subrc = 0 AND xsystems-zapprovsequ = ''.
    SET PF-STATUS 'SALV_STANDARD_D'.
  ELSE.
    SET PF-STATUS 'SALV_STANDARD_B'.
  ENDIF.
ENDFORM.                    "pf_status_set

*&---------------------------------------------------------------------*
*&      Form  pf_status_set_2
*&---------------------------------------------------------------------*
FORM pf_status_set_2 USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'SALV_STANDARD_C'.
ENDFORM.                    "pf_status_set

*&---------------------------------------------------------------------*
*&      Form  REJECT_CRS
*&---------------------------------------------------------------------*
FORM reject_crs.

  DATA: lv_objnr(16) TYPE n,
        lt_line      TYPE STANDARD TABLE OF  tline.

* Local Structures/Tables
  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.
  IF sy-subrc = 0.
    FREE tb_crs1.
    LOOP AT tb_crtrasp.
      MOVE-CORRESPONDING tb_crtrasp TO tb_crs1.
      MOVE tb_crtrasp-zsystem TO tb_crs1-tarsystem.
      APPEND tb_crs1.
    ENDLOOP.

    FREE lt_line.
    CLEAR lt_line.
    CALL FUNCTION 'ZTZBCTRK_NOTE'
      EXPORTING
        i_type  = 'R'
      IMPORTING
        e_objnr = lv_objnr
      TABLES
        tb_line = lt_line.

    IF NOT lv_objnr IS INITIAL OR NOT lt_line[] IS INITIAL .
      CALL FUNCTION 'ZTZBCTRK_REJECT_CR'
        DESTINATION va_zrfc
        EXPORTING
          i_uname               = sy-uname
        TABLES
          i_trkorr              = tb_crs1
          tb_line               = lt_line
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e29 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
    ENDIF.

  ENDIF.
ENDFORM.                    " REJECT_CRS
*&---------------------------------------------------------------------*
*&      Form  ADD_TO_BUFFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_to_buffer .

  DATA: lv_subrc,
        ls_field     TYPE sval,
        lt_field     TYPE STANDARD TABLE OF sval.

  MOVE : 'ZTZBCTRKSYS'  TO ls_field-tabname,
         'ZSYSTEM'      TO ls_field-fieldname.
  APPEND ls_field TO lt_field.
  MOVE : 'ZTZBCTRKSYS'  TO ls_field-tabname,
         'ZCLIENT'      TO ls_field-fieldname.
  APPEND ls_field TO lt_field.
  MOVE : 'E070'         TO ls_field-tabname,
         'TRKORR'       TO ls_field-fieldname.
  APPEND ls_field TO lt_field.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*     NO_VALUE_CHECK  = ' '
      popup_title     = text-520
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = lv_subrc
    TABLES
      fields          = lt_field
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc = 0 AND lv_subrc IS INITIAL.
    READ TABLE lt_field INTO ls_field INDEX 1.
    MOVE ls_field-value TO ls_zstrklayalv-zsystem.
    IF NOT ls_field-value IS INITIAL.
      READ TABLE lt_field INTO ls_field INDEX 2.
      MOVE ls_field-value TO ls_zstrklayalv-tarclient.
      IF NOT ls_field-value IS INITIAL.
        READ TABLE lt_field INTO ls_field INDEX 3.
        FREE tb_crs1.
        IF NOT ls_field-value IS INITIAL.
          MOVE: ls_field-value          TO tb_crs1-trkorr,
                ls_zstrklayalv-zsystem  TO tb_crs1-tarsystem.
          APPEND tb_crs1.
          SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.
          IF sy-subrc = 0.

            CALL FUNCTION 'ZTZBCTRK_DELBUF_CR_PREPARE'
              DESTINATION va_zrfc
              EXPORTING
                i_system              = ls_zstrklayalv-zsystem
                i_client              = ls_zstrklayalv-tarclient
              TABLES
                i_trkorr              = tb_crs1
              EXCEPTIONS
                not_found             = 1
                system_failure        = 2
                communication_failure = 3
                OTHERS                = 4.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e26 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS

            CALL FUNCTION 'ZTZBCTRK_ADD_CR_PREPARE'
              DESTINATION va_zrfc
              EXPORTING
                i_system              = ls_zstrklayalv-zsystem
                i_tarcli              = ls_zstrklayalv-tarclient
              TABLES
                i_trkorr              = tb_crs1
              EXCEPTIONS
                system_not_found      = 1
                system_failure        = 2
                communication_failure = 3
                OTHERS                = 4.
*   IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIEER
      IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
         IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
         ELSE."12-06-2023 NEW ENHANCEMENTS
            CASE SY-SUBRC."12-06-2023 NEW ENHANCEMENTS
                WHEN 1."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 ' system_not_found'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 2."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 'system_failure'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 3."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28 'communication_failure'. "12-06-2023 NEW ENHANCEMENTS.
                WHEN 4."12-06-2023 NEW ENHANCEMENTS
                 MESSAGE e000(ztrk) WITH text-e28. "12-06-2023 NEW ENHANCEMENTS.
               ENDCASE.
          ENDIF. "12-06-2023 NEW ENHANCEMENTS.
       ENDIF."12-06-2023 NEW ENHANCEMENTS
*    ENDIF. "12-06-2023 NEW ENHANCEMENTS
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " ADD_TO_BUFFER
*&---------------------------------------------------------------------*
*&      Form  SHOW_CR
*&---------------------------------------------------------------------*
FORM show_cr  USING    p_ls_zstrklayalv_trkorr
                       p_ls_zstrklayalv_zsystem
                       p_ls_zstrklayalv_tarclient.

  SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

  IF sy-subrc = 0.
    CALL FUNCTION 'ZTZBCTRK_CR_SHOW_PREPARE'
      DESTINATION va_zrfc
      EXPORTING
        iv_reqid              = p_ls_zstrklayalv_trkorr
        i_system              = p_ls_zstrklayalv_zsystem
        i_client              = p_ls_zstrklayalv_tarclient
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e30 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS

  ENDIF.

ENDFORM.                    " SHOW_CR

*&---------------------------------------------------------------------*
*&      Form  pop_up_for_values_sel_Block
*&---------------------------------------------------------------------*
FORM pop_up_for_values_sel_block CHANGING p_param_s_ch TYPE ztrkrelease_s
                                          p_subrc_ch   TYPE sy-subrc.

* Field Symbols
  FIELD-SYMBOLS: <fs1> TYPE any.

* Local Structures/tables
  DATA: ls_popup_data TYPE sval,
        lt_popup_data TYPE TABLE OF sval,
        lt_fields     TYPE slis_t_fieldcat_alv,
        ls_fields     TYPE slis_fieldcat_alv.

* Local Variables
  DATA: l_title_popup     TYPE string,
        l_answer          TYPE char1.

* B.o.M.
  CLEAR: ls_popup_data, lt_popup_data[], l_title_popup,
         p_param_s_ch, p_subrc_ch, lt_fields, ls_fields.

  l_title_popup = text-t02.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = 'ZTRKRELEASE_S'
    CHANGING
      ct_fieldcat            = lt_fields[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF lt_fields[] IS NOT INITIAL.
    LOOP AT lt_fields INTO ls_fields.
      ls_popup_data-tabname   = ls_fields-ref_tabname.
      ls_popup_data-fieldname = ls_fields-fieldname.
      ls_popup_data-fieldtext = ls_fields-seltext_m.
      ls_popup_data-field_obl = 'X'.
      APPEND ls_popup_data TO lt_popup_data[].
    ENDLOOP.
  ENDIF.

  IF lt_popup_data[] IS NOT INITIAL.
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = l_title_popup
      IMPORTING
        returncode  = l_answer
      TABLES
        fields      = lt_popup_data.

    CASE l_answer.
      WHEN 'A'.
        p_subrc_ch = 8.
      WHEN OTHERS.
        CLEAR: ls_popup_data.
        LOOP AT lt_popup_data INTO ls_popup_data.
          UNASSIGN: <fs1>.
          ASSIGN COMPONENT ls_popup_data-fieldname
              OF STRUCTURE p_param_s_ch  TO <fs1>.
          IF sy-subrc = 0.
            <fs1> = ls_popup_data-value.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDIF.

ENDFORM.                 "pop_up_for_values_sel_Block

*&---------------------------------------------------------------------*
*&      Form  pop_up_for_values
*&---------------------------------------------------------------------*

FORM pop_up_for_values CHANGING p_phone_ch TYPE /bev1/tselansp
                                p_subrc_ch TYPE sy-subrc.

* Local Structures/tables
  DATA: ls_popup_data TYPE sval,
        lt_popup_data TYPE TABLE OF sval.

* Local Variables
  DATA: l_title_popup     TYPE string,
        l_answer          TYPE char1,
        l_short_text      TYPE char20.


* B.o.M.
  CLEAR: ls_popup_data, lt_popup_data[], p_subrc_ch,
         l_title_popup, l_short_text, l_answer.

  l_title_popup = text-t01.
  ls_popup_data-tabname   = 'ZTZBCTRKCR'.
  ls_popup_data-fieldname ='PHONE'.

  CALL FUNCTION '/SAPDMC/DATAELEMENT_GET_TEXTS'
    EXPORTING
      name        = '/BEV1/TSELANSP'
    IMPORTING
      text_middle = l_short_text
    EXCEPTIONS
      not_found   = 1
      OTHERS      = 2.
  IF sy-subrc = 0.
    ls_popup_data-fieldtext = l_short_text.
  ENDIF.

  APPEND ls_popup_data TO lt_popup_data[].

  IF lt_popup_data[] IS NOT INITIAL.
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = l_title_popup
      IMPORTING
        returncode  = l_answer
      TABLES
        fields      = lt_popup_data.

    CASE l_answer.
      WHEN 'A'.
        p_subrc_ch = 8.
      WHEN OTHERS.
        READ TABLE lt_popup_data INTO ls_popup_data
          WITH KEY fieldname = 'PHONE'.
        IF sy-subrc = 0.
          p_phone_ch = ls_popup_data-value.
          IF p_phone_ch IS INITIAL.
            p_subrc_ch = 8.
          ENDIF.
        ELSE.
          p_subrc_ch = 8.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    " pop_up_for_values
*&---------------------------------------------------------------------*
*&      Form  RANGE_USER
*&---------------------------------------------------------------------*
FORM range_user .
*tabella per SET
  DATA:p_set     LIKE rgsbm-shortname,
       value     LIKE sethier-setid,
       tb_set    TYPE TABLE OF rgsbv WITH HEADER LINE,
       va_header LIKE rgsbs.

  CLEAR:value, tb_set, r_user, va_header.
  REFRESH: tb_set, r_user.

  SELECT SINGLE * FROM usr21 WHERE bname = sy-uname.
  SELECT SINGLE * FROM adrp WHERE persnumber = usr21-persnumber.
  p_set = 'ZUSER_AUT'.

* ACQUISIZIONE IDENTIFICATIVO SET
  CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
    EXPORTING
      shortname                = p_set
    IMPORTING
      new_setid                = value
    EXCEPTIONS
      no_set_found             = 1
      no_set_picked_from_popup = 2
      wrong_class              = 3
      wrong_subclass           = 4
      table_field_not_found    = 5
      fields_dont_match        = 6
      set_is_empty             = 7
      formula_in_set           = 8
      set_is_dynamic           = 9.

  CHECK sy-subrc = 0.

* estrazione range relativo ai Conti IVA
  CALL FUNCTION 'G_SET_FETCH'
    EXPORTING
      setnr               = value
      no_setid_conversion = 'X'
    IMPORTING
      set_header          = va_header
    TABLES
      set_lines_basic     = tb_set.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT tb_set.
    r_user-sign = 'I'.
    r_user-low = tb_set-from.
    IF NOT tb_set-to IS INITIAL.
      r_user-option = 'BT'.
      r_user-high = tb_set-to.
    ELSE.
      r_user-option = 'EQ'.
    ENDIF.
    APPEND r_user.

*User not equal
    r_user_ne = r_user.
    r_user_ne-sign = 'E'.
    APPEND r_user_ne.

    CLEAR:r_user, r_user_ne.
  ENDLOOP.

ENDFORM.                    " RANGE_USER
*&---------------------------------------------------------------------*
*&      Form  SET_ICON_NRT
*&---------------------------------------------------------------------*

FORM set_icon_nrt  USING    p_ls_zstrklayalv-zsystem
                            p_ls_zstrklayalv-trkorr.

  DATA: sv_outtab TYPE STANDARD TABLE OF g_outtab_line.

  DATA: ls_outtab LIKE zstrklayalv.
  DATA: ls_data TYPE REF TO data.

  DATA: nodes TYPE REF TO cl_salv_nodes.
  DATA: node TYPE REF TO cl_salv_node.

  TRY.

      nodes = gr_tree->get_nodes( ).

      sv_outtab[] = gt_outtab[].

      nodes->delete_all( ).

      DELETE sv_outtab  WHERE trkorr = space.

      READ TABLE sv_outtab INTO ls_outtab WITH KEY zsystem = p_ls_zstrklayalv-zsystem
                                                   trkorr =  p_ls_zstrklayalv-trkorr.
      IF sy-subrc = 0.
        ls_outtab-zindidms = 'X'.
        MODIFY sv_outtab INDEX sy-tabix FROM ls_outtab.
      ENDIF.

      PERFORM supply_data USING sv_outtab.

*      nodes->expand_all( ).

    CATCH cx_salv_error.
  ENDTRY.

  gr_tree->display( ).
ENDFORM.                    " SET_ICON_NRT

*&---------------------------------------------------------------------*
*&      form  dynamic_sel_option
*&---------------------------------------------------------------------*
FORM dynamic_sel_option USING pt_fld_4_selop_im TYPE ty_fld_4_selop_t
                     CHANGING pt_dyn_sel_op_values_ch
                         TYPE ztrk_dyn_sel_opt_values_t
                              lv_ucomm.

* Local Ranges
  RANGES: r_pos    FOR e070a-pos,
          r_matnr  FOR mara-matnr,
          r_attrib FOR  e070a-attribute.

* Local Structure/Tables
  DATA: ls_code(72)  TYPE c,
        lt_code(72)  TYPE c OCCURS 1000,
*      lt_add_fld   TYPE TABLE OF ztzbctrkadd_fld,
        lt_add_fld_2 TYPE TABLE OF ztzbctrkadd_fld,
        ls_add_fld   TYPE ty_fld_4_selop_s.

* Local Variables
  DATA: l_report_name(100) TYPE c,
        l_count            TYPE c.

  CLEAR: l_report_name, ls_code, lt_code.

  IF sy-subrc = 0.

    CLEAR lv_ucomm.
    CONCATENATE 'ZTRK_' sy-datum sy-uzeit INTO l_report_name.
    CONDENSE l_report_name NO-GAPS.

    CLEAR: ls_code.
    CONCATENATE 'REPORT' l_report_name '.' INTO ls_code SEPARATED BY space.
    APPEND ls_code TO lt_code.

    CLEAR: lt_add_fld_2[].
    lt_add_fld_2[] = pt_fld_4_selop_im[].

    SORT lt_add_fld_2 BY reftab.
    DELETE ADJACENT DUPLICATES FROM lt_add_fld_2 COMPARING reftab.

    CLEAR: ls_add_fld, ls_code.
    LOOP AT lt_add_fld_2 INTO ls_add_fld.
      IF sy-tabix = 1.
        ls_code = ls_add_fld-reftab.
      ELSE.
        CONCATENATE: ls_code ',' ls_add_fld-reftab INTO ls_code SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    RANGES: r_pippo FOR mara-matnr.

    CONCATENATE: 'TABLES:' ls_code '.' INTO ls_code SEPARATED BY space.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'data: gt_select_values TYPE ztrk_dyn_sel_opt_values_t.'.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'data: gs_select_values TYPE ztrk_dyn_sel_opt_values_s.'.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'data: lv_ucomm    like sy-ucomm.'.
    APPEND ls_code TO lt_code.

*    CLEAR: ls_add_fld.
*    concatenate text-300 'Select Filter' text-300 into ls_code.
*    concatenate 'selection-screen : begin of block bl1 with frame title' ls_code '.'into ls_code
*    separated by space.
*    APPEND ls_code TO lt_code.

    CLEAR: ls_add_fld.
    LOOP AT pt_fld_4_selop_im INTO ls_add_fld.
      CLEAR: ls_code.
      CONCATENATE: 'RANGES:#R' ls_add_fld-returnfield '#FOR#'
                   ls_add_fld-reftab '-' ls_add_fld-returnfield '.' INTO ls_code.
      CONDENSE ls_code NO-GAPS.
      TRANSLATE ls_code USING '# '.
      APPEND ls_code TO lt_code.
    ENDLOOP.
    CLEAR: ls_add_fld.
*       CLEAR: ls_code.
* move 'selection-screen : end of block bl1.' to ls_code.
*    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'SELECTION-SCREEN BEGIN OF SCREEN 901 AS WINDOW.'.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'SELECTION-SCREEN BEGIN OF BLOCK B01.'.
*       concatenate text-300 'Select Filter' text-300 into ls_code.
*    concatenate 'selection-screen : begin of block b01 with frame title' ls_code '.'into ls_code.
    APPEND ls_code TO lt_code.

    CLEAR: ls_add_fld.
    LOOP AT pt_fld_4_selop_im INTO ls_add_fld.
      CLEAR: ls_code.
      ls_code = 'SELECTION-SCREEN BEGIN OF LINE.'.
      APPEND ls_code TO lt_code.

      IF ls_add_fld-text IS NOT INITIAL.
        CLEAR: ls_code.
        CONCATENATE: 'SELECTION-SCREEN COMMENT (20)' '#L' ls_add_fld-returnfield '.' INTO ls_code.
        TRANSLATE ls_code USING '# '. APPEND ls_code TO lt_code.
      ENDIF.

      CLEAR: ls_code.
      IF ls_add_fld-matchcode IS INITIAL.
        CONCATENATE: 'SELECT-OPTIONS:#S' ls_add_fld-returnfield '#FOR#'
                     ls_add_fld-reftab '-' ls_add_fld-returnfield '.' INTO ls_code.
        CONDENSE ls_code NO-GAPS.
        TRANSLATE ls_code USING '# '.
        APPEND ls_code TO lt_code.
      ELSE.
        CONCATENATE: 'SELECT-OPTIONS:#S' ls_add_fld-returnfield '#FOR#'
                     ls_add_fld-reftab '-' ls_add_fld-returnfield INTO ls_code.
        CONDENSE ls_code NO-GAPS.
        TRANSLATE ls_code USING '# '.
        APPEND ls_code TO lt_code.
        CLEAR: ls_code.
        CONCATENATE:  '#MAtchcode#object#' ls_add_fld-matchcode '.' INTO ls_code.
        CONDENSE ls_code NO-GAPS.
        TRANSLATE ls_code USING '# '.
        APPEND ls_code TO lt_code.
      ENDIF.
      CLEAR: ls_code.
      ls_code = 'SELECTION-SCREEN END OF LINE.'.
      APPEND ls_code TO lt_code.
    ENDLOOP.
    CLEAR: ls_code.
    ls_code = 'SELECTION-SCREEN END OF BLOCK B01.'.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'SELECTION-SCREEN END OF SCREEN 901.'.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'CALL SCREEN 901 STARTING AT 10 5 ENDING AT 100 10.'.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'AT SELECTION-SCREEN.'.
    APPEND ls_code TO lt_code.

    ls_code = 'sy-ucomm'.
    CONCATENATE 'lv_ucomm = '
    ls_code '.' INTO ls_code SEPARATED BY space.
    APPEND ls_code TO lt_code.

    ls_code = '''COM'''.
    CONCATENATE 'EXPORT lv_ucomm TO MEMORY  ID'
    ls_code '.' INTO ls_code SEPARATED BY space.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'AT SELECTION-SCREEN output.'.
    APPEND ls_code TO lt_code.

    LOOP AT pt_fld_4_selop_im INTO ls_add_fld.
      CLEAR: ls_code.
      IF ls_add_fld-text IS NOT INITIAL.
        CONCATENATE: 'L' ls_add_fld-returnfield '#=#'
                     '''' ls_add_fld-text '''' '.' INTO ls_code.
        CONDENSE ls_code NO-GAPS.
        TRANSLATE ls_code USING '# '.
        APPEND ls_code TO lt_code.
      ENDIF.
    ENDLOOP.

    CLEAR: ls_code.
    ls_code = 'START-OF-SELECTION.'.
    APPEND ls_code TO lt_code.

    CLEAR: ls_code.
    ls_code = 'CLEAR: gt_select_values[].'.
    APPEND ls_code TO lt_code.

    CLEAR: ls_add_fld.
    LOOP AT pt_fld_4_selop_im INTO ls_add_fld.

      CLEAR: ls_code.
      ls_code = 'CLEAR: gs_select_values.'.
      APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      CONCATENATE: 'gs_select_values-reftab#=#' '''' ls_add_fld-reftab '''' '.' INTO ls_code.
      CONDENSE ls_code NO-GAPS. TRANSLATE ls_code USING '# '. APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      CONCATENATE: 'gs_select_values-RETURNFIELD#=#' '''' ls_add_fld-returnfield '''' '.'
                      INTO ls_code SEPARATED BY space.
      CONDENSE ls_code NO-GAPS. TRANSLATE ls_code USING '# '. APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      CONCATENATE: 'loop#at#' 'S' ls_add_fld-returnfield '.' INTO ls_code.
      CONDENSE ls_code NO-GAPS. TRANSLATE ls_code USING '# '. APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      ls_code = 'gs_select_values-pos = sy-tabix.'.
      APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      CONCATENATE 'gs_select_values-sign#=#s' ls_add_fld-returnfield '-sign.' INTO ls_code.
      CONDENSE ls_code NO-GAPS. TRANSLATE ls_code USING '# '. APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      CONCATENATE 'gs_select_values-option#=#s' ls_add_fld-returnfield '-option.' INTO ls_code.
      CONDENSE ls_code NO-GAPS. TRANSLATE ls_code USING '# '. APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      CONCATENATE 'gs_select_values-low#=#s' ls_add_fld-returnfield '-low.' INTO ls_code.
      CONDENSE ls_code NO-GAPS. TRANSLATE ls_code USING '# '. APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      CONCATENATE 'gs_select_values-high#=#s' ls_add_fld-returnfield '-high.' INTO ls_code.
      CONDENSE ls_code NO-GAPS. TRANSLATE ls_code USING '# '. APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      ls_code = 'Append gs_select_values to gt_select_values[].'.
      APPEND ls_code TO lt_code.

      CLEAR: ls_code.
      ls_code = 'endloop.'.
      APPEND ls_code TO lt_code.

    ENDLOOP.

    CLEAR: ls_code.
    ls_code = '''COD'''.
    CONCATENATE 'EXPORT gt_select_values TO MEMORY  ID'
    ls_code '.' INTO ls_code SEPARATED BY space.
    APPEND ls_code TO lt_code.


    APPEND ls_code TO lt_code.

    INSERT REPORT l_report_name FROM lt_code.

    SUBMIT (l_report_name) AND RETURN.

    IMPORT gt_select_values[] FROM MEMORY ID 'COD' .

    IMPORT lv_ucomm FROM MEMORY ID 'COM' .

    FREE MEMORY ID 'COM'.
    FREE MEMORY ID 'COD'.
    pt_dyn_sel_op_values_ch[] = gt_select_values[].

    DELETE REPORT l_report_name.

  ENDIF.


ENDFORM.                    "dynamic_sel_option

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD constructor.
* Local Structures/Tables.
    DATA: ls_tab_outtab_int TYPE ty_outtab_in_alv_s,
          ls_outtab_ext     TYPE g_outtab_line.

* B.o.M.
    CLEAR: ls_tab_outtab_int, me->a_tab_outtab_int[],
           ls_outtab_ext.

    LOOP AT pt_outtab_im INTO ls_outtab_ext.
      MOVE-CORRESPONDING ls_outtab_ext TO ls_tab_outtab_int.
      APPEND ls_tab_outtab_int TO me->a_tab_outtab_int[].
    ENDLOOP.
    me->a_str_index_show = ps_index_show_im.
    me->a_obj_tree_event ?= po_tree_events_im.
    me->register_events( ).
  ENDMETHOD.                    "constructor

  METHOD register_events.
    SET HANDLER me->on_user_command FOR me->a_obj_tree_event.
    SET HANDLER me->on_double_click FOR me->a_obj_tree_event.
    SET HANDLER me->on_raise_checkbox_change FOR me->a_obj_tree_event.
  ENDMETHOD.                    "register_Events

  METHOD on_raise_checkbox_change.

* Local Structures/Tables
    FIELD-SYMBOLS: <outtab_int> TYPE ty_outtab_in_alv_s.

* Local Variables
    DATA: l_index TYPE i,
          l_no_find_parent_flag TYPE xfeld,
          l_node_key TYPE salv_de_node_key.

* B.o.M .
    CLEAR: l_index, l_node_key.

    l_node_key = node_key.
    CONDENSE l_node_key NO-GAPS.
    l_index = l_node_key.

    UNASSIGN <outtab_int>.
    READ TABLE me->a_tab_outtab_int ASSIGNING <outtab_int> INDEX l_index.
    IF sy-subrc = 0.
      <outtab_int>-checked_item = checked.
    ENDIF.

  ENDMETHOD.                    "on_raise_checkbox_change

  METHOD on_double_click.

* Field Symbols
    FIELD-SYMBOLS: <outtab_int> TYPE ty_outtab_in_alv_s.

* Local Structures/Tables
    DATA: ls_outtab_int TYPE ty_outtab_in_alv_s,
          lt_sel_opt_val TYPE ztrk_dyn_sel_opt_values_t,
          ls_sel_opt_val TYPE ztrk_dyn_sel_opt_values_s,
          lt_zcts_r TYPE ty_zdzcts_r_t,
          lt_trkorr_r  TYPE ty_trkorr_r_t,
          ls_trkorr_r  TYPE ty_trkorr_r_s,
          lt_zfield1_r TYPE ty_zdzfieldx_r_t,
          lt_zfield2_r TYPE ty_zdzfieldx_r_t,
          lt_zfield3_r TYPE ty_zdzfieldx_r_t,
          lt_zfield4_r TYPE ty_zdzfieldx_r_t,
          lt_zfield5_r TYPE ty_zdzfieldx_r_t,
          ls_zcts_r    TYPE ty_zdzcts_r_s,
          ls_zfieldx_r TYPE ty_zdzfieldx_r_s,
          lt_tree TYPE salv_t_nodes,
          ls_tree TYPE salv_s_nodes,
          lt_salv_node TYPE salv_t_nodes,
          lo_nodes TYPE REF TO cl_salv_nodes,
          lo_item TYPE REF TO cl_salv_item.

* Local Variables
    DATA: l_index TYPE i,
          l_no_find_parent_flag TYPE xfeld,
          l_node_key TYPE salv_de_node_key.
* B.o.M .
    CLEAR: ls_outtab_int, l_index, l_node_key.

    l_node_key = node_key.
    CONDENSE l_node_key NO-GAPS.
    l_index = l_node_key.
    READ TABLE me->a_tab_outtab_int INTO ls_outtab_int INDEX l_index.
    IF sy-subrc = 0.
      IF ( ( ls_outtab_int-znode IS NOT INITIAL )
      AND ( ls_outtab_int-ctsproject IS NOT INITIAL ) ).
        me->show_popup_select_opt( EXPORTING ps_index_show_im = me->a_str_index_show
                                   IMPORTING pt_sel_opt_val_ch = lt_sel_opt_val ).

        CLEAR: ls_sel_opt_val, lt_trkorr_r[].
        LOOP AT lt_sel_opt_val INTO ls_sel_opt_val.
          CASE ls_sel_opt_val-returnfield.
            WHEN 'TRKORR'. .
              CLEAR: ls_trkorr_r.
              ls_trkorr_r-sign = ls_sel_opt_val-sign.
              ls_trkorr_r-option = ls_sel_opt_val-option.
              ls_trkorr_r-low = ls_sel_opt_val-low.
              ls_trkorr_r-high = ls_sel_opt_val-high.
              APPEND ls_trkorr_r TO lt_trkorr_r[].
            WHEN 'ZCTS'.
              CLEAR: ls_zcts_r.
              ls_zcts_r-sign = ls_sel_opt_val-sign.
              ls_zcts_r-option = ls_sel_opt_val-option.
              ls_zcts_r-low = ls_sel_opt_val-low.
              ls_zcts_r-high = ls_sel_opt_val-high.
              APPEND ls_zcts_r TO lt_zcts_r[].
            WHEN 'ZFIELD1'.
              CLEAR: ls_zfieldx_r.
              ls_zfieldx_r-sign = ls_sel_opt_val-sign.
              ls_zfieldx_r-option = ls_sel_opt_val-option.
              ls_zfieldx_r-low = ls_sel_opt_val-low.
              ls_zfieldx_r-high = ls_sel_opt_val-high.
              APPEND ls_zfieldx_r TO lt_zfield1_r[].
            WHEN 'ZFIELD2'.
              CLEAR: ls_zfieldx_r.
              ls_zfieldx_r-sign = ls_sel_opt_val-sign.
              ls_zfieldx_r-option = ls_sel_opt_val-option.
              ls_zfieldx_r-low = ls_sel_opt_val-low.
              ls_zfieldx_r-high = ls_sel_opt_val-high.
              APPEND ls_zfieldx_r TO lt_zfield2_r[].
            WHEN 'ZFIELD3'.
              CLEAR: ls_zfieldx_r.
              ls_zfieldx_r-sign = ls_sel_opt_val-sign.
              ls_zfieldx_r-option = ls_sel_opt_val-option.
              ls_zfieldx_r-low = ls_sel_opt_val-low.
              ls_zfieldx_r-high = ls_sel_opt_val-high.
              APPEND ls_zfieldx_r TO lt_zfield3_r[].
            WHEN 'ZFIELD4'.
              CLEAR: ls_zfieldx_r.
              ls_zfieldx_r-sign = ls_sel_opt_val-sign.
              ls_zfieldx_r-option = ls_sel_opt_val-option.
              ls_zfieldx_r-low = ls_sel_opt_val-low.
              ls_zfieldx_r-high = ls_sel_opt_val-high.
              APPEND ls_zfieldx_r TO lt_zfield4_r[].
            WHEN 'ZFIELD5'.
              CLEAR: ls_zfieldx_r.
              ls_zfieldx_r-sign = ls_sel_opt_val-sign.
              ls_zfieldx_r-option = ls_sel_opt_val-option.
              ls_zfieldx_r-low = ls_sel_opt_val-low.
              ls_zfieldx_r-high = ls_sel_opt_val-high.
              APPEND ls_zfieldx_r TO lt_zfield5_r[].
          ENDCASE.
        ENDLOOP.

        UNASSIGN: <outtab_int>.
        LOOP AT me->a_tab_outtab_int ASSIGNING <outtab_int>
          WHERE znode = ls_outtab_int-znode
            AND ctsproject IN lt_zcts_r[]
*            AND ctsproject = ls_outtab_int-ctsproject
            AND tarclient IS NOT INITIAL
            AND trkorr  IN lt_trkorr_r[]
            AND zfield1 IN lt_zfield1_r[]
            AND zfield2 IN lt_zfield2_r[]
            AND zfield3 IN lt_zfield3_r[]
            AND zfield4 IN lt_zfield4_r[]
            AND zfield5 IN lt_zfield5_r[].
          <outtab_int>-checked_item = 'X'.
        ENDLOOP.
        IF sy-subrc NE 0.
          MESSAGE i037(/bev2/edmess1).
        ENDIF.
      ELSE.
        MESSAGE i000(db) WITH text-e10.
      ENDIF.
    ENDIF.
    CLEAR: lt_tree, ls_tree, lo_nodes, l_index.

    IF lv_ucomm = 'CRET'.
      TRY.
          lo_nodes = gr_tree->get_nodes( ).
          lt_tree = lo_nodes->get_all_nodes( ).
        CATCH cx_salv_msg.
          EXIT.
      ENDTRY.
      UNASSIGN <outtab_int>.

      LOOP AT me->a_tab_outtab_int ASSIGNING <outtab_int>
        WHERE checked_item = 'X'.
        l_index = sy-tabix.

        CLEAR: ls_tree, l_no_find_parent_flag.
        READ TABLE lt_tree INTO ls_tree INDEX l_index.
        IF sy-subrc = 0.
          TRY.
              CALL METHOD ls_tree-node->get_children
                RECEIVING
                  value = lt_salv_node.
            CATCH cx_salv_msg .
              l_no_find_parent_flag = 'X'.
          ENDTRY.

          IF l_no_find_parent_flag = 'X'.
            lo_item = ls_tree-node->get_hierarchy_item( ).
            lo_item->set_checked( 'X' ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "on_double_click

  METHOD on_user_command.

* Field Symbols
    FIELD-SYMBOLS: <outtab_int> TYPE ty_outtab_in_alv_s.

* Local Structures/Tables
    DATA: lt_tree TYPE salv_t_nodes,
          ls_tree TYPE salv_s_nodes.

* local Objects/Variables
    DATA: lo_nodes TYPE REF TO cl_salv_nodes,
          lo_item TYPE REF TO cl_salv_item,
          l_index TYPE i.

* B.o.M.
    PERFORM user_action.

    IF e_salv_function = 'DESL_BLOCK'.
      CLEAR: lt_tree, lo_nodes, l_index.
      TRY.
          lo_nodes = gr_tree->get_nodes( ).
          lt_tree = lo_nodes->get_all_nodes( ).
        CATCH cx_salv_msg.
          EXIT.
      ENDTRY.

      UNASSIGN: <outtab_int>.
      LOOP AT me->a_tab_outtab_int ASSIGNING <outtab_int>
        WHERE checked_item = 'X'.
        l_index = sy-tabix.
        CLEAR: ls_tree, lo_item.
        READ TABLE lt_tree INTO ls_tree INDEX l_index.
        IF sy-subrc = 0.
          lo_item = ls_tree-node->get_hierarchy_item( ).
          lo_item->set_checked(' ').
          CLEAR: <outtab_int>-checked_item.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "on_user_command
  METHOD show_popup_select_opt.

* Local Variables/Structures
    DATA: ls_fieldname_r TYPE ty_fieldname_r_s,
          lt_fieldname_r TYPE ty_fieldname_r_t,
          lt_fld_4_selop TYPE ty_fld_4_selop_t,
          ls_fld_4_selop TYPE ty_fld_4_selop_s,
          ls_ztzbctrkadd_fld TYPE ztzbctrkadd_fld,
          lt_ztzbctrkadd_fld TYPE TABLE OF ztzbctrkadd_fld.

* B.o.M.
    CLEAR: lt_fieldname_r[], lt_ztzbctrkadd_fld[],
           pt_sel_opt_val_ch[].

    IF ps_index_show_im-zfield1 IS NOT INITIAL.
      CLEAR: ls_fieldname_r.
      ls_fieldname_r-sign = 'I'.
      ls_fieldname_r-option = 'EQ'.
      ls_fieldname_r-low = 'ZFIELD1'.
      APPEND ls_fieldname_r TO lt_fieldname_r[].
    ENDIF.

    IF ps_index_show_im-zfield2 IS NOT INITIAL.
      CLEAR: ls_fieldname_r.
      ls_fieldname_r-sign = 'I'.
      ls_fieldname_r-option = 'EQ'.
      ls_fieldname_r-low = 'ZFIELD2'.
      APPEND ls_fieldname_r TO lt_fieldname_r[].
    ENDIF.

    IF ps_index_show_im-zfield3 IS NOT INITIAL.
      CLEAR: ls_fieldname_r.
      ls_fieldname_r-sign = 'I'.
      ls_fieldname_r-option = 'EQ'.
      ls_fieldname_r-low = 'ZFIELD3'.
      APPEND ls_fieldname_r TO lt_fieldname_r[].
    ENDIF.

    IF ps_index_show_im-zfield4 IS NOT INITIAL.
      CLEAR: ls_fieldname_r.
      ls_fieldname_r-sign = 'I'.
      ls_fieldname_r-option = 'EQ'.
      ls_fieldname_r-low = 'ZFIELD4'.
      APPEND ls_fieldname_r TO lt_fieldname_r[].
    ENDIF.

    IF ps_index_show_im-zfield5 IS NOT INITIAL.
      CLEAR: ls_fieldname_r.
      ls_fieldname_r-sign = 'I'.
      ls_fieldname_r-option = 'EQ'.
      ls_fieldname_r-low = 'ZFIELD5'.
      APPEND ls_fieldname_r TO lt_fieldname_r[].
    ENDIF.

    SELECT * FROM ztzbctrkadd_fld INTO TABLE lt_ztzbctrkadd_fld
            WHERE zfield_pointer IN lt_fieldname_r[]
              AND langu = sy-langu.
    IF sy-subrc NE 0.
      SELECT * FROM ztzbctrkadd_fld INTO TABLE lt_ztzbctrkadd_fld
          WHERE zfield_pointer IN lt_fieldname_r[]
            AND langu = ''.
    ENDIF.

    CLEAR: lt_fld_4_selop, ls_ztzbctrkadd_fld, ls_fld_4_selop.

    ls_fld_4_selop-reftab = 'ZTZBCTRKCR_NEXT'.
    ls_fld_4_selop-returnfield = 'ZCTS'.
    ls_fld_4_selop-text        = text-t04   .
*    ls_fld_4_selop-matchcode     = ls_ztzbctrkadd_fld-matchcode .
    APPEND ls_fld_4_selop TO lt_fld_4_selop.

    ls_fld_4_selop-reftab = 'ZTZBCTRKCR_NEXT'.
    ls_fld_4_selop-returnfield = 'TRKORR'.
    ls_fld_4_selop-text        = text-t03   .
*    ls_fld_4_selop-matchcode     = ls_ztzbctrkadd_fld-matchcode .
    APPEND ls_fld_4_selop TO lt_fld_4_selop.

    LOOP AT lt_ztzbctrkadd_fld INTO ls_ztzbctrkadd_fld.
      CLEAR: ls_fld_4_selop.
      ls_fld_4_selop-reftab = 'ZTZBCTRKCR_NEXT'.
      ls_fld_4_selop-returnfield = ls_ztzbctrkadd_fld-zfield_pointer.
      ls_fld_4_selop-text        = ls_ztzbctrkadd_fld-text    .
      ls_fld_4_selop-matchcode   = ls_ztzbctrkadd_fld-matchcode .
      APPEND ls_fld_4_selop TO lt_fld_4_selop.
    ENDLOOP.

    PERFORM dynamic_sel_option USING lt_fld_4_selop
                            CHANGING pt_sel_opt_val_ch[]
                                     lv_ucomm.

  ENDMETHOD.                    "show_popup_select_opt

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  CHECK_APPEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ZSTRKLAYALV  text
*----------------------------------------------------------------------*
FORM check_append  USING p_ls_zstrklayalv STRUCTURE ls_zstrklayalv.

  CONSTANTS: ca_tabname_e071    TYPE tabname VALUE 'E071',
             ca_tabname_log    TYPE tabname VALUE 'ZTZBCTRKCR_LOG'.


  DATA: v_trkorr TYPE e071-trkorr.
  TYPES: BEGIN OF t_e071,
         trkorr LIKE e071-trkorr,
         pgmid LIKE e071-pgmid,
         object LIKE e071-object,
         obj_name LIKE e071-obj_name,
         END OF t_e071.
  DATA: tb_e071 TYPE TABLE OF e071,
        tb_log TYPE TABLE OF ztzbctrkcr_log.
  DATA: ls_e070    LIKE e070,
        st_log TYPE ztzbctrkcr_log.
  DATA: BEGIN OF tb_ztzbctrkcr OCCURS 1000.
          INCLUDE STRUCTURE ztzbctrkcr_st.
  DATA: END OF tb_ztzbctrkcr.


  TYPES: BEGIN OF t_dd02l,
         tabname LIKE dd02l-tabname,
         tabclass LIKE dd02l-tabclass,
         sqltab LIKE dd02l-sqltab,
         END OF t_dd02l.
  DATA: tb_dd02l TYPE TABLE OF t_dd02l,
        wa_dd02l TYPE t_dd02l,
        v_objname TYPE tabname,
        vn_count LIKE sy-tabix,
        va_danger TYPE zdzbcdanger.

  FIELD-SYMBOLS: <fs_e071> TYPE e071,
                 <fs_log> TYPE ztzbctrkcr_log.
  FIELD-SYMBOLS: <crtrasp> TYPE zstrklayalv.


  LOOP AT tb_crtrasp ASSIGNING <crtrasp>.
    CLEAR tb_e071[].
    CLEAR: lt_fields, lt_data, lt_options.
    FREE : lt_fields, lt_data, lt_options.
    CONCATENATE '''' <crtrasp>-trkorr '''' INTO ls_options.
    CONCATENATE 'TRKORR =' ls_options INTO ls_options SEPARATED BY space.
    APPEND ls_options TO lt_options.


    CALL FUNCTION 'ZTRK_RFC_READ_TABLE'
      DESTINATION va_zrfc1
      EXPORTING
        query_table           = ca_tabname_e071
        i_system              = <crtrasp>-zsystem
      TABLES
        fields                = lt_fields
        options               = lt_options
        data                  = lt_data
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e15 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
    CLEAR fl_ko.
    tb_e071[] = lt_data[].
    LOOP AT tb_e071 ASSIGNING <fs_e071>.
      IF   ( <fs_e071>-pgmid = 'R3TR' AND <fs_e071>-object = 'TABL' ) OR
           ( <fs_e071>-pgmid = 'LIMU' AND <fs_e071>-object = 'TABD' ).

        MOVE <fs_e071>-obj_name TO v_objname.
        SELECT SINGLE tabname tabclass sqltab FROM dd02l INTO wa_dd02l
                       WHERE tabname = v_objname.
        IF wa_dd02l-tabclass = 'APPEND'.
*  ZTZBCTRKCR_LOG-DANGER = 'X'.
**************************
          CLEAR tb_log[].
          CLEAR: lt_fields, lt_data, lt_options.
          FREE : lt_fields, lt_data, lt_options.
          CONCATENATE '''' <crtrasp>-trkorr '''' INTO ls_options.
          CONCATENATE 'TRKORR =' ls_options INTO ls_options SEPARATED BY space.
          APPEND ls_options TO lt_options.

          CALL FUNCTION 'ZTRK_RFC_READ_TABLE'
            DESTINATION va_zrfc1
            EXPORTING
              query_table           = ca_tabname_log
              i_system              = <crtrasp>-zsystem
            TABLES
              fields                = lt_fields
              options               = lt_options
              data                  = lt_data
            EXCEPTIONS
              system_failure        = 1
              communication_failure = 2
              OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e15 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
          CLEAR fl_ko.
          CLEAR: vn_count, st_log.
          tb_log[] = lt_data[].
          SORT tb_log BY zdatum ztime.
          READ TABLE tb_log INTO st_log WITH KEY zsystem = 'IF3'
                                                 zaction = 'SD'.

*          LOOP AT tb_log ASSIGNING <fs_log> WHERE zsystem = 'IF3' AND
*                                                  zaction = 'SD'.
*            vn_count = vn_count + 1.
*            IF vn_count > 1 AND
*            st_log-danger = ' '.
*              CLEAR fl_ko.
*              EXIT.
*            ENDIF.
*          ENDLOOP.
          IF sy-subrc NE 0.
*          IF st_log-zindidng = 'X'.
***************************
            fl_ko = 'X'.
            FREE tb_ztzbctrkcr.
            MOVE-CORRESPONDING p_ls_zstrklayalv TO tb_ztzbctrkcr.
            MOVE-CORRESPONDING <crtrasp> TO tb_ztzbctrkcr.
            tb_ztzbctrkcr-externalid = p_ls_zstrklayalv-ctsproject.
            tb_ztzbctrkcr-as4date    = sy-datum.
            tb_ztzbctrkcr-as4time    = sy-uzeit.
            tb_ztzbctrkcr-tarsystem = p_ls_zstrklayalv-zsystem.
            tb_ztzbctrkcr-zindidng  = 'X'.
            APPEND tb_ztzbctrkcr.

            SELECT SINGLE zrfc INTO va_zrfc FROM ztzbctrkmainc.

            IF sy-subrc = 0.

              CALL FUNCTION 'ZTZBCTRK_SAVE_CRS'
                DESTINATION va_zrfc
                EXPORTING
                  i_uname               = sy-uname
                  i_action              = 'D'
                  i_system              = p_ls_zstrklayalv-zsystem
                TABLES
                  tb_cr                 = tb_ztzbctrkcr
                EXCEPTIONS
                  system_failure        = 1
                  communication_failure = 2
                  OTHERS                = 3.
*            IF SY-UNAME = 'GF10030'. "12-06-2023 TOGLIERE
               IF SY-SUBRC NE 0. "12-06-2023 NEW ENHANCEMENTS
                  IF SY-MSGNO IS NOT INITIAL. "12-06-2023 NEW ENHANCEMENTS
                     if sy-msgty is not initial. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "12-06-2023 NEW ENHANCEMENTS
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. "12-06-2023 NEW ENHANCEMENTS
                     else. "12-06-2023 NEW ENHANCEMENTS
                       clear w_text. "12-06-2023 NEW ENHANCEMENTS
                       SELECT SINGLE text FROM t100 into w_text "12-06-2023 NEW ENHANCEMENTS
                              WHERE sprsl = sy-langu "12-06-2023 NEW ENHANCEMENTS
                              AND   arbgb = SY-MSGID "12-06-2023 NEW ENHANCEMENTS
                              AND   msgnr = SY-MSGNO. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '&' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv1(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv2(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv3(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       REPLACE '$' WITH SY-msgv4(18) INTO w_text. "12-06-2023 NEW ENHANCEMENTS
                       CONDENSE w_text. "12-06-2023 NEW ENHANCEMENTS
                       MESSAGE e000(ztrk) WITH w_text. "12-06-2023 NEW ENHANCEMENTS
                   endif. "12-06-2023 NEW ENHANCEMENTS
                  ELSE. "12-06-2023 NEW ENHANCEMENTS
                     MESSAGE e000(ztrk) WITH text-e20 va_zrfc. "12-06-2023 NEW ENHANCEMENTS
                  ENDIF. "12-06-2023 NEW ENHANCEMENTS
               ENDIF. "12-06-2023 NEW ENHANCEMENTS
*            ENDIF. "12-06-2023 NEW ENHANCEMENTS
            ENDIF.
          ELSE.
            IF st_log-danger = ' '.
              CLEAR fl_ko.
            ELSE.
              fl_ko = 'X'.
            ENDIF.
          ENDIF.
***************************
        ENDIF.
        CLEAR: v_objname, wa_dd02l.
      ENDIF.
    ENDLOOP.
  ENDLOOP.


ENDFORM.                    " CHECK_APPEND
