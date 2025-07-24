
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_structure,
           mara type mara,
           END OF ty_structure,
    tt_structure TYPE TABLE OF ty_structure.

    DATA: mt_table TYPE tt_structure,
          mo_grid  TYPE cl_gui_alv_grid.
  
    METHODS execute.

  PRIVATE SECTION.
    METHODS init_container.
    METHODS display_alv.
    methods handle_toolbar      for event toolbar       of cl_gui_alv_grid importing e_object .
    methods handle_user_command for event user_command  of cl_gui_alv_grid importing e_ucomm .
ENDCLASS.                    

DATA go_report TYPE REF TO lcl_report.
* INCLUDE report_name_pbo.
* INCLUDE report_name_pai.
*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD execute.
    display_alv( ).
  ENDMETHOD.                    "execute

  METHOD display_alv.

    data lt_fcat type lvc_t_fcat.
    data(ls_layout)  = value lvc_s_layo( sel_mode = 'A' no_rowmark  = ''  edit = 0 cwidth_opt = abap_true ).
    data(ls_variant) = value disvariant( report = sy-repid  username = sy-uname ).

    data(lt_toolbar_excluding) = value ui_functions(
    ( cl_gui_alv_grid=>mc_fc_loc_cut           ) ( cl_gui_alv_grid=>mc_fc_loc_append_row )
    ( cl_gui_alv_grid=>mc_fc_loc_insert_row    ) ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
    ( cl_gui_alv_grid=>mc_fc_loc_copy          ) ( cl_gui_alv_grid=>mc_fc_loc_undo       )
    ( cl_gui_alv_grid=>mc_fc_loc_copy_row      ) ( cl_gui_alv_grid=>mc_fc_loc_paste      )
    ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ) ( cl_gui_alv_grid=>mc_fc_info           )
    ( cl_gui_alv_grid=>mc_fc_check             ) ( cl_gui_alv_grid=>mc_fc_refresh        )
    ( cl_gui_alv_grid=>mc_fc_graph             ) ).

    lt_fcat = value lvc_t_fcat(  ( fieldname = 'MATNR'   ref_table = 'MARA' ) ).
            
    init_container( ).

    set handler : handle_toolbar
                  handle_user_command for mo_grid.

    mo_grid->set_table_for_first_display(
       exporting
         i_save               = 'X'
         is_layout            = ls_layout
         is_variant           = ls_variant
         it_toolbar_excluding = lt_toolbar_excluding
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_table ).
            
  CALL SCREEN 100.
      
  ENDMETHOD.                    "display_alv

  method handle_user_command.

    data: lv_answer type char1.

    case e_ucomm.

      when 'FC_STANZIAMENTI'.
    ENDCASE.
            
ENDMETHOD.

 method handle_toolbar.
    data: ls_toolbar type stb_button.

    e_object->mt_toolbar = value #( base e_object->mt_toolbar
        ( function = 'FC_STANZIAMENTI' text = 'Registra stanziamenti' icon = icon_execute_object           quickinfo = 'Registra stanziamenti' )
        ( function = 'FC_LOGS'         text = 'Log Registrazioni'     icon = icon_display_text             quickinfo = 'Log Registrazioni'   ) ).

  endmethod.    "handle_toolbar

  METHOD init_container.
            
    data: lo_cont      type ref to cl_gui_custom_container.

    if mo_grid is bound.
      return.
    endif.

    create object lo_cont
      exporting
        container_name              = 'CONT_100'
        repid                       = sy-repid
        dynnr                       = '0100'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    create object mo_grid
      exporting
        i_parent          = lo_cont
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
            
  ENDMETHOD.                    "init_container

ENDCLASS.    


START-OF-SELECTION.
  CREATE OBJECT go_report.
  go_report->execute( ).
