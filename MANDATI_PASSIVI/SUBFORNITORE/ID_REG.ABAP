*&---------------------------------------------------------------------*
*& Report  YMCMS_ARCH_ID_REG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report ymcms_arch_id_reg.

data: begin of gs_screen100,
        ok_code type syucomm,
      end of gs_screen100.

data: begin of gs_screen101,
        ok_code type syucomm,
      end of gs_screen101.

data: gv_ucomm  type syucomm,
      gv_screen type char1. " 'R' screen od Id registration , 'S' screen of societa

tables: bkpf.

selection-screen begin of screen 1010 as subscreen.
selection-screen begin of block b2 with frame title text-001.
parameters: p_reid_b type ymcms_id_reg lower case obligatory,
            p_dscr_b type ymcms_arc_id_reg-descr_reg lower case obligatory,
            p_manute type ymcms_arc_id_reg-manutentore obligatory,
            p_tipent type ymcms_arc_id_reg-tipo_entita obligatory
              matchcode object ymcms_arc_tipo_entita_help,                  "Mod DF 17.01.2025
            p_auth   type ymcms_arc_id_reg-autorita_comp obligatory.
select-options: s_bukrs for  bkpf-bukrs.
selection-screen end of block b2.
selection-screen end of screen 1010.


selection-screen begin of block b1 with frame title text-001.
parameters: rb_bank radiobutton group g1,
            rb_dora radiobutton group g1.
selection-screen end of block b1.


*----------------------------------------------------------------------*
*       CLASS lcl_class_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_arch_id_reg definition final.
  public section.

    types: begin of ty_regid,
             id_reg        type ymcms_arc_id_reg-id_reg,
             tipo_reg      type ymcms_arc_id_reg-tipo_reg,
             descr_reg     type ymcms_arc_id_reg-descr_reg,
             manutentore   type ymcms_arc_id_reg-manutentore,
             butxt         type t001-butxt,
             tipo_entita   type ymcms_arc_id_reg-tipo_entita,
             autorita_comp type ymcms_arc_id_reg-autorita_comp,
             uname_cre     type ymcms_arc_id_reg-uname_cre,
             datum_cre     type ymcms_arc_id_reg-datum_cre,
             uzeit_cre     type ymcms_arc_id_reg-uzeit_cre,
             uname_mod     type ymcms_arc_id_reg-uname_mod,
             datum_mod     type ymcms_arc_id_reg-datum_mod,
             uzeit_mod     type ymcms_arc_id_reg-uzeit_mod,
           end of ty_regid,
           tt_regid type table of ty_regid.

    data: mo_grid  type ref to cl_gui_alv_grid,
          mt_regid type tt_regid.

    methods execute.
    methods save_id_registro.
    methods modif_id_registro.
    methods visualizza_societa importing iv_regid type ymcms_arc_id_reg-id_reg.
    methods display_register.
    methods check_dora_fields exporting e_check type char01."Mod DF 17.01.2025
    methods clear_subscreen_fields.                         "Mdo DF 17.01.2025

    methods: handle_user_command
                for event user_command of cl_gui_alv_grid
      importing e_ucomm .

  private section.
    methods extract_data.
    methods banca_italia.
    methods start_container.

    methods on_data_changed
    for event data_changed of cl_gui_alv_grid importing  er_data_changed .
    methods: handle_toolbar
                for event toolbar of cl_gui_alv_grid
      importing e_object .
endclass.                    "

data go_arch_id_reg type ref to lcl_arch_id_reg.

include ymcms_arch_id_reg_pbo.
include ymcms_arch_id_reg_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_arch_id_reg implementation.

  method on_data_changed.
    field-symbols: <lt_data> type tt_regid.
    assign er_data_changed->mp_mod_rows->* to <lt_data>.
  endmethod.

  method execute.

    start_container( ).

    case abap_true.
      when rb_bank.
        banca_italia( ).
        display_register( ).
        call screen 100.
      when rb_dora.
        banca_italia( ).
        display_register( ).
        call screen 100.
    endcase.

  endmethod.                    "execute

  method extract_data.

  endmethod.                    "extract_data

  method banca_italia.

    case abap_true.
      when rb_bank.
        data(lv_tipo_reg) = 'BI'.
      when rb_dora.
        lv_tipo_reg = 'DO'.
    endcase.

    select  r~id_reg
            r~tipo_reg
            r~descr_reg
            r~manutentore
            t~butxt
            r~tipo_entita
            r~autorita_comp
            r~uname_cre
            r~datum_cre
            r~uzeit_cre
            r~uname_mod
            r~datum_mod
            r~uzeit_mod
      from ymcms_arc_id_reg as r
      left join t001 as t on t~bukrs = r~manutentore
      into corresponding fields of table mt_regid
      where   tipo_reg  = lv_tipo_reg.

    sort mt_regid by id_reg.
    delete adjacent duplicates from mt_regid comparing id_reg.

  endmethod.                    "extract_data

  method save_id_registro.

    data: ls_reg_id type ymcms_arc_id_reg,
          ls_loc_id type ty_regid,
          ls_regsoc type ymcms_arc_regsoc.

    data: lva_check type char01.                                                  "Mod DF 17.01.2025

    clear: lva_check.                                                             "Mod DF 17.01.2025

    "Controlli campi Dora
    if rb_dora is not initial.                                                    "Mod DF 17.01.2025
      check_dora_fields( importing e_check = lva_check ).                         "Mod DF 17.01.2025
      if lva_check is not initial.                                                "Mod DF 17.01.2025
        return.                                                                   "Mod DF 17.01.2025
      endif.                                                                      "Mod DF 17.01.2025
    endif.                                                                        "Mod DF 17.01.2025

    if s_bukrs[] is initial.                                                      "Mod DF 17.01.2025
      message 'Inserire almeno una società afferente al registro'                 "Mod DF 17.01.2025
        type 'I' display like 'E'.                                                "Mod DF 17.01.2025
      return.                                                                     "Mod DF 17.01.2025
    endif.                                                                        "Mod DF 17.01.2025

    select single id_reg
      from ymcms_arc_id_reg
      into @data(lv_id)
      where id_reg = @p_reid_b.

    if sy-subrc = 0.
      message 'ERRORE: Id Registro gia esiste' type 'I' display like 'E'.
      return.
    endif.

    select bukrs,
           butxt
      from t001
      into table @data(lt_bukrs)
      where bukrs in @s_bukrs.

    if sy-subrc <> 0.
      message 'ERRORE: Societa afferenti inserite non esistono' type 'I' display like 'E'.
      return.
    endif.

    ls_loc_id-id_reg        = p_reid_b.
    ls_regsoc-id_reg        = p_reid_b.
    ls_loc_id-descr_reg     = p_dscr_b.
    ls_loc_id-manutentore   = p_manute.
    ls_loc_id-tipo_entita   = p_tipent.
    ls_loc_id-autorita_comp = p_auth.
    ls_loc_id-tipo_reg      = cond #( when rb_bank = abap_true then 'BI' else 'DO').
    ls_loc_id-uname_cre     = sy-uname.
    ls_loc_id-uzeit_cre     = sy-uzeit.
    ls_loc_id-datum_cre     = sy-datum.

    append ls_loc_id to mt_regid.
    move-corresponding ls_loc_id to ls_reg_id.

    insert ymcms_arc_id_reg from ls_reg_id .
    if sy-subrc = 0.                                                              "Mod DF 17.01.2025
      commit work and wait.                                                       "Mod DF 17.01.2025
    else.                                                                         "Mod DF 17.01.2025
      rollback work.                                                              "Mod DF 17.01.2025
      message 'ERRORE Tecnico inserimento ID Reg.' type 'I' display like 'E'.     "Mod DF 17.01.2025
      return.                                                                     "Mod DF 17.01.2025
    endif.                                                                        "Mod DF 17.01.2025

    loop at lt_bukrs assigning field-symbol(<ls_bukrs>).
      ls_regsoc-bukrs = <ls_bukrs>-bukrs.
      ls_regsoc-id_reg = p_reid_b.
      insert ymcms_arc_regsoc from ls_regsoc.
      if sy-subrc = 0.                                                            "Mod DF 17.01.2025
        commit work and wait.                                                     "Mod DF 17.01.2025
      else.                                                                       "Mod DF 17.01.2025
        rollback work.                                                            "Mod DF 17.01.2025
        message 'ERRORE Tecnico inserimento Soc.' type 'I' display like 'E'.      "Mod DF 17.01.2025
        return.                                                                   "Mod DF 17.01.2025
      endif.                                                                      "Mod DF 17.01.2025
    endloop.

    mo_grid->refresh_table_display( ).
    message 'ID Registro salvato con successo' type 'S'.
    leave to screen 0.

  endmethod.                    "extract_data

  method modif_id_registro.

    data: ls_reg_id type ymcms_arc_id_reg,
          ls_regsoc type ymcms_arc_regsoc,
          lva_check type char01.                                                  "Mod DF 17.01.2025

    clear: lva_check.                                                             "Mod DF 17.01.2025

    "Controlli campi Dora
    if rb_dora is not initial.                                                    "Mod DF 17.01.2025
      check_dora_fields( importing e_check = lva_check ).                         "Mod DF 17.01.2025
      if lva_check is not initial.                                                "Mod DF 17.01.2025
        return.                                                                   "Mod DF 17.01.2025
      endif.                                                                      "Mod DF 17.01.2025
    endif.                                                                        "Mod DF 17.01.2025

    if s_bukrs[] is initial.                                                      "Mod DF 17.01.2025
      message 'Inserire almeno una società afferente al registro'                 "Mod DF 17.01.2025
        type 'I' display like 'E'.                                                "Mod DF 17.01.2025
      return.                                                                     "Mod DF 17.01.2025
    endif.                                                                        "Mod DF 17.01.2025

    "Cancellazione elenco società del registro
    delete from ymcms_arc_regsoc where id_reg = p_reid_b.
    if sy-subrc = 0.                                                              "Mod DF 17.01.2025
      commit work and wait.                                                       "Mod DF 17.01.2025
    else.                                                                         "Mod DF 17.01.2025
      rollback work.                                                              "Mod DF 17.01.2025
      message 'ERRORE Tecnico cancellazione società' type 'I' display like 'E'.   "Mod DF 17.01.2025
      return.                                                                     "Mod DF 17.01.2025
    endif.                                                                        "Mod DF 17.01.2025

    "Aggiornamento dei dati del registro
    update ymcms_arc_id_reg
      set descr_reg     = @p_dscr_b,
          manutentore   = @p_manute,
          tipo_entita   = @p_tipent,
          autorita_comp = @p_auth,
          uname_mod     = @sy-uname,
          uzeit_mod     = @sy-uzeit,
          datum_mod     = @sy-datum
      where id_reg      = @p_reid_b.

    if sy-subrc = 0.                                                              "Mod DF 17.01.2025
      commit work and wait.                                                       "Mod DF 17.01.2025
    else.                                                                         "Mod DF 17.01.2025
      rollback work.                                                              "Mod DF 17.01.2025
      message 'ERRORE Tecnico aggiornamento dati' type 'I' display like 'E'.      "Mod DF 17.01.2025
      return.                                                                     "Mod DF 17.01.2025
    endif.                                                                        "Mod DF 17.01.2025

    read table mt_regid assigning field-symbol(<ls_mod_id>) with key id_reg = p_reid_b.
    if sy-subrc = 0 .
      <ls_mod_id>-descr_reg     = p_dscr_b.
      <ls_mod_id>-manutentore   = p_manute.
      <ls_mod_id>-tipo_entita   = p_tipent.
      <ls_mod_id>-autorita_comp = p_auth.
      <ls_mod_id>-uname_mod     = sy-uname.
      <ls_mod_id>-uzeit_mod     = sy-uzeit.
      <ls_mod_id>-datum_mod     = sy-datum.
    endif.

    select bukrs,
           butxt
      from t001
      into table @data(lt_bukrs)
      where bukrs in @s_bukrs.

    loop at lt_bukrs assigning field-symbol(<ls_bukrs>).
      ls_regsoc-bukrs  = <ls_bukrs>-bukrs.
      ls_regsoc-id_reg = p_reid_b.
      insert ymcms_arc_regsoc from ls_regsoc.
      if sy-subrc = 0.                                                              "Mod DF 17.01.2025
        commit work and wait.                                                       "Mod DF 17.01.2025
      else.                                                                         "Mod DF 17.01.2025
        rollback work.                                                              "Mod DF 17.01.2025
        message 'ERRORE Tecnico inserimento società' type 'I' display like 'E'.     "Mod DF 17.01.2025
        exit.                                                                       "Mod DF 17.01.2025
      endif.                                                                        "Mod DF 17.01.2025
    endloop.

    mo_grid->refresh_table_display( ).
    message 'ID Registro aggiornato con successo' type 'S'.
    leave to screen 0.

  endmethod.                    "extract_data


  method handle_user_command.

    data: lv_answer   type char1,
          lt_rowindex type lvc_t_row.

    mo_grid->check_changed_data( ).

    gv_ucomm = e_ucomm.

    case e_ucomm.

      when 'FC_NUOVO'.
        "Caso Creazione nuovo ID Registro

        clear_subscreen_fields( ).
        call screen 0101 ending at 130 15 starting at 1 1 .

      when 'FC_MODIFICA'.
        "Caso modifica ID Registro

        clear_subscreen_fields( ).

        mo_grid->get_selected_rows(
        importing
          et_index_rows = lt_rowindex ).

        data(lt_lines) = lines( lt_rowindex  ).

        if lt_lines = 0.
          message 'Selezionare almeno una riga per continuare' type 'S' display like 'E'.
          return.
        elseif lt_lines > 1.
          message 'Non è possibile modificare più di un ID alla volta.' type 'S' display like 'E'.
          return.
        endif.

        data(lv_mod_ind) =  lt_rowindex[ 1 ]-index.
        read table mt_regid assigning field-symbol(<ls_modif_id>) index lv_mod_ind.

        if sy-subrc <> 0.
          message 'Dati con errori. Per favore provare di nuovo' type 'S' display like 'E'.
          return.
        endif.

        select 'I'   as sign,
               'EQ'  as option,
               bukrs as low
          from ymcms_arc_regsoc
          into table  @s_bukrs
          where id_reg = @<ls_modif_id>-id_reg.

        p_reid_b      = <ls_modif_id>-id_reg.
        p_dscr_b      = <ls_modif_id>-descr_reg.
        p_manute      = <ls_modif_id>-manutentore.
        p_auth        = <ls_modif_id>-autorita_comp.
        p_tipent      = <ls_modif_id>-tipo_entita.

        call screen 0101 ending at 130 15 starting at 1 1 .

      when 'FC_CANCELLA'.
        "Caso cancellazione ID Registro

        mo_grid->get_selected_rows(
        importing
          et_index_rows = lt_rowindex ).

        lt_lines = lines( lt_rowindex  ).

        if lt_lines = 0.
          message 'Selezionare almeno una riga per continuare' type 'S' display like 'E'.
          return.
        elseif lt_lines > 1.
          message 'Non è possibile cancellare più di un ID alla volta.' type 'S' display like 'E'.
          return.
        endif.

        lv_mod_ind =  lt_rowindex[ 1 ]-index.
        read table mt_regid assigning <ls_modif_id> index lv_mod_ind.

        if sy-subrc <> 0.
          message 'Data con errori. Per favore provare di nuovo' type 'S' display like 'E'.
          return.
        endif.

        call function 'POPUP_TO_CONFIRM'
          exporting
            titlebar              = 'Attenzione'
            text_question         = 'Si desidera eliminare il registro selezionato?'
            default_button        = '2'               "Mod DF 17.01.2025
            display_cancel_button = ' '               "Mod DF 17.01.2025
          importing
            answer                = lv_answer
          exceptions
            text_not_found        = 1
            others                = 2.

        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.

        if lv_answer = '1'.
          delete from ymcms_arc_id_reg where id_reg = <ls_modif_id>-id_reg.
          delete from ymcms_arc_regsoc where id_reg = <ls_modif_id>-id_reg.
          delete mt_regid where id_reg = <ls_modif_id>-id_reg.

          mo_grid->refresh_table_display( ).
          message 'ID Registro cancellato con successo' type 'S'.
        else.                                                                         "Mod DF 17.01.2025
          message 'Azione annullata. Nessuna cancellazione effettuata' type 'S'.      "Mod DF 17.01.2025
        endif.

      when 'FC_VISUALIZZA'.
        "Caso visualizzazione Società ID Registro

        mo_grid->get_selected_rows(
        importing
          et_index_rows = lt_rowindex ).

        lt_lines = lines( lt_rowindex  ).

        if lt_lines = 0.
          message 'Selezionare almeno una riga per continuare' type 'S' display like 'E'.
          return.
        elseif lt_lines > 1.
          message 'Non è possibile visualizzare più di un ID alla volta.' type 'S' display like 'E'.
          return.
        endif.

        lv_mod_ind =  lt_rowindex[ 1 ]-index.
        read table mt_regid assigning <ls_modif_id> index lv_mod_ind.

        if sy-subrc <> 0.
          message 'Data con errori. Per favore provare di nuovo' type 'S' display like 'E'.
          return.
        endif.

        gv_screen = 'S'.
        visualizza_societa( iv_regid = <ls_modif_id>-id_reg ).
        call screen 100.

    endcase.

    clear gv_ucomm.
  endmethod.                    "handle_user_command

  method handle_toolbar.
    data: ls_toolbar type stb_button.

    loop at e_object->mt_toolbar into ls_toolbar.
      case ls_toolbar-function.
        when '&LOCAL&CUT'
          or '&LOCAL&APPEND'
          or '&LOCAL&INSERT_ROW'
          or '&LOCAL&DELETE_ROW'
          or '&LOCAL&PASTE'
          or '&INFO'
          or '&CHECK'
          or '&REFRESH'
          or '&GRAPH'
          or '&LOCAL&COPY'
          or '&LOCAL&COPY_ROW'
          or '&LOCAL&UNDO'.

          delete e_object->mt_toolbar.

          continue.
      endcase.
    endloop.

    if gv_screen = 'R'.

      ls_toolbar-function   = 'FC_NUOVO'.
      ls_toolbar-text       = 'Nuovo ID registro'.
      ls_toolbar-icon       = icon_create.
      ls_toolbar-quickinfo  = 'Nuovo ID registro'.
      append ls_toolbar to e_object->mt_toolbar.

      ls_toolbar-function   = 'FC_MODIFICA'.
      ls_toolbar-text       = 'Modifica ID registro'.
      ls_toolbar-icon       = icon_change.
      ls_toolbar-quickinfo  = 'Modifica ID registro'.
      append ls_toolbar to e_object->mt_toolbar.

      ls_toolbar-function   = 'FC_CANCELLA'.
      ls_toolbar-text       = 'Cancella ID registro'.
      ls_toolbar-icon       = icon_delete.
      ls_toolbar-quickinfo  = 'Cancella ID registro'.
      append ls_toolbar to e_object->mt_toolbar.

      ls_toolbar-function   = 'FC_VISUALIZZA'.
      ls_toolbar-text       = 'Visualizza Società'.
      ls_toolbar-icon       = icon_display.
      ls_toolbar-quickinfo  = 'Visualizza Società'.
      append ls_toolbar to e_object->mt_toolbar.

    else.

      delete e_object->mt_toolbar where function = 'FC_NUOVO'    or
                                        function = 'FC_MODIFICA' or
                                        function = 'FC_CANCELLA' or
                                        function = 'FC_VISUALIZZA'.
    endif.

  endmethod.    "handle_toolbar

  method display_register.

    data: lt_fcat    type lvc_t_fcat,
          ls_layout  type lvc_s_layo,
          ls_variant type disvariant.

    gv_screen = 'R'.

    case abap_true.
      when rb_bank.

        lt_fcat = value #(
        ( fieldname = 'ID_REG'     ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'DESCR_REG'  ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'UNAME_CRE'  ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'DATUM_CRE'  ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'UNAME_MOD'  ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'DATUM_MOD'  ref_table = 'YMCMS_ARC_ID_REG' ) ).

      when rb_dora.

        lt_fcat = value #(
        ( fieldname = 'ID_REG'        ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'DESCR_REG'     ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'MANUTENTORE'   ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'BUTXT'         ref_table = 'T001' )
        ( fieldname = 'TIPO_ENTITA'   ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'AUTORITA_COMP' ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'UNAME_CRE'     ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'DATUM_CRE'     ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'UNAME_MOD'     ref_table = 'YMCMS_ARC_ID_REG' )
        ( fieldname = 'DATUM_MOD'     ref_table = 'YMCMS_ARC_ID_REG' ) ).

    endcase.



    ls_layout-sel_mode    = 'A'.
    ls_layout-stylefname  = 'STYLE'.
    ls_layout-cwidth_opt  = abap_true.
    if rb_bank = 'X'.                     "Mod DF 17.01.2025
      ls_layout-grid_title  = text-m01.   "Mod DF 17.01.2025
    else.                                 "Mod DF 17.01.2025
      ls_layout-grid_title  = text-m02.   "Mod DF 17.01.2025
    endif.                                "Mod DF 17.01.2025
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.

    set handler : handle_toolbar
                  handle_user_command
                  on_data_changed
                  for mo_grid.

    mo_grid->set_table_for_first_display(
       exporting
         i_save             = 'X'
         is_layout          = ls_layout
         is_variant         = ls_variant
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_regid ).
  endmethod.

  method visualizza_societa.

    data: lt_fcat    type lvc_t_fcat,
          ls_layout  type lvc_s_layo,
          ls_variant type disvariant.

    select s~id_reg,
           r~descr_reg,
           t~bukrs,
           t~butxt
      from ymcms_arc_id_reg as r
      join ymcms_arc_regsoc as s on s~id_reg = r~id_reg
      left join t001 as t on t~bukrs = s~bukrs
      into table @data(lt_visualizza)
      where r~id_reg = @iv_regid.

    lt_fcat = value #(
    ( fieldname = 'ID_REG'     ref_table = 'YMCMS_ARC_ID_REG' )
    ( fieldname = 'DESCR_REG'  ref_table = 'YMCMS_ARC_ID_REG' )
    ( fieldname = 'BUKRS'      ref_table = 'T001'             )
    ( fieldname = 'BUTXT'      ref_table = 'T001' ) ).

    ls_layout-sel_mode    = 'A'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-cwidth_opt  = abap_true.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.

    mo_grid->set_table_for_first_display(
       exporting
         i_save             = 'X'
         is_layout          = ls_layout
         is_variant         = ls_variant
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = lt_visualizza ).
  endmethod.

  method start_container.

    data: lo_cont   type ref to cl_gui_custom_container.

    if mo_grid is bound.
      return.
    endif.

    create object lo_cont
      exporting
        container_name              = 'CUSTOM_CONTAINER'
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
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endmethod.                    "start_container

  method check_dora_fields.

    data: lva_bukrs       type t001-bukrs,
          lva_tipo_entita type ymcms_tipo_ent.

    select single bukrs
      into lva_bukrs
      from t001
      where bukrs = p_manute.

    if sy-subrc <> 0.
      move 'X' to e_check.
      message 'ERRORE: Società manutentrice non esiste in SAP' type 'I' display like 'E'.
      exit.
    endif.

    select single tipo_entita
      into lva_tipo_entita
      from ymcms_arc_tp_ent
      where tipo_entita = p_tipent.

    if sy-subrc <> 0.
      move 'X' to e_check.
      message 'ERRORE: Tipo entità non esiste in SAP' type 'I' display like 'E'.
      exit.
    endif.

  endmethod.

  method clear_subscreen_fields.

    clear: p_reid_b, p_dscr_b, p_manute, p_tipent, p_auth, s_bukrs.
    refresh: s_bukrs.

  endmethod.

endclass.

at selection-screen output.

  if gv_ucomm = 'FC_MODIFICA'.
    loop at screen.
      if screen-name = 'P_REID_B'.
        screen-input = 0.
      endif.
      modify screen.
    endloop.
  endif.

  if rb_bank = abap_true.
    loop at screen.
      if ( screen-name = 'P_MANUTE' ) or
         ( screen-name = '%_P_MANUTE_%_APP_%-TEXT' ) or
         ( screen-name = 'P_TIPENT' ) or
         ( screen-name = '%_P_TIPENT_%_APP_%-TEXT' ) or
         ( screen-name = 'P_AUTH' ) or
         ( screen-name = '%_P_AUTH_%_APP_%-TEXT' ) .
        screen-active = 0.
      endif.
      modify screen.
    endloop.
  endif.

start-of-selection.
  create object go_arch_id_reg.
  go_arch_id_reg->execute( ).
