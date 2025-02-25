*&---------------------------------------------------------------------*
*& Report  YMCMS_ARCH_SUBFOR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report ymcms_arch_subfor.
tables ymcms_arc_subfo.

data: begin of gs_screen100,
        ok_code type syucomm,
      end of gs_screen100.

data: begin of gs_screen101,
        ok_code type syucomm,
      end of gs_screen101.

data: gv_mod   type char1.

selection-screen begin of block b1 with frame title text-001.
parameters : rb_sin radiobutton group g1 default 'X' user-command u01,
             rb_exe radiobutton group g1,
             rb_mob radiobutton group g1,
             rb_vis radiobutton group g1.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
parameters p_lifnr type bseg-lifnr modif id bl2.
selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-003.
*parameters p_subfor type bseg-lifnr modif id bl3.
parameters p_subfor type ymcms_arc_subfo-subfor modif id bl3.
selection-screen end of block b3.

selection-screen begin of block b4 with frame title text-004.
parameters :p_file type string modif id bl4,
            c_text as checkbox modif id bl4.
selection-screen end of block b4.

selection-screen begin of block b5 with frame title text-005.
select-options: s_subfor for ymcms_arc_subfo-subfor     modif id bl5,
                s_rag    for ymcms_arc_subfo-rag_soc2   modif id bl5,
                s_lei    for ymcms_arc_subfo-codice_lei modif id bl5,
                s_as     for ymcms_arc_subfo-codice_as  modif id bl5,
                s_stdc2  for ymcms_arc_subfo-stcd2      modif id bl5.
selection-screen end of block b5.

selection-screen begin of screen 1010 as subscreen.
selection-screen begin of block b6 with frame title text-002.
parameters: p_ragsc  type ymcms_arc_subfo-rag_soc2 obligatory lower case,
            p_iva    type ymcms_arc_subfo-stcd2 obligatory lower case,
            p_codlei type ymcms_arc_subfo-codice_lei lower case,
            p_codas  type ymcms_arc_subfo-codice_as lower case.
selection-screen end of block b6.
selection-screen end of screen 1010.

data lv_screename type screen-name.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_arch_subfor definition final.

  public section.

    types: begin of ty_fornitore,
             lifnr      type ymcms_arc_subfo-subfor,
             rag_soc2   type ymcms_arc_subfo-rag_soc2,
             stcd2      type ymcms_arc_subfo-stcd2,
             codice_lei type ymcms_arc_subfo-codice_lei,
             codice_as  type ymcms_arc_subfo-codice_as,
             subfor     type ymcms_arc_subfo-subfor,
             name1      type adrc-name1,
             name2      type adrc-name2,
           end of ty_fornitore,
           tt_fornitore type table of ty_fornitore.

    types: begin of ty_excel_for,
             rag_soc2   type ymcms_arc_subfo-rag_soc2,
             stcd2      type ymcms_arc_subfo-stcd2,
             codice_lei type ymcms_arc_subfo-codice_lei,
             codice_as  type ymcms_arc_subfo-codice_as,
             status     type icon-name,
             item       type sy-tabix,
             subfor     type ymcms_arc_subfo-subfor,
             messagio   type char100,
             blocked    type char1,
           end of ty_excel_for,
           tt_excel_for type table of ty_excel_for.

    data: mt_fornitore type tt_fornitore,
          ms_fornitore type ty_fornitore,
          mt_exc_forn  type tt_excel_for,
          mo_grid      type ref to cl_gui_alv_grid,
          mt_exc_msg   type tt_excel_for,
          mt_subfor    type table of ymcms_arc_subfo.

    methods execute.
    methods save_data importing iv_rag_soc2   type ymcms_arc_subfo-rag_soc2
                                iv_stcd2      type ymcms_arc_subfo-stcd2
                                iv_codice_lei type ymcms_arc_subfo-codice_lei
                                iv_codice_as  type ymcms_arc_subfo-codice_as
                                iv_subfor     type ymcms_arc_subfo-subfor optional
                                iv_modify     type abap_bool
                      changing  cv_number     type ymcms_arc_subfo-subfor optional.

    class-methods value_request.
    methods check_input_data     importing ev_modify       type abap_bool optional
                                 returning value(rv_error) type abap_bool.
    methods upload_file_to_itab returning value(rv_error) type abap_bool.
* inizio mod DF 13.01.2025
    methods pop_up_info importing iv_title  type string
                                  iv_text   type string
                        changing  cv_answer type char01.
* fine mod DF 13.01.2025

  private section.

    methods display_furnitore.
    methods display_exl_msg.
    methods display_all.
    methods start_container.
    methods creazione_singola.
    methods creazione_file_excel.
    methods modifica.
    methods visualizzazione.
    methods check_register_data importing iv_test type abap_bool.
* inizio mod DF 13.01.2024
    methods check_double_items importing iv_double     type tt_excel_for
                                         iv_stcd2      type stcd2
                                         iv_codice_as  type ymcms_cod_as
                                         iv_codice_lei type ymcms_cod_lei
                                         iv_tabix      type sy-tabix
                               changing  cv_check      type char01.
* fine mod DF 13.01.2024

    methods: handle_toolbar
                for event toolbar of cl_gui_alv_grid
      importing e_object .

endclass.                    "

data go_arch_subfor type ref to lcl_arch_subfor.

include ymcms_arch_subfor_pbo.
include ymcms_arch_subfor_pai.
*----------------------------------------------------------------------*
*       CLASS lcl_class_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_arch_subfor implementation.

  method execute.

    start_container( ).

    case abap_true.
      when rb_sin.
        creazione_singola( ).
      when rb_exe.
        creazione_file_excel( ).
      when rb_mob.
        gv_mod = rb_mob.
        modifica( ).
      when rb_vis.
        visualizzazione( ).
    endcase.
  endmethod.


  method creazione_singola.

    if p_lifnr is not initial.
      select l~lifnr
             l~stcd2
             l~codice_lei
             l~codice_as
             a~name1
             a~name2
        from lfa1 as l
        join adrc as a on a~addrnumber = l~adrnr
        into corresponding fields of table mt_fornitore
        where l~lifnr = p_lifnr.

      if mt_fornitore is not initial.
        ms_fornitore = mt_fornitore[ 1 ].
      endif.
    endif.

    if ms_fornitore is  not initial.

      concatenate ms_fornitore-name1 ms_fornitore-name2 into p_ragsc separated by space.
      p_codas   = ms_fornitore-codice_as.
      p_codlei  = ms_fornitore-codice_lei.
      p_iva     = ms_fornitore-stcd2.

    endif.

    call screen 0101 ending at 130 15 starting at 1 1 .

  endmethod.

  method creazione_file_excel.

    data(lv_filename_len) = strlen( p_file ) - 5.
    data(lv_extension) = p_file+lv_filename_len(5).

* inizio mod DF 13.01.2025
    if p_file is initial.
      message 'Percorso File Excel obbligatorio e non valorizzato' type 'S' display like 'E'.
      return.
    endif.
* fine mod DF 13.01.2025

    if ( lv_extension <> '.XLSX') and ( lv_extension+1 <> '.XLS').
      message 'ERRORE: Estensione file ammesse XLSX o XLS' type 'S' display like 'E'.
      return.
    endif.

    if upload_file_to_itab( ) = abap_true.
      return.
    endif.

    check_register_data( c_text ).

    display_exl_msg( ).
    call screen 100.

  endmethod.

  method check_register_data.
    data: "ls_double type ty_excel_for,"Mod DF 13.01.2025
      lt_double type tt_excel_for,     "Mod DF 13.01.2025
      lva_check type c length 1,       "Mod DF 13.01.2025
      lva_mess  type char100,          "Mod DF 13.01.2025
      lv_msg_nr type sy-tabix,
      lv_subfor type ymcms_arc_subfo-subfor.

    field-symbols: <ls_code_wa> type ymcms_arc_subfo.

    refresh: lt_double.                "Mod DF 13.01.2025

    select *
    from ymcms_arc_subfo
      into table @data(lt_code_exist)
      for all entries in @mt_exc_forn
      where codice_lei = @mt_exc_forn-codice_lei
      or    codice_as  = @mt_exc_forn-codice_as
      or    stcd2      = @mt_exc_forn-stcd2.

*    sort mt_exc_forn by stcd2 codice_as codice_lei ascending."Mod DF 13.01.2025

    move-corresponding mt_exc_forn[] to lt_double[]."Mod DF 13.01.2025

    loop at mt_exc_forn assigning field-symbol(<ls_exc_wa>).

      lv_msg_nr = sy-tabix.

* inizio mod DF 13.01.2025
* Controllo duplicazione riga per Partita IVA, Codice LEI e Codice AS

*      if sy-tabix = 1.
*
*        ls_double-stcd2      =  <ls_exc_wa>-stcd2.
*        ls_double-codice_as  =  <ls_exc_wa>-codice_as.
*        ls_double-codice_lei =  <ls_exc_wa>-codice_lei.
*
*      else.

*        if ( <ls_exc_wa>-stcd2      <> ls_double-stcd2      ) and
*           ( <ls_exc_wa>-codice_as  <> ls_double-codice_as  ) and
*           ( <ls_exc_wa>-codice_lei <> ls_double-codice_lei ).

*          ls_double-stcd2      =  <ls_exc_wa>-stcd2.
*          ls_double-codice_as  =  <ls_exc_wa>-codice_as.
*          ls_double-codice_lei =  <ls_exc_wa>-codice_lei.

*        else.

      clear lva_check.

      check_double_items(
          exporting iv_double = lt_double
                    iv_stcd2 = <ls_exc_wa>-stcd2
                    iv_codice_as = <ls_exc_wa>-codice_as
                    iv_codice_lei = <ls_exc_wa>-codice_lei
                    iv_tabix = sy-tabix
          changing cv_check = lva_check ).

      if lva_check is not initial.
        "Caso OK non duplicata

        clear lva_mess.
        if lva_check = 'X'.
          move text-m01 to lva_mess.
        elseif lva_check = 'I'.
          move text-m02 to lva_mess.
        elseif lva_check = 'L'.
          move text-m03 to lva_mess.
        elseif lva_check = 'A'.
          move text-m04 to lva_mess.
        endif.
* fine mod DF 13.01.2025

        <ls_exc_wa>-blocked = abap_true.

        append value #( status     = icon_red_light
                        item       = lv_msg_nr
                        rag_soc2   = <ls_exc_wa>-rag_soc2
                        stcd2      = <ls_exc_wa>-stcd2
                        codice_lei = <ls_exc_wa>-codice_lei
                        codice_as  = <ls_exc_wa>-codice_as
*                        messagio   = 'Riga duplicata') to mt_exc_msg.
                        messagio   = lva_mess ) to mt_exc_msg.
      endif.
*      endif. "Mod DF 13.01.2025


      if ( <ls_exc_wa>-rag_soc2 is initial ).
        <ls_exc_wa>-blocked = abap_true.

        append value #( status     = icon_red_light
                        item       = lv_msg_nr
                        rag_soc2   = <ls_exc_wa>-rag_soc2
                        stcd2      = <ls_exc_wa>-stcd2
                        codice_lei = <ls_exc_wa>-codice_lei
                        codice_as  = <ls_exc_wa>-codice_as
                        messagio   = 'Ragione sociale mancante') to mt_exc_msg.

      endif.

      if ( <ls_exc_wa>-stcd2 is initial ).
        <ls_exc_wa>-blocked = abap_true.

        append value #( status     = icon_red_light
                        item       = lv_msg_nr
                        rag_soc2   = <ls_exc_wa>-rag_soc2
                        stcd2      = <ls_exc_wa>-stcd2
                        codice_lei = <ls_exc_wa>-codice_lei
                        codice_as  = <ls_exc_wa>-codice_as
                        messagio   = 'Partita IVA mancante') to mt_exc_msg.

      endif.

      read table lt_code_exist assigning <ls_code_wa> with key stcd2    = <ls_exc_wa>-stcd2.
      if sy-subrc = 0.
        <ls_exc_wa>-blocked = abap_true.
        append value #( status     = icon_red_light
                        item       = lv_msg_nr
                        rag_soc2   = <ls_exc_wa>-rag_soc2
                        stcd2      = <ls_exc_wa>-stcd2
                        codice_lei = <ls_exc_wa>-codice_lei
                        codice_as  = <ls_exc_wa>-codice_as
                        subfor     = <ls_code_wa>-subfor
                        messagio   = 'Partita IVA esiste per un altro subfornitore') to mt_exc_msg.

      endif.

      if <ls_exc_wa>-codice_as is not initial.
        read table lt_code_exist assigning <ls_code_wa> with key codice_as = <ls_exc_wa>-codice_as.
        if sy-subrc = 0.
          <ls_exc_wa>-blocked = abap_true.
          append value #( status     = icon_red_light
                          item       = lv_msg_nr
                          rag_soc2   = <ls_exc_wa>-rag_soc2
                          stcd2      = <ls_exc_wa>-stcd2
                          codice_lei = <ls_exc_wa>-codice_lei
                          codice_as  = <ls_exc_wa>-codice_as
                          subfor     = <ls_code_wa>-subfor
                          messagio   = 'Codice AS esiste per un altro subfornitore') to mt_exc_msg.

        endif.
      endif.

      if <ls_exc_wa>-codice_lei is not initial.
        read table lt_code_exist assigning <ls_code_wa> with key codice_lei = <ls_exc_wa>-codice_lei.
        if sy-subrc = 0.
          <ls_exc_wa>-blocked = abap_true.
          append value #( status     = icon_red_light
                          item       = lv_msg_nr
                          rag_soc2   = <ls_exc_wa>-rag_soc2
                          stcd2      = <ls_exc_wa>-stcd2
                          codice_lei = <ls_exc_wa>-codice_lei
                          codice_as  = <ls_exc_wa>-codice_as
                          subfor     = <ls_code_wa>-subfor
                          messagio   = 'Codice LEI esiste per un altro subfornitore') to mt_exc_msg.

        endif.
      endif.

      if <ls_exc_wa>-blocked = abap_false.

        if iv_test = abap_true.

          append value #( status     = icon_green_light
                          item       = lv_msg_nr
                          rag_soc2   = <ls_exc_wa>-rag_soc2
                          stcd2      = <ls_exc_wa>-stcd2
                          codice_lei = <ls_exc_wa>-codice_lei
                          codice_as  = <ls_exc_wa>-codice_as
                          messagio   = 'OK – Lancio TEST') to mt_exc_msg.

        else.

          save_data(
          exporting
            iv_rag_soc2   = <ls_exc_wa>-rag_soc2
            iv_stcd2      = <ls_exc_wa>-stcd2
            iv_codice_lei = <ls_exc_wa>-codice_lei
            iv_codice_as  = <ls_exc_wa>-codice_as
            iv_modify     = abap_false
          changing
            cv_number = lv_subfor ).

          append value #( status     = icon_green_light
                          item       = lv_msg_nr
                          rag_soc2   = <ls_exc_wa>-rag_soc2
                          stcd2      = <ls_exc_wa>-stcd2
                          codice_lei = <ls_exc_wa>-codice_lei
                          codice_as  = <ls_exc_wa>-codice_as
                          subfor     = lv_subfor
                          messagio   = | OK – Subfornitore { lv_subfor } creato| ) to mt_exc_msg.

        endif.

      endif.

    endloop.
  endmethod.

  method modifica.

    data: lva_mess type string.

* inizio mod DF 13.01.2025
    if p_subfor is initial.
      clear lva_mess.
      concatenate 'ERRORE: campo Subfornitore'
                  'obbligatorio e non valorizzato.'
                  into lva_mess separated by space.
      condense lva_mess.
      message lva_mess type 'I' display like 'E'.
      return.
    endif.
* fine mod DF 13.01.2025

    select * from
      ymcms_arc_subfo
      into corresponding fields of table mt_fornitore
      where subfor = p_subfor.

    if sy-subrc <> 0.
* inizio mod DF 13.01.2025
*      message 'Subfornitore selezionato non esiste' type 'I' display like 'E'.
      clear lva_mess.
      concatenate 'ERRORE: Subfornitore'
                  p_subfor
                  'non esiste'
                  into lva_mess separated by space.
      condense lva_mess.
      message lva_mess type 'I' display like 'E'.
* fine mod DF 13.01.2025
      return.
    endif.

    if mt_fornitore is not initial.
      data(ls_fornitore) = mt_fornitore[ 1 ].
    endif.

    if ls_fornitore is  not initial.

      p_ragsc   = ls_fornitore-rag_soc2.
      p_codas   = ls_fornitore-codice_as.
      p_codlei  = ls_fornitore-codice_lei.
      p_iva     = ls_fornitore-stcd2.

    endif.

    call screen 0101 ending at 130 15 starting at 1 1 .


  endmethod.

  method visualizzazione.

    select * from ymcms_arc_subfo
      into table mt_subfor
      where subfor   in s_subfor
      and rag_soc2   in s_rag
      and codice_lei in s_lei
      and codice_as  in s_as
      and stcd2      in s_stdc2.

    sort mt_subfor by subfor.

    display_all( ).
    call screen 100.

  endmethod.

  method upload_file_to_itab.

    call function 'Z_EXCEL_TO_INTERNAL_TABLE'
      exporting
        filename                = conv file_table-filename( p_file )
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = 4
        i_end_row               = 65536
      tables
        e_tab                   = mt_exc_forn
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2
        others                  = 3.
    if sy-subrc <> 0.
      message 'Errore nel caricamento del file ' type 'S' display like 'E'.
      rv_error = abap_true.
    endif.

  endmethod.

  method save_data.

    data: number(10)   type c,
          ls_fornitore type ymcms_arc_subfo.

    if iv_modify = abap_false.

      call function 'NUMBER_GET_NEXT'
        exporting
          nr_range_nr             = '1'
          object                  = 'Z_SUBFOR'
          subobject               = ymcms_arc_subfo-subfor
        importing
          number                  = number
        exceptions
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          others                  = 8.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                   with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        return.
      endif.

      ls_fornitore-subfor     = number.

    endif.

    ls_fornitore-rag_soc2   = iv_rag_soc2.
    ls_fornitore-stcd2      = iv_stcd2.
    ls_fornitore-codice_lei = iv_codice_lei.
    ls_fornitore-codice_as  = iv_codice_as.
    ls_fornitore-uname      = sy-uname.
    ls_fornitore-datum      = sy-datum.
    ls_fornitore-uzeit      = sy-uzeit.

    if iv_modify = abap_false.
      modify ymcms_arc_subfo from ls_fornitore.
      clear ls_fornitore.

      message |Subfornitore con numero { number } creato con successo| type 'I' display like 'S'.
      clear: p_ragsc, p_codas, p_codlei, p_iva.

      if ms_fornitore is  not initial.

        concatenate ms_fornitore-name1 ms_fornitore-name2 into p_ragsc separated by space.
        p_codas   = ms_fornitore-codice_as.
        p_codlei  = ms_fornitore-codice_lei.
        p_iva     = ms_fornitore-stcd2.

      endif.

    else.

      update ymcms_arc_subfo set codice_as  = iv_codice_as
                                 codice_lei = iv_codice_lei
                                 rag_soc2   = iv_rag_soc2
                                 stcd2      = iv_stcd2
                                 uname      = sy-uname  "Mod DF 13.01.2025
                                 datum      = sy-datum  "Mod DF 13.01.2025
                                 uzeit      = sy-uzeit  "Mod DF 13.01.2025
                              where subfor = iv_subfor.
* inizio mod DF 13.01.2025
*      message |Subfornitore con numero { iv_subfor } aggiornato con successo| type 'S'.
      if sy-subrc = 0.
        commit work and wait.
        message |Subfornitore con numero { iv_subfor } aggiornato con successo| type 'I' display like 'S'.
      else.
        rollback work.
        message |ERRORE Aggiornamento Subfornitore { iv_subfor } | type 'I' display like 'E'.
      endif.
      leave to screen 0.
* fine mod DF 13.01.2025
    endif.

    cv_number = number.

  endmethod.

  method check_input_data.

    data: ls_codici type ymcms_arc_subfo.

    case abap_true.
      when rb_mob.
        data(lv_query) = 'subfor <> @p_subfor'.
    endcase.

* Inizio mod DF 13.01.2025

* Controllo che almeno un campo tra Codice LEI e Codice AS sia valorizzato
    if  p_codas is initial
    and p_codlei is initial.
      rv_error = abap_true.
      message 'ERRORE: valorizzare Codice LEI o Codice AS o entrambi.' type 'I' display like 'E'.
      return.
    endif.

* Controllo per partita IVA
    select single *
      from ymcms_arc_subfo
      into corresponding fields of @ls_codici
      where stcd2 = @p_iva
        and (lv_query) .

    if sy-subrc = 0.
      rv_error = abap_true.
      if rb_mob = abap_true.
        message |ERRORE: Partita IVA agganciata al subofornitore { ls_codici-subfor alpha = out }| type 'I' display like 'E'.
      elseif rb_sin = abap_true.
        message |ERRORE: Partita IVA agganciata al subofornitore { ls_codici-subfor alpha = out }| type 'I' display like 'E'.
      endif.
      return.
    endif.
* Fine mod DF 13.01.2025

    select single *
    from ymcms_arc_subfo
    into corresponding fields of @ls_codici
    where ( ( codice_as = @p_codas ) or (  codice_lei = @p_codlei ) )
    and  (lv_query) .

    if sy-subrc = 0.
      rv_error = abap_true.
      if rb_mob = abap_true.
        message |ERRORE: Codice LEI/AS esiste per subofornitore { ls_codici-subfor alpha = out }| type 'I' display like 'E'.
      elseif rb_sin = abap_true.
        message |ERRORE: Codice LEI/AS esiste per subofornitore { ls_codici-subfor alpha = out }| type 'I' display like 'E'.
      endif.
      return.
    endif.

  endmethod.                    "check_table_data

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

*    IF gv_check = abap_true.
*      ls_toolbar-function   = 'FC_SAVE'.
*      ls_toolbar-text       = 'Salva'.
*      ls_toolbar-icon       = icon_system_save.
*      ls_toolbar-quickinfo  = 'Salva i dati'.
*      APPEND ls_toolbar TO e_object->mt_toolbar.
*
*      ls_toolbar-function   = 'FC_ANULLA'.
*      ls_toolbar-text       = 'Anulla'.
*      ls_toolbar-icon       = icon_cancel.
*      ls_toolbar-quickinfo  = 'Anulla'.
*      APPEND ls_toolbar TO e_object->mt_toolbar.
*    ENDIF.

  endmethod.    "handle_toolbar

  method display_all.

    data: lt_fcat    type lvc_t_fcat,
          ls_layout  type lvc_s_layo,
          ls_variant type disvariant.

    lt_fcat = value #(
    ( fieldname = 'SUBFOR'     ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'RAG_SOC2'   ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'STCD2'      reptext   = 'Partita IVA'     )
    ( fieldname = 'CODICE_LEI' ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'CODICE_AS'  ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'UNAME'      ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'DATUM'      ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'UZEIT'      ref_table = 'YMCMS_ARC_SUBFO' )
    ).

    ls_layout-sel_mode    = 'D'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-stylefname  = 'STYLE'.
    ls_layout-cwidth_opt  = abap_true.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.


    set handler : handle_toolbar
                  for mo_grid.

    mo_grid->set_table_for_first_display(
       exporting
         i_save             = 'X'
         is_layout          = ls_layout
         is_variant         = ls_variant
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_subfor ).
  endmethod.


  method display_exl_msg.
    data: lt_fcat    type lvc_t_fcat,
          ls_layout  type lvc_s_layo,
          ls_variant type disvariant.

    lt_fcat = value #(
    ( fieldname = 'STATUS'     reptext   = 'Status'          )
    ( fieldname = 'ITEM'       reptext   = 'Item'            )
    ( fieldname = 'RAG_SOC2'   ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'STCD2'      reptext   = 'Partita IVA'     )
    ( fieldname = 'CODICE_LEI' ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'CODICE_AS'  ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'SUBFOR'     ref_table = 'YMCMS_ARC_SUBFO' )
    ( fieldname = 'MESSAGIO'   reptext   = 'Messagio'        )
    ).

    ls_layout-sel_mode    = 'D'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-stylefname  = 'STYLE'.
    ls_layout-cwidth_opt  = abap_true.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.


    set handler : handle_toolbar
                  for mo_grid.

    mo_grid->set_table_for_first_display(
       exporting
         i_save             = 'X'
         is_layout          = ls_layout
         is_variant         = ls_variant
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_exc_msg ).
  endmethod.

  method display_furnitore.

    data: lt_fcat    type lvc_t_fcat,
          ls_layout  type lvc_s_layo,
          ls_variant type disvariant.

    lt_fcat = value #(
    ( fieldname = 'RAG_SOC2'   ref_table = 'YMCMS_ARC_SUBFO' outputlen = 50 )
    ( fieldname = 'STCD2'      reptext   = 'Partita IVA'     outputlen = 15 )
    ( fieldname = 'CODICE_LEI' ref_table = 'YMCMS_ARC_SUBFO' outputlen = 25 )
    ( fieldname = 'CODICE_AS'  ref_table = 'YMCMS_ARC_SUBFO' outputlen = 25 )
    ).

    ls_layout-sel_mode    = 'D'.
    ls_layout-no_rowmark  = 'X'.
    ls_layout-stylefname  = 'STYLE'.
    ls_layout-edit        = 1.
    ls_variant-report     = sy-repid.
    ls_variant-username   = sy-uname.


    set handler : handle_toolbar
                  for mo_grid.


    mo_grid->set_ready_for_input( ).

    mo_grid->set_table_for_first_display(
       exporting
         i_save             = 'X'
         is_layout          = ls_layout
         is_variant         = ls_variant
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_fornitore ).

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

  method value_request.

    data: lt_filetable type filetable,
          lv_rc        type i.

    cl_gui_frontend_services=>file_open_dialog(
      exporting
        default_filename      = '*.XLS'
        file_filter           = '*.XLS'
        multiselection        = abap_false
    changing
      file_table              = lt_filetable
      rc                      = lv_rc
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5 ).

    if sy-subrc = 0.
      read table lt_filetable index 1 into p_file.
    endif.

  endmethod.

* inizio mod DF 13.01.2024
  method check_double_items.

    data: lt_tab_net type tt_excel_for,
          ls_tab_net type ty_excel_for.

    clear: ls_tab_net.

    refresh: lt_tab_net.

    move-corresponding iv_double[] to lt_tab_net[].

    "Cancello la riga attuale
    delete lt_tab_net index iv_tabix.

    "Ricerco un'altra riga con i medesimi valori
    loop at lt_tab_net into ls_tab_net
      where stcd2 = iv_stcd2
        and codice_as = iv_codice_as
        and codice_lei = iv_codice_lei.
      move 'X' to cv_check.
      exit.
    endloop.

    check cv_check is initial.

    "Ricerco un'altra riga con la medesima partita IVA
    loop at lt_tab_net into ls_tab_net
      where stcd2 = iv_stcd2.
      move 'I' to cv_check.
      exit.
    endloop.

    check cv_check is initial.

    "Ricerco un'altra riga con la medesimo Cocice LEI
    loop at lt_tab_net into ls_tab_net
      where codice_lei = iv_codice_lei.
      move 'L' to cv_check.
      exit.
    endloop.

    check cv_check is initial.

    "Ricerco un'altra riga con la medesimo Cocice AS
    loop at lt_tab_net into ls_tab_net
      where codice_lei = iv_codice_as.
      move 'A' to cv_check.
      exit.
    endloop.

  endmethod.

  method pop_up_info.

    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = iv_title
*       DIAGNOSE_OBJECT       = ' '
        text_question         = iv_text
        text_button_1         = 'SI'
*       ICON_BUTTON_1         = ' '
        text_button_2         = 'NO'
*       ICON_BUTTON_2         = ' '
        default_button        = '1'
        display_cancel_button = ' '
*       USERDEFINED_F1_HELP   = ' '
*       START_COLUMN          = 25
*       START_ROW             = 6
*       POPUP_TYPE            =
*       IV_QUICKINFO_BUTTON_1 = ' '
*       IV_QUICKINFO_BUTTON_2 = ' '
      importing
        answer                = cv_answer
*     TABLES
*       PARAMETER             =
      exceptions
        text_not_found        = 1
        others                = 2.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endmethod.
* fine mod DF 13.01.2024

endclass.

initialization.
  create object go_arch_subfor.

at selection-screen output.

  loop at screen.
    case abap_true.
      when rb_exe.
        case screen-group1.
          when 'BL2'.
            screen-active = 0.
          when 'BL3'.
            screen-active = 0.
          when 'BL4'.
            screen-active = 1.
          when 'BL5'.
            screen-active = 0.
        endcase.
      when rb_mob.
        case screen-group1.
          when 'BL2'.
            screen-active = 0.
          when 'BL3'.
            screen-active = 1.
          when 'BL4'.
            screen-active = 0.
          when 'BL5'.
            screen-active = 0.
        endcase.
      when rb_sin.
        case screen-group1.
          when 'BL2'.
            screen-active = 1.
          when 'BL3'.
            screen-active = 0.
          when 'BL4'.
            screen-active = 0.
          when 'BL5'.
            screen-active = 0.
        endcase.
      when rb_vis.
        case screen-group1.
          when 'BL2'.
            screen-active = 0.
          when 'BL3'.
            screen-active = 0.
          when 'BL4'.
            screen-active = 0.
          when 'BL5'.
            screen-active = 1.
        endcase.
    endcase.

    modify screen.
  endloop.

at selection-screen on value-request for p_file.
  lcl_arch_subfor=>value_request( ).

start-of-selection.
  go_arch_subfor->execute( ).
