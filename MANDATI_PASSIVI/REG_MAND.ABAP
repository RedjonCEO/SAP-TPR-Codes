*&---------------------------------------------------------------------*
*& Report  YMCMS_ARCH_MAND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report ymcms_arch_mand.

tables: ymcms_arc_pa_m, ymcms_arc_pa_e, sscrfields.

data: begin of gs_screen109,
        ok_code type sy-ucomm,
        ebeln   type ekko-ebeln,
        bukrs   type ekko-bukrs,
        bsart   type ekko-bsart,
        batxt   type t161t-batxt,
        bedat   type ekko-bedat,
        kdatb   type ekko-kdatb,
        kdate   type ekko-kdate,
        butxt   type t001-butxt,
      end of gs_screen109.

types: begin of ty_arc_subf,
         subfornitore      type ymcms_arc_subf-subfornitore,
         rag_soc2_subf     type ymcms_arc_subfo-rag_soc2,     "Mod DF 21.02.2025
         rank              type ymcms_arc_subf-rank,
         capogruppo_subfor type ymcms_arc_subf-subfornitore,
         rag_soc2_capg     type ymcms_arc_subfo-rag_soc2,     "Mod DF 21.02.2025
         trattam_dati      type ymcms_arc_subf-trattam_dati,
         descr_trat_dati   type ymcms_arc_tra_dt-descrizione, "Mod DF 21.02.2025
         selopt_pae_m      type standard table of selopt with empty key,
         selopt_pae_e      type standard table of selopt with empty key,
         style             type lvc_t_styl,
       end of ty_arc_subf,
       tt_inf_subfor type standard table of ty_arc_subf.

types: begin of ty_fornalt,
         sel_col  type char1,
         forn_alt type ymcms_arc_f_alt-forn_alt,
         ebeln    type ymcms_arc_f_alt-ebeln,
       end of ty_fornalt,
       tt_fornalt type table of ty_fornalt.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABSTRIP109'
constants: begin of c_tabstrip109,
             tab1 like sy-ucomm value 'TABSTRIP109_FC102',
             tab2 like sy-ucomm value 'TABSTRIP109_FC103',
             tab3 like sy-ucomm value 'TABSTRIP109_FC104',
             tab4 like sy-ucomm value 'TABSTRIP109_FC105',
             tab5 like sy-ucomm value 'TABSTRIP109_FC106',
             tab6 like sy-ucomm value 'TABSTRIP109_FC107',
           end of c_tabstrip109.
*&SPWIZARD: DATA FOR TABSTRIP 'TABSTRIP109'
controls:  tabstrip109 type tabstrip.
data: begin of g_tabstrip109,
        subscreen   like sy-dynnr,
        prog        like sy-repid value 'YMCMS_ARCH_MAND',
        pressed_tab like sy-ucomm value c_tabstrip109-tab1,
      end of g_tabstrip109.
*DATA:      gs_screen109-ok_code LIKE sy-ucomm.

data: begin of gs_screen_fields,
*--------------------------------------------------------------------*
*  SCREEN 102 informazioni di dettaglio del contratto
*--------------------------------------------------------------------*
        esternalizzazione   type ymcms_arc_mand-esternalizzazione,
        protocollo          type ymcms_arc_mand-protocollo,
        preavviso_pre       type ymcms_arc_mand-preavviso_pre,
        preavviso_for       type ymcms_arc_mand-preavviso_for,
        stato_pros_rin      type ymcms_arc_mand-stato_pros_rin,
        data_pros_rin       type ymcms_arc_mand-data_pros_rin,
        data_val_fei        type ymcms_arc_mand-data_val_fei,
        valitaz_conf        type ymcms_arc_mand-valitaz_conf,
        note                type string,
        id_funzione         type ymcms_arc_mand-id_funzione,
        num_rif_contr       type ymcms_arc_mand-num_rif_contr,
        tipo_accordo        type ymcms_arc_mand-tipo_accordo,
        tipo_servizi        type ymcms_arc_mand-tipo_servizi,
*--------------------------------------------------------------------*
*  SCREEN 103 informazioni firmatario ed utilizzatore
*--------------------------------------------------------------------*
        firmatario          type ymcms_arc_mand-firmatario,
        butxt_firm          type t001-butxt,
        utilizzatore        type ymcms_arc_mand-utilizzatore,
        butxt_util          type t001-butxt,
*--------------------------------------------------------------------*
*  SCREEN 104 informazioni funzione esternalizzata
*--------------------------------------------------------------------*
        categ_funz_est      type ymcms_arc_mand-categ_funz_est,
        descr_funz_est      type ymcms_arc_mand-descr_funz_est,
*--------------------------------------------------------------------*
*  SCREEN 105 informazioni sul fornitore
*--------------------------------------------------------------------*
        capogruppo          type ymcms_arc_mand-capogruppo,
        capogrp_rag         type ymcms_arc_subfo-rag_soc2,
        autorita_vigil      type ymcms_arc_mand-autorita_vigil,
        trattam_dati        type ymcms_arc_mand-trattam_dati,
        natura_dati_cloud   type string,
        paese_mem_dati      type ymcms_arc_mand-paese_mem_dati,
        pmd_landx           type t005t-landx,
        paese_erog_dati     type ymcms_arc_mand-paese_erog_dati,
        pes_landx           type t005t-landx,
        cloud_serv_mod      type ymcms_arc_mand-cloud_serv_mod,
        cloud_depl_mod      type ymcms_arc_mand-cloud_depl_mod,
        provider_cloud      type ymcms_arc_mand-provider_cloud,
        cloud_rag           type ymcms_arc_subfo-rag_soc2,
*--------------------------------------------------------------------*
*  SCREEN 106 informazioni sulle funzione essenziali e importanti
*--------------------------------------------------------------------*
        funz_ess_imp        type ymcms_arc_mand-funz_ess_imp,
        data_val_rischi     type ymcms_arc_mand-data_val_rischi,
        esito_val_rischi    type ymcms_arc_mand-esito_val_rischi,
        organo_dec          type ymcms_arc_mand-organo_dec,
        normativa_appl      type ymcms_arc_mand-normativa_appl,
        data_ult_ver_audit  type ymcms_arc_mand-data_ult_ver_audit,
        data_pros_ver_audit type ymcms_arc_mand-data_pros_ver_audit,
        sost_forn_d         type ymcms_arc_mand-sost_forn_d,
        motivo_insost       type ymcms_arc_mand-motivo_insost,
        piano_uscita        type ymcms_arc_mand-piano_uscita,
        reinternaliz_d      type ymcms_arc_mand-reinternaliz_d,
        impatto_inter_d     type ymcms_arc_mand-impatto_inter_d,
        funz_crit_tempo     type ymcms_arc_mand-funz_crit_tempo,
        sub_esternaliz      type ymcms_arc_mand-sub_esternaliz,
        motivo_fei          type ymcms_arc_mand-motivo_fei,
        forn_alternativi    type ymcms_arc_mand-forn_alternativi,
        motivo_risoluz      type ymcms_arc_mand-motivo_risoluz,
        paese_legge         type ymcms_arc_mand-paese_legge,
        paese_landx         type t005t-landx,
        sensibilita_dati    type ymcms_arc_mand-sensibilita_dati,
        livello_dip_serv    type ymcms_arc_mand-livello_dip_serv,
      end of gs_screen_fields.

* inizio mod DF 21.02.2025
data: ms_inf_subfor       type ty_arc_subf.

data: begin of ms_paesi_subfor,
        paese_mem_dati  like ymcms_td_mand-paese_mem_dati,
        paese_erog_dati like ymcms_td_mand-paese_erog_dati,
      end of ms_paesi_subfor.
* fine mod DF 21.02.2025

data: begin of gs_screen108,
        ok_code type sy-ucomm,
      end of gs_screen108.

data: subscr_nr type sy-dynnr,
      gv_ucomm  type sy-ucomm,
      gv_string type string.

selection-screen skip.
parameters p_mand type ekko-ebeln obligatory matchcode object mekk.

selection-screen begin of screen 1010 as subscreen.
selection-screen begin of block b2 with frame.
select-options: s_pmd for ymcms_arc_pa_m-paese_mem_dati,
                s_pes for ymcms_arc_pa_e-paese_erg_ser.
selection-screen end of block b2.
selection-screen end of screen 1010.

*----------------------------------------------------------------------*
*       CLASS lcl_arch_mand DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_arch_mand definition final.
  public section.

    data: mt_inf_subfor    type tt_inf_subfor,
          mt_arc_info      type standard table of ymcms_arc_info,
          mo_grid          type ref to cl_gui_alv_grid,
          mo_grid_f_alt    type ref to cl_gui_alv_grid,
          mt_fornalt       type table of ty_fornalt,
          ms_fornalt       type ty_fornalt,
          ms_arc_mand      like gs_screen_fields,
          mv_table_changed type abap_bool.

    methods execute.
    methods check_table_changes.
    methods display_inf_subfor.
* inizio mod DF 21.02.2025
    methods toolbar_excluding_sufor changing ch_toolbar type ui_functions.
* fine mod DF 21.02.2025
    methods start_container.
    methods display_for_alt.
    methods refresh_information.
    methods message_store       importing iv_msg_field type sy-msgv1.
    methods display_info        importing iv_fieldname type fieldname.
    methods message_init_show   importing iv_action    type char1.
    methods check_entered_data  returning value(rv_error) type abap_bool.
    methods save_data.
    methods populate_vrm_values importing iv_table_name type string
                                          iv_key_field  type string
                                          iv_text_field type string
                                          iv_vrm_id     type vrm_id.
    methods set_obligatory_fields_bankit changing cv_bankit type char1.
    methods set_obligatory_fields_dora changing cv_dora   type char1.
    methods: handle_user_command
                for event user_command of cl_gui_alv_grid
      importing e_ucomm .
    methods pop_up_info importing iv_title  type string
                                  iv_text   type string
                        changing  cv_answer type char01.
* inizio mod Df 19.02.2025
    methods check_anag_subfor importing i_subfor type ymcms_subfor
                                        i_fname  type string
                              exporting e_check  type char01.
    methods check_bukrs importing i_bukrs type bukrs
                                  i_fname type string
                        exporting e_check type char01.
    methods check_land1 importing i_land1 type land1
                                  i_fname type string
                        exporting e_check type char01.
* fine mod DF 19.02.2025
  private section.

    methods extract_data.
    methods prepare_data.
    methods check_duplicates importing iv_tablename type tablename.
    methods gos_implementation importing iv_objtype type borident-objtype.
    methods subforn_on_data_changed
    for event data_changed of cl_gui_alv_grid importing  er_data_changed .
    methods fornalt_on_data_changed
    for event data_changed of cl_gui_alv_grid importing  er_data_changed .
    methods: handle_toolbar
                for event toolbar of cl_gui_alv_grid
      importing e_object .

endclass.                    "

data go_arch_mand type ref to lcl_arch_mand.
include ymcms_arch_mand_pbo.
include ymcms_arch_mand_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_arch_mand IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_arch_mand implementation.

  method subforn_on_data_changed.
    field-symbols: <lt_subforn_chng_data> type tt_inf_subfor.
    assign er_data_changed->mp_mod_rows->* to <lt_subforn_chng_data>.
    if <lt_subforn_chng_data> is not initial.
      mv_table_changed = abap_true.
    endif.
  endmethod.

  method fornalt_on_data_changed.
    field-symbols: <lt_fornalt_chng_data> type tt_fornalt.
    assign er_data_changed->mp_mod_rows->* to <lt_fornalt_chng_data>.
    if <lt_fornalt_chng_data> is not initial.
      mv_table_changed = abap_true.
    endif.
  endmethod.

  method gos_implementation.

    data lo_gos_manager type ref to cl_gos_manager.
    data(ls_obj) = value borident( objtype = iv_objtype objkey = p_mand ).

    create object lo_gos_manager
      exporting
        is_object      = ls_obj
        ip_no_commit   = abap_false
      exceptions
        object_invalid = 1.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endmethod.

  method execute.

    start_container( ).
    extract_data( ).
    display_inf_subfor( ).
    display_for_alt( ).
    call screen 109.

  endmethod.                    "execute

  method extract_data.

    data: lva_id_reg type ymcms_arc_id_reg-id_reg.

    select single *
      from ekko
      into @data(ls_ekko)
      where ebeln = @p_mand
        and bstyp in ( 'K','F' ).

    if ls_ekko is initial.
      message 'Codice inserito non esiste in SAP' type 'S' display like 'E'.
      return.
    endif.

    case ls_ekko-bstyp.
      when 'K'.
        gos_implementation( iv_objtype = 'BUS2014').
      when 'F'.
        gos_implementation( iv_objtype = 'BUS2012').
    endcase.

    select *
    from ymcms_arc_info
    into corresponding fields of table mt_arc_info
    where tabname = 'YMCMS_ARC_MAND'.

* inizio mod DF 21.02.2025

*    select *
*      from ymcms_arc_subf
*      into corresponding fields of table mt_inf_subfor
*      where ebeln = p_mand.

    select subf~subfornitore as subfornitore,
           subfo~rag_soc2 as rag_soc2_subf,
           subf~rank as rank,
           subf~capogruppo_subfor as capogruppo_subfor,
           subfo2~rag_soc2 as rag_soc2_capg,
           subf~trattam_dati as trattam_dati,
           tra_dt~descrizione as descr_trat_dati
      into corresponding fields of table @mt_inf_subfor
      from ymcms_arc_subf as subf
      left outer join ymcms_arc_subfo as subfo
        on subfo~subfor = subf~subfornitore
      left outer join ymcms_arc_subfo as subfo2
        on subfo2~subfor = subf~capogruppo_subfor
      left outer join ymcms_arc_tra_dt as tra_dt
        on tra_dt~trattam_dati = subf~trattam_dati
      where subf~ebeln = @p_mand.
* fine mod DF 21.02.2025

    select single *
      from ymcms_arc_mand
      into corresponding fields of ms_arc_mand
      where ebeln = p_mand.

    if sy-subrc = 0.
      move-corresponding ms_arc_mand to gs_screen_fields.
    else.

      select single k~ebeln
        from ekko as k
        inner join ekpo as p on  p~ebeln = k~ebeln
        inner join eban as e on  e~banfn = p~banfn
        and e~bnfpo = p~bnfpo
        into @data(lv_ebeln)
        where k~ebeln = @p_mand
        and   e~zzfoi = 'X'.

      if sy-subrc = 0.
        gs_screen_fields-funz_ess_imp = 'SI'.
        ms_arc_mand-funz_ess_imp = 'SI'.
      endif.

* inizio mod DF 14.02.2025
* Prevalorizzazione esternalizzazione
      select single id_reg~id_reg
        into lva_id_reg
        from ekpo as ekpo
        inner join ymcms_arc_grp_mr as grp_mer
          on grp_mer~matkl = ekpo~matkl
        inner join ymcms_arc_id_reg as id_reg
          on id_reg~id_reg = grp_mer~id_reg
        where ekpo~ebeln = p_mand
          and id_reg~tipo_reg = 'BI'.

      if sy-subrc = 0.
        gs_screen_fields-esternalizzazione = 'SI'.
        ms_arc_mand-esternalizzazione = 'SI'.
      else.
        gs_screen_fields-esternalizzazione = 'NO'.
        ms_arc_mand-esternalizzazione = 'NO'.
      endif.
* fine mod DF 14.02.2025

    endif.

    select *
    from ymcms_arc_f_alt
    into corresponding fields of table mt_fornalt
    where ebeln = p_mand.

    prepare_data( ).
    refresh_information( ).

    move-corresponding gs_screen_fields to ms_arc_mand.

  endmethod.

  method prepare_data.

    loop at mt_inf_subfor assigning field-symbol(<ls_arc_subf>).

      append value lvc_s_styl( fieldname = 'SUBFORNITORE' style = cl_gui_alv_grid=>mc_style_disabled ) to <ls_arc_subf>-style.

      select 'I'            as sign,
             'EQ'           as option,
             paese_mem_dati as low
        from ymcms_arc_pa_m
        into table  @<ls_arc_subf>-selopt_pae_m
        where ebeln = @p_mand
        and   subfornitore = @<ls_arc_subf>-subfornitore.

      select 'I'            as sign,
             'EQ'           as option,
             paese_erg_ser  as low
        from ymcms_arc_pa_e
        into table  @<ls_arc_subf>-selopt_pae_e
        where ebeln = @p_mand
        and   subfornitore = @<ls_arc_subf>-subfornitore.

    endloop.

  endmethod.

  method refresh_information.

    select single
           k~ebeln  as ebeln,
           k~bedat  as bedat,
           k~bukrs  as bukrs,
           t~butxt  as butxt,
           k~bsart  as bsart,
           tx~batxt as batxt,
           k~kdatb  as kdatb,
           k~kdate  as kdate
      from ekko as k
      left join t001  as t
        on t~bukrs = k~bukrs
      left join t161t as tx
        on  tx~spras = @sy-langu
        and tx~bsart = k~bsart
      into  corresponding fields of @gs_screen109
      where k~ebeln = @p_mand
      and k~bstyp in ( 'K','F' ).


    select single butxt
      from t001
      into gs_screen_fields-butxt_firm
      where bukrs = gs_screen_fields-firmatario.

    select single butxt
      from t001
      into gs_screen_fields-butxt_util
      where bukrs = gs_screen_fields-utilizzatore.

    select single rag_soc2
      from ymcms_arc_subfo
      into gs_screen_fields-capogrp_rag
      where subfor = gs_screen_fields-capogruppo.

    select single rag_soc2
      from ymcms_arc_subfo
      into gs_screen_fields-cloud_rag
      where subfor = gs_screen_fields-provider_cloud.

    select single landx
      from t005t
      into gs_screen_fields-pmd_landx
      where land1 = gs_screen_fields-paese_mem_dati
      and   spras = sy-langu.

    select single landx
      from t005t
      into gs_screen_fields-pes_landx
      where land1 = gs_screen_fields-paese_erog_dati
      and   spras = sy-langu.

    select single landx
      from t005t
      into gs_screen_fields-paese_landx
      where land1 = gs_screen_fields-paese_legge
      and   spras = sy-langu.

  endmethod.

  method save_data.

    data: ls_arc_mand  type ymcms_arc_mand.

    if check_entered_data( ) = abap_true.
      return.
    endif.

    delete from: ymcms_arc_f_alt,
                 ymcms_arc_mand,
                 ymcms_arc_subf,
                 ymcms_arc_pa_e,
                 ymcms_arc_pa_m  where ebeln = p_mand.

    move-corresponding: gs_screen_fields to ls_arc_mand.
    ls_arc_mand-ebeln = p_mand.

    insert ymcms_arc_mand from ls_arc_mand.

    loop at mt_fornalt assigning field-symbol(<ls_fornalt>).
      data(ls_arc_f_alt) = value ymcms_arc_f_alt( ebeln = p_mand forn_alt = <ls_fornalt>-forn_alt ).
      insert  ymcms_arc_f_alt from ls_arc_f_alt .
    endloop.

    loop at mt_inf_subfor assigning field-symbol(<ls_inf_subfor>).
      data(ls_inf_subfor) = value ymcms_arc_subf( ebeln              = p_mand
                                                  subfornitore       = <ls_inf_subfor>-subfornitore
                                                  rank               = <ls_inf_subfor>-rank
                                                  capogruppo_subfor  = <ls_inf_subfor>-capogruppo_subfor
                                                  trattam_dati       = <ls_inf_subfor>-trattam_dati ).
      insert  ymcms_arc_subf from ls_inf_subfor .

      loop at <ls_inf_subfor>-selopt_pae_e assigning field-symbol(<ls_pae_erog>).
        data(ls_arc_pa_e) = value ymcms_arc_pa_e( ebeln = p_mand paese_erg_ser = <ls_pae_erog>-low subfornitore = <ls_inf_subfor>-subfornitore ).
        insert  ymcms_arc_pa_e from ls_arc_pa_e .
      endloop.

      loop at <ls_inf_subfor>-selopt_pae_m assigning field-symbol(<ls_pae_mem>).
        data(ls_arc_pa_m) = value ymcms_arc_pa_m( ebeln = p_mand paese_mem_dati = <ls_pae_mem>-low subfornitore = <ls_inf_subfor>-subfornitore ).
        insert  ymcms_arc_pa_m from ls_arc_pa_m .
      endloop.

    endloop.

    commit work and wait.

    message 'I dati vengono salvati con successo' type 'S'.
    clear mv_table_changed.

    select single *
    from ymcms_arc_mand
    into corresponding fields of ms_arc_mand
    where ebeln = p_mand.

  endmethod.

  method message_init_show.

    case iv_action.
      when 'I'.
        call function 'MESSAGES_INITIALIZE'
          exceptions
            others = 1.
      when 'S'.
        call function 'MESSAGES_SHOW'
          exporting
            i_use_grid         = 'X'  " Comment for list display
            batch_list_type    = 'L'
            send_if_one        = space
            show_linno         = ''
            show_linno_text    = space
          exceptions
            inconsistent_range = 1
            no_messages        = 2
            others             = 3.
    endcase.

  endmethod.

  method message_store.

    call function 'MESSAGE_STORE'
      exporting
        arbgb                  = 'N1ME'
        msgty                  = 'E'
        msgv1                  = iv_msg_field
        txtnr                  = '087'
      exceptions
        message_type_not_valid = 1
        not_active             = 2
        others                 = 3.
  endmethod.


  method check_entered_data.

    data: lv_bankit        type char1,
          lv_dora          type char1,
          lt_empty_fields  type standard table of string,
          lva_land1        type t005-land1,                     "Mod DF 19.02.205
          lva_trattam_dati type ymcms_arc_tra_dt-trattam_dati.  "Mod DF 19.02.205

    clear rv_error.

    check_table_changes( ).

    message_init_show( 'I' ).

    set_obligatory_fields_bankit(
      changing
        cv_bankit = lv_bankit ).

    set_obligatory_fields_dora(
      changing
        cv_dora   = lv_dora ).


    if lv_bankit = abap_true.
*********** Campi obbligatori BANKIT ********************

      if gs_screen_fields-data_pros_rin is initial.
        append 'Data prossimo rinnovo ' to lt_empty_fields.
      endif.

      if gs_screen_fields-stato_pros_rin is initial.
        append 'Stato data prossimo rinnovo ' to lt_empty_fields.
      endif.

      if gs_screen_fields-preavviso_for is initial.
        append 'Periodo preavviso per il fornitore ' to lt_empty_fields.
      endif.

      if gs_screen_fields-preavviso_pre is initial.
        append 'Periodo preavviso per Prelios ' to lt_empty_fields.
      endif.

      if  gs_screen_fields-data_val_fei is initial and gs_screen_fields-funz_ess_imp = 'SI'.     "Mod DF 19.02.205
        append 'Data ultima valutazione a FEI ' to lt_empty_fields.
      endif.

      if gs_screen_fields-valitaz_conf is initial.
        append 'Valutazione conformità ' to lt_empty_fields.
      endif.

* inizio mod DF 19.02.2025
      if gs_screen_fields-firmatario is initial.
        append 'Firmatario del contratto ' to lt_empty_fields.
      else.
        "Controllo esistenza società

        go_arch_mand->check_bukrs(
          exporting i_bukrs = gs_screen_fields-firmatario
                    i_fname = 'Firmatario del contratto'
          importing e_check = rv_error ).

      endif.

      if gs_screen_fields-utilizzatore is initial.
        append 'Utilizzatore del contratto ' to lt_empty_fields.
      else.
        "Controllo esistenza società

        go_arch_mand->check_bukrs(
          exporting i_bukrs = gs_screen_fields-utilizzatore
                    i_fname = 'Utilizzatore del contratto'
          importing e_check = rv_error ).

      endif.
* fine mod DF 19.02.2025

      if gs_screen_fields-categ_funz_est is initial.
        append 'Categoria funzione esternalizzata ' to lt_empty_fields.
* inizio mod DF 19.02.2025
      else.
        "Controllo Descrizione funzione esternalizzata
        select single obb_descr_funz
          into @data(lva_obb_descr_funz)
          from ymcms_arc_catfun
          where categ_funz_est = @gs_screen_fields-categ_funz_est.

        if  sy-subrc = 0
        and lva_obb_descr_funz = abap_true
        and gs_screen_fields-descr_funz_est is initial.
          append 'Descrizione funzione esternalizzata' to lt_empty_fields.
        endif.
* fine mod DF 19.02.2025
      endif.

      if gs_screen_fields-trattam_dati is initial.
        append 'Trattamento dati personali ' to lt_empty_fields.
      endif.

      if gs_screen_fields-paese_mem_dati is initial.
        append 'Paese memorizzazione dei dati ' to lt_empty_fields.
      endif.

      if gs_screen_fields-paese_erog_dati is initial.
        append 'Paese erogazione dei servizi ' to lt_empty_fields.
      endif.

      if gs_screen_fields-cloud_serv_mod is initial.
        append 'Cloud service model ' to lt_empty_fields.
      endif.

      if gs_screen_fields-cloud_depl_mod is initial.
        append 'Cloud deployment model ' to lt_empty_fields.
      endif.

      if gs_screen_fields-funz_ess_imp is initial.
        append 'Funzione essenziale e importante ' to lt_empty_fields.
      endif.

      if gs_screen_fields-data_val_rischi is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Data ultima valutazione dei rischi ' to lt_empty_fields.
      endif.

      if gs_screen_fields-esito_val_rischi is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Esito valutazione dei rischi ' to lt_empty_fields.
      endif.

      if gs_screen_fields-organo_dec is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Organo decisionale ' to lt_empty_fields.
      endif.

      if gs_screen_fields-normativa_appl is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Normativa applicabile ' to lt_empty_fields.
      endif.

      if gs_screen_fields-data_ult_ver_audit is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Data ultima verifica audit ' to lt_empty_fields.
      endif.

      if gs_screen_fields-data_pros_ver_audit is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Data prossima verifica audit ' to lt_empty_fields.
      endif.

      if gs_screen_fields-sost_forn_d is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Sostituibilità fornitore ' to lt_empty_fields.
      endif.

      if gs_screen_fields-reinternaliz_d is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Reinternalizzazione ' to lt_empty_fields.
      endif.

      if gs_screen_fields-impatto_inter_d is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Impatto interruzione ' to lt_empty_fields.
      endif.

      if gs_screen_fields-funz_crit_tempo is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Funzione critica in termini di tempo ' to lt_empty_fields.
      endif.

      if gs_screen_fields-sub_esternaliz is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Sub-esternalizzazione ' to lt_empty_fields.
      endif.

      if gs_screen_fields-motivo_fei is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Motivazione FEI ' to lt_empty_fields.
      endif.

* inizio mod DF 19.02.2025
*      if ls_sos_fo-obb_motivo_insost = abap_true and gs_screen_fields-motivo_insost is initial.
*        append 'Motivo insostituibilità fornitore ' to lt_empty_fields.
*      endif.
* fine mod DF 19.02.2025

      read table mt_fornalt into ms_fornalt index 1.
      if ms_fornalt-forn_alt is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Fornitore alternativo ' to lt_empty_fields.
      endif.

      data(lv_tablename) = conv tablename( 'MT_FORNALT' ).
      check_duplicates( iv_tablename = lv_tablename ).
    endif.


    if lv_dora = abap_true.
*********** Campi obbligatori DORA ********************

      if gs_screen_fields-preavviso_for is initial.
        append 'Periodo preavviso per il fornitore ' to lt_empty_fields.
      endif.

      if gs_screen_fields-preavviso_pre is initial.
        append 'Periodo preavviso per Prelios ' to lt_empty_fields.
      endif.

      if gs_screen_fields-tipo_accordo is initial.
        append 'Tipo di accordo contrattuale ' to lt_empty_fields.
* inizio mod DF 19.02.2025
      else.
        "Controllo Numero di riferimento dell'accordo contrattuale generale
        select single obb_rif_contr
          into @data(lva_obb_rif_contr)
          from ymcms_arc_tp_acc
          where tipo_accordo = @gs_screen_fields-tipo_accordo.

        if sy-subrc = 0
        and lva_obb_rif_contr = abap_true
        and gs_screen_fields-num_rif_contr is initial.
          append 'Numero di riferimento accordo contrattuale' to lt_empty_fields.
        endif.
* fine mod DF 19.02.2025
      endif.

      if gs_screen_fields-id_funzione is initial.
        append 'ID Funzione ' to lt_empty_fields.
      endif.

* inizio mod DF 19.02.2025
      if gs_screen_fields-firmatario is initial.
        append 'Firmatario del contratto ' to lt_empty_fields.
      else.
        "Controllo esistenza società

        go_arch_mand->check_bukrs(
          exporting i_bukrs = gs_screen_fields-firmatario
                    i_fname = 'Firmatario del contratto'
          importing e_check = rv_error ).

      endif.

      if gs_screen_fields-utilizzatore is initial.
        append 'Utilizzatore del contratto ' to lt_empty_fields.
      else.
        "Controllo esistenza società

        go_arch_mand->check_bukrs(
          exporting i_bukrs = gs_screen_fields-utilizzatore
                    i_fname = 'Utilizzatore del contratto'
          importing e_check = rv_error ).

      endif.
* fine mod DF 19.02.2025

      if gs_screen_fields-tipo_servizi is initial.
        append 'Tipologia di servizi ICT ' to lt_empty_fields.
      endif.

      if gs_screen_fields-paese_mem_dati is initial.
        append 'Paese memorizzazione dei dati ' to lt_empty_fields.
      endif.

      if gs_screen_fields-paese_erog_dati is initial.
        append 'Paese erogazione dei servizi mancante' to lt_empty_fields.
      endif.

      if gs_screen_fields-funz_ess_imp is initial.
        append 'Funzione essenziale e importante ' to lt_empty_fields.
      endif.

      if gs_screen_fields-data_val_rischi is initial." and gs_screen_fields-funz_ess_imp = 'SI'. Mod DF 19.02.2025
        append 'Data ultima valutazione dei rischi ' to lt_empty_fields.
      endif.

      if gs_screen_fields-data_ult_ver_audit is initial." and gs_screen_fields-funz_ess_imp = 'SI'. "Mod DF 19.02.2025
        append 'Data ultima verifica audit ' to lt_empty_fields.
      endif.

      if gs_screen_fields-sost_forn_d is initial." and gs_screen_fields-funz_ess_imp = 'SI'. "Mod DF 19.02.2025
        append 'Sostituibilità fornitore ' to lt_empty_fields.
* inizio mod DF 19.02.2025
      else.
        "Controllo motivo insostituibilità fornitore

        select single *
          from ymcms_arc_sos_fo
          into @data(ls_sos_fo)
          where sost_forn_d = @gs_screen_fields-sost_forn_d.

        if sy-subrc = 0
        and ls_sos_fo-obb_motivo_insost = abap_true
        and gs_screen_fields-motivo_insost is initial.
          append 'Motivo insostituibilità fornitore ' to lt_empty_fields.
        endif.

* fine mod DF 19.02.2025
      endif.

      if gs_screen_fields-piano_uscita is initial.
        append 'Esistenza di un piano di uscita ' to lt_empty_fields.
      endif.

      if gs_screen_fields-reinternaliz_d is initial." and gs_screen_fields-funz_ess_imp = 'SI'. "Mod DF 19.02.2025
        append 'Reinternalizzazione ' to lt_empty_fields.
      endif.

      if gs_screen_fields-impatto_inter_d is initial." and gs_screen_fields-funz_ess_imp = 'SI'. "Mod DF 19.02.2025
        append 'Impatto interruzione ' to lt_empty_fields.
      endif.

      if gs_screen_fields-forn_alternativi is initial.
        append 'Presenza di alternative al fornitore ' to lt_empty_fields.
      endif.

      if  gs_screen_fields-paese_legge is initial
      "and gs_screen_fields-funz_ess_imp = 'SI'.                                                  "Mod DF 19.02.2025
      and gs_screen_fields-forn_alternativi = 'SI'.                                               "Mod DF 19.02.2025
        append 'Paese della legge applicabile al contratto ' to lt_empty_fields.
      endif.

      if gs_screen_fields-sensibilita_dati is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Sensibilità dei dati archiviati dal fornitore ' to lt_empty_fields.
      endif.

      if gs_screen_fields-livello_dip_serv is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Livello di dipendenza dal servizio fornitore ' to lt_empty_fields.
      endif.

      read table mt_fornalt into ms_fornalt index 1.
      if ms_fornalt-forn_alt is initial and gs_screen_fields-funz_ess_imp = 'SI'.
        append 'Fornitore alternativo' to lt_empty_fields.
      endif.

      lv_tablename = 'MT_FORNALT'.
      check_duplicates( iv_tablename = lv_tablename ).
    endif.

    if gs_screen_fields-protocollo is not initial."Mod DF 19.02.2025

      select single protocollo
        from ymcms_arc_mand
        into @data(lv_prot)
        where protocollo = @gs_screen_fields-protocollo
        and ebeln <> @p_mand.

      if sy-subrc = 0.
        call function 'MESSAGE_STORE'
          exporting
            arbgb                  = 'AD'
            msgty                  = 'E'
            msgv1                  = 'Protocollo'
            msgv2                  = gs_screen_fields-protocollo
            msgv3                  = 'assegnato ad altro contratto'
            txtnr                  = '010'
          exceptions
            message_type_not_valid = 1
            not_active             = 2
            others                 = 3.
        rv_error = abap_true.
      endif.

    endif."Mod DF 19.02.2025

* inizio mod DF 19.02.2025
    if mt_inf_subfor[] is initial.

      if ( lv_dora is not initial ) or
         ( lv_bankit is not initial and gs_screen_fields-funz_ess_imp = 'SI' ).

        call function 'MESSAGE_STORE'
          exporting
            arbgb                  = 'AD'
            msgty                  = 'E'
            msgv1                  = 'Inserire almeno una riga afferente ai sub-fornitori'
            txtnr                  = '010'
          exceptions
            message_type_not_valid = 1
            not_active             = 2
            others                 = 3.
        rv_error = abap_true.

      endif.

    else.
* fine mod DF 19.02.2025

      loop at mt_inf_subfor assigning field-symbol(<ls_inf_subfor>).
        if ( lv_dora is not initial ) or
           ( lv_bankit is not initial and gs_screen_fields-funz_ess_imp = 'SI' ).

          if <ls_inf_subfor>-subfornitore is initial.
            append 'Codice subfornitore' to lt_empty_fields.
* ini<io mod DF 19.02.2025
          else.
            "Controllo esistenza anagrafica
            go_arch_mand->check_anag_subfor(
             exporting i_subfor = <ls_inf_subfor>-subfornitore
                       i_fname = 'Codice subfornitore'
             importing e_check = rv_error ).
* fine mod DF 19.02.2025
          endif.

* ini<io mod DF 19.02.2025
*          if <ls_inf_subfor>-rank is initial.
*            append 'Rank subfornitore' to lt_empty_fields.
*          endif.

          if  <ls_inf_subfor>-rank is not initial.
            "Controllo correttezza RANK

            if <ls_inf_subfor>-rank = 1.
              "Controllo sul rank che deve essere maggiore di 1

              call function 'MESSAGE_STORE'
                exporting
                  arbgb                  = 'AD'
                  msgty                  = 'E'
                  msgv1                  = 'Campo Rank: non ammesso il valore 1'
                  txtnr                  = '010'
                exceptions
                  message_type_not_valid = 1
                  not_active             = 2
                  others                 = 3.
              rv_error = abap_true.

            elseif <ls_inf_subfor>-rank < 0.
              "Controllo che il rank sia negativo

              call function 'MESSAGE_STORE'
                exporting
                  arbgb                  = 'AD'
                  msgty                  = 'E'
                  msgv1                  = 'Campo Rank: non ammessi valori negativi'
                  txtnr                  = '010'
                exceptions
                  message_type_not_valid = 1
                  not_active             = 2
                  others                 = 3.
              rv_error = abap_true.

            endif.

          endif.

*          if <ls_inf_subfor>-capogruppo_subfor is initial.
*            append 'Capogruppo subfornitore' to lt_empty_fields.
*          endif.

          if <ls_inf_subfor>-capogruppo_subfor is not initial.
            "Capogruppo subfornitore

            go_arch_mand->check_anag_subfor(
             exporting i_subfor = <ls_inf_subfor>-capogruppo_subfor
                       i_fname = 'Capogruppo subfornitore'
             importing e_check = rv_error ).

          endif.
* fine mod DF 19.02.2025

          if <ls_inf_subfor>-trattam_dati is initial.
            append 'Trattamento dati personali subfornitore' to lt_empty_fields.
* inizio mod DF 19.02.2025
          else.
            "Controllo correttezza formale

            clear lva_trattam_dati.
            select single trattam_dati
              into lva_trattam_dati
              from ymcms_arc_tra_dt
              where trattam_dati = <ls_inf_subfor>-trattam_dati.

            if sy-subrc <> 0.
              "Errore

              call function 'MESSAGE_STORE'
                exporting
                  arbgb                  = 'AD'
                  msgty                  = 'E'
                  msgv1                  = 'Trattamento dati personali subfornitore:'
                  msgv2                  = 'valore inserito non ammesso'
                  txtnr                  = '010'
                exceptions
                  message_type_not_valid = 1
                  not_active             = 2
                  others                 = 3.
              rv_error = abap_true.

            endif.

* fine mod DF 19.02.2025
          endif.

          if <ls_inf_subfor>-selopt_pae_e[] is initial.
            append 'Paese erogazione servizi subfornitore' to lt_empty_fields.
* inizio mod DF 19.02.2025
          else.

            clear lva_land1.
            select single land1
              into lva_land1
              from t005
              where land1 in <ls_inf_subfor>-selopt_pae_e[].

            if sy-subrc <> 0.
              "Errore

              call function 'MESSAGE_STORE'
                exporting
                  arbgb                  = 'AD'
                  msgty                  = 'E'
                  msgv1                  = 'Paese erogazione servizi subfornitore: '
                  msgv2                  = 'valori inseriti non corretti'
                  txtnr                  = '010'
                exceptions
                  message_type_not_valid = 1
                  not_active             = 2
                  others                 = 3.
              rv_error = abap_true.

            endif.
* fine mod DF 19.02.2025
          endif.

          if <ls_inf_subfor>-selopt_pae_m[] is initial.
            append 'Paese memorizzazione servizi subfornitore' to lt_empty_fields.
* inizio mod DF 19.02.2025
          else.

            clear lva_land1.
            select single land1
              into lva_land1
              from t005
              where land1 in <ls_inf_subfor>-selopt_pae_e[].

            if sy-subrc <> 0.
              "Errore

              call function 'MESSAGE_STORE'
                exporting
                  arbgb                  = 'AD'
                  msgty                  = 'E'
                  msgv1                  = 'Paese memorizzazione servizi subfornitore: '
                  msgv2                  = 'valori inseriti non corretti'
                  txtnr                  = '010'
                exceptions
                  message_type_not_valid = 1
                  not_active             = 2
                  others                 = 3.
              rv_error = abap_true.

            endif.
* fine mod DF 19.02.2025
          endif.

        endif.

      endloop.

    endif."Mod DF 19.02.2025

* inizio mod DF 19.02.2025
    if gs_screen_fields-capogruppo is not initial.
      "Controllo che il valore inserito esista nelle anagrafiche dei sub-fornitori / capogruppi

      go_arch_mand->check_anag_subfor(
        exporting i_subfor = gs_screen_fields-capogruppo
                  i_fname = 'Capogruppo fornitore'
        importing e_check = rv_error ).

    endif.

    if gs_screen_fields-paese_mem_dati is not initial.
      "Controllo che Paese memorizzazione dati fornitore sia corretto

      clear lva_land1.
      select single land1
        into lva_land1
        from t005
        where land1 = gs_screen_fields-paese_mem_dati.

      if sy-subrc <> 0.
        "Errore

        call function 'MESSAGE_STORE'
          exporting
            arbgb                  = 'AD'
            msgty                  = 'E'
            msgv1                  = 'Paese memorizzazione dati fornitore:'
            msgv2                  = 'valore'
            msgv3                  = gs_screen_fields-paese_mem_dati
            msgv4                  = 'non è un codice paese corretto.'
            txtnr                  = '010'
          exceptions
            message_type_not_valid = 1
            not_active             = 2
            others                 = 3.
        rv_error = abap_true.

      endif.

    endif.

    if gs_screen_fields-paese_erog_dati is not initial.
      "Controllo che Paese erogazione dati fornitore sia corretto

      go_arch_mand->check_land1(
        exporting i_land1 = gs_screen_fields-paese_erog_dati
                  i_fname = 'Paese erogazione dati fornitore'
        importing e_check = rv_error ).

    endif.

    if gs_screen_fields-provider_cloud is not initial.
      "Controllo che il provider cloud sia corretto

      go_arch_mand->check_anag_subfor(
        exporting i_subfor = gs_screen_fields-provider_cloud
                  i_fname = 'Provider Cloud'
        importing e_check = rv_error ).

    endif.


    if gs_screen_fields-paese_legge is not initial.
      "Controllo esistenza codice paese

      go_arch_mand->check_land1(
        exporting i_land1 = gs_screen_fields-paese_legge
                  i_fname = 'Paese della legge applicabile al contratto'
        importing e_check = rv_error ).

    endif.

* fine mod DF 19.02.2025

    sort lt_empty_fields .
    delete adjacent duplicates from lt_empty_fields comparing table_line.

    loop at lt_empty_fields assigning field-symbol(<ls_empty_field>).
      data(lv_msg) = conv sy-msgv1( <ls_empty_field> ).
      message_store(  lv_msg ).
    endloop.

    message_init_show( 'S' ).
    if lt_empty_fields is not initial.
      rv_error = abap_true.
    endif.

  endmethod.

  method check_anag_subfor.

    data: lva_subfor type ymcms_subfor,
          lva_msgv1  type char50.

    clear lva_subfor.

    select single subfor
      into lva_subfor
      from ymcms_arc_subfo
      where subfor = i_subfor.

    if sy-subrc <> 0.
      "Errore

      concatenate i_fname
                  ': valore'
                  into lva_msgv1.

      call function 'MESSAGE_STORE'
        exporting
          arbgb                  = 'AD'
          msgty                  = 'E'
          msgv1                  = lva_msgv1
          msgv2                  = i_subfor
          msgv3                  = 'non è una anagrafica corretta'
          txtnr                  = '010'
        exceptions
          message_type_not_valid = 1
          not_active             = 2
          others                 = 3.

      move abap_true to e_check.

    endif.

  endmethod.

  method check_bukrs.

    data: lva_bukrs type t001-bukrs,
          lva_msgv1 type string.

    clear lva_bukrs.

    select single bukrs
      into lva_bukrs
      from t001
      where bukrs = i_bukrs.

    if sy-subrc <> 0.

      concatenate i_fname
                  ': valore'
                  into lva_msgv1.

      call function 'MESSAGE_STORE'
        exporting
          arbgb                  = 'AD'
          msgty                  = 'E'
          msgv1                  = lva_msgv1
          msgv2                  = gs_screen_fields-firmatario
          msgv3                  = 'non è corretto.'
          txtnr                  = '010'
        exceptions
          message_type_not_valid = 1
          not_active             = 2
          others                 = 3.

      move abap_true to e_check.

    endif.

  endmethod.

  method check_land1.

    data: lva_land1 type t005-land1,
          lva_msgv1 type string.

    clear lva_land1.

    select single land1
      into lva_land1
      from t005
      where land1 = i_land1.

    if sy-subrc <> 0.

      concatenate i_fname
                  ': valore'
                  into lva_msgv1.

      call function 'MESSAGE_STORE'
        exporting
          arbgb                  = 'AD'
          msgty                  = 'E'
          msgv1                  = lva_msgv1
          msgv2                  = i_land1
          msgv3                  = 'non è un codice paese corretto.'
          txtnr                  = '010'
        exceptions
          message_type_not_valid = 1
          not_active             = 2
          others                 = 3.

      move abap_true to e_check.

    endif.

  endmethod.

  method set_obligatory_fields_dora.

    data lv_buksr type bkpf-bukrs.

    select single e~bukrs
    from ekko as e
    join ymcms_arc_regsoc as r on r~bukrs = e~bukrs
    inner join ymcms_arc_id_reg as i on i~id_reg = r~id_reg
    into @lv_buksr
    where i~tipo_reg = 'DO'
    and e~ebeln = @p_mand.

    if sy-subrc = 0.

      select single e~matkl
      from ekpo as e
      inner join ymcms_arc_grp_mr as m on m~matkl = e~matkl
      inner join ymcms_arc_id_reg as r on r~id_reg = m~id_reg
      into @data(lv_matkl)
      where r~tipo_reg = 'DO'
      and e~ebeln = @p_mand.

      if sy-subrc = 0.
        cv_dora = abap_true.
      endif.

    endif.


  endmethod.

  method set_obligatory_fields_bankit.

    data lv_buksr type bkpf-bukrs.

    select single e~bukrs
    from ekko as e
    join ymcms_arc_regsoc as r on r~bukrs = e~bukrs
    inner join ymcms_arc_id_reg as i on i~id_reg = r~id_reg
    into @lv_buksr
    where i~tipo_reg = 'BI'
    and e~ebeln = @p_mand.

    if sy-subrc = 0.

      select single e~matkl
      from ekpo as e
      inner join ymcms_arc_grp_mr as m on m~matkl = e~matkl
      inner join ymcms_arc_id_reg as r on r~id_reg = m~id_reg
      into @data(lv_matkl)
      where r~tipo_reg = 'BI'
      and e~ebeln = @p_mand.

      if sy-subrc = 0.
        cv_bankit = abap_true.
      endif.

    endif.

  endmethod.

  method check_duplicates.

    types : begin of ty_fornitore,
              lifnr type char100,
            end of ty_fornitore.

    data lt_fornitore type sorted table of ty_fornitore with unique key lifnr.

    field-symbols: <lt_table>     type standard table.

    case iv_tablename.
      when 'MT_INF_SUBFOR'.
        data(lv_key_field) = 'SUBFORNITORE'.
      when 'MT_FORNALT'.
        lv_key_field = 'FORN_ALT'.
    endcase.

    assign (iv_tablename) to <lt_table>.

    if sy-subrc <> 0.
      return.
    endif.

    loop at <lt_table> assigning field-symbol(<ls_row>) .
      assign component lv_key_field of structure <ls_row> to field-symbol(<lv_lifnr>).

      read table lt_fornitore transporting no fields with table key lifnr = <lv_lifnr>.
      if sy-subrc = 0.

        call function 'MESSAGE_STORE'
          exporting
            arbgb                  = 'R11'
            msgty                  = 'E'
            msgv1                  = | Il campo chiave { lv_key_field } |
            msgv2                  = | con valore '{ <lv_lifnr> }' |
            msgv3                  = 'nella tabella '
            msgv4                  = iv_tablename
            txtnr                  = '343'
          exceptions
            message_type_not_valid = 1
            not_active             = 2
            others                 = 3.

        continue.
      endif.

      insert value #( lifnr = <lv_lifnr> ) into table lt_fornitore.

    endloop.

  endmethod.


  method display_info.

    data: lt_lines   type standard table of tline,
          lt_texttab type standard table of swastrtab,
          lt_txtnote type standard table of txw_note,
          lv_string  type string,
          lv_name    type thead-tdname.

    read table mt_arc_info assigning field-symbol(<ls_arch_info>) with key fieldname = iv_fieldname.

    if sy-subrc = 0.
      move <ls_arch_info>-tdname to lv_name.
    else.
      message 'Descrizione non esiste per questo campo' type 'I' display like 'E'.
      return.
    endif.

    call function 'READ_TEXT'
      exporting
        client                  = sy-mandt
        id                      = 'ST'
        language                = sy-langu
        name                    = lv_name
        object                  = 'TEXT'
      tables
        lines                   = lt_lines
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      return.
    endif.

    loop at lt_lines assigning field-symbol(<ls_lines>).
      concatenate lv_string
*                  <ls_lines>                               "Mod DF 18.02.2025
                  <ls_lines>-tdline                         "Mod DF 18.02.2025
                  into lv_string separated by space.
    endloop.

    call function 'SWA_STRING_SPLIT'
      exporting
        input_string                 = lv_string
        max_component_length         = 72
      tables
        string_components            = lt_texttab
      exceptions
        max_component_length_invalid = 1
        others                       = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      return.
    endif.

    loop at lt_texttab assigning field-symbol(<ls_row>).
      data(ls_txtnote) = value txw_note( line = <ls_row>-str ).
      append ls_txtnote to lt_txtnote.
    endloop.

    call function 'TXW_TEXTNOTE_EDIT'
      exporting
        edit_mode = ' '
      tables
        t_txwnote = lt_txtnote.
  endmethod.


  method display_inf_subfor.

    data: lt_outtab type ref to data.
    data: lt_toolbar_escl type ui_functions."Mod DF 21.02.2025

    data(lt_fcat) = value lvc_t_fcat(
     ( fieldname = 'SUBFORNITORE'        ref_table = 'YMCMS_ARC_SUBF'   outputlen = 10 key = abap_true )
     ( fieldname = 'RAG_SOC2_SUBF'       ref_table = 'YMCMS_ARC_SUBFO'  outputlen = 40 scrtext_s = 'Rag.Soc.Subf'  scrtext_m = 'Rag.Soc. Subfornitore' scrtext_l = 'Ragione sociale Subfornitore' )
     ( fieldname = 'RANK'                ref_table = 'YMCMS_ARC_SUBF'   outputlen = 5  scrtext_s = 'RANK'          scrtext_m = 'RANK'                  scrtext_l = 'RANK')
     ( fieldname = 'CAPOGRUPPO_SUBFOR'   ref_table = 'YMCMS_ARC_SUBF'   outputlen = 10 f4availabl = abap_true      scrtext_s = 'Capogruppo'            scrtext_m = 'Capogruppo'                     scrtext_l = 'Capogruppo' )
     ( fieldname = 'RAG_SOC2_CAPG'       ref_table = 'YMCMS_ARC_SUBFO'  outputlen = 40 scrtext_s = 'Rag.Soc.Capg'  scrtext_m = 'Rag.Soc. Capogruppo'   scrtext_l = 'Ragione sociale Capogruppo' )
     ( fieldname = 'TRATTAM_DATI'        ref_table = 'YMCMS_ARC_SUBF'   outputlen = 5  f4availabl = abap_true )
     ( fieldname = 'DESCR_TRAT_DATI'     ref_table = 'YMCMS_ARC_TRA_DT' outputlen = 30 scrtext_s = 'Descr.Trat.Dt' scrtext_m = 'Descr.Tratt.Dati'      scrtext_l = 'Descrizione trattamento dati' ) ).

    mo_grid->set_ready_for_input( ).

    data(ls_layout)  = value lvc_s_layo( sel_mode    = 'M' no_rowmark  = '' stylefname  = 'STYLE' ).
    data(ls_variant) = value disvariant( report = sy-repid  username = sy-uname ).

    set handler : handle_toolbar
                  handle_user_command
                  subforn_on_data_changed
                  for mo_grid.

* inizio mod DF 21.02.2025
* Escludi pulsanti standard di inserimento / cancellazione riga
    toolbar_excluding_sufor(
      changing ch_toolbar = lt_toolbar_escl ).
* fine mod DF 21.02.2025

    mo_grid->set_table_for_first_display(
       exporting
         i_save               = 'X'
         is_layout            = ls_layout
         it_toolbar_excluding	= lt_toolbar_escl "Mod DF 21.02.2025
         is_variant           = ls_variant
       changing
         it_fieldcatalog      = lt_fcat
         it_outtab            = mt_inf_subfor ).

  endmethod.

  method toolbar_excluding_sufor.

    data: ls_toolbar type line of ui_functions.

    move mo_grid->mc_fc_loc_append_row to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_copy to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_copy_row to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_cut to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_delete_row to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_insert_row to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_move_row to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_paste to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_paste_new_row to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

    move mo_grid->mc_fc_loc_undo to ls_toolbar.
    append ls_toolbar to ch_toolbar.
    clear ls_toolbar.

  endmethod.

  method check_table_changes.

    data: lst_stable type lvc_s_stbl.             "Mod DF 24.02.2025

    move 'X' to lst_stable-row.                   "Mod DF 24.02.2025
    move 'X' to lst_stable-col.                   "Mod DF 24.02.2025

    mo_grid->check_changed_data( ).
    mo_grid_f_alt->check_changed_data( ).
    mo_grid->refresh_table_display(               "Mod DF 24.02.2025
      exporting is_stable = lst_stable ).         "Mod DF 24.02.2025

  endmethod.



  method handle_user_command.

    data: lt_selected_rows type lvc_t_row,
          lv_table         type tabname,
          lv_answ          type char01.   "Mod DF 21.02.2025

    data: ltb_selopt type standard table of selopt with empty key, "Mod DF 21.02.2025
          lst_selopt type selopt.                                  "Mod DF 21.02.2025

    gv_ucomm = e_ucomm.

    mo_grid->get_selected_rows(
      importing
        et_index_rows = lt_selected_rows ).

    case e_ucomm.
      when 'FC_PMD'.

        if lines( lt_selected_rows ) <> 1.
          message 'Seleziona un subfornitore per continuare' type 'I' display like 'E'.
          return.
        endif.

        data(lv_selected_row) = lt_selected_rows[ 1 ].
        data(lv_index) = lv_selected_row-index.

        clear: s_pmd.                                 "Mod DF 18.02.2025
        refresh: s_pmd.                               "Mod DF 18.02.2025

        read table mt_inf_subfor assigning field-symbol(<ls_inf_subfor>) index lv_index.

        loop at <ls_inf_subfor>-selopt_pae_m assigning field-symbol(<ls_selopt_m>).
          append value #( option =  <ls_selopt_m>-option
                          sign   =  <ls_selopt_m>-sign
                          low    =  <ls_selopt_m>-low
                          high   =  <ls_selopt_m>-high ) to s_pmd.
        endloop.

        call screen 108 starting at 10 10 ending at 130 17.

      when 'FC_PES'.

        if lines( lt_selected_rows ) <> 1.
          message 'Seleziona un subfornitore per continuare' type 'I' display like 'E'.
          return.
        endif.

        lv_selected_row = lt_selected_rows[ 1 ].
        lv_index = lv_selected_row-index.

        clear: s_pmd.                                 "Mod DF 18.02.2025
        refresh: s_pmd.                               "Mod DF 18.02.2025

        read table mt_inf_subfor assigning <ls_inf_subfor> index lv_index.

        loop at <ls_inf_subfor>-selopt_pae_e assigning field-symbol(<ls_selopt_e>).
          append value #( option =  <ls_selopt_e>-option
                          sign   =  <ls_selopt_e>-sign
                          low    =  <ls_selopt_e>-low
                          high   =  <ls_selopt_e>-high ) to s_pes.
        endloop.

        call screen 108 starting at 10 10 ending at 130 17.

* inizio mod DF 21.02.2025
      when 'FC_INS'.
        "Inserimento riga subfornitore

        clear: ms_inf_subfor, ms_paesi_subfor.

        call screen 110 starting at 10 10.

        if ms_inf_subfor is not initial.
          "Inserimento riga ed aggiornamento output

          clear: lst_selopt.
          refresh: ltb_selopt.

          if ms_paesi_subfor-paese_mem_dati is not initial.
            "Caso paese memorizzazione dei dati valorizzato
            move 'I'                            to lst_selopt-sign.
            move 'EQ'                           to lst_selopt-option.
            move ms_paesi_subfor-paese_mem_dati to lst_selopt-low.
            append lst_selopt                   to ltb_selopt.
            move ltb_selopt[]                   to ms_inf_subfor-selopt_pae_m[].
          endif.

          clear: lst_selopt.
          refresh: ltb_selopt.

          if ms_paesi_subfor-paese_erog_dati is not initial.
            "Caso paese memorizzazione dei dati valorizzato
            move 'I'                             to lst_selopt-sign.
            move 'EQ'                            to lst_selopt-option.
            move ms_paesi_subfor-paese_erog_dati to lst_selopt-low.
            append lst_selopt                    to ltb_selopt.
            move ltb_selopt[]                    to ms_inf_subfor-selopt_pae_e[].
          endif.

          append ms_inf_subfor to mt_inf_subfor.
          clear ms_inf_subfor.

          check_table_changes( ).
          message 'Riga inserita con successo' type 'S'.

        endif.

      when 'FC_DEL'.
        "Cancellazione riga subfornitore

        if lines( lt_selected_rows ) <> 1.
          message 'Seleziona un subfornitore per continuare' type 'I' display like 'E'.
          return.
        endif.

        clear: lv_answ, lv_index.

        lv_selected_row = lt_selected_rows[ 1 ].
        lv_index = lv_selected_row-index.

        pop_up_info(
         exporting iv_title  = 'ATTENZIONE'
                   iv_text   = 'Si è richiesto di cancellare una riga. Continuare?'
         changing  cv_answer = lv_answ ).

        if lv_answ = '1'.
          "Risposta OK alla cancellazione

          delete mt_inf_subfor index lv_index.
          check_table_changes( ).
          message 'Riga cancellata con successo' type 'I' display like 'S'.

        endif.

* fine mod DF 21.02.2025
    endcase.
  endmethod.

  method handle_toolbar.

    e_object->mt_toolbar = value #( base e_object->mt_toolbar
    ( butn_type = 3 )
    ( function = 'FC_INS' text = 'Inserisci'                     icon = icon_create    quickinfo  = 'Inserisci Subfornitore') "Mod DF 21.02.2025
    ( function = 'FC_DEL' text = 'Cancella'                      icon = icon_delete    quickinfo  = 'Cancella Subfornitore')  "Mod DF 21.02.2025
    ( function = 'FC_PMD' text = 'Paese memorizzazione dei dati' icon = icon_bw_apd_db quickinfo  = 'Paese memorizzazione dei dati')
    ( function = 'FC_PES' text = 'Paese erogazione servizi'      icon = icon_delivery  quickinfo  = 'Paese erogazione servizi')
    ).

  endmethod.    "handle_toolbar

  method start_container.

    data: lo_cont       type ref to cl_gui_custom_container,
          lo_cont_f_alt type ref to cl_gui_custom_container.

    if mo_grid is not bound.

      create object lo_cont
        exporting
          container_name              = 'CUST_CONT107'
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

    endif.

    if mo_grid_f_alt is not bound.

      create object lo_cont_f_alt
        exporting
          container_name              = 'CUST_CONT106'
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

      create object mo_grid_f_alt
        exporting
          i_parent          = lo_cont_f_alt
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

    endif.

  endmethod.


  method display_for_alt.

    data: lt_outtab type ref to data.
    data(lt_fcat) = value lvc_t_fcat( ( fieldname = 'FORN_ALT'  ref_table = 'YMCMS_ARC_F_ALT' ) ).

    mo_grid_f_alt->set_ready_for_input( ).

    data(ls_layout)  = value lvc_s_layo( sel_mode    = 'A' no_rowmark  = '' stylefname  = 'STYLE' edit = 0 ).
    data(ls_variant) = value disvariant( report = sy-repid  username = sy-uname ).

    set handler : fornalt_on_data_changed
                  for mo_grid_f_alt.

    mo_grid_f_alt->set_table_for_first_display(
       exporting
         i_save             = 'X'
         is_layout          = ls_layout
         is_variant         = ls_variant
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_fornalt ).

  endmethod.


  method pop_up_info.

    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = iv_title
        text_question         = iv_text
        text_button_1         = 'SI'
        text_button_2         = 'NO'
        default_button        = '1'
        display_cancel_button = ' '
      importing
        answer                = cv_answer
      exceptions
        text_not_found        = 1
        others                = 2.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endmethod.

  method  populate_vrm_values.

    data: lt_data  type standard table of string,
          lt_list  type vrm_values,
          ls_value type vrm_value,
          lt_ref   type ref to data,
          lo_descr type ref to cl_abap_structdescr,
          lo_data  type ref to cl_abap_datadescr.

    field-symbols: <lt_data> type standard table,
                   <ls_data> type any,
                   <lv_key>  type any,
                   <lv_text> type any.

    create data lt_ref type table of (iv_table_name).
    assign lt_ref->* to <lt_data>.

    select * from (iv_table_name) into table <lt_data>.

    loop at <lt_data> assigning <ls_data>.
      assign component iv_key_field of structure <ls_data> to <lv_key>.
      assign component iv_text_field of structure <ls_data> to <lv_text>.

      if sy-subrc = 0.
        ls_value = value vrm_value( key = <lv_key> text = <lv_text> ).
        append ls_value to lt_list.
      endif.
    endloop.

    call function 'VRM_SET_VALUES'
      exporting
        id     = iv_vrm_id
        values = lt_list.

  endmethod.


endclass.

at selection-screen output.

  if g_tabstrip109-pressed_tab = 'TABSTRIP109_FC107'.
    case gv_ucomm.
      when 'FC_PMD'.
        gv_string = 'S_PMD'.
      when 'FC_PES'.
        gv_string = 'S_PES'.
    endcase.

    loop at screen.
      if ( screen-name ns gv_string ) and ( screen-name ns '%B%_F001' ).
        screen-active = 0.
        modify screen.
      endif.
    endloop.
  endif.

at selection-screen.
  if sscrfields-ucomm eq space.
    sscrfields-ucomm = 'ONLI'.
  endif.

start-of-selection.
  create object go_arch_mand.
  go_arch_mand->execute( ).
