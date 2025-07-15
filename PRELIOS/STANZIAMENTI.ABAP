*&---------------------------------------------------------------------*
*& Report  YFCMS_STANZIAMENTI_V2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report yfcms_stanziamenti_v2.

tables: bkpf.

data: begin of gs_screen101,
        ok_code type sy-ucomm,
      end of gs_screen101.

"Selezioni generali
selection-screen begin of block b1 with frame title text-001.
parameters: p_integ  type bukrs default '0017' obligatory,    "Società Prelios Integra
            p_fondo  type bukrs obligatory,                   "Società Fondo SGR
            p_dt_reg type budat obligatory.                   "Data registrazione stanziamenti
select-options: s_blart for bkpf-blart.                       "Tipo doc. FI
parameters: c_test   as checkbox,                             "Lancio TEST
            c_an_com as checkbox.                             "Visualizza analisi completa

selection-screen end of block b1.

"Blocco Dati stanziamento passivo - Fondo SGR
selection-screen begin of block b2 with frame title text-002.
parameters: c_stan_p as checkbox     default 'X',             "Stanziamento passivo
            p_tdoc_p type bkpf-blart default 'SX',            "Tipo documento passivo
            p_td_p_f type bkpf-blart default 'SZ',            "Tipo documento passivo fine esercizio
            p_text_p type bkpf-bktxt default 'STANZIAMENTO',  "Testo testata doc. Passivo
            p_coge_p type bseg-hkont default 'D220500002'.    "Conto fatture da ricevere Fondo SGR
selection-screen end of block b2.

"Blocco Dati stanziamento attivo - Prelios Integra
selection-screen begin of block b3 with frame title text-003.
parameters: c_stan_a as checkbox     default 'X',             "Stanziamento attivo
            p_tdoc_a type bkpf-blart default 'SX',            "Tipo documento attivo
            p_td_a_f type bkpf-blart default 'SZ',            "Tipo documento attivo fine esercizio
            p_text_a type bkpf-bktxt default 'STANZIAMENTO',  "Testo testata doc. Attivo
            p_text_g type bkpf-bktxt default 'GIROCONTO',     "Testo testata giroconto
            p_coge_r type bseg-hkont default 'M240700000',    "Conto Ricavo Prelios Integra
            p_coge_f type bseg-hkont default 'C220600106',    "Costo Fatture da emettere Prelios Integra
            p_cdc_p  type bseg-kostl default 'EXXXXX'.        "Centro di costo Prelios Integra
selection-screen end of block b3.

*----------------------------------------------------------------------*
*       CLASS lcl_calcolo_stanziamenti DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_calcolo_stanziamenti definition final.

  public section.

    methods execute.
    methods constructor.
    class-methods initialization.
    class-methods at_selection_screen returning value(ev_error) type abap_bool.

  private section.

    types: ty_stanziamenti_type type yficms_stanziamento_out_main.

    types: begin of ty_stanziamenti.
             include type ty_stanziamenti_type.
             types: blocked    type abap_bool,
             calculated type abap_bool.
    types: end of ty_stanziamenti.

    types: tt_stanziamenti type table of ty_stanziamenti,
           ty_fatt_conto   type yfcms_fatt_conto,
           tt_fatt_conto   type table of yfcms_fatt_conto,
           ty_xref3        type range of xref3.

    types: begin of ty_fattura,
             ebeln      type ebeln,
             ebelp      type ebelp,
             zekkn      type dzekkn,
             vgabe      type vgabe,
             gjahr      type gjahr,
             belnr      type ekbe-belnr,
             buzei      type buzei,
             bewtp      type bewtp,
             bwart      type bwart,
             budat      type budat,
             dmbtr      type dmbtr,
             shkzg      type shkzg,
             lfgja      type lfgja,
             lfbnr      type lfbnr,
             lfpos      type lfpos,
             txz01      type txz01,
             sakto      type ekkn-sakto,
             prctr      type ekkn-prctr,
             ribaltata  type char1,
             compensata type char1,
             blocked    type char1,
           end of ty_fattura,
           tt_fattura type standard table of ty_fattura.

    types: trg_xref3 type range of bseg-xref3.

    types: begin of ty_doc_fat,
             bukrs   type bkpf-bukrs,
             belnr   type ekbe-belnr,
             gjahr   type gjahr,
             buzei   type buzei,
             shkzg   type shkzg,
             dmbtr   type dmbtr,
             waers   type waers,
             zuonr   type dzuonr,
             hkont   type hkont,
             txt50   type txt50,
             prctr   type prctr,
             ltext   type ltext,
             awtyp   type awtyp,
             awkey   type awkey,
             ebeln   type ebeln,
             ebelp   type ebelp,
             sgtxt   type sgtxt,
             xref3   type xref3,
             blocked type char1,
           end of ty_doc_fat,
           tt_doc_fat type standard table of ty_doc_fat.

    constants: begin of ms_status,
                 fatto  type char1 value 'S',
                 attesa type char1 value 'W',
                 errore type char1 value 'E',
               end of ms_status,
               ca_bwart_102 type ekbe-bwart value '102',
               ca_31129999  type datum value '99991231',
               ca_green     type icon-id value '@08@',
               ca_yellow    type icon-id value '@09@',
               ca_red       type icon-id value '@0A@',
               ca_eur       type bkpf-waers value 'EUR',
               ca_mkpf      type bkpf-awtyp value 'MKPF'.


    data: mt_stanziamenti type tt_stanziamenti,
          mt_stanz_copy   type tt_stanziamenti,
          mt_fattura      type tt_fattura,
          mt_logs         type bapiret2_t,
          mo_grid         type ref to cl_gui_alv_grid,
          mo_top          type ref to cl_dd_document,
          mo_top_cnt      type ref to cl_gui_container,
          mr_msg_type     type range of bapiret2-type,
          mv_fatt         type blart,
          mv_nc           type blart,
          mr_budat        type range of budat,
          mr_prctr        type range of prctr,
          mr_bewtp        type range of bewtp,
          mv_stanz_status type char1 value ms_status-attesa,
          mv_registered   type abap_bool,
          mv_test_mode    type abap_bool.

    data: begin of ms_reg_type,
            giroconto type char9 value 'GIROCONTO',
            s_attivo  type char9 value 'ATTIVO',
            s_passivo type char9 value 'PASSIVO',
          end of ms_reg_type.

    methods estrarre_dati.

    methods start_container.

    methods visualizza_fattura.

    methods registra_stanziamenti.

    methods registrazione_effetivo.

    methods registrazione_test.

    methods contiene_errori returning value(rv_error) type abap_bool.

    methods recupera_prossimo_pacchetto  exporting et_package     type tt_stanziamenti.

    methods eseguire_registrazione  importing iv_type type char9.

    methods controlla_numero_doc    importing ir_xref3       type ty_xref3
                                              iv_bukrs       type bukrs optional
                                              iv_blart1      type blart optional
                                              iv_blart2      type blart optional
                                              iv_budat       type budat optional
                                              ir_budat       type bkk_r_budat optional
                                    exporting ev_belnr_check type belnr_d
                                              ev_xref3       type xref3.

    methods estrarre_entrate_merci  importing it_fat_cont     type tt_fatt_conto.

    methods split_fatture           exporting et_fatt    type tt_fattura
                                    changing  ct_all_doc type tt_fattura.

    methods calculate_res_fat        importing is_bem      type ty_fattura
                                     exporting ev_imp_fatt type bseg-wrbtr
                                     changing  it_fatt     type tt_fattura.

    methods check_fatt_em_rib       importing is_bem       type ty_fattura
                                              it_fatt      type tt_fattura
                                    exporting es_bem_fat   type ty_fattura
                                              ev_fat_ribal type char01.

    methods set_xref3               importing iv_low_1 type bseg-xref3
                                              iv_low_2 type bseg-belnr
                                              iv_low_3 type bseg-gjahr
                                              iv_low_4 type bseg-buzei
                                              iv_high  type bseg-xref3 optional
                                    exporting er_xref3 type trg_xref3.

    methods somma_entrate_merci     changing ct_bem           type tt_fattura.

    methods estratto_societa        returning value(rv_bukrs) type bukrs.

    methods estratto_fatt_nc        importing it_fat_cont     type tt_fatt_conto.

    methods refresh_top_of_page.

    methods message_init_show       importing iv_action  type char4.

    methods get_fcat                returning value(rt_fieldcat)  type lvc_t_fcat.

    methods visualizza_log.

    methods pop_up_info             importing iv_title  type string
                                              iv_text   type string
                                    changing  cv_answer type char01.

    methods message_store           importing iv_arbgb  type smesg-arbgb optional
                                              iv_msgty  type smesg-msgty optional
                                              iv_msg1   type sy-msgv1
                                              iv_msg2   type sy-msgv2
                                              iv_msg3   type sy-msgv3
                                              iv_msg4   type sy-msgv4
                                              iv_txtnr  type numc3    optional
                                              iv_lineno type i.

    methods handle_toolbar      for event toolbar       of cl_gui_alv_grid importing e_object .

    methods top_of_page         for event top_of_page   of cl_gui_alv_grid.

    methods handle_user_command for event user_command  of cl_gui_alv_grid importing e_ucomm .

    methods on_hotspot          for event hotspot_click of cl_gui_alv_grid importing e_column_id
                                                                                     e_row_id.
endclass.                    "

data go_calcolo_stanziamenti type ref to lcl_calcolo_stanziamenti.

include yfcms_stanziamenti_pbo.

include yfcms_stanziamenti_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_calcolo_stanziamenti IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_calcolo_stanziamenti implementation.

  method execute.

    "Estrazione e lavorazione dati
    visualizza_fattura( ).

    if mt_stanziamenti is not initial.
      call screen 0101.
    endif.

  endmethod.                    "execute

  method handle_user_command.

    data: lv_answer type char1.

    case e_ucomm.

      when 'FC_STANZIAMENTI'.

        if mv_registered = abap_true.

          message 'Registrazione già eseguita (vedi apposito log).' type 'I' display like 'E'.

          return.

        endif.

        read table mt_stanziamenti transporting no fields with key id = ca_red.

        if sy-subrc = 0.

          message 'Registrazione non possibile causa presenza errori.' type 'I' display like 'E'.

          return.

        endif.

        if c_stan_a is initial and c_stan_p is initial.

          message 'Errore: non è stato selezionata la registrazione di nessun stanziamento' type 'I' display like 'E'.

          return.

        elseif c_stan_a is initial or c_stan_p is initial.

          if c_stan_a is initial.

            data(lv_text) = exact string('Si è deciso di registrare i soli stanziamenti passivi. Continuare?').

          else.

            lv_text = 'Si è deciso di registrare i soli stanziamenti attivi. Continuare?'.

          endif.

          pop_up_info(
            exporting
              iv_title  = 'ATTENZIONE'
              iv_text   = lv_text
            changing
              cv_answer = lv_answer ).

          if lv_answer = '2'.

            message 'Registrazione stanziamenti annullata dall’utente' type 'S'.

            return.

          endif.
        endif.

        clear mt_logs.

        registra_stanziamenti( ).

        refresh_top_of_page( ).

      when 'FC_LOGS'.

        if mt_logs is initial.

          message 'Nessun Log da riportare' type 'I'.

          return.

        endif.

        visualizza_log( ).

    endcase.

  endmethod.                    "handle_user_command

  method registra_stanziamenti.

    registrazione_test( ).

    if contiene_errori( ) = abap_true.

      message 'Attenzione: sussistono errori bloccanti (vedi LOG dedicato)' type 'I' display like 'E'.

      mv_stanz_status = ms_status-errore.

      return.

    endif.

    if c_test is initial.

      registrazione_effetivo( ).

      if contiene_errori( ) = abap_false.

        message 'Registrazioni completate con successo (vedi LOG per dettagli)' type 'I'.

      endif.

    endif.

  endmethod.                    "registra_stanziamenti

  method registrazione_test.

    mv_test_mode = abap_true.

    if c_stan_a is not initial.

      eseguire_registrazione( ms_reg_type-s_attivo ).

      eseguire_registrazione( ms_reg_type-giroconto ).

    endif.

    if c_stan_p is not initial.

      eseguire_registrazione( ms_reg_type-s_passivo ).

    endif.

  endmethod.                    "registrazione_test

  method registrazione_effetivo.

    clear mv_test_mode.

    if c_stan_a is not initial and contiene_errori( ) = abap_false.

      eseguire_registrazione( ms_reg_type-s_attivo ).

    endif.

    if c_stan_a is not initial and contiene_errori( ) = abap_false.

      eseguire_registrazione( ms_reg_type-giroconto ).

    endif.

    if c_stan_p is not initial and contiene_errori( ) = abap_false.

      eseguire_registrazione( ms_reg_type-s_passivo ).

    endif.

  endmethod.                    "registrazione_effetivo

  method contiene_errori.

    loop at mt_logs assigning field-symbol(<ls_logs>) where type in mr_msg_type.

      rv_error = abap_true.

      exit.

    endloop.

  endmethod.                    "contiene_errori

  method eseguire_registrazione.

    data: lt_package        type tt_stanziamenti,
          ls_documentheader type bapiache09,
          lt_accountgl      type bapiacgl09_tab,
          lt_currencyamount type bapiaccr09_tab,
          lt_realestate     type bapiacre09_tab,
          lt_return         type bapiret2_t,
          lv_gjahr          type gjahr,
          lv_monat          type monat,
          lv_poper          type poper,
          lv_itemnr         type posnr_acc,
          lv_bukrs          type bukrs,
          lv_rcomp          type rcomp_d,
          lv_tot_ricavo     type wrbtr,
          lv_ittext         type sgtxt,
          lv_refkey3        type xref3,
          lv_alloc_nr       type acpi_zuonr,
          lv_message        type sy-msgv1,
          lv_obj_type       type  bapiache09-obj_type,
          lv_obj_key        type  bapiache09-obj_key,
          lv_obj_sys        type  bapiache09-obj_sys.

    read table mt_stanziamenti transporting no fields with key calculated = abap_false.

    if sy-subrc <> 0.

      modify mt_stanziamenti from value #( calculated = abap_false )
      transporting calculated where calculated = abap_true.

      return.

    endif.

    recupera_prossimo_pacchetto(
      importing
        et_package = lt_package ).

    select single rcomp
      from t001
      into lv_rcomp
      where bukrs = p_integ.

    clear lv_bukrs.
    "Costruzione della società su cui scrivere gli stanziamenti
    "Se la data registazione è il 31/12/AAAA allora la è Prelios Integra (soc. in selezione)
    "Se la data registrazione è diversa dal 31/12/AAAA è la società di stanziamento
    if p_dt_reg+4(4) = '1231'.
      move p_integ to lv_bukrs.
    else.
      lv_bukrs = estratto_societa( ).
    endif.

    if lv_bukrs is initial.

      modify mt_stanziamenti from value #( calculated = abap_false )
      transporting calculated where calculated = abap_true.

      return.

    endif.

    case iv_type.
      when ms_reg_type-s_attivo.

        lv_message = 'Stanziamento attivo'.

        data(lv_doctype) = cond #( when p_dt_reg+4(4) = '1231' then p_td_a_f else p_tdoc_a ).

      when ms_reg_type-s_passivo.

        lv_message = 'Stanziamento passivo'.

        lv_doctype = cond #( when p_dt_reg+4(4) = '1231' then p_td_p_f else p_tdoc_p ).

      when ms_reg_type-giroconto.

        lv_message = 'Giroconto Costi/Ricavi'.

        lv_doctype = cond #( when p_dt_reg+4(4) = '1231' then p_td_a_f else p_tdoc_a ).

    endcase.

    call function 'FI_PERIOD_DETERMINE'
      exporting
        i_budat        = p_dt_reg
        i_bukrs        = lv_bukrs
      importing
        e_gjahr        = lv_gjahr
        e_monat        = lv_monat
        e_poper        = lv_poper
      exceptions
        fiscal_year    = 1
        period         = 2
        period_version = 3
        posting_period = 4
        special_period = 5
        version        = 6
        posting_date   = 7
        others         = 8.

    if sy-subrc <> 0.
      lv_gjahr = p_dt_reg(4).
      lv_monat = p_dt_reg+4(2).
      lv_poper = p_dt_reg+4(2).
    endif.


    ls_documentheader = value #( bus_act    = 'RFBU'
                                 username   = sy-uname
                                 header_txt = cond #( when iv_type = ms_reg_type-s_attivo  then p_text_a
                                                      when iv_type = ms_reg_type-s_passivo then p_text_p else p_text_g )
                                 ref_doc_no = lv_rcomp
                                 comp_code  = lv_bukrs
                                 doc_date   = p_dt_reg
                                 pstng_date = p_dt_reg
                                 fisc_year  = lv_gjahr
                                 fis_period = cond #( when p_dt_reg+4(4) = '1231' then '13' else lv_monat )
                                 doc_type   = lv_doctype ).

    clear lv_tot_ricavo.

    loop at lt_package assigning field-symbol(<ls_package>).

      clear: lv_alloc_nr.

      lv_itemnr = lv_itemnr + 1.

      lv_tot_ricavo = lv_tot_ricavo + ( <ls_package>-importo_residuo * -1 ).

      if <ls_package>-ebeln is not initial.
        concatenate <ls_package>-ebeln <ls_package>-txz01 into lv_ittext separated by ' '.

        select  single l~name1, l~name2
          from ekko as k
          inner join lfa1 as l
          on l~lifnr = k~lifnr
          inner join adrc as a
          on a~addrnumber = l~adrnr
          into  @data(ls_alloc)
          where k~ebeln = @<ls_package>-ebeln.

        lv_alloc_nr = ls_alloc-name1 && ls_alloc-name2.

      else.

        lv_ittext = <ls_package>-sgtxt.

        select  single l~name1, l~name2
          from bseg as b
          inner join lfa1 as l
          on l~lifnr = b~lifnr
          inner join adrc as a
          on a~addrnumber = l~adrnr
          into  @ls_alloc
          where b~belnr = @<ls_package>-belnr_fat
          and   b~gjahr = @<ls_package>-gjahr_fat
          and   b~koart = 'K'
          and   b~bukrs = @<ls_package>-bukrs_integ.

        lv_alloc_nr = ls_alloc-name1 && ls_alloc-name2.

      endif.

      if <ls_package>-belnr_fat is not initial.
        concatenate 'EF' <ls_package>-belnr_fat <ls_package>-gjahr_fat <ls_package>-buzei_fat into lv_refkey3 separated by ''.
      else.
        concatenate 'EM' <ls_package>-belnr_em  <ls_package>-gjahr_em  <ls_package>-buzei_em  into lv_refkey3 separated by ''.
      endif.

      append value #( itemno_acc   = lv_itemnr
                      gl_account   = p_coge_r
                      item_text    = lv_ittext
                      ref_key_3    = lv_refkey3
                      doc_type     = lv_doctype
                      comp_code    = lv_bukrs
                      fis_period   = cond #( when p_dt_reg+4(4) = '1231' then '13'     else lv_monat )
                      fisc_year    = lv_gjahr
                      pstng_date   = p_dt_reg
                      alloc_nmbr   = lv_alloc_nr
                      profit_ctr   = cond #( when iv_type = ms_reg_type-giroconto then p_cdc_p
                                             when iv_type = ms_reg_type-s_attivo  then <ls_package>-prctr
                                             else 'E00000' && <ls_package>-prctr+6(4) )
                      trade_id     = lv_rcomp ) to lt_accountgl.

      append value #( itemno_acc   = lv_itemnr
                      curr_type    = '00'
                      currency     = 'EUR'
                      currency_iso = 'EUR'
                      amt_doccur   = cond #( when iv_type = ms_reg_type-s_attivo
                                             then <ls_package>-importo_residuo * -1
                                             else <ls_package>-importo_residuo ) ) to lt_currencyamount.
      if iv_type <> ms_reg_type-giroconto.

        append value #( itemno_acc      = lv_itemnr
                        business_entity = cond #( when iv_type = ms_reg_type-s_attivo then <ls_package>-prctr+2
                                                  else '0000' && <ls_package>-prctr+6(4) ) ) to lt_realestate.
      endif.

      if iv_type = ms_reg_type-giroconto.

        lv_itemnr = lv_itemnr + 1.

        if <ls_package>-ebeln is not initial.
          concatenate 'G/c Ordine' <ls_package>-ebeln '-Posizione' <ls_package>-ebelp  into lv_ittext separated by ' '.
        endif.

        append value #( itemno_acc   = lv_itemnr
                        gl_account   = <ls_package>-hkont_integ
                        item_text    = lv_ittext
                        ref_key_3    = lv_refkey3
                        doc_type     = cond #( when p_dt_reg+4(4) = '1231' then p_td_a_f else p_tdoc_a )
                        comp_code    = lv_bukrs
                        fis_period   = cond #( when p_dt_reg+4(4) = '1231' then '13'     else lv_monat )
                        fisc_year    = lv_gjahr
                        pstng_date   = p_dt_reg
                        alloc_nmbr   = lv_alloc_nr
                        costcenter   = p_cdc_p
                        trade_id     = '999999' ) to lt_accountgl.

        append value #( itemno_acc   = lv_itemnr
                        curr_type    = '00'
                        currency     = 'EUR'
                        currency_iso = 'EUR'
                        amt_doccur   = <ls_package>-importo_residuo * -1 ) to lt_currencyamount.
      endif.

    endloop.

    if iv_type <> ms_reg_type-giroconto.

      lv_itemnr = lv_itemnr + 1.

      lv_ittext =  'Stanziamento al' && ' ' && p_dt_reg+6 && p_dt_reg+4(2) && p_dt_reg(4).

      append value #( itemno_acc   = lv_itemnr
                      gl_account   = cond #( when iv_type = ms_reg_type-s_attivo then p_coge_r else p_coge_p )
                      item_text    = lv_ittext
                      doc_type     = lv_doctype
                      comp_code    = lv_bukrs
                      fis_period   = cond #( when p_dt_reg+4(4) = '1231' then '13'     else lv_monat )
                      fisc_year    = lv_gjahr
                      pstng_date   = p_dt_reg ) to lt_accountgl.

      append value #( itemno_acc   = lv_itemnr
                      curr_type    = '00'
                      currency     = 'EUR'
                      currency_iso = 'EUR'
                      amt_doccur   = lv_tot_ricavo * -1 ) to lt_currencyamount.
    endif.

    if mv_test_mode = abap_true.

      call function 'BAPI_ACC_DOCUMENT_CHECK'
        exporting
          documentheader = ls_documentheader
        tables
          accountgl      = lt_accountgl
          currencyamount = lt_currencyamount
          return         = lt_return
          realestate     = lt_realestate.

      data(lv_msg_nr) = 614.

    else.

      call function 'BAPI_ACC_DOCUMENT_POST'
        exporting
          documentheader = ls_documentheader
        importing
          obj_type       = lv_obj_type
          obj_key        = lv_obj_key
          obj_sys        = lv_obj_sys
        tables
          accountgl      = lt_accountgl
          currencyamount = lt_currencyamount
          return         = lt_return
          realestate     = lt_realestate.

      lv_msg_nr = 605.

    endif.

    if lt_return is initial and mv_test_mode = abap_true.

      append value #( message_v1 = |ERRORE: Generico Check { lv_message }| ) to mt_logs.

      return.

    else.

      sort lt_return by message.
      delete adjacent duplicates from lt_return comparing message.

      read table lt_return transporting no fields with key type = 'S' id = 'RW' number = lv_msg_nr.

      if sy-subrc <> 0.

        call function 'BAPI_TRANSACTION_ROLLBACK'.

        append value #( message_v1 = |ERRORE: registrazione { lv_message }.|
                        message_v2 = 'Di seguito gli errori riscontrati' ) to mt_logs.

        append lines of lt_return to mt_logs.

      elseif  mv_test_mode = abap_false.

        call function 'BAPI_TRANSACTION_COMMIT'.

        concatenate 'n.' lv_obj_key(10) 'del' lv_obj_key+10(4) 'Soc.' lv_obj_key+14(4)
        into data(lv_stanz_codice) separated by ' '.

        append value #( type       = 'S'
                        message_v1 = |OK – Registrazione|
                        message_v2 = lv_message
                        message_v3 = lv_stanz_codice ) to mt_logs.

        mv_stanz_status = ms_status-fatto.
        mv_registered   = abap_true.

      endif.

    endif.

    clear: lt_return, lt_accountgl, ls_documentheader, lt_currencyamount, lt_realestate.

    eseguire_registrazione( iv_type ).

  endmethod.                    "eseguire_registrazione

  method recupera_prossimo_pacchetto.

    data: lv_pckg_rows_nr type i.

    loop at mt_stanziamenti assigning field-symbol(<ls_stanziamenti>) where calculated is initial.

      lv_pckg_rows_nr = lv_pckg_rows_nr + 1.

      append <ls_stanziamenti> to et_package.
      <ls_stanziamenti>-calculated = abap_true.

      if lv_pckg_rows_nr = 450.
        exit.
      endif.

    endloop.

  endmethod.                    "recupera_prossimo_pacchetto

  method estrarre_dati.

    "Tabella della transcodifica dei conti CoGe
    select *
      from yfcms_fatt_conto
      into table @data(lt_fatt_conto)
      where active <> 'X'.

    if sy-subrc <> 0.
      message text-m01 type 'S' display like 'E'.
      leave list-processing.
    endif.

*******************************************
    "Caso estrazione Entrate merci
*******************************************
    estrarre_entrate_merci( lt_fatt_conto ).

*******************************************
    "Caso estrazoine delle fatture
*******************************************
    estratto_fatt_nc( lt_fatt_conto ).

    if c_an_com is initial.
      "Cancellazione record LOG di dettaglio
      delete mt_stanziamenti where analisi is not initial.
    endif.

    if mt_stanziamenti is initial.
      message 'Nessun dato estratto' type 'S' display like 'E'.
      leave list-processing.
    endif.

  endmethod.                    "estrarre_dati

  method  on_hotspot.

    read table mt_stanziamenti assigning field-symbol(<ls_stanziamenti>) index e_row_id-index.

    if sy-subrc <> 0.
      return.
    endif.

    case e_column_id.

      when 'EBELN'.

        if <ls_stanziamenti>-ebeln is initial.
          return.
        endif.

        set parameter id 'BES' field <ls_stanziamenti>-ebeln.

        call transaction 'ME23N'.

      when 'BELNR_EM'.

        if <ls_stanziamenti>-belnr_em is initial.
          return.
        endif.

        data(lv_awkey) = exact awkey( <ls_stanziamenti>-belnr_em && <ls_stanziamenti>-gjahr_em  ).

        select single *
          from bkpf
          into @data(ls_bkpf)
          where bukrs = @<ls_stanziamenti>-bukrs_integ
          and   awtyp = 'MKPF'
          and   awkey = @lv_awkey.

        if sy-subrc <> 0.
          return.
        endif.

        set parameter id: 'BUK' field ls_bkpf-bukrs,
                          'BLN' field ls_bkpf-belnr,
                          'GJR' field ls_bkpf-gjahr.

        call transaction 'FB03' and skip first screen.

      when 'BELNR_FAT'.

        if <ls_stanziamenti>-belnr_fat is initial.
          return.
        endif.

        if <ls_stanziamenti>-awtyp = 'BKPF'.

          set parameter id: 'BUK' field p_integ,
                            'BLN' field <ls_stanziamenti>-belnr_fat,
                            'GJR' field <ls_stanziamenti>-gjahr_fat.

          call transaction 'FB03' and skip first screen.
        else.

          lv_awkey = exact awkey( <ls_stanziamenti>-belnr_fat && <ls_stanziamenti>-gjahr_fat ).

          select single *
            from bkpf
            into @ls_bkpf
            where bukrs = @<ls_stanziamenti>-bukrs_integ
            and   awtyp = 'RMRP'
            and   awkey = @lv_awkey.

          if sy-subrc <> 0.
            return.
          endif.

          set parameter id: 'BUK' field ls_bkpf-bukrs,
                            'BLN' field ls_bkpf-belnr,
                            'GJR' field ls_bkpf-gjahr.

          call transaction 'FB03' and skip first screen.

        endif.

    endcase.

  endmethod.                    "on_hotspot

  method estratto_fatt_nc.

    data: lt_doc_fatt    type tt_doc_fat,
          lv_hkont       type bseg-hkont,
          lv_prctr       type ekkn-prctr,
          lv_check_stanz type bkpf-belnr,
          lv_bukrs       type bkpf-bukrs,
          lv_belnr_check type bkpf-belnr,
          lr_xref3       type range of xref3,
          lv_txt50_fondo type skat-txt50,
          lv_xwetext     type vibdbe-xwetext,
          lv_txz01       type ekpo-txz01,
          lv_swenr       type vibdbe-swenr,
          lv_xref3_full  type bseg-xref3,
          lv_icon        type icon-id,
          lv_casistica   type bseg-sgtxt,
          lv_msg         type char100,
          lv_ribaltata   type char01,
          lv_conpensata  type char01,
          lr_budat       type range of budat,
          ls_budat       like line of lr_budat,
          ls_fat_cont    type ty_fatt_conto,
          rg_belnr       type range of bkpf-belnr,
          rg_gjahr       type range of bkpf-gjahr.

    "Piano dei conti Prelios Integra
    select single ktopl
      into @data(lv_ktopl)
      from t001
     where bukrs = @p_integ.

    refresh: rg_belnr, rg_gjahr.

*************************************************
****** Estrazione Fattur e note di credito ******
*************************************************
    select  b~bukrs,
            b~belnr,
            b~gjahr,
            s~buzei,
            s~shkzg,
            s~dmbtr,
            b~waers,
            s~zuonr,
            s~hkont,
            t~txt50,
            s~prctr,
            c~ltext,
            b~awtyp,
            b~awkey,
            s~ebeln,
            s~ebelp,
            s~xref3,
            s~sgtxt
      into corresponding fields of table @lt_doc_fatt
      from bkpf as b
      inner join bseg as s
        on b~bukrs = s~bukrs
       and b~belnr = s~belnr
       and b~gjahr = s~gjahr
      left outer join cepct as c
        on c~spras = @sy-langu
       and c~prctr = s~prctr
       and c~datbi = @ca_31129999
       and c~kokrs = s~bukrs
      left outer join skat as t
        on t~spras = @sy-langu
       and t~ktopl = @lv_ktopl
       and t~saknr = s~hkont
      where b~bukrs = @p_integ
        and b~belnr in @rg_belnr
        and b~gjahr in @rg_gjahr
        and b~blart in @s_blart
        and b~budat in @mr_budat
        and s~koart = 'S'
        and s~buzid <> 'T'
        and s~prctr in @mr_prctr.

    if sy-subrc <> 0.
      "Nessuna fatture / NC trovata
      return.
    endif.

    "Ragions sociale Fondo SGR
    select single butxt
      from t001
      into @data(lv_fondo_txt)
      where bukrs = @p_fondo.

    "Ragione sociale Prelios Integra
    select single butxt
      from t001
      into @data(lv_integ_txt)
      where bukrs = @p_integ.

    clear lv_bukrs.
    "Costruzione della società su cui scrivere gli stanziamenti
    "Se la data registazione è il 31/12/AAAA allora la è Prelios Integra (soc. in selezione)
    "Se la data registrazione è diversa dal 31/12/AAAA è la società di stanziamento
    if p_dt_reg+4(4) = '1231'.
      move p_integ to lv_bukrs.
    else.
      lv_bukrs = estratto_societa( ).
    endif.

    if lv_bukrs is initial.
      return.
    endif.

    loop at lt_doc_fatt assigning field-symbol(<ls_doc_fatt>).

      clear: lv_hkont, ls_fat_cont.

      "Transcodifica Conti CoGe
      read table it_fat_cont
        into ls_fat_cont
        with key hkont_terzi = <ls_doc_fatt>-hkont.

      if sy-subrc = 0.
        lv_hkont = ls_fat_cont-hkont_gruppi.
      endif.

      clear lv_txt50_fondo.

      "Descrizione conto Gruppo
      if ls_fat_cont-hkont_gruppi is not initial.

        select single txt50
          from skat
          into  @lv_txt50_fondo
          where saknr = @ls_fat_cont-hkont_gruppi
          and   spras = @sy-langu
          and   ktopl = @lv_ktopl.

      endif.

      clear lv_swenr.
      "valorizzazione unità economica
      move <ls_doc_fatt>-prctr+2(8) to lv_swenr.

      clear lv_xwetext.
      "Testo Unità economica
      select single xwetext
        from vibdbe
        into @lv_xwetext
        where swenr = @lv_swenr
        and   bukrs = @p_integ.

      clear lv_txz01.
      if <ls_doc_fatt>-ebeln is not initial.
        "Estraggo il testo della posizione dell'OdA

        select single txz01
          from ekpo
          into @lv_txz01
          where ebeln = @<ls_doc_fatt>-ebeln
            and ebelp = @<ls_doc_fatt>-ebelp.

      endif.

*      lr_xref3 = value #( ( sign = 'I' option = 'CP'  low  = <ls_doc_fatt>-belnr && <ls_doc_fatt>-gjahr && '*' ) ).

      if <ls_doc_fatt>-awtyp <> 'RMRP'.
*********************************************************
****************** Caso Fattura FI **********************
*********************************************************

        refresh: lr_xref3.
        "Costruzione della chiave di riferimento
        "per controllo fattura ribaltata
        set_xref3( exporting iv_low_1 = ''
                             iv_low_2 = <ls_doc_fatt>-belnr
                             iv_low_3 = <ls_doc_fatt>-gjahr
                             iv_low_4 = '000'
                   importing er_xref3 = lr_xref3 ).

        "Intervallo date per controllo del ribaltamento
        "Escludere ribaltamento successivo allo stanziamento
        clear: ls_budat.
        refresh: lr_budat.

        move 'I' to ls_budat-sign.
        move 'LE' to ls_budat-option.
        move p_dt_reg to ls_budat-low.

        append ls_budat to lr_budat.
        clear ls_budat.

        clear lv_belnr_check.

        "Check fattura ribaltata
        "fino al
        controlla_numero_doc(
        exporting
           ir_xref3 = lr_xref3
           ir_budat = lr_budat
        importing
           ev_belnr_check = lv_belnr_check ).

        if lv_belnr_check is not initial.
          "******************************************************
          "Caso fattura ribaltata in periodo attuale o precedente
          "******************************************************

          clear: lv_casistica, lv_icon, lv_msg.

          if lv_hkont is initial.

          else.
            lv_icon = ca_red.
            lv_casistica = text-m20.
            lv_msg = text-m21.
          endif.

          append value #( id              = lv_icon
                          bukrs_integ     = p_integ
                          butxt_integ     = lv_integ_txt
                          bukrs_fondo     = p_fondo
                          butxt_fondo     = lv_fondo_txt
                          ebeln           = <ls_doc_fatt>-ebeln
                          ebelp           = <ls_doc_fatt>-ebelp
                          belnr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+4(10) )
                          gjahr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full(4) )
                          buzei_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+14(4) )
                          belnr_fat       = <ls_doc_fatt>-belnr
                          gjahr_fat       = <ls_doc_fatt>-gjahr
                          buzei_fat       = <ls_doc_fatt>-buzei
                          casistica       = lv_casistica
                          hkont_integ     = <ls_doc_fatt>-hkont
                          txt50_integ     = <ls_doc_fatt>-txt50
                          hkont_fondo     = lv_hkont
                          txt50_fondo     = cond #( when lv_hkont is not initial then lv_txt50_fondo )
                          importo_residuo = cond #( when <ls_doc_fatt>-shkzg = 'S' then <ls_doc_fatt>-dmbtr else ( <ls_doc_fatt>-dmbtr * -1 ) )
                          waers           = <ls_doc_fatt>-waers
                          prctr           = <ls_doc_fatt>-prctr
                          ltext           = <ls_doc_fatt>-ltext
                          swenr           = lv_swenr
                          xwetext         = lv_xwetext
                          mess            = lv_msg
                          awtyp           = 'BKPF'
*                          txz01           = lv_txz01
                          sgtxt           = <ls_doc_fatt>-sgtxt ) to mt_stanziamenti.

        else.

          refresh: lr_xref3.
          "Costruzione della chiave di riferimento
          "per controllo fattura già stanziata
          set_xref3( exporting iv_low_1 = 'EF'
                               iv_low_2 = <ls_doc_fatt>-belnr
                               iv_low_3 = <ls_doc_fatt>-gjahr
                               iv_low_4 = <ls_doc_fatt>-buzei
                     importing er_xref3 = lr_xref3 ).

          clear: lv_check_stanz, lv_xref3_full.

          "Controllo che la fattura sia stata già stanziata
          controlla_numero_doc(
          exporting
             ir_xref3 = lr_xref3
             iv_bukrs = lv_bukrs
             iv_blart1 = p_td_a_f
             iv_blart2 = p_tdoc_a
*             iv_budat  = p_dt_reg
          importing
             ev_belnr_check = lv_check_stanz
             ev_xref3       = lv_xref3_full ).

          clear: lv_msg, lv_icon, lv_casistica.
          "Gestione Semaforo e messaggio
          if lv_check_stanz is not initial.
            "Fattura stanziata
            lv_icon = ca_red.
            lv_msg = |ATTENZIONE: Fattura precedentemente stanziata (N. Doc. SAP { lv_check_stanz })| .
            lv_casistica = 'FATTURA STANZIATA'.
          else.
            if lv_hkont is initial.
              "Errore di transcofica
              lv_icon = ca_red.
              lv_msg = 'ERRORE: Transcodifica conto non presente in tabella (YFCMS_FATT_CONTO)'.
              lv_casistica = 'ERRORE TRANSCODIFICA'.
            else.
              "OK fattura
              lv_icon = ca_green.
              lv_msg = 'OK - Fattura per cui effettuare lo stanziamento.'.
              lv_casistica = 'FATTURA DA STANZIARE'.
            endif.
          endif.

          append value #( id              = lv_icon
                          bukrs_integ     = p_integ
                          butxt_integ     = lv_integ_txt
                          bukrs_fondo     = p_fondo
                          butxt_fondo     = lv_fondo_txt
                          ebeln           = <ls_doc_fatt>-ebeln
                          ebelp           = <ls_doc_fatt>-ebelp
                          belnr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+4(10) )
                          gjahr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full(4) )
                          buzei_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+14(4) )
                          belnr_fat       = <ls_doc_fatt>-belnr
                          gjahr_fat       = <ls_doc_fatt>-gjahr
                          buzei_fat       = <ls_doc_fatt>-buzei
                          casistica       = lv_casistica
                          hkont_integ     = <ls_doc_fatt>-hkont
                          txt50_integ     = <ls_doc_fatt>-txt50
                          hkont_fondo     = lv_hkont
                          txt50_fondo     = cond #( when lv_hkont is not initial then lv_txt50_fondo )
                          importo_residuo = cond #( when <ls_doc_fatt>-shkzg = 'S' then <ls_doc_fatt>-dmbtr else ( <ls_doc_fatt>-dmbtr * -1 ) )
                          waers           = <ls_doc_fatt>-waers
                          prctr           = <ls_doc_fatt>-prctr
                          ltext           = <ls_doc_fatt>-ltext
                          swenr           = lv_swenr
                          xwetext         = lv_xwetext
                          mess            = lv_msg
                          awtyp           = 'BKPF'
*                          txz01           = lv_txz01
                          sgtxt           = <ls_doc_fatt>-sgtxt ) to mt_stanziamenti.

        endif.

      else.
*********************************************************
****************** Caso Fattura MM **********************
*********************************************************

        clear lv_ribaltata.
        "Controllare che la fattura sia ribaltata
        loop at mt_fattura assigning field-symbol(<ls_fattura>) where vgabe = '2'
                                                                and   bewtp in mr_bewtp
                                                                and   belnr = <ls_doc_fatt>-awkey(10)
                                                                and   gjahr = <ls_doc_fatt>-awkey+10(4)
                                                                and   ribaltata = abap_true.
          lv_ribaltata = abap_true.
          exit.
        endloop.

        if lv_ribaltata = abap_true.
          "*******************************************
          "Caso Fattura ribaltata nel periodo attuale
          "*******************************************

          clear: lv_icon, lv_msg, lv_casistica.

          if lv_hkont is initial.
            "Errore di transcofica
            lv_icon = ca_red.
            lv_msg = 'ERRORE: Transcodifica conto non presente in tabella (YFCMS_FATT_CONTO)'.
            lv_casistica = 'ERRORE TRANSCODIFICA'.
          else.
            "Fattura ribaltata
            lv_icon = ca_red.
            lv_msg = 'FATTURA RIBALTATA nel periodo. Stanziamento non necessario.'.
            lv_casistica = 'FATTURA RIBALTATA'.
          endif.

          append value #( id              = lv_icon
                          bukrs_integ     = p_integ
                          butxt_integ     = lv_integ_txt
                          bukrs_fondo     = p_fondo
                          butxt_fondo     = lv_fondo_txt
                          ebeln           = <ls_doc_fatt>-ebeln
                          ebelp           = <ls_doc_fatt>-ebelp
                          belnr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+4(10) )
                          gjahr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full(4) )
                          buzei_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+14(4) )
                          belnr_fat       = <ls_doc_fatt>-belnr
                          gjahr_fat       = <ls_doc_fatt>-gjahr
                          buzei_fat       = <ls_doc_fatt>-buzei
                          casistica       = lv_casistica
                          hkont_integ     = <ls_doc_fatt>-hkont
                          txt50_integ     = <ls_doc_fatt>-txt50
                          hkont_fondo     = lv_hkont
                          txt50_fondo     = cond #( when lv_hkont is not initial then lv_txt50_fondo )
                          importo_residuo = cond #( when <ls_doc_fatt>-shkzg = 'S' then <ls_doc_fatt>-dmbtr else ( <ls_doc_fatt>-dmbtr * -1 ) )
                          waers           = <ls_doc_fatt>-waers
                          prctr           = <ls_doc_fatt>-prctr
                          ltext           = <ls_doc_fatt>-ltext
                          swenr           = lv_swenr
                          xwetext         = lv_xwetext
                          mess            = lv_msg
                          awtyp           = 'RMRP'
                          analisi         = cond #( when lv_hkont is not initial then abap_true )
                          txz01           = lv_txz01
                          sgtxt           = <ls_doc_fatt>-sgtxt ) to mt_stanziamenti.

        else.

          clear lv_conpensata.
          "Controllare che la fattura sia stata compensata al passo precedente
          loop at mt_fattura assigning <ls_fattura> where vgabe = '2'
                                                    and   bewtp in mr_bewtp
                                                    and   belnr = <ls_doc_fatt>-awkey(10)
                                                    and   gjahr = <ls_doc_fatt>-awkey+10(4)
                                                    and   compensata = abap_true.
            lv_conpensata = abap_true.
            exit.
          endloop.

          if lv_conpensata = abap_true.
            "****************************************************
            "Caso Fattura Compensata con EM quindi da escludere
            "****************************************************

            clear: lv_icon, lv_msg, lv_casistica.

            if lv_hkont is initial.
              "Errore di transcofica
              lv_icon = ca_red.
              lv_msg = 'ERRORE: Transcodifica conto non presente in tabella (YFCMS_FATT_CONTO)'.
              lv_casistica = 'ERRORE TRANSCODIFICA'.
            else.
              "Fattura ribaltata
              lv_icon = ca_red.
              lv_msg = 'FATTURA COMPENSATA con l’entrata merci quindi da non considerare autonomamente.'.
              lv_casistica = 'FATTURA COMPENSATA'.
            endif.

            append value #( id              = lv_icon
                            bukrs_integ     = p_integ
                            butxt_integ     = lv_integ_txt
                            bukrs_fondo     = p_fondo
                            butxt_fondo     = lv_fondo_txt
                            ebeln           = <ls_doc_fatt>-ebeln
                            ebelp           = <ls_doc_fatt>-ebelp
                            belnr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+4(10) )
                            gjahr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full(4) )
                            buzei_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+14(4) )
                            belnr_fat       = <ls_doc_fatt>-belnr
                            gjahr_fat       = <ls_doc_fatt>-gjahr
                            buzei_fat       = <ls_doc_fatt>-buzei
                            casistica       = lv_casistica
                            hkont_integ     = <ls_doc_fatt>-hkont
                            txt50_integ     = <ls_doc_fatt>-txt50
                            hkont_fondo     = lv_hkont
                            txt50_fondo     = cond #( when lv_hkont is not initial then lv_txt50_fondo )
                            importo_residuo = cond #( when <ls_doc_fatt>-shkzg = 'S' then <ls_doc_fatt>-dmbtr else ( <ls_doc_fatt>-dmbtr * -1 ) )
                            waers           = <ls_doc_fatt>-waers
                            prctr           = <ls_doc_fatt>-prctr
                            ltext           = <ls_doc_fatt>-ltext
                            swenr           = lv_swenr
                            xwetext         = lv_xwetext
                            mess            = lv_msg
                            awtyp           = 'RMRP'
                            analisi         = cond #( when lv_hkont is not initial then abap_true )
                            txz01           = lv_txz01
                            sgtxt           = <ls_doc_fatt>-sgtxt ) to mt_stanziamenti.

          else.

            refresh: lr_xref3.
            "Valorizzazione SET chiave
            "riferimento 3 per check già stanziato
            set_xref3( exporting iv_low_1 = 'EF'
                                 iv_low_2 = <ls_doc_fatt>-belnr
                                 iv_low_3 = <ls_doc_fatt>-gjahr
                                 iv_low_4 = <ls_doc_fatt>-buzei
                       importing er_xref3 = lr_xref3 ).

            clear lv_belnr_check.
            "Controllare se è stata stanziata in precedenza
            controlla_numero_doc(
            exporting
               ir_xref3 = lr_xref3
            importing
               ev_belnr_check = lv_belnr_check ).

            if lv_belnr_check is not initial.
              "************************************
              "Documento Stanziato in precedenza
              "************************************

              clear: lv_icon, lv_msg, lv_casistica.

              if lv_hkont is initial.
                "Errore di transcofica
                lv_icon = ca_red.
                lv_msg = 'ERRORE: Transcodifica conto non presente in tabella (YFCMS_FATT_CONTO)'.
                lv_casistica = 'ERRORE TRANSCODIFICA'.
              else.
                "Fattura ribaltata
                lv_icon = ca_red.
                lv_msg = 'ATTENZIONE: la fattura risulta già stanziata.'.
                lv_casistica = 'FATTURA STANZIATA'.
              endif.

              append value #( id              = lv_icon
                              bukrs_integ     = p_integ
                              butxt_integ     = lv_integ_txt
                              bukrs_fondo     = p_fondo
                              butxt_fondo     = lv_fondo_txt
                              ebeln           = <ls_doc_fatt>-ebeln
                              ebelp           = <ls_doc_fatt>-ebelp
                              belnr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+4(10) )
                              gjahr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full(4) )
                              buzei_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+14(4) )
                              belnr_fat       = <ls_doc_fatt>-belnr
                              gjahr_fat       = <ls_doc_fatt>-gjahr
                              buzei_fat       = <ls_doc_fatt>-buzei
                              casistica       = lv_casistica
                              hkont_integ     = <ls_doc_fatt>-hkont
                              txt50_integ     = <ls_doc_fatt>-txt50
                              hkont_fondo     = lv_hkont
                              txt50_fondo     = cond #( when lv_hkont is not initial then lv_txt50_fondo )
                              importo_residuo = cond #( when <ls_doc_fatt>-shkzg = 'S' then <ls_doc_fatt>-dmbtr else ( <ls_doc_fatt>-dmbtr * -1 ) )
                              waers           = <ls_doc_fatt>-waers
                              prctr           = <ls_doc_fatt>-prctr
                              ltext           = <ls_doc_fatt>-ltext
                              swenr           = lv_swenr
                              xwetext         = lv_xwetext
                              mess            = lv_msg
                              awtyp           = 'RMRP'
                              analisi         = cond #( when lv_hkont is not initial then abap_true )
                              txz01           = lv_txz01
                              sgtxt           = <ls_doc_fatt>-sgtxt ) to mt_stanziamenti.
            else.


              refresh: lr_xref3.
              "Valorizzazione SET chiave
              "riferimento 3 per check ribaltato
              "in un periodo precedente
              set_xref3( exporting iv_low_1 = ''
                                   iv_low_2 = <ls_doc_fatt>-belnr
                                   iv_low_3 = <ls_doc_fatt>-gjahr
                                   iv_low_4 = '000'
                         importing er_xref3 = lr_xref3 ).

              clear ls_budat.
              refresh: lr_budat.

              "Valorizzazione SET Data registrazione
              "per Check già stanziato in periodo precedente
              move 'I' to ls_budat-sign.
              move 'LT' to ls_budat-option.
              concatenate p_dt_reg+0(4)
                          '0101'
                          into ls_budat-low.
              append ls_budat to lr_budat.
              clear ls_budat.

              clear lv_belnr_check.
              "Controllare se è stata ribaltata fuori dal periodo precedente
              controlla_numero_doc(
              exporting
                 ir_xref3 = lr_xref3
                 ir_budat = lr_budat[]
              importing
                 ev_belnr_check = lv_belnr_check ).

              if lv_belnr_check is not initial.
                "********************************************
                "Caso Fattura Ribaltata in periodi precedenti
                "********************************************

                clear: lv_icon, lv_msg, lv_casistica.

                if lv_hkont is initial.
                  "Errore di transcofica
                  lv_icon = ca_red.
                  lv_msg = 'ERRORE: Transcodifica conto non presente in tabella (YFCMS_FATT_CONTO)'.
                  lv_casistica = 'ERRORE TRANSCODIFICA'.
                else.
                  "Fattura ribaltata
                  lv_icon = ca_red.
                  lv_msg = |ATTENZIONE: La fattura risulta ribaltata ante { p_dt_reg+0(4) }|.
                  lv_casistica = 'FATTURA RIBALTATA'.
                endif.

                append value #( id              = lv_icon
                                bukrs_integ     = p_integ
                                butxt_integ     = lv_integ_txt
                                bukrs_fondo     = p_fondo
                                butxt_fondo     = lv_fondo_txt
                                ebeln           = <ls_doc_fatt>-ebeln
                                ebelp           = <ls_doc_fatt>-ebelp
                                belnr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+4(10) )
                                gjahr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full(4) )
                                buzei_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+14(4) )
                                belnr_fat       = <ls_doc_fatt>-belnr
                                gjahr_fat       = <ls_doc_fatt>-gjahr
                                buzei_fat       = <ls_doc_fatt>-buzei
                                casistica       = lv_casistica
                                hkont_integ     = <ls_doc_fatt>-hkont
                                txt50_integ     = <ls_doc_fatt>-txt50
                                hkont_fondo     = lv_hkont
                                txt50_fondo     = cond #( when lv_hkont is not initial then lv_txt50_fondo )
                                importo_residuo = cond #( when <ls_doc_fatt>-shkzg = 'S' then <ls_doc_fatt>-dmbtr else ( <ls_doc_fatt>-dmbtr * -1 ) )
                                waers           = <ls_doc_fatt>-waers
                                prctr           = <ls_doc_fatt>-prctr
                                ltext           = <ls_doc_fatt>-ltext
                                swenr           = lv_swenr
                                xwetext         = lv_xwetext
                                mess            = lv_msg
                                awtyp           = 'RMRP'
                                analisi         = cond #( when lv_hkont is not initial then abap_true )
                                txz01           = lv_txz01
                                sgtxt           = <ls_doc_fatt>-sgtxt ) to mt_stanziamenti.

              else.
                "*******************************************
                "Caso Fattura OK - No stanziata No ribaltata
                "*******************************************

                clear: lv_msg, lv_casistica, lv_icon.
                "Gestione del campo semaforo, messaggio e casistica
                if lv_check_stanz is not initial.
                  lv_icon = ca_red.
                  lv_msg = |ATTENZIONE: Fattura precedentemente stanziata (N. Doc. SAP { lv_check_stanz })| .
                  lv_casistica = 'FATTURA STANZIATA'.
                else.
                  if lv_hkont is initial.
                    lv_icon = ca_red.
                    lv_msg = 'ERRORE: Transcodifica conto non presente in tabella (YFCMS_FATT_CONTO)'.
                    lv_casistica = 'ERRORE TRANSCODIFICA'.
                  else.
                    lv_icon = ca_green.
                    lv_msg = 'OK - Fattura per cui effettuare lo stanziamento.'.
                    lv_casistica = 'FATTURA DA STANZIARE'.
                  endif.
                endif.

                append value #( id              = lv_icon
                                bukrs_integ     = p_integ
                                butxt_integ     = lv_integ_txt
                                bukrs_fondo     = p_fondo
                                butxt_fondo     = lv_fondo_txt
                                ebeln           = <ls_doc_fatt>-ebeln
                                ebelp           = <ls_doc_fatt>-ebelp
                                belnr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+4(10) )
                                gjahr_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full(4) )
                                buzei_em        = cond #( when lv_xref3_full is not initial then lv_xref3_full+14(4) )
                                belnr_fat       = <ls_doc_fatt>-belnr
                                gjahr_fat       = <ls_doc_fatt>-gjahr
                                buzei_fat       = <ls_doc_fatt>-buzei
                                casistica       = lv_casistica
                                hkont_integ     = <ls_doc_fatt>-hkont
                                txt50_integ     = <ls_doc_fatt>-txt50
                                hkont_fondo     = lv_hkont
                                txt50_fondo     = cond #( when lv_hkont is not initial then lv_txt50_fondo )
                                importo_residuo = cond #( when <ls_doc_fatt>-shkzg = 'S' then <ls_doc_fatt>-dmbtr else ( <ls_doc_fatt>-dmbtr * -1 ) )
                                waers           = <ls_doc_fatt>-waers
                                prctr           = <ls_doc_fatt>-prctr
                                ltext           = <ls_doc_fatt>-ltext
                                swenr           = lv_swenr
                                xwetext         = lv_xwetext
                                mess            = lv_msg
                                awtyp           = 'RMRP'
                                txz01           = lv_txz01
                                sgtxt           = <ls_doc_fatt>-sgtxt ) to mt_stanziamenti.
              endif.
            endif.
          endif.
        endif.
      endif.
    endloop.

  endmethod.                    "estratto_fatt_nc

  method estrarre_entrate_merci.

    data: lt_bem         type tt_fattura,
          lv_imp_fatt    type bseg-wrbtr,
          lv_ktopl       type skat-ktopl,
          lv_check_stanz type bkpf-belnr,
          lv_bukrs       type bkpf-bukrs,
          lr_xref3       type trg_xref3,
          lv_awkey       type bkpf-awkey,
          lv_belnr_check type bkpf-belnr,
          lv_swenr       type vibdbe-swenr,
          lv_txt50_integ type skat-txt50,
          lv_txt50_fondo type skat-txt50,
          lv_ltext       type cepct-ltext,
          ls_fat_cont    type ty_fatt_conto,
          lv_icon        type icon-id,
          lv_casistica   type bseg-sgtxt,
          lv_msg         type char100,
          lv_fat_ribal   type char01,
          ls_bem_fat     type ty_fattura.

    data: rg_ebeln type range of ekko-ebeln.

    refresh: rg_ebeln.

    "Estrazione Entrate merci nel perimetro
    select b~ebeln,
           b~ebelp,
           b~zekkn,
           b~vgabe,
           b~gjahr,
           b~belnr,
           b~buzei,
           b~bewtp,
           b~bwart,
           b~budat,
           b~dmbtr,
           b~shkzg,
           b~lfgja,
           b~lfbnr,
           b~lfpos,
           p~txz01,
           n~sakto,
           n~prctr
    from ekko as k
    inner join ekkn as n
      on k~ebeln = n~ebeln
    inner join ekbe as b
      on n~ebeln = b~ebeln
     and n~ebelp = b~ebelp
    left outer join ekpo as p
      on n~ebeln = p~ebeln
     and n~ebelp = p~ebelp
    into corresponding fields of table @lt_bem
    where k~ebeln in @rg_ebeln
      and k~bukrs = @p_integ
      and n~prctr in @mr_prctr
      and b~vgabe in ('1', '2')
      and b~budat in @mr_budat.

    if sy-subrc <> 0.
      "Nessun dato estratto
      "Passo al caso successivo
      return.
    else.
      "Cancello eventuali duplicazioni
      "dovuto all'accesso alla EKKN
      sort lt_bem by ebeln
                     ebelp
                     zekkn
                     vgabe
                     gjahr
                     belnr
                     buzei.
      delete adjacent duplicates from
        lt_bem comparing ebeln
                         ebelp
                         zekkn
                         vgabe
                         gjahr
                         belnr
                         buzei.
    endif.

    "Divido del fatture dalle entrate merci
    split_fatture( importing et_fatt = mt_fattura
                    changing ct_all_doc = lt_bem ).

    sort lt_bem by bwart ascending.

    "Check EM stornate
    somma_entrate_merci(
     changing
       ct_bem = lt_bem ).

    "Estrazione del piano dei conti
    select single ktopl
      into lv_ktopl
      from t001
     where bukrs = p_integ.

    clear lv_bukrs.
    "Costruzione della società su cui scrivere gli stanziamenti
    "Se la data registazione è il 31/12/AAAA allora la è Prelios Integra (soc. in selezione)
    "Se la data registrazione è diversa dal 31/12/AAAA è la società di stanziamento
    if p_dt_reg+4(4) = '1231'.
      move p_integ to lv_bukrs.
    else.
      lv_bukrs = estratto_societa( ).
    endif.

    if lv_bukrs is initial.
      return.
    endif.

    "Ragione sociale del fondo SGR
    select single butxt
      from t001
      into @data(lv_fondo_txt)
      where bukrs = @p_fondo.

    "Ragione sociale di Prelios Integra
    select single butxt
      from t001
      into @data(lv_integ_txt)
      where bukrs = @p_integ.

**********************************************
*********** Costruzione output ***************
**********************************************
    loop at lt_bem
      assigning field-symbol(<ls_bem>) .

      clear ls_fat_cont.
      "Transcodifica conto coge
      read table it_fat_cont
        into ls_fat_cont
        with key hkont_terzi = <ls_bem>-sakto.

      clear lv_txt50_integ.
      "Descrizione Coge costo Prelios Integra
      select single txt50
        from skat
        into  @lv_txt50_integ
        where saknr = @<ls_bem>-sakto
        and   spras = @sy-langu
        and   ktopl = @lv_ktopl.

      clear lv_txt50_fondo.
      "Descrizione Coge costo Fondo SGR
      if ls_fat_cont-hkont_gruppi is not initial.
        select single txt50
          from skat
          into  @lv_txt50_fondo
          where saknr = @ls_fat_cont-hkont_gruppi
          and   spras = @sy-langu
          and   ktopl = @lv_ktopl.
      endif.

      clear lv_ltext.
      "Descrizione Profit center
      select single ltext
        from cepct
        into @lv_ltext
        where prctr = @<ls_bem>-prctr
        and   datbi = @ca_31129999
        and   kokrs = @p_integ.

      clear: lv_swenr.
      move <ls_bem>-prctr+2(8) to lv_swenr.

      "Descrizione Unità Economica
      select single xwetext
        from vibdbe
        into @data(lv_xwetext)
        where swenr = @lv_swenr
        and   bukrs = @p_integ.

      if <ls_bem>-dmbtr = 0.
**********************************************
        "Caso Entrata merci stornata
**********************************************

        "Inserisco la riga nella tabella di output
        append value #( id              = ca_red
                        bukrs_integ     = p_integ
                        butxt_integ     = lv_integ_txt
                        bukrs_fondo     = p_fondo
                        butxt_fondo     = lv_fondo_txt
                        ebeln           = <ls_bem>-ebeln
                        ebelp           = <ls_bem>-ebelp
                        belnr_em        = <ls_bem>-belnr
                        gjahr_em        = <ls_bem>-gjahr
                        buzei_em        = <ls_bem>-buzei
                        casistica       = text-m04
                        hkont_integ     = <ls_bem>-sakto
                        txt50_integ     = lv_txt50_integ
                        hkont_fondo     = ls_fat_cont-hkont_gruppi
                        txt50_fondo     = lv_txt50_fondo
                        importo_residuo = 0
                        waers           = ca_eur
                        prctr           = <ls_bem>-prctr
                        ltext           = lv_ltext
                        swenr           = lv_swenr
                        xwetext         = lv_xwetext
                        mess            = text-m05
                        awtyp           = ca_mkpf
                        analisi         = cond #( when ls_fat_cont is not initial then abap_true )
                        txz01           = <ls_bem>-txz01
           ) to mt_stanziamenti.

      else.
**********************************************
        "Caso Entrata merci NON Stornata
**********************************************

        clear: lv_imp_fatt,
               ls_bem_fat,
               lv_fat_ribal.

*        "Calcolo il residuo rispetto al fatturato
        calculate_res_fat( exporting is_bem  = <ls_bem>
                           importing ev_imp_fatt = lv_imp_fatt
                            changing it_fatt = mt_fattura ).

        "Check se è collegata ad una fattura ribaltata
        check_fatt_em_rib( exporting is_bem = <ls_bem>
                                     it_fatt = mt_fattura
                           importing es_bem_fat = ls_bem_fat
                                     ev_fat_ribal = lv_fat_ribal ).

        refresh: lr_xref3.
        "Costruzione della chiave di riferimento
        set_xref3( exporting iv_low_1 = 'EM'
                             iv_low_2 = <ls_bem>-belnr
                             iv_low_3 = <ls_bem>-gjahr
                             iv_low_4 = <ls_bem>-buzei
                   importing er_xref3 = lr_xref3 ).

        clear: lv_check_stanz.
        "Controllo se la EM è stata già stanziata
        controlla_numero_doc(
        exporting
           ir_xref3 = lr_xref3
           iv_bukrs = lv_bukrs
           iv_blart1 = p_td_a_f
           iv_blart2 = p_tdoc_a
*           iv_budat  = p_dt_reg
        importing
           ev_belnr_check = lv_check_stanz ).

*        clear lv_icon.
*        if ls_fat_cont is initial.
*          lv_icon = ca_red.
*        else.
*          if lv_check_stanz is not initial.
*            lv_icon = ca_red.
*          else.
*            if <ls_bem>-dmbtr = lv_imp_fatt.
*              lv_icon = ca_yellow.
*            else.
*              lv_icon = ca_green.
*            endif.
*          endif.
*        endif.

        clear: lv_casistica, lv_msg, lv_icon.

        "Campo casistica e messaggio
        if ls_fat_cont is initial.
          lv_icon = ca_red.
          lv_casistica = text-m06.
          lv_msg = text-m07.
        else.
          if lv_check_stanz is not initial.
            lv_icon = ca_red.
            lv_casistica = text-m08.
            lv_msg = text-m09.
            replace '&' with lv_check_stanz into lv_msg.
          else.
            if lv_imp_fatt is initial.
              lv_icon = ca_green.
              lv_casistica = text-m10.
              lv_msg = text-m11.
            else.
              if <ls_bem>-dmbtr = lv_imp_fatt.
                if lv_fat_ribal is initial.
                  lv_icon = ca_yellow.
                  lv_casistica = text-m12.
                  lv_msg = text-m13.
                else.
                  lv_icon = ca_yellow.
                  lv_casistica = text-m14.
                  lv_msg = text-m15.
                endif.
              else.
                if lv_fat_ribal is initial.
                  lv_icon = ca_green.
                  lv_casistica = text-m16.
                  lv_msg = text-m17.
                else.
                  lv_icon = ca_green.
                  lv_casistica = text-m18.
                  lv_msg = text-m19.
                endif.
              endif.
            endif.
          endif.
        endif.

        "Appendo record nella tabella di output
        append value #( id              = lv_icon
                        bukrs_integ     = p_integ
                        butxt_integ     = lv_integ_txt
                        bukrs_fondo     = p_fondo
                        butxt_fondo     = lv_fondo_txt
                        ebeln           = <ls_bem>-ebeln
                        ebelp           = <ls_bem>-ebelp
                        belnr_em        = <ls_bem>-belnr
                        gjahr_em        = <ls_bem>-gjahr
                        buzei_em        = <ls_bem>-buzei
                        belnr_fat       = cond #( when lv_imp_fatt is not initial then ls_bem_fat-belnr )
                        gjahr_fat       = cond #( when lv_imp_fatt is not initial then ls_bem_fat-gjahr )
                        buzei_fat       = cond #( when lv_imp_fatt is not initial then ls_bem_fat-buzei )
                        casistica       = lv_casistica
                        hkont_integ     = <ls_bem>-sakto
                        txt50_integ     = lv_txt50_integ
                        hkont_fondo     = ls_fat_cont-hkont_gruppi
                        txt50_fondo     = lv_txt50_fondo
                        importo_residuo = <ls_bem>-dmbtr - lv_imp_fatt
                        waers           = ca_eur
                        prctr           = <ls_bem>-prctr
                        ltext           = lv_ltext
                        swenr           = lv_swenr
                        xwetext         = lv_xwetext
                        mess            = lv_msg
                        awtyp           = ca_mkpf
                        analisi         = cond #( when <ls_bem>-dmbtr = lv_imp_fatt then abap_true )
                        txz01           = <ls_bem>-txz01
           ) to mt_stanziamenti.

      endif.
    endloop.

  endmethod.                    "estrarre_entrate_merci

  method set_xref3.

    data: ls_xref3 type line of trg_xref3,
          lva_low  type bseg-xref3.

    if iv_low_4 <> '000'.

      concatenate iv_low_1
                  iv_low_2
                  iv_low_3
                  iv_low_4
                  into lva_low.

    else.

      concatenate iv_low_1
                  iv_low_2
                  iv_low_3
                  '*'
                  into lva_low.

    endif.

    condense lva_low no-gaps.

    move 'I' to ls_xref3-sign.

    if iv_high is not initial.
      move 'BT' to ls_xref3-option.
    else.
      if lva_low cs '*'.
        move 'CP' to ls_xref3-option.
      else.
        move 'EQ' to ls_xref3-option.
      endif.
    endif.

    move lva_low to ls_xref3-low.

    if iv_high is not initial.
      move iv_high to ls_xref3-high.
    endif.

    append ls_xref3 to er_xref3.

  endmethod.

  method check_fatt_em_rib.

    loop at it_fatt
      assigning field-symbol(<ls_bem_wa>)
      where ebeln = is_bem-ebeln
        and ebelp = is_bem-ebelp
        and lfgja = is_bem-gjahr
        and lfbnr = is_bem-belnr
        and lfpos = is_bem-buzei
        and bewtp in mr_bewtp.

      es_bem_fat = <ls_bem_wa>.

      if <ls_bem_wa>-ribaltata = abap_true.
        "fattura ribaltata
        ev_fat_ribal = abap_true.
        exit.
      endif.

    endloop.

  endmethod.

  method calculate_res_fat.

    loop at it_fatt
      assigning field-symbol(<ls_bem_wa>)
      where ebeln = is_bem-ebeln
        and ebelp = is_bem-ebelp
        and lfgja = is_bem-gjahr
        and lfbnr = is_bem-belnr
        and lfpos = is_bem-buzei
        and bewtp in mr_bewtp.

      "Calcolo importo totale fatture collegate
      if <ls_bem_wa>-shkzg = 'H'.
        ev_imp_fatt = ev_imp_fatt - <ls_bem_wa>-dmbtr.
      elseif <ls_bem_wa>-shkzg = 'S'.
        ev_imp_fatt = ev_imp_fatt + <ls_bem_wa>-dmbtr.
      endif.

      "Segno che la fattura è stata compensata da una EM
      <ls_bem_wa>-compensata = abap_true.

    endloop.

  endmethod.

  method split_fatture.

    data: lv_belnr       type bkpf-belnr,
          lv_gjahr       type bkpf-gjahr,
          lr_xref3       type range of xref3,
          lv_belnr_check type bkpf-belnr,
          lv_awkey       type bkpf-awkey,
          lva_index      type sy-tabix.

    loop at ct_all_doc
      assigning field-symbol(<ls_bem>)
      where vgabe = '2'
        and bewtp in mr_bewtp.
      "Controllo le fatture

      clear lva_index.
      move sy-tabix to lva_index.

      clear lv_awkey.
      "Chiave per accesso a documento contabile
      concatenate <ls_bem>-belnr
                  <ls_bem>-gjahr
                  into lv_awkey.

      clear: lv_belnr, lv_gjahr.

      "Estraggo il numero documento contabile della fattura
      select single belnr,
             gjahr
        from bkpf
        into ( @lv_belnr, @lv_gjahr )
        where bukrs = @p_integ
        and   awtyp = 'RMRP'
        and   awkey = @lv_awkey.

      if sy-subrc <> 0.
        "Errore non trovata
        continue.
      endif.

      refresh: lr_xref3.
      "Costruzione della chiave di riferimento
      set_xref3( exporting iv_low_1 = ''
                           iv_low_2 = lv_belnr
                           iv_low_3 = lv_gjahr
                           iv_low_4 = '000'
                 importing er_xref3 = lr_xref3 ).

      clear lv_belnr_check.
      "Verifica se il documento è stato ribaltato
      controlla_numero_doc(
      exporting
         ir_xref3 = lr_xref3
      importing
         ev_belnr_check = lv_belnr_check ).

      "Fattura ribaltata
      if lv_belnr_check is not initial.
        <ls_bem>-ribaltata = abap_true.
      endif.

      "valorizzo la tabella delle fatture
      append <ls_bem> to et_fatt.

      "Cancello la riga delle fatture
      delete ct_all_doc index lva_index.

    endloop.

  endmethod.

  method  somma_entrate_merci.

    data tb_bem_102 type tt_fattura.

    refresh: tb_bem_102.

    "Estraggo solo gli storni
    move-corresponding ct_bem[] to tb_bem_102[].
    delete tb_bem_102 where vgabe <> '1'.
    delete tb_bem_102 where bwart <> ca_bwart_102.

    "Lo scopo della FORM è di appurare se le entrate merci
    "sono stornate e se lo storno è nel medesimo periodo di analisi

    loop at tb_bem_102
      assigning field-symbol(<ls_bem_102>).

      loop at ct_bem
        assigning field-symbol(<ls_bem>)
        where ebeln = <ls_bem_102>-ebeln
          and ebelp = <ls_bem_102>-ebelp
          and vgabe = '1'
          and gjahr = <ls_bem_102>-lfgja
          and belnr = <ls_bem_102>-lfbnr
          and buzei = <ls_bem_102>-lfpos.
        "Trovata l'entra merci stornata

        "Aggiorno l'importo dell'EM stornata
        if <ls_bem_102>-shkzg = 'H' and <ls_bem>-shkzg = 'H'.
          <ls_bem>-dmbtr = ( <ls_bem>-dmbtr * -1 ) + ( <ls_bem_102>-dmbtr * -1 ).
        elseif <ls_bem_102>-shkzg = 'H' and <ls_bem>-shkzg = 'S'.
          <ls_bem>-dmbtr = <ls_bem>-dmbtr + ( <ls_bem_102>-dmbtr * -1 ).
        elseif <ls_bem_102>-shkzg = 'S' and <ls_bem>-shkzg = 'H'.
          <ls_bem>-dmbtr = ( <ls_bem>-dmbtr * -1 ) + <ls_bem_102>-dmbtr.
        elseif <ls_bem_102>-shkzg = 'S' and <ls_bem>-shkzg = 'S'.
          <ls_bem>-dmbtr = <ls_bem>-dmbtr + <ls_bem_102>-dmbtr.
        endif.

        "Segno come da cancellare lo storno che ho compensato
        read table ct_bem
          assigning field-symbol(<ls_bem_sto>)
          with key ebeln = <ls_bem_102>-ebeln
                   ebelp = <ls_bem_102>-ebelp
                   vgabe = '1'
                   gjahr = <ls_bem_102>-gjahr
                   belnr = <ls_bem_102>-belnr
                   buzei = <ls_bem_102>-buzei.
        if sy-subrc = 0.
          move 'X' to <ls_bem_sto>-blocked.
        endif.

      endloop.

    endloop.

    "Cancello gli storni compensati e lascio solo quelli non compensati
    delete ct_bem where blocked is not initial.

  endmethod.

  method  estratto_societa.

    data: lva_bukrs type bukrs.

    clear lva_bukrs.

    select single bukrs
      from tka02
      into lva_bukrs
      where kokrs  = p_integ
      and   bukrs <> p_integ.

    if sy-subrc <> 0.
      append value #(  message_v1 = text-m02
                       message_v2 = text-m03 ) to mt_logs.
    else.
      move lva_bukrs to rv_bukrs.
    endif.

  endmethod.

  method controlla_numero_doc.

    data(lv_bukrs)  = cond #( when iv_bukrs  is initial then p_integ else iv_bukrs  ).
    data(lv_blart1) = cond #( when iv_blart1 is initial then mv_fatt else iv_blart1 ).
    data(lv_blart2) = cond #( when iv_blart2 is initial then mv_nc   else iv_blart2 ).

    if iv_budat is initial.
      data(lr_budat) = mr_budat.
    else.
      if ir_budat[] is initial.
        lr_budat    = value #( ( sign = 'I' option = 'EQ'   low  = iv_budat ) ).
      else.
        move-corresponding ir_budat[] to lr_budat[].
      endif.
    endif.

    select single s~belnr, s~xref3
    from bkpf as k
    inner join bseg as s
      on k~bukrs = s~bukrs
     and k~belnr = s~belnr
     and k~gjahr = s~gjahr
    into ( @ev_belnr_check, @ev_xref3 )
    where k~bukrs  = @lv_bukrs
      and k~blart  in (@lv_blart1, @lv_blart2)
      and k~budat  in @lr_budat
      and k~stblg  = ''
      and s~xref3  in @ir_xref3.

  endmethod.                    "controlla_numero_doc

  method constructor.
    mr_msg_type = value #(   sign = 'I' option = 'EQ' ( low  = 'E' ) ( low  = 'A' ) ( low  = 'X' ) ).
    mr_budat    = value #( ( sign = 'I' option = 'BT'   low  = p_dt_reg(4) && '0101'  high = p_dt_reg ) ).
    mr_prctr    = value #( ( sign = 'I' option = 'CP'   low  = 'E0' && p_fondo && '*' ) ).
    mr_bewtp    = value #(   sign = 'I' option = 'EQ' ( low  = 'Q' ) ( low  = 'R' ) ).

    select single low
      from tvarvc
      into mv_fatt
      where name = 'YFICMS_FACILITY_PAS_FATT'.

    select single low
      from tvarvc
      into mv_nc
      where name = 'YFICMS_FACILITY_PAS_NC'.

  endmethod.                    "constructor

  method visualizza_fattura.

    data lt_fcat type lvc_t_fcat.

    "Estrazione dei dati e lavorazione
    estrarre_dati( ).

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

    lt_fcat = get_fcat( ) .

    start_container( ).

    set handler : handle_toolbar
                  top_of_page
                  on_hotspot
                  handle_user_command for mo_grid.

    mo_grid->set_table_for_first_display(
       exporting
         i_save               = 'X'
         is_layout            = ls_layout
         is_variant           = ls_variant
         it_toolbar_excluding = lt_toolbar_excluding
       changing
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_stanziamenti ).

    mo_top->initialize_document( ).

    mo_grid->list_processing_events(
     exporting
       i_event_name = 'TOP_OF_PAGE'
       i_dyndoc_id  = mo_top ).

  endmethod.                    "visualizza_fattura

  method visualizza_log.

    message_init_show( 'INIT' ).

    loop at mt_logs assigning field-symbol(<ls_logs>).
      message_store(
        exporting
          iv_arbgb  = <ls_logs>-id
          iv_msgty  = <ls_logs>-type
          iv_msg1   = <ls_logs>-message_v1
          iv_msg2   = <ls_logs>-message_v2
          iv_msg3   = <ls_logs>-message_v3
          iv_msg4   = <ls_logs>-message_v4
          iv_txtnr  = <ls_logs>-number
          iv_lineno = sy-tabix
      ).
    endloop.

    message_init_show( 'SHOW' ).

  endmethod.                    "visualizza_log

  method start_container.

    data: lo_cont      type ref to cl_gui_custom_container,
          lo_split_col type ref to cl_gui_splitter_container.

    if mo_grid is bound.
      return.
    endif.

    create object lo_cont
      exporting
        container_name              = 'CUST_CONT101'
        repid                       = sy-repid
        dynnr                       = '0101'
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

    create object lo_split_col
      exporting
        parent            = lo_cont
        rows              = 2
        columns           = 1
      exceptions
        cntl_error        = 1
        cntl_system_error = 2
        others            = 3.

    lo_split_col->set_row_sash( id    = 1
                                type  = cl_gui_splitter_container=>type_sashvisible
                                value = cl_gui_splitter_container=>false ).

    lo_split_col->set_row_height(
      exporting
        id                = 1
        height            = 11
      exceptions
        cntl_error        = 1
        cntl_system_error = 2
        others            = 3 ).

    mo_top_cnt = lo_split_col->get_container( row = 1 column = 1 ).

    create object mo_top
      exporting
        style = 'ALV_GRID'.

    create object mo_grid
      exporting
        i_parent          = lo_split_col->get_container( row = 2 column = 1 )
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endmethod.                    "start_container

  method  refresh_top_of_page.
    mo_grid->refresh_table_display( ).
    mo_top->initialize_document( ).
    top_of_page( ).
  endmethod.                    "refresh_top_of_page

  method handle_toolbar.
    data: ls_toolbar type stb_button.

    e_object->mt_toolbar = value #( base e_object->mt_toolbar
        ( function = 'FC_STANZIAMENTI' text = 'Registra stanziamenti' icon = icon_execute_object           quickinfo = 'Registra stanziamenti' )
        ( function = 'FC_LOGS'         text = 'Log Registrazioni'     icon = icon_display_text             quickinfo = 'Log Registrazioni'   ) ).

  endmethod.    "handle_toolbar

  method top_of_page.

    constants: lc_color   type string value '#E5E8E8'.

    data: lv_fatt_blocked type i,
          lv_fatt_unblckd type i,
          lv_fatt_error   type i,
          lt_html_tab     type sdydo_html_table.

    case mv_stanz_status.
      when ms_status-fatto.
        data(lv_status) = exact char9('Fatto').
        data(lv_color)  = exact char6('green').
        data(lv_title)  = exact string('Elaborazione con successo').
      when ms_status-errore.
        lv_status = 'Errore'.
        lv_color = 'red'.
        lv_title = |Errore durante l'elaborazione|.
      when ms_status-attesa.
        lv_status = 'In Attesa'.
        lv_color = 'orange'.
        lv_title = |In attesa di elaborazione|.
    endcase.

    loop at mt_stanziamenti assigning field-symbol(<ls_stanziamenti>).
      if <ls_stanziamenti>-id = ca_red
      or <ls_stanziamenti>-id = ca_yellow.
        lv_fatt_blocked = lv_fatt_blocked + 1.
      elseif <ls_stanziamenti>-id = ca_green.
        lv_fatt_unblckd = lv_fatt_unblckd + 1.
      endif.
    endloop.

    data(lv_text) = exact sdydo_text_element( |Fatture estratte : { lines( mt_stanziamenti ) }| ).

    lt_html_tab = value #(
    ( |<style>| )
    ( | .header2 { '{' }| )
    ( |   font-size: 10pt;| )
    ( |   font-family:Sans-Serif;| )
    ( |   user-select:none;| )
    ( |   font-weight:bold;| )
    ( |{ '}' }| )
    ( |</style>| )
    ( |<body style="background-color: { lc_color }; border: 1px solid gray; margin: 0; padding: 10px ">| )
    ( |  <div style="display: flex; justify-content: space-between;">| )
    ( |    <font style = "font-size: 12pt; font-weight:bold; color="black"> { lv_text } </font>| )
    ( |    <span>| )
    ( |      <font class="header2" color="black"; title ="{ lv_title }"> Stanziamento status: </font>| )
    ( |      <font class="header2" color="{ lv_color }" title ="{ lv_title }"> { lv_status } </font>| )
    ( |    </span>| )
    ( |  </div>| )
    ( |  <hr style=" margin-left: 0;">| )
    ( |  <font class="header2" color="black"> N. fatture stanziabile : </font>| )
    ( |  <font class="header2" color="green"> { lv_fatt_unblckd } </font> &nbsp &nbsp &nbsp| )
    ( |  <font class="header2" color="black"> N. fatture con errore : </font>| )
    ( |  <font class="header2" color="red"> { lv_fatt_blocked } </font>| )
    ( |</body>| ) ).

    mo_top->add_static_html( table_with_html  = lt_html_tab ).
    mo_top->display_document(
     exporting
       reuse_control      = 'X'
       parent             = mo_top_cnt
     exceptions
       html_display_error = 1
       others             = 2 ).

  endmethod.

  method get_fcat.

    rt_fieldcat = value lvc_t_fcat(
    ( fieldname = 'ID'              reptext   = 'Status'   icon      = abap_true                                              )
    ( fieldname = 'BUKRS_INTEG'     scrtext_s = text-t01   scrtext_m = text-t02  scrtext_l = text-t03  reptext = text-t03  ref_table = 'BKPF'   ref_field = 'BUKRS')
    ( fieldname = 'BUTXT_INTEG'     scrtext_s = text-t04   scrtext_m = text-t05  scrtext_l = text-t06  reptext = text-t06  ref_table = 'T001'   ref_field = 'BUTXT')
    ( fieldname = 'BUKRS_FONDO'     scrtext_s = text-t07   scrtext_m = text-t08  scrtext_l = text-t09  reptext = text-t09  ref_table = 'BKPF'   ref_field = 'BUKRS')
    ( fieldname = 'BUTXT_FONDO'     scrtext_s = text-t10   scrtext_m = text-t11  scrtext_l = text-t12  reptext = text-t12  ref_table = 'T001'   ref_field = 'BUTXT')
    ( fieldname = 'EBELN'           scrtext_s = text-t13   scrtext_m = text-t14  scrtext_l = text-t15  reptext = text-t15  ref_table = 'EKKO'   ref_field = 'EBELN'  hotspot = abap_true )
    ( fieldname = 'EBELP'           scrtext_s = text-t16   scrtext_m = text-t17  scrtext_l = text-t18  reptext = text-t18  ref_table = 'EKPO'   ref_field = 'EBELP')
    ( fieldname = 'BELNR_EM'        scrtext_s = text-t19   scrtext_m = text-t20  scrtext_l = text-t21  reptext = text-t21  ref_table = 'MKPF'   ref_field = 'BELNR'  hotspot = abap_true )
    ( fieldname = 'GJAHR_EM'        scrtext_s = text-t22   scrtext_m = text-t23  scrtext_l = text-t24  reptext = text-t24  ref_table = 'MKPF'   ref_field = 'GJAHR')
    ( fieldname = 'BUZEI_EM'        scrtext_s = text-t25   scrtext_m = text-t26  scrtext_l = text-t27  reptext = text-t27  ref_table = 'MSEG'   ref_field = 'BUZEI')
    ( fieldname = 'BELNR_FAT'       scrtext_s = text-t28   scrtext_m = text-t29  scrtext_l = text-t30  reptext = text-t30  ref_table = 'BKPF'   ref_field = 'BELNR'  hotspot = abap_true )
    ( fieldname = 'GJAHR_FAT'       scrtext_s = text-t31   scrtext_m = text-t32  scrtext_l = text-t33  reptext = text-t33  ref_table = 'BKPF'   ref_field = 'GJAHR')
    ( fieldname = 'BUZEI_FAT'       scrtext_s = text-t34   scrtext_m = text-t35  scrtext_l = text-t36  reptext = text-t36  ref_table = 'BSEG'   ref_field = 'BUZEI')
    ( fieldname = 'CASISTICA'       scrtext_s = text-t37   scrtext_m = text-t38  scrtext_l = text-t39  reptext = text-t39  ref_table = 'BSEG'   ref_field = 'SGTXT')
    ( fieldname = 'HKONT_INTEG'     scrtext_s = text-t40   scrtext_m = text-t41  scrtext_l = text-t42  reptext = text-t42  ref_table = 'BSEG'   ref_field = 'HKONT')
    ( fieldname = 'TXT50_INTEG'     scrtext_s = text-t43   scrtext_m = text-t44  scrtext_l = text-t45  reptext = text-t45  ref_table = 'SKAT'   ref_field = 'TXT50')
    ( fieldname = 'HKONT_FONDO'     scrtext_s = text-t46   scrtext_m = text-t47  scrtext_l = text-t48  reptext = text-t48  ref_table = 'BSEG'   ref_field = 'HKONT')
    ( fieldname = 'TXT50_FONDO'     scrtext_s = text-t49   scrtext_m = text-t50  scrtext_l = text-t51  reptext = text-t51  ref_table = 'SKAT'   ref_field = 'TXT50')
    ( fieldname = 'IMPORTO_RESIDUO' scrtext_s = text-t52   scrtext_m = text-t53  scrtext_l = text-t54  reptext = text-t54  ref_table = 'BSEG'   ref_field = 'WRBTR' cfieldname = 'WAERS')
    ( fieldname = 'WAERS'           ref_table = 'BKPF'     ref_field = 'WAERS'                                            )
    ( fieldname = 'PRCTR'           scrtext_s = text-t55   scrtext_m = text-t56  scrtext_l = text-t57  reptext = text-t57  ref_table = 'BSEG'   ref_field = 'PRCTR')
    ( fieldname = 'LTEXT'           scrtext_s = text-t58   scrtext_m = text-t59  scrtext_l = text-t60  reptext = text-t60  ref_table = 'CEPCT'  ref_field = 'LTEXT')
    ( fieldname = 'SWENR'           ref_table = 'VIBDBE'   ref_field = 'SWENR'                                            )
    ( fieldname = 'XWETEXT'         scrtext_s = text-t61   scrtext_m = text-t62  scrtext_l = text-t63  reptext = text-t63  ref_table = 'VIBDBE' ref_field = 'XWETEXT')
    ( fieldname = 'MESS'            reptext   = 'Messaggio'                                                               )
    ( fieldname = 'TXZ01'           ref_table = 'EKPO'     ref_field = 'TXZ01'   no_out = 'X'                                            )
    ( fieldname = 'SGTXT'           scrtext_s = text-t67   scrtext_m = text-t68  scrtext_l = text-t69  reptext = text-t69  ref_table = 'BSEG'   ref_field = 'SGTXT' no_out = 'X' ) ).

  endmethod.

  method message_store.

    data: lv_arbgb type bapiret2-id   value 'DB',
          lv_msgty type bapiret2-type value 'E'.

    call function 'MESSAGE_STORE'
      exporting
        arbgb                  = cond #( when iv_arbgb is initial then lv_arbgb else iv_arbgb )
        msgty                  = cond #( when iv_msgty is initial then lv_msgty else iv_msgty )
        msgv1                  = iv_msg1
        msgv2                  = iv_msg2
        msgv3                  = iv_msg3
        msgv4                  = iv_msg4
        txtnr                  = cond #( when iv_txtnr is initial then '000' else iv_txtnr )
        zeile                  = iv_lineno
      exceptions
        message_type_not_valid = 1
        not_active             = 2
        others                 = 3.

  endmethod.

  method message_init_show.

    case iv_action.
      when 'INIT'.
        call function 'MESSAGES_INITIALIZE'
          exceptions
            others = 1.
      when 'SHOW'.
        call function 'MESSAGES_SHOW'
          exporting
            i_use_grid         = 'X'
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
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endmethod.

  method at_selection_screen.

    constants: lc_date type dats value '99991231'.

    data lv_last_day type dats.

    "Controllo per codice Prelios Integra ===================================================================
    select single @abap_true
      from t001
      into @data(lv_exist)
      where bukrs = @p_integ.

    if sy-subrc <> 0.
      message |ERRORE: Soc.{ p_integ } non esiste in SAP | type 'S' display like 'E'.
      ev_error = abap_true.
      return.
    endif.

    "Controllo per codice Fondo SGR ===================================================================
    clear lv_exist.

    select single @abap_true
      from t001
      into @lv_exist
      where bukrs = @p_fondo.

    if sy-subrc <> 0.
      message |ERRORE: Soc.{ p_fondo } non esiste in SAP | type 'S' display like 'E'.
      ev_error = abap_true.
      return.
    endif.

    "Controllo per TipIi Documento ===================================================================
    clear lv_exist.

    select single @abap_true
      from t003
      into @lv_exist
      where blart in @s_blart.

    if sy-subrc <> 0.
      message |ERRORE: Tipo Doc.FI non esiste in SAP | type 'S' display like 'E'.
      ev_error = abap_true.
      return.
    endif.

    "Controllo per stanziamento selezionato ===================================================================
    if c_stan_a is initial and c_stan_p is initial.
      message |ERRORE: Selezionare almeno un tipo di stanziamento| type 'S' display like 'E'.
      ev_error = abap_true.
      return.
    elseif c_stan_a is initial and c_stan_p is not initial.
      message 'Attenzione: impostati solo gli stanziamenti passivi' type 'W'.
    elseif c_stan_p is initial and c_stan_a is not initial.
      message 'Attenzione: impostati solo gli stanziamenti attivi'  type 'W'.
    endif.

    "Controllo per tipo documento passivo ===================================================================

    if c_stan_p = abap_true.

      if p_tdoc_p is initial.
        message |ERRORE: Valorizzare il campo Tipo documento passivo| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.
      clear lv_exist.

      select single @abap_true
        from t003
        into @lv_exist
        where blart = @p_tdoc_p.

      if sy-subrc <> 0.
        message |ERRORE: Tipo doc. passivo non corretto| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.


    "Controllo per tipo documento passivo fine anno ===================================================================

    if c_stan_p = abap_true.

      if p_td_p_f is initial.
        message |ERRORE: Valorizzare Tipo doc. passivo fine anno| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.
      clear lv_exist.

      select single @abap_true
        from t003
        into @lv_exist
        where blart = @p_td_p_f.

      if sy-subrc <> 0.
        message |ERRORE: Tipo doc. passivo fine anno non corretto| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.

    "Check Conto CoGe fatture da ricevere ===================================================================

    if c_stan_p is not initial and p_coge_p is not initial and p_fondo is not initial.

      select single *
        from t001
        join ska1 on ska1~ktopl = t001~ktopl
        into @data(ls_t001)
        where t001~bukrs = @p_fondo
        and   ska1~saknr = @p_coge_p.

      if sy-subrc <> 0.
        message |ERRORE: Conto { p_coge_p } non esiste| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

      clear lv_exist.

      select single @abap_true
        from skb1
        into @lv_exist
        where bukrs = @p_fondo
        and   saknr = @p_coge_p.

      if sy-subrc <> 0.
        message |ERRORE: CoGe { p_coge_p } non aperto per soc.{ p_fondo }| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.

    "Check centro di costo Integra ===================================================================

    if c_stan_p is not initial and p_cdc_p is not initial and p_integ is not initial.

      clear lv_exist.

      select single bkzkp
        from csks
        into @data(lv_bkzkp)
        where kokrs = @p_integ
        and   kostl = @p_cdc_p
        and   datbi = @lc_date.

      if sy-subrc <> 0.
        message |ERRORE: CdC { p_cdc_p } soc.{ p_integ } non esiste| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

      if lv_bkzkp is not initial.
        message |ERRORE: CdC { p_cdc_p } bloccato per reg. primarie| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.

    "Controllo per tipo documento attivo ===================================================================

    if c_stan_a = abap_true.

      if p_tdoc_a is initial.
        message |ERRORE: Valorizzare il campo Tipo documento attivo| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.
      clear lv_exist.

      select single @abap_true
        from t003
        into @lv_exist
        where blart = @p_tdoc_a.

      if sy-subrc <> 0.
        message |ERRORE: Tipo doc. attivo non corretto| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.

    "Controllo per tipo documento attivo fine anno ===================================================================

    if c_stan_a = abap_true.

      if p_td_a_f is initial.
        message |ERRORE: Valorizzare Tipo doc. attivo fine anno| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.
      clear lv_exist.

      select single @abap_true
        from t003
        into @lv_exist
        where blart = @p_td_a_f.

      if sy-subrc <> 0.
        message |ERRORE: Tipo doc. attivo fine anno non corretto| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.

    "Check Conto Ricavo Prelios Integra  ===================================================================

    if c_stan_a is not initial and p_coge_r is not initial and p_integ is not initial.

      clear ls_t001.

      select single *
        from t001
        join ska1 on ska1~ktopl = t001~ktopl
        into @ls_t001
        where t001~bukrs = @p_integ
        and   ska1~saknr = @p_coge_r.

      if sy-subrc <> 0.
        message |ERRORE: Conto { p_coge_r } non esiste| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

      clear lv_exist.

      select single @abap_true
        from skb1
        into @lv_exist
        where bukrs = @p_integ
        and   saknr = @p_coge_r.

      if sy-subrc <> 0.
        message |ERRORE: CoGe { p_coge_r } non aperto per soc.{ p_integ }| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.

    "Check Costo Fatture da emettere Prelios Integra  ===================================================================

    if c_stan_a is not initial and p_coge_f is not initial and p_integ is not initial.

      clear ls_t001.

      select single *
        from t001
        join ska1 on ska1~ktopl = t001~ktopl
        into @ls_t001
        where t001~bukrs = @p_integ
        and   ska1~saknr = @p_coge_f.

      if sy-subrc <> 0.
        message |ERRORE: Conto { p_coge_f } non esiste| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

      clear lv_exist.

      select single @abap_true
        from skb1
        into @lv_exist
        where bukrs = @p_integ
        and   saknr = @p_coge_f.

      if sy-subrc <> 0.
        message |ERRORE: CoGe { p_coge_f } non aperto per soc.{ p_integ }| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.

    "Check data registrazione come ultimo giorno del mese   ===================================================================

    if p_dt_reg is not initial.

      data lv_date type datum.

      clear lv_date.
      concatenate p_dt_reg(6) '01' into lv_date.

      call function 'LAST_DAY_OF_MONTHS'
        exporting
          day_in            = lv_date
        importing
          last_day_of_month = lv_last_day
        exceptions
          day_in_no_date    = 1
          others            = 2.

      if sy-subrc <> 0.
        message |ERRORE: Data registrazione non corretta| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

      if lv_last_day <> p_dt_reg.
        message |ERRORE: Data registrazione deve essere ultimo giorno del mese| type 'S' display like 'E'.
        ev_error = abap_true.
        return.
      endif.

    endif.

  endmethod.                    "at_selection_screen

  method initialization.

    append value #( sign = 'I' option = 'CP'  low = 'M*' ) to s_blart.
    append value #( sign = 'I' option = 'CP'  low = 'K*' ) to s_blart.

  endmethod.                    "initialization

endclass.

initialization.
  lcl_calcolo_stanziamenti=>initialization( ).

at selection-screen.
  if lcl_calcolo_stanziamenti=>at_selection_screen( ) = abap_true .
    stop.
  endif.

start-of-selection.
  create object go_calcolo_stanziamenti.
  go_calcolo_stanziamenti->execute( ).
