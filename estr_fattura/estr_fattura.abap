*&---------------------------------------------------------------------*
*& Report  YFICMS_ESTRATTORE_FATTURE_V2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT yficms_estrattore_fatture_v2.

TABLES: bkpf.

DATA: BEGIN OF gs_screen100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen100.

*Dati Estrazione
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_integ   TYPE bkpf-bukrs DEFAULT '0017'.           " Codice Prelios Integra
SELECT-OPTIONS: s_dt_reg  FOR bkpf-budat,                       " Date registrazione Integra
                s_ft_int  FOR bkpf-blart DEFAULT 'K*' TO 'M*'.  " Tipo documento Integra
SELECTION-SCREEN END OF BLOCK b1.
*Fondo SGR
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_fondo TYPE bkpf-bukrs,                            " Fondo SGR
            p_for_i TYPE bseg-lifnr,                            " Fornitore Prelios Integra
            p_cli_f TYPE bseg-kunnr.                            " Cliente Fondo SGR
SELECTION-SCREEN END OF BLOCK b2.
*Dati documenti per il ribaltamento
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_dt_r   TYPE bkpf-budat,                           " Data registrazione documenti contabili
            p_td_fat TYPE bkpf-blart,                           " Tipo documento fattura attiva
            p_td_nc  TYPE bkpf-blart,                           " Tipo documento nota di credito attiva
            p_td_gir TYPE bkpf-blart DEFAULT 'SA',              " Tipo documento giroconto
            p_rec_cs TYPE bseg-hkont DEFAULT 'M240700000',      " Conto recupero costi
            p_cdc    TYPE bseg-kostl DEFAULT 'EXXXXX',          " Centro di costo giroconto costi
            p_bsart  TYPE ekko-bsart DEFAULT 'ZFTE' NO-DISPLAY, " Tipo Ordine di acquisto
            p_ekorg  TYPE ekko-ekorg DEFAULT 'OPRE' NO-DISPLAY, " Organizzazione acquisti
            p_ekgrp  TYPE ekko-ekgrp DEFAULT '010'  NO-DISPLAY, " Gruppo acquisti
            p_waers  TYPE bkpf-waers DEFAULT 'EUR'  NO-DISPLAY, " Divisa
            p_meins  TYPE ekpo-meins DEFAULT 'ATT'  NO-DISPLAY, " Unità di misura
            p_rif_gc TYPE bkpf-xblnr DEFAULT 'GC COSTI/RICAVI' NO-DISPLAY. " Riferimento Giroconto
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
*       CLASS lcl_estrattore_fatture DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_estrattore_fatture DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.
    CLASS-METHODS initialization.
    CLASS-METHODS at_selection_screen RETURNING VALUE(rv_error) TYPE abap_bool.

    METHODS: handle_user_command
                FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm .

  PRIVATE SECTION.

    TYPES: ty_fattura_type TYPE yficms_estrat_fat_out_main,
           BEGIN OF ty_invoice,
             bukrs  TYPE bkpf-bukrs,
             belnr  TYPE bkpf-belnr,
             gjahr  TYPE bkpf-gjahr,
             errore TYPE char1,
           END OF ty_invoice,
           tt_invoice TYPE STANDARD TABLE OF ty_invoice.

    DATA: BEGIN OF ms_fattura.
            INCLUDE TYPE ty_fattura_type.
            DATA: knttp   TYPE ekpo-knttp,
            blocked TYPE char1.
    DATA END OF ms_fattura.

    TYPES: ty_fattura LIKE ms_fattura,
           tt_fattura TYPE TABLE OF ty_fattura,
           tr_prctr   TYPE RANGE OF cepc-prctr.

    CONSTANTS: mc_output_str TYPE dd02l-tabname VALUE 'YFICMS_ESTRAT_FAT_OUT_MAIN'.

    DATA: mt_fattura      TYPE tt_fattura,
          mt_logs         TYPE bapiret2_t,
          mo_grid         TYPE REF TO cl_gui_alv_grid,
          mo_top          TYPE REF TO cl_dd_document,
          mo_split_col    TYPE REF TO cl_gui_splitter_container,
          mo_top_cnt      TYPE REF TO cl_gui_container,
          mt_selected_doc TYPE tt_invoice,
          mv_registered   TYPE char1.

    METHODS extract_data.
    METHODS block_unblock_document IMPORTING iv_action TYPE char7.
    METHODS execute_ribaltamento.
    METHODS display_logs.
    METHODS enrich_data IMPORTING ir_prctr TYPE tr_prctr.
    METHODS start_container.
    METHODS set_fcat EXPORTING ev_fieldcat TYPE lvc_t_fcat.
    METHODS display_fattura.
    METHODS refresh_top_of_page.
    METHODS message_init_show IMPORTING iv_action    TYPE char4.
    METHODS message_store    IMPORTING iv_arbgb TYPE smesg-arbgb
                                       iv_msgty TYPE smesg-msgty
                                       iv_msg1  TYPE sy-msgv1
                                       iv_msg2  TYPE sy-msgv2
                                       iv_msg3  TYPE sy-msgv3
                                       iv_msg4  TYPE sy-msgv4
                                       iv_txtnr TYPE numc3.
    METHODS pop_up_info IMPORTING iv_title  TYPE string
                                  iv_text   TYPE string
                        CHANGING  cv_answer TYPE char01.
    METHODS: handle_toolbar
                FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object .
    METHODS top_of_page
        FOR EVENT top_of_page OF cl_gui_alv_grid.
ENDCLASS.                    "

DATA go_estrattore_fatture TYPE REF TO lcl_estrattore_fatture.
INCLUDE yficms_estrat_fat_pbo.
INCLUDE yficms_estrat_fat_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_estrattore_fatture IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_estrattore_fatture IMPLEMENTATION.

  METHOD execute.

    start_container( ).
    display_fattura( ).

    IF mt_fattura IS NOT INITIAL.
      CALL SCREEN 0100.
    ELSE.
      MESSAGE 'Nessun dato estratto' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.                    "execute

  METHOD block_unblock_document.

    DATA: lv_answer   TYPE char1.

    CASE iv_action.
      WHEN 'BLOCK'.
        DATA(lv_icon)    = EXACT icon-id( icon_led_red ).
        DATA(lv_xref_hd) = 'BLOCCATO'.
        DATA(lv_message) = 'ATTENZIONE: Il documento risulta bloccato per il ribaltamento'.
        DATA(lv_pop_up_message) = EXACT string('Si desidera procedere con il blocco delle fatture bloccate?').
      WHEN 'UNBLOCK'.
        lv_icon    =  icon_led_green.
        lv_xref_hd = ''.
        lv_message = 'OK – Documento ribaltabile'.
        lv_pop_up_message = 'Si desidera procedere togliere il blocco alle fatture selezionate. Continuare?'.
    ENDCASE.

    pop_up_info(
      EXPORTING
        iv_title  = 'ATTENZIONE'
        iv_text   = lv_pop_up_message
      CHANGING
        cv_answer = lv_answer ).

    IF lv_answer = 2.
      RETURN.
    ENDIF.

    LOOP AT mt_selected_doc ASSIGNING FIELD-SYMBOL(<ls_selected_doc>)
    GROUP BY ( bukrs = <ls_selected_doc>-bukrs
               belnr = <ls_selected_doc>-belnr
               gjahr = <ls_selected_doc>-gjahr ).

      SELECT SINGLE *
        FROM bkpf
        INTO @DATA(ls_bkpf_old)
        WHERE bukrs = @<ls_selected_doc>-bukrs
        AND   belnr = @<ls_selected_doc>-belnr
        AND   gjahr = @<ls_selected_doc>-gjahr.

      UPDATE bkpf SET xref2_hd = lv_xref_hd WHERE bukrs = <ls_selected_doc>-bukrs
                                            AND   belnr = <ls_selected_doc>-belnr
                                            AND   gjahr = <ls_selected_doc>-gjahr.
      IF sy-subrc <> 0.
        ROLLBACK WORK.
        MESSAGE 'Errore aggiornamento Blocco documento' TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      COMMIT WORK.

      DATA(lv_objectid) = EXACT cdhdr-objectid( sy-mandt && <ls_selected_doc>-bukrs && <ls_selected_doc>-belnr && <ls_selected_doc>-gjahr ).

      CALL FUNCTION 'CHANGEDOCUMENT_OPEN'
        EXPORTING
          objectclass      = 'BELEG'
          objectid         = lv_objectid
        EXCEPTIONS
          sequence_invalid = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      DATA(ls_bkpf_new) = ls_bkpf_old.
      ls_bkpf_new-xref2_hd = lv_xref_hd.

      CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
        EXPORTING
          change_indicator       = 'U'
          tablename              = 'BKPF'
          workarea_new           = ls_bkpf_new
          workarea_old           = ls_bkpf_old
        EXCEPTIONS
          nametab_error          = 1
          open_missing           = 2
          position_insert_failed = 3
          OTHERS                 = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'CHANGEDOCUMENT_CLOSE'
        EXPORTING
          date_of_change         = sy-datum
          objectclass            = 'BELEG'
          objectid               = lv_objectid
          tcode                  = sy-tcode
          time_of_change         = sy-uzeit
          username               = sy-uname
        EXCEPTIONS
          header_insert_failed   = 1
          no_position_inserted   = 2
          object_invalid         = 3
          open_missing           = 4
          position_insert_failed = 5
          OTHERS                 = 6.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      MODIFY mt_fattura FROM VALUE #( id = lv_icon xref2_hd = lv_xref_hd mess = lv_message )
      TRANSPORTING id mess xref2_hd WHERE bukrs = <ls_selected_doc>-bukrs
                                    AND   belnr = <ls_selected_doc>-belnr
                                    AND   gjahr = <ls_selected_doc>-gjahr.
    ENDLOOP.

  ENDMETHOD.                    "exclude_document

  METHOD execute_ribaltamento.
    DATA: lt_fat_rev   TYPE tt_invoice,
          lt_fat_imm   TYPE tt_invoice,
          lt_nc_rev    TYPE tt_invoice,
          lt_nc_imm    TYPE tt_invoice,
          lt_documents TYPE tt_invoice,
          lr_mwskz_rev TYPE RANGE OF bseg-mwskz.

    SELECT 'I'            AS sign,
           'EQ'           AS option,
           mwskz_acquisti AS low
      FROM yfcms_fatt_iva
      INTO CORRESPONDING FIELDS OF TABLE @lr_mwskz_rev
      WHERE reverse_charge = 'X'.

    LOOP AT mt_fattura ASSIGNING FIELD-SYMBOL(<fs>) WHERE xref2_hd IS INITIAL
    GROUP BY ( bukrs = <fs>-bukrs belnr = <fs>-belnr gjahr = <fs>-gjahr )
    ASSIGNING FIELD-SYMBOL(<ls_doc_group>).

      LOOP AT GROUP <ls_doc_group> ASSIGNING FIELD-SYMBOL(<ls_group_item>).
        READ TABLE lr_mwskz_rev TRANSPORTING NO FIELDS WITH KEY low = <ls_group_item>-mwskz.
        IF sy-subrc = 0 AND lr_mwskz_rev IS NOT INITIAL.
          DATA(lv_reverse_charg) = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF <ls_group_item> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.




    ENDLOOP.

  ENDMETHOD.                    "execute_ribaltamento

  METHOD display_logs.

    message_init_show( 'INIT' ).

    LOOP AT mt_logs ASSIGNING FIELD-SYMBOL(<ls_logs>).
      message_store(
        EXPORTING
          iv_arbgb = <ls_logs>-id
          iv_msgty = <ls_logs>-type
          iv_msg1  = <ls_logs>-message_v1
          iv_msg2  = <ls_logs>-message_v2
          iv_msg3  = <ls_logs>-message_v3
          iv_msg4  = <ls_logs>-message_v4
          iv_txtnr = <ls_logs>-number
      ).
    ENDLOOP.

    message_init_show( 'SHOW' ).

  ENDMETHOD.                    "display_logs

  METHOD handle_user_command.

    mo_grid->get_selected_rows(
      IMPORTING
        et_index_rows = DATA(lt_selected_rows) ).

    DATA(lv_selected_nr)  = lines( lt_selected_rows ).
    CLEAR mt_selected_doc.

    CASE e_ucomm.
      WHEN 'FC_EXCLUDE' OR 'FC_ANULLA'.

        IF lv_selected_nr = 0.
          MESSAGE 'Selezionare almeno una riga' TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF e_ucomm = 'FC_EXCLUDE'.
          DATA(lv_message) = EXACT string('Selezionare solo righe da escludere').
          DATA(lv_action)  = EXACT char7( 'BLOCK' ) .
          DATA(lv_icon)    = EXACT icon-id( icon_led_green ).
        ELSE.
          lv_message = 'Selezionare solo righe escluse per cui si vuole procedere con l’annullo'.
          lv_action  = 'UNBLOCK'.
          lv_icon    = icon_led_red.
        ENDIF.

        LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected_row>).
          READ TABLE mt_fattura ASSIGNING FIELD-SYMBOL(<ls_fattura>) INDEX <ls_selected_row>-index.
          IF sy-subrc = 0.
            IF <ls_fattura>-id <> lv_icon.
              MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
            APPEND VALUE #( bukrs = <ls_fattura>-bukrs belnr = <ls_fattura>-belnr gjahr = <ls_fattura>-gjahr ) TO mt_selected_doc.
          ENDIF.
        ENDLOOP.
        block_unblock_document( lv_action ).

      WHEN 'FC_RIBALTAMENTO'.

        IF mv_registered = abap_true.
          MESSAGE 'ATTENZIONE Registrazioni già avvenute' TYPE 'I'.
          RETURN.
        ENDIF.

        execute_ribaltamento( ).

      WHEN 'FC_LOGS'.

        IF mt_logs IS INITIAL.
          MESSAGE 'Nessun Log da riportare' TYPE 'I'.
          RETURN.
        ENDIF.
        display_logs( ).

    ENDCASE.

    mo_grid->refresh_table_display( ).
    refresh_top_of_page( ).
  ENDMETHOD.                    "handle_user_command

  METHOD enrich_data.

    CONSTANTS: lc_awtyp TYPE awtyp  VALUE 'RMRP',
               lc_land1 TYPE land1  VALUE 'IT',
               lc_datbi TYPE datbi  VALUE '99991231',
               lc_koart TYPE koart  VALUE 'K',
               lc_fat   TYPE char20 VALUE 'FATTURA',
               lc_ndc   TYPE char20 VALUE 'NOTA DI CREDITO'.

    DATA: lr_blart TYPE RANGE OF bkpf-blart.

    lr_blart = VALUE #( ( sign = 'I' option = 'EQ' low = p_td_fat )
                        ( sign = 'I' option = 'EQ' low = p_td_nc  ) ).
    SELECT *
      FROM cepct
      INTO TABLE @DATA(lt_cepct)
      WHERE spras = @sy-langu
      AND   datbi = @lc_datbi
      AND   kokrs = @p_integ.

    SELECT SINGLE kalsm
      FROM t005
      INTO @DATA(lv_kalsm)
      WHERE land1 = @lc_land1.

    SELECT *
      FROM t007s
      INTO TABLE @DATA(lt_t007s)
      WHERE spras = @sy-langu
      AND kalsm = @lv_kalsm.

    SELECT bukrs,
           belnr,
           gjahr,
           shkzg,
           lifnr
      FROM bseg
      INTO TABLE @DATA(lt_bseg_for)
      FOR ALL ENTRIES IN @mt_fattura
      WHERE bukrs = @mt_fattura-bukrs
      AND   belnr = @mt_fattura-belnr
      AND   gjahr = @mt_fattura-gjahr
      AND   koart = @lc_koart.

    SELECT bukrs,
           belnr,
           gjahr,
           buzei,
           prctr
      FROM bseg
      INTO TABLE @DATA(lt_all_bseg)
      FOR ALL ENTRIES IN @mt_fattura
      WHERE bukrs = @mt_fattura-bukrs
      AND   belnr = @mt_fattura-belnr
      AND   gjahr = @mt_fattura-gjahr
      AND   buzei = @mt_fattura-buzei.

    SELECT  prctr
      FROM cepc
      INTO TABLE @DATA(lt_cepc)
      FOR ALL ENTRIES IN @mt_fattura
      WHERE prctr IN @ir_prctr
      AND   prctr =  @mt_fattura-prctr.

    SELECT  lifnr,
            name1
      FROM lfa1
      INTO TABLE @DATA(lt_lfa1)
      FOR ALL ENTRIES IN @lt_bseg_for
      WHERE lifnr = @lt_bseg_for-lifnr.

    LOOP AT mt_fattura ASSIGNING FIELD-SYMBOL(<ls_fattura>).

      IF <ls_fattura>-awtyp = lc_awtyp AND <ls_fattura>-prctr IS INITIAL.
        READ TABLE lt_all_bseg ASSIGNING FIELD-SYMBOL(<ls_all_bseg>) WITH KEY bukrs = <ls_fattura>-bukrs
                                                                              belnr = <ls_fattura>-belnr
                                                                              gjahr = <ls_fattura>-gjahr
                                                                              buzei = <ls_fattura>-buzei.
        IF sy-subrc <> 0.
          <ls_fattura>-blocked = abap_true.
          CONTINUE.
        ENDIF.
        <ls_fattura>-prctr = <ls_all_bseg>-prctr.
      ENDIF.

      IF <ls_fattura>-awtyp = lc_awtyp AND <ls_fattura>-prctr IS NOT INITIAL.
        READ TABLE lt_cepc TRANSPORTING NO FIELDS WITH KEY prctr = <ls_fattura>-prctr.
        IF sy-subrc <> 0.
          <ls_fattura>-blocked = abap_true.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE lt_bseg_for ASSIGNING FIELD-SYMBOL(<ls_bseg_for>) WITH KEY bukrs = <ls_fattura>-bukrs
                                                                            belnr = <ls_fattura>-belnr
                                                                            gjahr = <ls_fattura>-gjahr.
      IF sy-subrc = 0.
        IF <ls_bseg_for>-shkzg = 'H'.
          <ls_fattura>-tipo = lc_fat.
        ELSE.
          <ls_fattura>-tipo = lc_ndc.
        ENDIF.

        <ls_fattura>-lifnr = <ls_bseg_for>-lifnr.

        READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<ls_lfa1>) WITH KEY lifnr = <ls_bseg_for>-lifnr.
        IF sy-subrc = 0.
          <ls_fattura>-rag_soc = <ls_lfa1>-name1.
        ENDIF.
      ENDIF.

      READ TABLE lt_t007s ASSIGNING FIELD-SYMBOL(<ls_t007s>) WITH KEY mwskz = <ls_fattura>-mwskz.
      IF sy-subrc = 0.
        <ls_fattura>-text1 = <ls_t007s>-text1.
      ENDIF.

      CONCATENATE <ls_fattura>-belnr <ls_fattura>-gjahr <ls_fattura>-buzei INTO DATA(lv_xref3) SEPARATED BY ''.
      CONDENSE lv_xref3.

      READ TABLE lt_cepct ASSIGNING FIELD-SYMBOL(<ls_cepc>) WITH KEY prctr = <ls_fattura>-prctr.
      IF sy-subrc = 0.
        <ls_fattura>-ltext = <ls_cepc>-ltext.
      ENDIF.

      SELECT SINGLE bkpf~belnr, bkpf~gjahr
        FROM bseg
        INNER JOIN bkpf
        ON  bkpf~bukrs = bseg~bukrs
        AND bkpf~belnr = bseg~belnr
        AND bkpf~gjahr = bseg~gjahr
        INTO @DATA(ls_exist)
        WHERE bkpf~bukrs = @<ls_fattura>-bukrs
        AND   bkpf~blart IN @lr_blart
        AND   bkpf~stblg = ''
        AND   bseg~xref3 = @lv_xref3.

      IF sy-subrc = 0.
        <ls_fattura>-id = icon_led_red.
        <ls_fattura>-mess = 'ERRORE: Il documento risulta ribaltato nella fattura attiva n. ' && ls_exist-belnr && '-' && ls_exist-gjahr.
      ELSE.
        IF <ls_fattura>-xref2_hd IS NOT INITIAL.
          <ls_fattura>-id = icon_led_red.
          <ls_fattura>-mess = 'ATTENZIONE: Il documento risulta bloccato per il ribaltamento'.
        ELSE.
          <ls_fattura>-id = icon_led_green.
          <ls_fattura>-mess = 'OK – Documento ribaltabile'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DELETE mt_fattura WHERE blocked = abap_true.
  ENDMETHOD.                    "enrich_data

  METHOD extract_data.

    DATA(lr_prctr) = VALUE tr_prctr( ( sign = 'I' option = 'CP' low = 'EO' && p_fondo && '*' ) ).

    SELECT b~bukrs,
           b~belnr,
           b~gjahr,
           s~buzei,
           b~budat,
           b~bldat,
           b~xblnr,
           s~ebeln,
           s~ebelp,
           s~hkont,
           t~txt50,
           s~prctr,
           CASE s~shkzg
             WHEN 'H' THEN s~wrbtr * -1
             WHEN 'S' THEN s~wrbtr END AS wrbtr,
           b~waers,
           s~mwskz,
           b~xref2_hd,
           b~awkey,
           b~awtyp,
           s~vbund

        FROM bkpf AS b

        INNER JOIN bseg AS s
             ON  s~bukrs = b~bukrs
             AND s~belnr = b~belnr
             AND s~gjahr = b~gjahr

        LEFT OUTER JOIN skat AS t
             ON  t~saknr = s~hkont
             AND t~ktopl = 'STD'
             AND t~spras = @sy-langu

             WHERE b~bukrs = @p_integ
              AND  b~blart IN @s_ft_int
              AND  b~budat IN @s_dt_reg
              AND  s~koart = 'S'
              ##UNCOMMENT
*              AND  s~prctr IN @lr_prctr
              AND  s~buzid <> 'T'
              AND  b~awkey <> 'RMRP'
             INTO CORRESPONDING FIELDS OF TABLE @mt_fattura.

    SELECT b~bukrs,
           b~belnr,
           b~gjahr,
           s~buzei,
           b~budat,
           b~bldat,
           b~xblnr,
           s~ebeln,
           s~ebelp,
           s~hkont,
           t~txt50,
           s~prctr,
           CASE s~shkzg
             WHEN 'H' THEN s~wrbtr * -1
             WHEN 'S' THEN s~wrbtr END AS wrbtr,
           b~waers,
           s~mwskz,
           b~xref2_hd,
           b~awkey,
           b~awtyp,
           s~vbund,
           p~matkl,
           p~knttp,
           p~txz01
        FROM bkpf AS b

        INNER JOIN bseg AS s
             ON  s~bukrs = b~bukrs
             AND s~belnr = b~belnr
             AND s~gjahr = b~gjahr

        LEFT OUTER JOIN skat AS t
             ON  t~saknr = s~hkont
             AND t~ktopl = 'STD'
             AND t~spras = @sy-langu

        LEFT OUTER JOIN ekkn AS n
             ON  n~ebeln = s~ebeln
             AND n~ebelp = s~ebelp
             AND n~zekkn = 1

        LEFT OUTER JOIN ekpo AS p
             ON  p~ebeln = s~ebeln
             AND p~ebelp = s~ebelp

             WHERE b~bukrs =  @p_integ
              AND  b~blart IN @s_ft_int
              AND  b~budat IN @s_dt_reg
              AND  s~koart = 'S'
              ##UNCOMMENT
*              AND  s~prctr IN @lr_prctr
              AND  s~buzid <> 'T'
             AND   b~awkey = 'RMRP'
             AND   n~prctr IN @lr_prctr

        APPENDING CORRESPONDING FIELDS OF TABLE @mt_fattura.

    IF mt_fattura IS INITIAL.
      RETURN.
    ENDIF.

    enrich_data( EXPORTING ir_prctr = lr_prctr ).

  ENDMETHOD.                    "extract_data

  METHOD display_fattura.

    extract_data( ).

    DATA(ls_layout)  = VALUE lvc_s_layo( sel_mode    = 'A' no_rowmark  = ''  edit = 0 cwidth_opt = abap_true ).
    DATA(ls_variant) = VALUE disvariant( report = sy-repid  username = sy-uname ).

    DATA lt_fcat TYPE lvc_t_fcat.

    set_fcat( IMPORTING ev_fieldcat       = lt_fcat ) .

    SET HANDLER : handle_toolbar
                  top_of_page
                  handle_user_command
                  FOR mo_grid.

    DATA(lt_toolbar_excluding) = VALUE ui_functions(
    ( '&LOCAL&CUT'       ) ( '&LOCAL&APPEND'    ) ( '&LOCAL&INSERT_ROW')
    ( '&LOCAL&DELETE_ROW') ( '&LOCAL&PASTE'     ) ( '&INFO'            )
    ( '&CHECK'           ) ( '&REFRESH'         ) ( '&GRAPH'           )
    ( '&LOCAL&COPY'      ) ( '&LOCAL&COPY_ROW'  ) ( '&LOCAL&UNDO'      ) ).

    mo_grid->set_table_for_first_display(
       EXPORTING
         i_save               = 'X'
         is_layout            = ls_layout
         is_variant           = ls_variant
         it_toolbar_excluding = lt_toolbar_excluding
       CHANGING
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_fattura ).

    mo_top->initialize_document( ).

    mo_grid->list_processing_events(
     EXPORTING
       i_event_name = 'TOP_OF_PAGE'
       i_dyndoc_id  = mo_top ).

  ENDMETHOD.                    "display_fattura

  METHOD start_container.

    DATA: lo_cont TYPE REF TO cl_gui_custom_container.

    IF mo_grid IS BOUND.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_cont
      EXPORTING
        container_name              = 'CUSTOM_CONT100'
        repid                       = sy-repid
        dynnr                       = '0100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT mo_split_col
      EXPORTING
        parent            = lo_cont
        rows              = 2
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    mo_split_col->set_row_sash( id    = 1
                                type  = cl_gui_splitter_container=>type_sashvisible
                                value = cl_gui_splitter_container=>false ).

    mo_split_col->set_row_height(
      EXPORTING
        id                = 1
        height            = 11
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    mo_top_cnt = mo_split_col->get_container( row = 1 column = 1 ).

    CREATE OBJECT mo_top
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent          = mo_split_col->get_container( row = 2 column = 1 )
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "start_container

  METHOD  refresh_top_of_page.
    mo_top->initialize_document( ).
    top_of_page( ).
  ENDMETHOD.                    "refresh_top_of_page

  METHOD handle_toolbar.
    DATA: ls_toolbar TYPE stb_button.

    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
        ( function = 'FC_EXCLUDE'      text = 'Escludere documenti'   icon = icon_interval_exclude_red     quickinfo = 'Escludere documenti' )
        ( function = 'FC_ANULLA'       text = 'Annulla esclusione'    icon = icon_interval_include_green   quickinfo = 'Annulla esclusione' )
        ( function = 'FC_RIBALTAMENTO' text = 'Effettua ribaltamento' icon = icon_execute_object           quickinfo = 'Effettua ribaltamento' )
        ( function = 'FC_LOGS'         text = 'Log dei ribaltamenti'  icon = icon_display_text             quickinfo = 'Log dei ribaltamenti' ) ).

  ENDMETHOD.    "handle_toolbar

  METHOD top_of_page.

    CONSTANTS: lc_color TYPE string VALUE '#E5E8E8'.

    DATA: lv_blocked TYPE i,
          lv_unblckd TYPE i.

    LOOP AT mt_fattura ASSIGNING FIELD-SYMBOL(<ls_fattura>).
      IF <ls_fattura>-id = icon_led_red.
        lv_blocked = lv_blocked + 1.
      ELSE.
        lv_unblckd = lv_unblckd + 1.
      ENDIF.
    ENDLOOP.

    DATA(lv_text) = CONV sdydo_text_element( |Fatture estrate : { lines( mt_fattura ) }| ).
    DATA(lv_cont) = CONV string( |<body style="background-color: { lc_color };  border: 1px solid gray; margin: 0; padding: 10px ">| &&
                                 |<font style = "font-size: 12pt; font-family:Sans-Serif; font-weight:bold; user-select:none color="black"> | &&
                                 lv_text && |</font><hr style=" margin-left: 0;">| ).
    DATA(lv_row) = EXACT i( 1 ).

    mo_top->html_insert(
      EXPORTING
        contents = lv_cont
      CHANGING
        position = lv_row ).

    lv_cont = |<font style = "font-size: 10pt; font-family:Sans-Serif; user-select:none" color="black"> N. fatture ribaltabile : </font>| &&
              |<font style = "font-size: 10pt; font-family:Sans-Serif; font-weight:bold; user-select:none" color="green">| &&
              lv_unblckd && |</font> &nbsp&nbsp&nbsp|.
    lv_row  = 2 .

    mo_top->html_insert(
      EXPORTING
        contents = lv_cont
      CHANGING
        position = lv_row ).

    lv_cont = |<font style = "font-size: 10pt; font-family:Sans-Serif; user-select:none" color="black"> N. fatture bloccate : </font>| &&
              |<font style = "font-size: 10pt; font-family:Sans-Serif; font-weight:bold; user-select:none" color="red">{ lv_blocked }</font></body>|.
    lv_row  = 3.

    mo_top->html_insert(
      EXPORTING
        contents = lv_cont
      CHANGING
        position = lv_row ).

    mo_top->merge_document( ).
    mo_top->display_document(
     EXPORTING
       reuse_control      = 'X'
       parent             = mo_top_cnt
     EXCEPTIONS
       html_display_error = 1
       OTHERS             = 2 ).

  ENDMETHOD.

  METHOD set_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = mc_output_str
      CHANGING
        ct_fieldcat            = ev_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      MESSAGE e000(db) WITH 'Errore tecnico interno Fieldcat'.
    ENDIF.

    LOOP AT ev_fieldcat ASSIGNING FIELD-SYMBOL(<fcat>).
      <fcat>-reptext = <fcat>-scrtext_m.
      CASE <fcat>-fieldname.
        WHEN 'ID'.
          <fcat>-icon = abap_true.
          MOVE 'Status' TO <fcat>-scrtext_s.
          MOVE 'Status' TO <fcat>-reptext.
        WHEN 'RAG_SOC'.
          MOVE text-t01 TO <fcat>-scrtext_s.
          MOVE text-t02 TO <fcat>-scrtext_m.
          MOVE text-t03 TO <fcat>-scrtext_l.
          MOVE text-t03 TO <fcat>-reptext.
        WHEN 'MESS'.
          MOVE text-t04 TO <fcat>-scrtext_s.
          MOVE text-t05 TO <fcat>-scrtext_m.
          MOVE text-t06 TO <fcat>-scrtext_l.
          MOVE text-t06 TO <fcat>-reptext.
        WHEN 'TIPO'.
          MOVE text-t07 TO <fcat>-scrtext_s.
          MOVE text-t08 TO <fcat>-scrtext_m.
          MOVE text-t09 TO <fcat>-scrtext_l.
          MOVE text-t09 TO <fcat>-reptext.
        WHEN 'EBELN_ZFTE'.
          MOVE text-t10 TO <fcat>-scrtext_s.
          MOVE text-t10 TO <fcat>-scrtext_m.
          MOVE text-t10 TO <fcat>-scrtext_l.
          MOVE text-t10 TO <fcat>-reptext.
        WHEN 'VBUND' OR 'XREF2_HD' OR 'AWTYP' OR 'AWKEY' OR 'MATKL' OR 'TXZ01' OR 'KNTTP'.
          <fcat>-no_out = abap_true.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD message_store.

    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = iv_arbgb
        msgty                  = iv_msgty
        msgv1                  = iv_msg1
        msgv2                  = iv_msg2
        msgv3                  = iv_msg3
        msgv4                  = iv_msg4
        txtnr                  = iv_txtnr
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2
        OTHERS                 = 3.

  ENDMETHOD.

  METHOD message_init_show.

    CASE iv_action.
      WHEN 'I'.
        CALL FUNCTION 'MESSAGES_INITIALIZE'
          EXCEPTIONS
            OTHERS = 1.
      WHEN 'S'.
        CALL FUNCTION 'MESSAGES_SHOW'
          EXPORTING
            i_use_grid         = 'X'
            batch_list_type    = 'L'
            send_if_one        = space
            show_linno         = ''
            show_linno_text    = space
          EXCEPTIONS
            inconsistent_range = 1
            no_messages        = 2
            OTHERS             = 3.
    ENDCASE.

  ENDMETHOD.

  METHOD pop_up_info.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = iv_text
        text_button_1         = 'SI'
        text_button_2         = 'NO'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = cv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD at_selection_screen.

    "Controllo dei campi obbligatori ===================================================================
    IF p_integ    IS INITIAL OR s_dt_reg[] IS INITIAL OR
       s_ft_int[] IS INITIAL OR p_fondo    IS INITIAL OR
       p_for_i    IS INITIAL OR p_cli_f    IS INITIAL OR
       p_dt_r     IS INITIAL OR p_td_fat   IS INITIAL OR
       p_td_nc    IS INITIAL OR p_td_gir   IS INITIAL OR
       p_rec_cs   IS INITIAL OR p_cdc      IS INITIAL .

      MESSAGE 'ERRORE: Tutti i campi devono essere valorizzati' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per codice Prelios Integra ===================================================================
    SELECT SINGLE @abap_true
      FROM t001
      INTO @DATA(lv_exist)
      WHERE bukrs = @p_integ.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Codice Prelios Integra non valorizzato correttamente' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per tipo documento Integra ===================================================================
    SELECT SINGLE @abap_true
      FROM t003
      INTO @lv_exist
      WHERE blart IN @s_ft_int.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Tipo documento Integra non corretto' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per Fondo SGR ===================================================================
    SELECT SINGLE @abap_true
      FROM t001
      INTO @lv_exist
      WHERE bukrs = @p_fondo.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Fondo SGR non valorizzato correttamente' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per fornitore Prelios Integra ===================================================================
    SELECT SINGLE sperr, sperm
      FROM lfa1
      INTO @DATA(ls_lfa1)
      WHERE lifnr = @p_for_i.

    IF sy-subrc <> 0.
      MESSAGE 'Errore: Fornitore Prelios Integra non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ELSEIF ls_lfa1-sperr = abap_true OR ls_lfa1-sperm = abap_true.
      MESSAGE 'Errore: Fornitore Prelios Integra bloccato' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE sperr
      FROM lfb1
      INTO @DATA(lv_sperr)
      WHERE lifnr = @p_for_i
      AND   bukrs = @p_fondo.

    IF sy-subrc <> 0.
      MESSAGE |Errore Fornitore Prelios Integra non aperto per la soc. { p_fondo }| TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ELSEIF lv_sperr = abap_true.
      MESSAGE |Errore: Fornitore Prelios Integra bloccato per la soc. { p_fondo }| TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE sperm
      FROM lfm1
      INTO @DATA(lv_sperm)
      WHERE lifnr = @p_for_i
      AND   ekorg = @p_ekorg.

    IF sy-subrc <> 0.
      MESSAGE 'Errore: Fornitore Prelios Integra non aperto lato MM' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ELSEIF lv_sperm = abap_true.
      MESSAGE 'Errore: Fornitore Prelios Integra bloccato per gli acquisti' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per cliente Fondo SGR ===================================================================
    SELECT SINGLE sperr
      FROM kna1
      INTO @DATA(ls_sperr)
      WHERE kunnr = @p_cli_f.

    IF sy-subrc <> 0.
      MESSAGE 'Errore: Cliente Fondo SGR non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ELSEIF ls_sperr = abap_true.
      MESSAGE 'Errore: Cliente Fondo SGR è bloccato per le registrazioni' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE sperr
      FROM knb1
      INTO @ls_sperr
      WHERE kunnr = @p_cli_f
      AND   bukrs = @p_integ.

    IF sy-subrc <> 0.
      MESSAGE |Errore: Cliente Fondo SGR non aperto per la soc. { p_integ }| TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ELSEIF lv_sperr = abap_true.
      MESSAGE 'Errore: Cliente Fondo SGR bloccato a livello societario' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per data registrazione documenti contabili ===================================================================
    IF p_dt_r > sy-datum.
      MESSAGE 'Attenzione: inserita una data registrazione futura' TYPE 'W'.
    ENDIF.

    "Controllo per tipo documento fattura attiva ===================================================================
    SELECT blart
      FROM t003
      INTO TABLE @DATA(lt_t003)
      WHERE blart IN ( @p_td_fat, @p_td_nc, @p_td_gir ).

    READ TABLE lt_t003 TRANSPORTING NO FIELDS WITH KEY blart = p_td_fat.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Tipo documento fattura attiva non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per tipo documento nota di credito attiva ===================================================================
    READ TABLE lt_t003 TRANSPORTING NO FIELDS WITH KEY blart = p_td_nc.
    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Tipo documento nota di credito attiva non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per tipo documento giroconto ===================================================================
    READ TABLE lt_t003 TRANSPORTING NO FIELDS WITH KEY blart = p_td_gir.
    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Tipo documento giroconto non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per conto recupero costi ===================================================================
    SELECT SINGLE xspeb
      FROM ska1
      INTO @DATA(lv_xspeb)
      WHERE ktopl = 'STD'
      AND   saknr = @p_rec_cs.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Conto recupero costi non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ELSEIF lv_xspeb = abap_true.
      MESSAGE 'Errore: Conto recupero costi bloccato per registrazioni' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE xspeb
      FROM skb1
      INTO @lv_xspeb
      WHERE saknr = @p_rec_cs
      AND   bukrs = @p_integ.

    IF sy-subrc <> 0.
      MESSAGE |Errore: Conto recupero costi non aperto per la soc. { p_integ }| TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ELSEIF lv_xspeb = abap_true.
      MESSAGE 'Errore: Conto recupero costi bloccato per le registrazioni' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per centro di costo giroconto costi ===================================================================
    SELECT SINGLE bkzkp
      FROM csks
      INTO @DATA(lv_bkzkp)
      WHERE kostl = @p_cdc
      AND   kokrs = @p_integ
      AND   datbi = '99991231'.

    IF sy-subrc <> 0.
      MESSAGE 'Errore: Centro di costo giroconto costi non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ELSEIF lv_bkzkp = abap_true.
      MESSAGE 'Errore: Centro di costo giroconto costi bloccato' TYPE 'S' DISPLAY LIKE 'E'.
      rv_error = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.                    "at_selection_screen

  METHOD initialization.

    DATA: lc_pas_fatt TYPE tvarvc-name VALUE 'YFICMS_FACILITY_PAS_FATT', "fattura attiva
          lc_pas_nc   TYPE tvarvc-name VALUE 'YFICMS_FACILITY_PAS_NC'.   "nota di credito

    SELECT name,
           low
      FROM tvarvc
      INTO TABLE @DATA(lt_tvarvc)
      WHERE name IN ( @lc_pas_fatt, @lc_pas_nc ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "Read per tipo documento fattura attiva
    READ TABLE lt_tvarvc ASSIGNING FIELD-SYMBOL(<ls_tvarvc>) WITH KEY name = lc_pas_fatt.
    IF sy-subrc = 0.
      p_td_fat = <ls_tvarvc>-low.
    ENDIF.

    "Read per tipo documento nota di credito attiva
    READ TABLE lt_tvarvc ASSIGNING <ls_tvarvc> WITH KEY name = lc_pas_nc.
    IF sy-subrc = 0.
      p_td_nc = <ls_tvarvc>-low.
    ENDIF.

  ENDMETHOD.                    "initialization

ENDCLASS.

INITIALIZATION.
  lcl_estrattore_fatture=>initialization( ).

AT SELECTION-SCREEN.
  IF lcl_estrattore_fatture=>at_selection_screen( ) = abap_true .
    STOP.
  ENDIF.

START-OF-SELECTION.
  CREATE OBJECT go_estrattore_fatture.
  go_estrattore_fatture->execute( ).
