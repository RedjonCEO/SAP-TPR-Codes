*&---------------------------------------------------------------------*
*&  Include           ZRD_ALV_ODATA_IMP
*&---------------------------------------------------------------------*
CLASS lcl_alv_display_odata IMPLEMENTATION.

  METHOD execute.

    CASE abap_true.
      WHEN p_post.
        post_method( ).
      WHEN OTHERS.
        get_method( ).
    ENDCASE.

  ENDMETHOD.

  METHOD get_method.
    DATA: lv_odata_link  TYPE string,
          lo_http_client TYPE REF TO if_http_client,
          lv_response    TYPE string.

    CASE abap_true.
      WHEN p_exp.
        lv_odata_link = 'http://IDES03.crystal-system.ro:8000/sap/opu/odata/sap/ZRD_ODATA_TRY_SRV/zrd_headerSet?$expand=ToMaraNav,ToMaktNav&$format=json'.
      WHEN OTHERS.
        lv_odata_link = 'http://IDES03.crystal-system.ro:8000/sap/opu/odata/SAP/ZRD_ODATA_TRY_SRV/zrd_headerSet?$format=json'.
    ENDCASE.

    cl_http_client=>create_by_url(
      EXPORTING
        url = lv_odata_link
      IMPORTING
        client = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active = 2
        internal_error = 3
      OTHERS = 4 ).

    lo_http_client->authenticate( EXPORTING username = 'STUD29' password = '199909' ).

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2.

    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    lv_response = lo_http_client->response->get_cdata( ).

    display_tables( iv_response =  lv_response ).

  ENDMETHOD.

  METHOD display_tables.

    TYPES: BEGIN OF ty_data,
             BEGIN OF d,
               results TYPE STANDARD TABLE OF ty_deserialize WITH EMPTY KEY,
             END OF d,
           END OF ty_data.

    DATA: lt_data        TYPE  ty_data,
          lt_mara        TYPE STANDARD TABLE OF ty_mara,
          lt_makt        TYPE STANDARD TABLE OF ty_makt,
          lt_header_fcat TYPE slis_t_fieldcat_alv,
          lt_mara_fcat   TYPE slis_t_fieldcat_alv,
          lt_makt_fcat   TYPE slis_t_fieldcat_alv.

    /ui2/cl_json=>deserialize( EXPORTING
                                json = iv_response
                                pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                CHANGING
                                  data = lt_data ).

    LOOP AT lt_data-d-results ASSIGNING FIELD-SYMBOL(<fs_data>).
      APPEND <fs_data>-tomaranav TO lt_mara.
      LOOP AT <fs_data>-tomaktnav-results ASSIGNING FIELD-SYMBOL(<fs_makt>).
        APPEND <fs_makt> TO lt_makt.
      ENDLOOP.
    ENDLOOP.

    CALL METHOD convert_timestamp(
      CHANGING
        ct_time_table = lt_data-d-results ).

    CALL METHOD create_fieldcatalog(
      CHANGING
        ct_fieldcat_header = lt_header_fcat
        ct_fieldcat_makt   = lt_makt_fcat
        ct_fieldcat_mara   = lt_mara_fcat ).

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
      EXPORTING
        i_callback_program = sy-cprog.

    DATA ls_layout TYPE slis_layout_alv.
    DATA lt_events TYPE slis_t_event.

    ls_layout-colwidth_optimize = abap_true.

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = ls_layout
        it_fieldcat                = lt_header_fcat
        i_tabname                  = 'Header'
        it_events                  = lt_events
      TABLES
        t_outtab                   = lt_data-d-results
      EXCEPTIONS
        program_error              = 1
        maximum_of_appends_reached = 2
        OTHERS                     = 3.

    IF p_exp = abap_true.

      CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
        EXPORTING
          is_layout                  = ls_layout
          it_fieldcat                = lt_mara_fcat
          i_tabname                  = 'Mara'
          it_events                  = lt_events
        TABLES
          t_outtab                   = lt_mara
        EXCEPTIONS
          program_error              = 1
          maximum_of_appends_reached = 2
          OTHERS                     = 3.

      CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
        EXPORTING
          is_layout                  = ls_layout
          it_fieldcat                = lt_makt_fcat
          i_tabname                  = 'Makt'
          it_events                  = lt_events
        TABLES
          t_outtab                   = lt_makt
        EXCEPTIONS
          program_error              = 1
          maximum_of_appends_reached = 2
          OTHERS                     = 3.

    ENDIF.

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'.

  ENDMETHOD.

  METHOD create_fieldcatalog.

    ct_fieldcat_header = VALUE #(
           ( fieldname = 'MATNR'          seltext_m = 'Material' key = abap_true )
           ( fieldname = 'QUANTITY'       seltext_m = 'Quantity' )
           ( fieldname = 'TIMESTAMP'      seltext_l = 'Date Time'     )
           ( fieldname = 'DELIND'         seltext_m = 'Deleted'  checkbox = abap_true ) ).

    ct_fieldcat_makt = VALUE #(
           ( fieldname = 'MATNR'          seltext_m = 'Material'  key = abap_true    )
           ( fieldname = 'SPRAS'          seltext_m = 'Language'  key = abap_true    )
           ( fieldname = 'MAKTX'          seltext_m = 'Description'  )
           ( fieldname = 'MAKTG'          seltext_m = 'Group'        ) ).

    ct_fieldcat_mara = VALUE #(
           ( fieldname = 'MATNR'          seltext_m = 'Material'   key = abap_true   )
           ( fieldname = 'ERNAM'          seltext_m = 'Created by'    )
           ( fieldname = 'MTART'          seltext_m = 'Material ART'  )
           ( fieldname = 'MATKL'          seltext_m = 'Material class')
           ( fieldname = 'NTGEW'          seltext_m = 'Net weight'    ) ).

  ENDMETHOD.


  METHOD post_method.

    TYPES: BEGIN OF ty_serializer,
             mandt     TYPE string,
             matnr     TYPE string,
             quantity  TYPE string,
             timestamp TYPE string,
             delind    TYPE string,
           END OF ty_serializer,
           tt_serializer TYPE TABLE OF ty_serializer.

    TYPES: BEGIN OF ty_post_data,
             d TYPE  ty_serializer,
           END OF ty_post_data.

    DATA: lo_client            TYPE REF TO  if_http_client,
          lo_response          TYPE REF TO  if_rest_entity,
          lv_response          TYPE string,
          lv_token             TYPE string,
          lv_json              TYPE string,
          lv_session           TYPE string,
          lt_post_data         TYPE  ty_post_data,
          lv_timestamp         TYPE timestamp,
          gc_content_type_form TYPE string VALUE 'application/json; charset=utf-8',
          gc_url_get           TYPE string VALUE 'http://IDES03.crystal-system.ro:8000/sap/opu/odata/sap/ZRD_ODATA_TRY_SRV/zrd_headerSet?$format=json',
          gc_url_post          TYPE string VALUE 'http://IDES03.crystal-system.ro:8000/sap/opu/odata/sap/ZRD_ODATA_TRY_SRV/zrd_headerSet'.

    GET TIME STAMP FIELD lv_timestamp.
    lt_post_data-d = VALUE #( mandt = sy-mandt matnr = 'T-ATU27'
    quantity = 585 timestamp = '\/Date(1716282605000)\/' delind = '') .

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = lt_post_data
      RECEIVING
        r_json = lv_json.

    REPLACE 'D'         WITH 'd'         INTO lv_json.
    REPLACE 'MANDT'     WITH 'Mandt'     INTO lv_json.
    REPLACE 'MATNR'     WITH 'Matnr'     INTO lv_json.
    REPLACE 'QUANTITY'  WITH 'Quantity'  INTO lv_json.
    REPLACE 'TIMESTAMP' WITH 'Timestamp' INTO lv_json.
    REPLACE 'DELIND'    WITH 'DelInd'    INTO lv_json.

    cl_http_client=>create_by_url(
        EXPORTING
          url     = gc_url_post
        IMPORTING
          client  = lo_client
        EXCEPTIONS
          OTHERS  = 4 ).

    lo_client->authenticate( username = 'STUD29' password = '199909' ).
    lo_client->request->set_header_field( name = 'X-CSRF-Token' value = 'Fetch'   ).
    lo_client->propertytype_accept_cookie = if_http_client=>co_enabled.

    lo_client->send(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5  ).

    lo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    lv_token    = lo_client->response->get_header_field('X-CSRF-Token').
    lv_session  = lo_client->response->get_header_field('set-cookie').

    lo_client->request->set_content_type( content_type = gc_content_type_form ).
    lo_client->request->set_method( if_http_request=>co_request_method_post ).
    lo_client->authenticate( username =  'STUD29' password =  '199909' ).
    lo_client->request->set_header_field( name  = 'X-CSRF-Token' value = lv_token  ).
    lo_client->request->set_form_field( name = 'Cookie'  value = lv_session   ).

    CALL METHOD lo_client->request->set_cdata
      EXPORTING
        data = lv_json.

    lo_client->send(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5 ).

    lo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    lv_response = lo_client->response->get_cdata( ).
    cl_demo_output=>display_xml( lv_response ).

  ENDMETHOD.


  METHOD convert_timestamp.

    DATA: lv_timestamp_msec TYPE string,
          lv_date           TYPE datum,
          lv_time           TYPE uzeit,
          lv_timezone       TYPE timezone,
          lv_timestamp      TYPE timestamp.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE' IMPORTING timezone = lv_timezone.

    LOOP AT ct_time_table ASSIGNING FIELD-SYMBOL(<fs_row>).

      lv_timestamp_msec = <fs_row>-timestamp+7(13).

      cl_pco_utility=>convert_java_timestamp_to_abap(
        EXPORTING
          iv_timestamp = lv_timestamp_msec
        IMPORTING
          ev_date      = lv_date
          ev_time      = lv_time ).

      CONCATENATE lv_date  lv_time INTO <fs_row>-timestamp  SEPARATED BY space.
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
