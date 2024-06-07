 METHOD retrieve_data.

    DATA: lt_prod_data TYPE zrd_aufk_table,
          lv_dbcur     TYPE cursor.

    OPEN CURSOR @lv_dbcur
    FOR SELECT a~werks,
               a~auart,
               a~aufnr,
               a~erdat,
               a~aenam,
               p~matnr,
               a~loekz
        FROM aufk AS a
        JOIN afpo AS p
        ON p~aufnr = a~aufnr
        WHERE a~werks IN @s_werks
        AND   a~auart IN @s_auart
        AND   a~erdat IN @s_erdat
        AND   a~aenam IN @s_aenam
        AND   a~autyp IN @s_autyp.

    DO.
      FETCH NEXT CURSOR @lv_dbcur
        INTO TABLE @lt_prod_data PACKAGE SIZE @p_packet.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      CALL FUNCTION 'ZPP_INT_PROD_QUALIWARE' DESTINATION 'NONE'
        EXPORTING
          it_transferring_table = lt_prod_data.
    ENDDO.
    CLOSE CURSOR: @lv_dbcur.
  ENDMETHOD.
