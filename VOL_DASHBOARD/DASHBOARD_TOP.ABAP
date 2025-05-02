*&---------------------------------------------------------------------*
*&  Include           ZLPBNS_PROMO_MD_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: icon, esp1.

TABLES: zlvsd_link_defau,
        zlpbns_vol_MDH.

TYPES: BEGIN OF ty_tabstrip_header,
         name     TYPE screen-name,
         data_tab TYPE char24,
         empty    TYPE char50,
         full     TYPE char50,
       END OF ty_tabstrip_header.

CONSTANTS: BEGIN OF gc_state,
             new  TYPE string VALUE 'NEW',
             edit TYPE string VALUE 'EDIT',
             view TYPE string VALUE 'VIEW',
             copy TYPE string VALUE 'COPY',
           END OF gc_state.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TS_BONUS'
CONSTANTS: BEGIN OF C_TS_BONUS,
             TAB1 LIKE SY-UCOMM VALUE 'FC_MODEL',
             TAB2 LIKE SY-UCOMM VALUE 'FC_TARGET',
             TAB3 LIKE SY-UCOMM VALUE 'FC_INCENT',
             TAB4 LIKE SY-UCOMM VALUE 'FC_VIN',
             TAB5 LIKE SY-UCOMM VALUE 'FC_SALE',
           END OF C_TS_BONUS.
*&SPWIZARD: DATA FOR TABSTRIP 'TS_BONUS'
CONTROLS:  TS_BONUS TYPE TABSTRIP.
DATA:      BEGIN OF G_TS_BONUS,
             SUBSCREEN   LIKE SY-DYNNR,
             PROG        LIKE SY-REPID VALUE 'ZLPBNS_VOL_DASH_MD',
             PRESSED_TAB LIKE SY-UCOMM VALUE C_TS_BONUS-TAB1,
           END OF G_TS_BONUS.

DATA: BEGIN OF g_ts_promo,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZLPBNS_VOL_DASH_MD',
        pressed_tab LIKE sy-ucomm VALUE C_TS_BONUS-tab1,
      END OF g_ts_promo.

DATA: ts_bonus_tab1      TYPE char30,
      ts_bonus_tab2      TYPE char30,
      ts_bonus_tab3      TYPE char30,
      ts_bonus_tab4      TYPE char30,
      ts_bonus_tab5      TYPE char30,
      gt_tabstrip_header TYPE STANDARD TABLE OF ty_tabstrip_header.

DATA: gt_status_text TYPE STANDARD TABLE OF zlpbns_pr_stat_t,
      gv_locked  TYPE char1.
