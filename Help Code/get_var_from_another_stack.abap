DATA: lv_active  TYPE i,
      lr_gl_type TYPE RANGE OF ska1-glaccount_type.

FIELD-SYMBOLS <lt_coblf> TYPE ANY TABLE.

MOVE-CORRESPONDING i_cobl TO e_cobl_cust.

CASE sy-tcode.
  WHEN 'FB01' OR 'FB02' OR 'FB03'.
    " ok
  WHEN OTHERS.
    RETURN.
ENDCASE.

* Custom fields are on screen 9999 - if it's another one, we skip the logic
* Here is how to get the variable from anbother stack

* ASSIGN ('(~programm/include/whatever_name~)varaible_name') TO FIELD-SYMBOL(<lv_dynnr>).
* IF <lv_dynnr> IS NOT ASSIGNED OR <lv_dynnr> <> 9999.
*   RETURN.
* ENDIF.

ASSIGN ('(SAPLKACB)FSSUB_DYNNR') TO FIELD-SYMBOL(<lv_dynnr>).
IF <lv_dynnr> IS NOT ASSIGNED OR <lv_dynnr> <> 9999.
  RETURN.
ENDIF.

* GT_COBLF has the entries for LOOP AT SCREEN
ASSIGN ('(SAPLKACB)GT_COBLF') TO <lt_coblf>.
IF <lt_coblf> IS NOT ASSIGNED.
  RETURN.
ENDIF.

SELECT CASE WHEN sign = ' ' THEN 'I'  ELSE sign END AS sign,
       CASE WHEN opti = ' ' THEN 'EQ' ELSE opti END AS option   ,
       low,
       high
  FROM tvarvc
  INTO TABLE @lr_gl_type
  WHERE name = 'ZGLACCOUNT_TYPE'
    AND type = 'S'.

* get the active field from GL Account + Type
SELECT SINGLE 1
  FROM ska1
  INTO @lv_active
  WHERE ktopl          =  'VIMO'
    AND saknr          =  @i_cobl-hkont
    AND glaccount_type IN @lr_gl_type.

* Display or not based on the value retrieved from select
LOOP AT <lt_coblf> ASSIGNING FIELD-SYMBOL(<ls_coblf>)
  WHERE (` ( FDNAM = 'ZVALITY_FROM' or FDNAM = 'ZVALITY_TO' ) AND active = 1`).

  ASSIGN COMPONENT 'ACTIVE' OF STRUCTURE <ls_coblf> TO FIELD-SYMBOL(<lv_active>).
  <lv_active> = lv_active.

ENDLOOP.
