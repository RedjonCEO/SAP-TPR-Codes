INCLUDE zrd_alv_odata_top.
INCLUDE zrd_alv_odata_imp.

START-OF-SELECTION.

go_alv_odata = NEW lcl_alv_display_odata( ).
go_alv_odata->execute( ).
