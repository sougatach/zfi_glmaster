FUNCTION-POOL zfg_bapi_glmaster MESSAGE-ID zfi.

* INCLUDE LZFG_BAPI_GLMASTERD...             " Local class definition

DEFINE _mac_add_return.
  zcl_com_bapireturn_services=>add_message_to_bapiret(
   EXPORTING
      iv_id         = sy-msgid
      iv_message    = CONV #( sy-msgv1 )
    CHANGING
      ct_return     = &1
  ).
END-OF-DEFINITION.

DEFINE _mac_add_sy_return.
  zcl_com_bapireturn_services=>add_msg_to_itab(
    CHANGING
      ct_return     = &1                " Table with BAPI Return Information
  ).
END-OF-DEFINITION.
