*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE _mac_add_msg.
  zcl_com_bapireturn_services=>add_message_to_bapiret(
     EXPORTING
       iv_type       = sy-msgty          " Message type: S Success, E Error, W Warning, I Info, A Abort
       iv_id         = sy-msgid          " Message Class
     CHANGING
       ct_return     = &1                " Table with BAPI Return Information
   ).
   &2 = zcl_com_bapireturn_services=>add_msg_to_str1( ).     "Return structure (required by some Methods)
END-OF-DEFINITION.

DEFINE _mac_check_field.
  IF NOT me->get_fldvalue( &1 ).
   MESSAGE &2 WITH &3 &4 INTO me->l_dummy.
   _mac_add_msg me->lt_messages me->ls_return.
  ENDIF.
END-OF-DEFINITION.

DEFINE _mac_resume_if_resumable.
  IF &1 = abap_true.
   RESUME.
  ELSE.
   CHECK me->get_fldvalue( |REQUEST_TYPE| ) BETWEEN 1 AND 2.
   me->lo_exception = NEW #( gv_symsg = CORRESPONDING #( sy ) ).
   me->lo_exception->add_sy_message( ).
   RAISE EXCEPTION me->lo_exception.
  ENDIF.
END-OF-DEFINITION.
