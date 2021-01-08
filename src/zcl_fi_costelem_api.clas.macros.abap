*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE _mac_raise_multi.
  DELETE ADJACENT DUPLICATES FROM &1 COMPARING ALL FIELDS.
  &1 = zcl_com_bapireturn_services=>add_errors_to_bapiret( &1 ).
  LOOP AT &1 INTO return.
   MESSAGE ID return-id TYPE return-type NUMBER return-number
        WITH  return-message_v1 return-message_v2 return-message_v3 return-message_v4
        INTO  dummy.
   AT FIRST.
    me->lo_exception = NEW #( ).
   ENDAT.
   me->lo_exception->add_sy_message( ).
   AT LAST.
    RAISE RESUMABLE EXCEPTION me->lo_exception.
   ENDAT.
  ENDLOOP.
END-OF-DEFINITION.
