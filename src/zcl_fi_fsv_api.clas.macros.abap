*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE _mac_raise.
  me->lo_exception = NEW #( gv_symsg = CORRESPONDING #( sy ) ).
  me->lo_exception->add_sy_message( ).
  RAISE RESUMABLE EXCEPTION me->lo_exception.
END-OF-DEFINITION.
