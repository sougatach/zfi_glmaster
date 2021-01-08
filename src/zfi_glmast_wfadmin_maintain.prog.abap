*&---------------------------------------------------------------------*
*& Report zfi_glmast_wfadmin_maintain
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_glmast_wfadmin_maintain.

*************************************************************************
* Author: Sougata Chatterjee (11034616 - SAP Support WF Administrator)
* Date:   28/10/2020
* Reason: WF Administrator Tool to maintain GL master data when WF fails
*************************************************************************

PARAMETERS p_notif TYPE qmel-qmnum.

TRY.
    cl_identity_factory=>retrieve(
      EXPORTING
        it_bname                = VALUE #( ( bname = sy-uname ) )
        iv_enqueue_mode         =  if_identity=>co_enqueue_mode_no_lock
      IMPORTING
        et_node_root            = DATA(lt_node)
        et_bname_not_authorized = DATA(lt_no_auth)
    ).

    CHECK lt_no_auth IS INITIAL AND
          lt_node IS NOT INITIAL AND lt_node[ 1 ]-idref IS BOUND.   "not doing any further exception handling

    lt_node[ 1 ]-idref->get_roles( IMPORTING et_roles = DATA(lt_roles) ).

    DATA(wfsupport) = VALUE #( lt_roles[ agr_name = |YEC.SSG.WF.SUPPORT| ] OPTIONAL ).

    CHECK ( wfsupport IS NOT INITIAL AND
            sy-datum BETWEEN wfsupport-from_dat AND wfsupport-to_dat )
      OR sy-uname    = |11034616|.         "WF Admin -> (Sougata Chatterjee)

    DATA(return) = zcl_swf_glmast=>get_instance( p_notif )->maintain_masterdata( ).
    NEW zcl_swf_bus7051( p_notif )->changestatus( CONV #( |PMM4| ) ).       " Business Transaction

  CATCH cx_suid_identity cx_bo_instance_not_found zcx_fi_general zcx_swf_status_notification INTO DATA(lo_exception).
    return-type = if_suid_msg_buffer=>co_type_error.
ENDTRY.

cl_demo_output=>display( SWITCH #( return-type
                           WHEN if_suid_msg_buffer=>co_type_error
                           THEN lo_exception->get_text( )
                           ELSE |Update was successful|
                                 )
                       ).
