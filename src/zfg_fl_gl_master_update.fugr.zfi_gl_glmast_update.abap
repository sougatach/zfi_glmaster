FUNCTION ZFI_GL_GLMAST_UPDATE.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_NOTIFICATION) TYPE  QMEL-QMNUM
*"  EXCEPTIONS
*"      EX_UPDATE_ERROR
*"----------------------------------------------------------------------
*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

  TRY.
      zcl_swf_glmast=>get_instance( iv_notification )->maintain_masterdata( iv_update_task = abap_false ).

    CATCH cx_bo_instance_not_found INTO DATA(lo_bo_exception).
      MESSAGE e098 WITH lo_bo_exception->get_text( ) RAISING ex_update_error.

    CATCH zcx_fi_general INTO DATA(lo_general_exception).
      READ TABLE lo_general_exception->get_messages( ) INDEX 1 INTO DATA(return).
      MESSAGE e098 WITH return-message RAISING ex_update_error.
  ENDTRY.

ENDFUNCTION.
