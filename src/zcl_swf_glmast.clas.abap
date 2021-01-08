CLASS zcl_swf_glmast DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES if_workflow.

    "! <p class="shorttext synchronized" lang="en">Singleton</p>
    "!
    "! @parameter iv_notification | <p class="shorttext synchronized" lang="en">Key: Notification Number</p>
    "! @parameter ro_result | <p class="shorttext synchronized" lang="en">Returns Instance of Object ZCL_SWF_GLMAST</p>
    CLASS-METHODS get_instance
      IMPORTING
                iv_notification  TYPE qmnum
      RETURNING VALUE(ro_result) TYPE REF TO zcl_swf_glmast
      RAISING   cx_bo_instance_not_found.

    "! <p class="shorttext synchronized" lang="en">Factory Create</p>
    "!
    "! @parameter iv_notification | <p class="shorttext synchronized" lang="en">Key: Notification Number</p>
    "! @parameter ro_result | <p class="shorttext synchronized" lang="en">Returns Instance of Object ZCL_SWF_GLMAST</p>
    CLASS-METHODS create
      IMPORTING
                iv_notification  TYPE qmnum
      RETURNING VALUE(ro_result) TYPE REF TO zcl_swf_glmast
      RAISING   cx_bo_instance_not_found.

    "! <p class="shorttext synchronized" lang="en">Is the Notification Complete?</p>
    "!
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">Complete = True</p>
    METHODS is_complete RETURNING VALUE(r_result) TYPE boole_d.

    "! <p class="shorttext synchronized" lang="en">Get value of a Characteristic for the ISR Form instance</p>
    "!
    "! @parameter iv_fieldname | <p class="shorttext synchronized" lang="en">Name of the Characteristic</p>
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">Returns value of the Characteristic</p>
    METHODS get_value
      IMPORTING !iv_fieldname   TYPE qisrdfieldname
      RETURNING VALUE(r_result) TYPE qisrdfieldvalue.

    "! <p class="shorttext synchronized" lang="en">Get all Characteristics for the ISR Form instance</p>
    "!
    "! @parameter rt_results | <p class="shorttext synchronized" lang="en">ISR Special Data</p>
    METHODS get_value_all
      RETURNING VALUE(rt_results) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Maintain Master Data</p>
    "!
    "! @parameter iv_mode | <p class="shorttext synchronized" lang="en">ISR Mode (Optional)</p>
    "! @parameter iv_view | <p class="shorttext synchronized" lang="en">ISR View (Optional)</p>
    "! @parameter iv_update_task | <p class="shorttext synchronized" lang="en">Update Task? (default 'X')</p>
    "! @parameter rt_result | <p class="shorttext synchronized" lang="en">Result of Master data maintenance</p>
    "! @raising cx_bo_instance_not_found | <p class="shorttext synchronized" lang="en">BO Exception Object</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized" lang="en">Custom Exception Object</p>
    METHODS maintain_masterdata
      IMPORTING VALUE(iv_update_task) TYPE boole_d       DEFAULT abap_true
      RETURNING VALUE(rs_result)      TYPE bapiret2
      RAISING   cx_bo_instance_not_found zcx_fi_general.

    "! <p class="shorttext synchronized" lang="en">Get Approvers for GL Master Data Maintenance</p>
    "!
    "! @parameter iv_bukrs | <p class="shorttext synchronized" lang="en">Company Code (Default 1020)</p>
    "! @parameter iv_level | <p class="shorttext synchronized" lang="en">Approval Level</p>
    "! @parameter rt_results | <p class="shorttext synchronized" lang="en">List of Approver Agents</p>
    METHODS get_approvers
      IMPORTING iv_bukrs          TYPE bukrs DEFAULT zif_fi_global_constants=>comp_code_tafe
                iv_level          TYPE int4
      RETURNING VALUE(rt_results) TYPE tswhactor
      RAISING   zcx_swf_no_agent_found.

    "! <p class="shorttext synchronized" lang="en">Get GL Account Description for WF Binding</p>
    "!
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">GL Account Text</p>
    METHODS get_description
      RETURNING VALUE(r_result) TYPE qisrdfieldvalue.
    "! <p class="shorttext synchronized" lang="en">Get Approver Level</p>
    METHODS get_apprv_lvl RETURNING VALUE(r_result) TYPE int4.
    "! <p class="shorttext synchronized" lang="en">Get GL Account Number</p>
    METHODS get_glaccount RETURNING VALUE(r_result) TYPE saknr.
    "! <p class="shorttext synchronized" lang="en">Get Request Type Text</p>
    METHODS get_request_text IMPORTING iv_req_type     TYPE i OPTIONAL
                             RETURNING VALUE(r_result) TYPE stext.

    "! <p class="shorttext synchronized" lang="en">Get list of Email addresses from Customising</p>
    "!
    "! @parameter i_on_success | <p class="shorttext synchronized" lang="en">True = Success list; False = Error list</p>
    "! @parameter rt_result | <p class="shorttext synchronized" lang="en">Table of Email Addresses from Customising</p>
    "! @raising zcx_fi_general | Common Exception Object (if Raised then maintain Email Entries)
    METHODS get_email_list  IMPORTING VALUE(i_on_success) TYPE boole_d DEFAULT abap_true
                            RETURNING VALUE(rt_result)    TYPE safm_apt_pp_email
                            RAISING zcx_fi_general.

    "! <p class="shorttext synchronized" lang="en">Update of Master Data Failed?</p>
    "!
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">True = Failed; False = Success</p>
    METHODS is_update_error RETURNING VALUE(r_result) TYPE boole_d.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA lo_swf_glmast  TYPE REF TO zcl_swf_glmast.

    DATA lpor           TYPE sibflpor.
    DATA scenario       TYPE qisrdscenario.
    DATA key            TYPE qmnum.
    DATA lt_special     TYPE qisrtspecial_param.
    DATA plvar          TYPE plvar.
    DATA api_mode       TYPE glaccount_action.
    DATA apprv_lvl      TYPE int4.
    DATA dummy          TYPE c.
    DATA return         TYPE bapiret2 .
    DATA o_exception    TYPE REF TO zcx_fi_general .

    "! <p class="shorttext synchronized" lang="en">Object Constructor</p>
    "!
    "! @parameter iv_key | <p class="shorttext synchronized" lang="en">Key: Notification Number</p>
    METHODS constructor
      IMPORTING
                iv_notification TYPE qmnum
      RAISING   cx_bo_instance_not_found.

    "! <p class="shorttext synchronized" lang="en">Get ISR Form data</p>
    METHODS get_form_data
      RETURNING
                VALUE(rt_result) TYPE qisrtspecial_param
      RAISING   cx_bo_instance_not_found.

    "! <p class="shorttext synchronized" lang="en">Set Plan Version</p>
    METHODS set_plvar IMPORTING iv_plvar TYPE plvar DEFAULT c_default_plvar.
    "! <p class="shorttext synchronized" lang="en">Create/Update all Master Data objects</p>
    METHODS maintain RETURNING VALUE(rs_result) TYPE bapiret2
                     RAISING   cx_bo_instance_not_found zcx_fi_general.
    "! <p class="shorttext synchronized" lang="en">Simulate Master Data Object Creation</p>
    METHODS simulate
      RETURNING VALUE(rt_result) TYPE bapiret2_t
      RAISING   zcx_fi_general cx_bo_instance_not_found.
    "! <p class="shorttext synchronized" lang="en">Set Approver Level</p>
    METHODS set_apprv_lvl IMPORTING iv_level TYPE int4.
    METHODS: "! <p class="shorttext synchronized" lang="en">Get API Mode</p>
      get_api_mode RETURNING VALUE(r_result) TYPE glaccount_action,
      "! <p class="shorttext synchronized" lang="en">Set API Mode</p>
      set_api_mode IMPORTING iv_api_mode TYPE glaccount_action,
      "! <p class="shorttext synchronized" lang="en">Read GL Account Text</p>
      read_gltext  IMPORTING i_glaccount     TYPE saknr
                             i_ktopl         TYPE ktopl DEFAULT zif_fi_global_constants=>coa_operating_tafe
                   RETURNING VALUE(r_result) TYPE txt20_skat.

ENDCLASS.



CLASS zcl_swf_glmast IMPLEMENTATION.

  METHOD get_instance.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
* Notes:  In Singleton instantiation the same Notification is expected.
*         If you require instantiation for multiple Notification objects
*         then use the FACTORY (CREATE) method in this Class.
*----------------------------------------------------------------------

    lo_swf_glmast  = COND #( WHEN lo_swf_glmast IS BOUND
                             THEN lo_swf_glmast
                             ELSE NEW zcl_swf_glmast( iv_notification )
                           ).

    ro_result = lo_swf_glmast.

  ENDMETHOD.

  METHOD constructor.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->set_plvar( ).
    me->key  = iv_notification.
    me->lpor = VALUE #( catid  = |CL|
                        typeid = |ZCL_SWF_GLMAST|
                        instid = iv_notification
                      ).
    me->lt_special = me->get_form_data( ).
    me->set_api_mode( SWITCH #( me->get_value( |REQUEST_TYPE| )
                        WHEN 1              "1 = Create GL Account
                        THEN zif_fi_global_constants=>co_action-insert
                        ELSE zif_fi_global_constants=>co_action-update
                              )
                    ).

  ENDMETHOD.

  METHOD bi_object~default_attribute_value.

  ENDMETHOD.

  METHOD bi_object~execute_default_method.

  ENDMETHOD.

  METHOD bi_persistent~find_by_lpor.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    TRY.
        result = CAST zcl_swf_glmast( zcl_swf_glmast=>get_instance( CONV qmnum( lpor-instid ) ) ).
      CATCH cx_bo_instance_not_found.
    ENDTRY.

  ENDMETHOD.

  METHOD bi_persistent~lpor.
    result = me->lpor.
  ENDMETHOD.

  METHOD bi_persistent~refresh.

  ENDMETHOD.

  METHOD bi_object~release.

  ENDMETHOD.

  METHOD get_request_text.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    r_result = SWITCH #( COND #( WHEN iv_req_type IS INITIAL
                                 THEN me->get_value( |REQUEST_TYPE| )
                                 ELSE iv_req_type
                               )
                WHEN 1 THEN |Create GL Account|
                WHEN 2 THEN |Change GL Account Description|
                WHEN 3 THEN |Lock/Unlock GL Account|
                WHEN 4 THEN |Set Deletion Flag|
                WHEN 5 THEN |Reassign GL Account|
                       ).

  ENDMETHOD.


  METHOD get_form_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    CALL FUNCTION 'ISR_SPECIAL_DATA_GET'
      EXPORTING
        notification_no               = me->key
      IMPORTING
        scenario                      = me->scenario
        special_data                  = rt_result
      EXCEPTIONS
        no_internal_service_request   = 1
        invalid_notif_number          = 2
        int_service_request_not_found = 3
        OTHERS                        = 4.

    CHECK sy-subrc <> 0.

    MESSAGE e063 WITH me->key INTO dummy.

    RAISE EXCEPTION TYPE cx_bo_instance_not_found
      EXPORTING
        class_name = |ZCL_SWF_GLMAST|.

  ENDMETHOD.

  METHOD get_value.
    r_result = VALUE #( me->lt_special[ fieldname = iv_fieldname ]-fieldvalue OPTIONAL ).
  ENDMETHOD.

  METHOD maintain_masterdata.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    ASSERT me->scenario = |ZGLF|.        "only valid for QSCENARIO = |ZGLF|

    IF iv_update_task IS NOT INITIAL.
      "running with iv_update_task = True
      CALL FUNCTION 'ZFI_GL_GLMAST_UPDATE'
        IN UPDATE TASK
        EXPORTING
          iv_notification = me->key
        EXCEPTIONS
          ex_update_error = 1
          OTHERS          = 2.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'   "<- Synchronous Update
        EXPORTING
          wait   = abap_true
        IMPORTING
          return = rs_result.

      me->return = rs_result.

      CHECK rs_result-type CA zcl_com_bapireturn_services=>gc_error_msgtypes.

      RAISE EXCEPTION TYPE zcx_fi_general
        EXPORTING
          textid    = zcx_fi_general=>update_error
          gv_string = CONV #( rs_result-message ).

      RETURN.     "<-- my job is done here, goodbye!
    ELSE.
      "running with iv_update_task = False
      me->simulate( ).
      me->return = rs_result = me->maintain( ).
    ENDIF.

    CHECK cl_system_transaction_state=>get_in_update_task( ) = 0.  "is the work process running in Dialog Task?
    "Asynchronous Update
    cl_soap_commit_rollback=>commit( ).

  ENDMETHOD.

  METHOD get_approvers.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA lt_actor_container TYPE swconttab.

    me->set_apprv_lvl( iv_level ).

    swc_clear_container lt_actor_container.
    swc_set_element lt_actor_container 'Company_Code' iv_bukrs.
    swc_set_element lt_actor_container 'Apprvr_Lvl'   iv_level.

* read agents as customised in Responsibility Rule 'AC91000041'
    CALL FUNCTION 'RH_GET_ACTORS'
      EXPORTING
        act_object                = 'AC91000041'
        search_date               = sy-datum
      TABLES
        actor_container           = lt_actor_container
        actor_tab                 = rt_results
      EXCEPTIONS
        no_active_plvar           = 1
        no_actor_found            = 2
        exception_of_role_raised  = 3
        no_valid_agent_determined = 4
        no_container              = 5
        OTHERS                    = 6.

* ignore vacant positions
    LOOP AT rt_results ASSIGNING FIELD-SYMBOL(<result>).
      CHECK zcl_swf_common=>is_position_vacant(
              iv_objid = CONV #( <result>-objid )
              iv_plvar = me->plvar
            ).
      DELETE rt_results.
    ENDLOOP.

    CHECK rt_results IS INITIAL.

    RAISE EXCEPTION TYPE zcx_swf_no_agent_found
      EXPORTING
        messages = VALUE #( ( type        = zif_fi_global_constants=>error
                              id          = |ZFI_GLMAST|
                              number      = 064
                              message_v1  = CONV char02( iv_level )
                              message_v2  = |AC91000041| )
                          ).


  ENDMETHOD.

  METHOD set_plvar.

    CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
      IMPORTING
        act_plvar       = me->plvar
      EXCEPTIONS
        no_active_plvar = 1
        OTHERS          = 2.

    CHECK sy-subrc <> 0.

    me->plvar = iv_plvar.

  ENDMETHOD.

  METHOD get_description.

    r_result = SWITCH #( me->get_value( |REQUEST_TYPE| )
                WHEN 1
                THEN me->get_value( |NEW_PROP_DESC| )
                ELSE me->read_gltext( me->get_glaccount( ) )
                       ).

  ENDMETHOD.

  METHOD get_value_all.
    rt_results = me->lt_special.
  ENDMETHOD.


  METHOD maintain.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_badi_impl) = NEW zcl_im_fi_qisr1_gl_maint( ).

    rs_result = lo_badi_impl->maintain_masterdata(
                  iv_notification = me->key
                  iv_mode         = CONV #( me->get_value( |ISR_MODE| ) )
                  iv_view         = CONV #( me->get_value( |ISR_FORM_VIEW| ) )
                  iv_command      = CONV #( me->get_value( |ISR_EVENT| ) )
                ).

  ENDMETHOD.


  METHOD simulate.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_badi_impl) = NEW zcl_im_fi_qisr1_gl_maint( ).

    rt_result = lo_badi_impl->simulate_masterdata( me->key ).

    CHECK zcl_com_bapireturn_services=>check_itab_for_errors( rt_result ) = 0.

    _mac_raise_multi rt_result.

  ENDMETHOD.

  METHOD get_apprv_lvl.
    r_result = me->apprv_lvl.
  ENDMETHOD.

  METHOD set_apprv_lvl.
    me->apprv_lvl = iv_level.
  ENDMETHOD.

  METHOD get_api_mode.
    r_result = me->api_mode.
  ENDMETHOD.

  METHOD set_api_mode.
    me->api_mode = iv_api_mode.
  ENDMETHOD.

  METHOD is_complete.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA ls_viqmel TYPE viqmel.

    CALL FUNCTION 'IQS4_GET_NOTIFICATION'
      EXPORTING
        i_qmnum  = me->key
      IMPORTING
        e_viqmel = ls_viqmel.

    r_result = xsdbool( ls_viqmel-qmdab IS NOT INITIAL AND ls_viqmel-qmzab IS NOT INITIAL ).

  ENDMETHOD.

  METHOD create.
    ro_result = NEW zcl_swf_glmast( iv_notification ).
  ENDMETHOD.

  METHOD get_glaccount.

    r_result = SWITCH #( me->get_value( |REQUEST_TYPE| )
                WHEN 1
                THEN me->get_value( |NEW_GLACCT| )
                WHEN 2
                THEN me->get_value( |REF_GLACCT| )
                WHEN 3
                THEN me->get_value( |REF_ACCTLOCK| )
                WHEN 4
                THEN me->get_value( |REF_ACCTDEL| )
                WHEN 5
                THEN me->get_value( |REF_GLFSV| )
                      ).

  ENDMETHOD.


  METHOD read_gltext.

    DATA ls_skat TYPE skat.

    CALL FUNCTION 'SKAT_READ'
      EXPORTING
        ktopl     = i_ktopl
        saknr     = i_glaccount
        spras     = sy-langu
      IMPORTING
        struct    = ls_skat
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      "not possible in the context of this scenario
    ELSE.
      r_result = ls_skat-txt50.
    ENDIF.

  ENDMETHOD.

  METHOD get_email_list.

    DATA(var) = SWITCH #( i_on_success
                 WHEN abap_true
                 THEN `ZFI_GLFORM_SUCCESS_EMAIL`
                 ELSE `ZFI_GLFORM_ERROR_EMAIL`
               ).

    rt_result = VALUE #( FOR tvarvc IN zcl_com_fi_utilities=>get_tvarvc( CONV #( var ) )
                   ( CONV #( tvarvc-low ) )
                ).

  ENDMETHOD.

  METHOD is_update_error.
    r_result = xsdbool( me->return-type CA zcl_com_bapireturn_services=>gc_error_msgtypes ).
  ENDMETHOD.

ENDCLASS.
