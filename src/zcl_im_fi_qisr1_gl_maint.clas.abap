CLASS zcl_im_fi_qisr1_gl_maint DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_qisr1 .
    INTERFACES zif_fi_global_constants.

    "! <p class="shorttext synchronized" lang="en">Create/Update GLMAST Masterdata Objects</p>
    "!
    "! @parameter iv_notification | <p class="shorttext synchronized" lang="en">ISR Notification Number</p>
    "! @parameter iv_mode | <p class="shorttext synchronized" lang="en">Mode (CREATE, DISPLAY) Optional</p>
    "! @parameter iv_view | <p class="shorttext synchronized" lang="en">Form View (Optional)</p>
    "! @parameter iv_command | <p class="shorttext synchronized" lang="en">User Command (Optional)</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized" lang="en">Custom FI Exception Object</p>
    "! @raising cx_bo_instance_not_found | <p class="shorttext synchronized" lang="en">BO Instance Exception Object</p>
    METHODS maintain_masterdata
      IMPORTING
                VALUE(iv_notification) TYPE qmnum
                !iv_mode               TYPE qisrdmode     OPTIONAL
                !iv_view               TYPE qisrdformview OPTIONAL
                !iv_command            TYPE syucomm       OPTIONAL
      RETURNING VALUE(rs_result)       TYPE bapiret2
      RAISING   cx_bo_instance_not_found zcx_fi_general.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Simulate MD Objects on Check event</p>
      "! @parameter iv_notification | <p class="shorttext synchronized" lang="en">ISR Notification Number</p>
      "! @parameter iv_mode | <p class="shorttext synchronized" lang="en">Mode (CREATE, DISPLAY) Optional</p>
      "! @parameter iv_view | <p class="shorttext synchronized" lang="en">Form View (Optional)</p>
      "! @parameter rt_return | <p class="shorttext synchronized" lang="en">Simulation Results</p>
      "! @raising zcx_fi_general | <p class="shorttext synchronized" lang="en">Custom Exception Object</p>
      "! @raising cx_bo_instance_not_found | <p class="shorttext synchronized" lang="en">BO Instance Exception Object</p>
      simulate_masterdata
        IMPORTING VALUE(iv_notification) TYPE qmnum
        RETURNING VALUE(rt_return)       TYPE bapiret2_t
        RAISING   RESUMABLE(zcx_fi_general) cx_bo_instance_not_found.

  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES:
      coa_group_tafe FOR zif_fi_global_constants~coa_group_tafe,
      coa_oper_tafe  FOR zif_fi_global_constants~coa_operating_tafe,
      tafe_compcode  FOR zif_fi_global_constants~comp_code_tafe,
      tafe_fmarea    FOR zif_fi_global_constants~fm_area_1000,
      fsv_versn      FOR zif_fi_global_constants~co_fsv_versn,
      tafe_kokrs     FOR zif_fi_global_constants~co_area_1000,
      tafe_fsv       FOR zif_fi_global_constants~co_fsv_versn,
      action         FOR zif_fi_global_constants~co_action.


    DATA:
      lo_gl_factory  TYPE REF TO zcl_fico_masterdata_factory,
      lo_exception   TYPE REF TO zcx_fi_general,
      lv_mode        TYPE glaccount_action,
      lv_simulate    TYPE boole_d,
      ls_return      TYPE bapiret1,
      ls_coa_data    TYPE glaccount_coa_data,
      ls_ccode_data  TYPE glaccount_ccode_data,
      ls_acct_names  TYPE glaccount_name_data,
      ls_commitment  TYPE zfi_s_commitment_data,
      ls_costelement TYPE bapi1030_ceoutputlist,
      lt_messages    TYPE qisrtreturn,
      lt_special     TYPE qisrtspecial_param,
      lt_additional  TYPE qisrtspecial_param,
      lt_info        TYPE qisrtfield_info,
      l_objectkey    TYPE qmnum,
      l_mode         TYPE qisrdmode,
      l_view         TYPE qisrdformview,
      l_command      TYPE syucomm,
      l_dummy        TYPE bapi_msg.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Return an Instance of the Master data Factory Object</p>
      get_md_factory IMPORTING is_keys              TYPE zfi_s_glmast_isr_key
                     RETURNING VALUE(ro_md_factory) TYPE REF TO zcl_fico_masterdata_factory
                     RAISING   RESUMABLE(zcx_fi_general),
      "! <p class="shorttext synchronized" lang="en">Get value of a field from the Form</p>
      get_fldvalue IMPORTING it_data         TYPE qisrtspecial_param OPTIONAL
                             iv_name         TYPE qisrdfieldname
                   RETURNING VALUE(r_result) TYPE qisrdfieldvalue,
      "! <p class="shorttext synchronized" lang="en">Set value of a field on the Form</p>
      set_fldvalue IMPORTING it_fields     TYPE qisrtspecial_param
                             iv_additional TYPE boole_d DEFAULT abap_false,
      "! <p class="shorttext synchronized" lang="en">Get Form Run Mode</p>
      get_mode RETURNING VALUE(r_result) TYPE qisrdmode,
      "! <p class="shorttext synchronized" lang="en">Set Form Run Mode</p>
      set_mode IMPORTING iv_mode TYPE qisrdmode,
      "! <p class="shorttext synchronized" lang="en">Get Form View</p>
      get_view RETURNING VALUE(r_result) TYPE qisrdformview,
      "! <p class="shorttext synchronized" lang="en">Set Form View</p>
      set_view IMPORTING iv_view TYPE qisrdformview,
      "! <p class="shorttext synchronized" lang="en">Get ISR framework Command</p>
      get_command RETURNING VALUE(r_result) TYPE syucomm,
      "! <p class="shorttext synchronized" lang="en">Set ISR framework Command</p>
      set_command IMPORTING iv_ucomm TYPE syucomm,
      "! <p class="shorttext synchronized" lang="en">Get ISR Message List</p>
      get_messages RETURNING VALUE(rt_result) TYPE qisrtreturn,
      "! <p class="shorttext synchronized" lang="en">Set ISR Message List</p>
      set_messages IMPORTING it_messages TYPE qisrtreturn,
      "! <p class="shorttext synchronized" lang="en">Get ISR Form fields</p>
      get_special RETURNING VALUE(rt_result) TYPE qisrtspecial_param,
      "! <p class="shorttext synchronized" lang="en">Set ISR Form fields</p>
      set_special IMPORTING it_special TYPE qisrtspecial_param,
      "! <p class="shorttext synchronized" lang="en">Get ISR Form additional fields</p>
      get_additional RETURNING VALUE(rt_result) TYPE qisrtspecial_param,
      "! <p class="shorttext synchronized" lang="en">Set ISR Form additional fields</p>
      set_additional IMPORTING it_aditional TYPE qisrtspecial_param,
      "! <p class="shorttext synchronized" lang="en">Validate Form data</p>
      validate_data IMPORTING iv_check TYPE boole_d DEFAULT abap_false
                    RAISING   RESUMABLE(zcx_fi_general),
      "! <p class="shorttext synchronized" lang="en">Check Existence of the Master Data Object</p>
      check_md_exist IMPORTING is_keys TYPE zfi_s_glmast_isr_key
                               iv_ref  TYPE boole_d DEFAULT abap_true
                     RAISING   RESUMABLE(zcx_fi_general),
      "! <p class="shorttext synchronized" lang="en">Check mandatory fields on the Form</p>
      check_mandatory RAISING   zcx_fi_general,
      "! <p class="shorttext synchronized" lang="en">Clear fields on the Form</p>
      clear_fields IMPORTING VALUE(iv_type) TYPE char3 DEFAULT `REF`,
      "! <p class="shorttext synchronized" lang="en">Set all dropdowns on the Form</p>
      set_dropdowns,
      "! <p class="shorttext synchronized" lang="en">Return COA data</p>
      get_coa_data RETURNING VALUE(rs_result) TYPE glaccount_coa_data,
      "! <p class="shorttext synchronized" lang="en">Set COA data</p>
      set_coa_data IMPORTING is_coa_data      TYPE glaccount_coa_data,
      "! <p class="shorttext synchronized" lang="en">Map COA data</p>
      coa_mapper   IMPORTING iv_ktopl         TYPE ktopl DEFAULT coa_group_tafe
                   RETURNING VALUE(rs_result) TYPE glaccount_coa_data,
      "! <p class="shorttext synchronized" lang="en">Map Cost Element data</p>
      costelem_mapper RETURNING VALUE(rs_result) TYPE bapi1030_ceinputlist,
      "! <p class="shorttext synchronized" lang="en">Get account name</p>
      get_acct_names RETURNING VALUE(rs_result) TYPE glaccount_name_data,
      "! <p class="shorttext synchronized" lang="en">Set account name</p>
      set_acct_names IMPORTING is_acct_names TYPE glaccount_name_data,
      "! <p class="shorttext synchronized" lang="en">Map account name</p>
      acctname_mapper IMPORTING iv_ktopl         TYPE ktopl DEFAULT coa_group_tafe
                      RETURNING VALUE(rs_result) TYPE glaccount_name_data,
      "! <p class="shorttext synchronized" lang="en">Get API mode</p>
      get_api_mode RETURNING VALUE(r_result) TYPE glaccount_action,
      "! <p class="shorttext synchronized" lang="en">Set API mode</p>
      set_api_mode IMPORTING iv_mode TYPE glaccount_action DEFAULT action-insert,
      "! <p class="shorttext synchronized" lang="en">Get Company Code data</p>
      get_ccode_data RETURNING VALUE(rs_result) TYPE glaccount_ccode_data,
      "! <p class="shorttext synchronized" lang="en">Set Company Code data</p>
      set_ccode_data IMPORTING is_ccode_data TYPE glaccount_ccode_data,
      "! <p class="shorttext synchronized" lang="en">Map Company Code data</p>
      ccode_mapper  RETURNING VALUE(rs_result) TYPE glaccount_ccode_data,
      "! <p class="shorttext synchronized" lang="en">Map Commitment Item data</p>
      commitment_mapper RETURNING VALUE(rs_result) TYPE zfi_s_commitment_data,
      "! <p class="shorttext synchronized" lang="en">Read GL account master data</p>
      read_glmast IMPORTING is_keys          TYPE zfi_s_glmast_isr_key
                  RETURNING VALUE(rs_result) TYPE /eby/pdmdgl_sbapi_data,
      "! <p class="shorttext synchronized" lang="en">Get Simulation</p>
      get_simulate RETURNING VALUE(r_result) TYPE boole_d,
      "! <p class="shorttext synchronized" lang="en">Set Simulation</p>
      set_simulate IMPORTING iv_simulate TYPE boole_d,
      "! <p class="shorttext synchronized" lang="en">Refresh Form</p>
      refresh,
      "! <p class="shorttext synchronized" lang="en">Copy Reference values</p>
      copy_ref IMPORTING
                 is_coa_group TYPE /eby/pdmdgl_sbapi_data OPTIONAL
                 is_coa_oper  TYPE /eby/pdmdgl_sbapi_data OPTIONAL
                 is_ccode     TYPE /eby/pdmdgl_sbapi_data OPTIONAL
                 is_citemdata TYPE zfi_s_commitment_data  OPTIONAL
                 is_fsvdata   TYPE fagl_011zc             OPTIONAL
                 is_celemdata TYPE bapi1030_ceoutputlist  OPTIONAL,
      "! <p class="shorttext synchronized" lang="en">Get Commitment attribute</p>
      get_commitment RETURNING VALUE(rs_result) TYPE zfi_s_commitment_data,
      "! <p class="shorttext synchronized" lang="en">Set Commitment attribute</p>
      set_commitment IMPORTING is_commitment TYPE zfi_s_commitment_data,
      "! <p class="shorttext synchronized" lang="en">Derive Financial Year start date</p>
      get_fy_start   RETURNING VALUE(r_result) TYPE datum,
      "! <p class="shorttext synchronized" lang="en">Return info attribute for Form field changes </p>
      get_info        RETURNING VALUE(rt_result) TYPE qisrtfield_info,
      "! <p class="shorttext synchronized" lang="en">Set info attribute for Form field changes </p>
      set_info        IMPORTING it_info TYPE qisrtfield_info,
      "! <p class="shorttext synchronized" lang="en">Update info for Form field changes </p>
      update_fieldinfo,
      "! <p class="shorttext synchronized" lang="en">Get Cost Element attribute </p>
      get_costelement RETURNING VALUE(rs_result) TYPE bapi1030_ceinputlist,
      "! <p class="shorttext synchronized" lang="en">Set Cost Element attribute </p>
      set_costelement IMPORTING is_costelement TYPE bapi1030_ceinputlist,
      "! <p class="shorttext synchronized" lang="en">Set Form Groupings/Sections </p>
      set_groups,
      "! <p class="shorttext synchronized" lang="en">Get Form Groupings/Sections </p>
      get_groups      RETURNING VALUE(rt_result) TYPE tty_isr_group,
      "! <p class="shorttext synchronized" lang="en">Set User Command attribute</p>
      get_usercomm    RETURNING VALUE(rt_result) TYPE tty_ucomm,
      "! <p class="shorttext synchronized" lang="en">Set User Command attribute</p>
      set_usercomm,
      "! <p class="shorttext synchronized" lang="en">Handle User Commands</p>
      handle_commands,
      "! <p class="shorttext synchronized" lang="en">Check for Text changes</p>
      check_changes,
      "! <p class="shorttext synchronized" lang="en">Check if FSV updates required</p>
      is_fsv_required RETURNING VALUE(r_result) TYPE boole_d,
      "! <p class="shorttext synchronized" lang="en">Check if Alternate FSV is required</p>
      is_alt_fsv RETURNING VALUE(r_result) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Check if FSV is locked</p>
      check_fsv_locks RAISING RESUMABLE(zcx_fi_general),
      "! <p class="shorttext synchronized" lang="en">Check if Cost Element is required for this dataset</p>
      is_costelem_required RETURNING VALUE(r_result)      TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Check master data object locks</p>
      check_locks RAISING zcx_fi_general,
      "! <p class="shorttext synchronized" lang="en">Check Costelement locks</p>
      check_costelem_locks RAISING RESUMABLE(zcx_fi_general),
      "! <p class="shorttext synchronized" lang="en">Maintain the required master data objects</p>
      maintain RETURNING VALUE(rs_result) TYPE bapiret2  RAISING zcx_fi_general,
      "! <p class="shorttext synchronized" lang="en">Simulate MD Objects on Check event</p>
      simulate RAISING RESUMABLE(zcx_fi_general),
      "! <p class="shorttext synchronized" lang="en">Set ISR current Approval Processing Status</p>
      set_approval_status   RETURNING VALUE(rt_result) TYPE qisrtreturn,
      "! <p class="shorttext synchronized" lang="en">Set Control Parameters for every form stage</p>
      set_control  CHANGING ct_special_data TYPE qisrtspecial_param,
      "! <p class="shorttext synchronized" lang="en">Get ObjectKey</p>
      get_objectkey RETURNING VALUE(r_result) TYPE qmnum,
      "! <p class="shorttext synchronized" lang="en">Set ObjectKey</p>
      set_objectkey IMPORTING iv_objectkey TYPE qmnum,
      "! <p class="shorttext synchronized" lang="en">Writes Form data to ISR Notification database</p>
      set_special_isr IMPORTING it_special       TYPE qisrtspecial_param OPTIONAL
                      RETURNING VALUE(rt_result) TYPE qisrtreturn,
      "! <p class="shorttext synchronized" lang="en">Maintain new GL account number for EBS</p>
      persist_ebs_glmast RAISING zcx_fi_general,
      "! <p class="shorttext synchronized" lang="en">Check mandatory fields for Approvers</p>
      check_approvers    CHANGING cs_return TYPE bapiret1.

    CLASS-DATA:
      l_notif       TYPE qmnum,
      l_bus_unit    TYPE stext,
      l_chklist     TYPE boole_d,
      ls_refaccts   TYPE ty_refaccts,
      lt_groups     TYPE tty_isr_group,
      lt_usercomm   TYPE tty_ucomm,
      lt_navigator  TYPE qisrtspecial_param,
      lt_comp_codes TYPE qisrtspecial_param,
      lt_acct_group TYPE qisrtspecial_param,
      lt_gl_toes    TYPE qisrtspecial_param,
      lt_finstat    TYPE qisrtspecial_param,
      lt_finstnote  TYPE qisrtspecial_param,
      lt_gl_cflow   TYPE qisrtspecial_param,
      lt_ncver_dsc  TYPE qisrtspecial_param,
      lt_ncver_act  TYPE qisrtspecial_param,
      lt_ncver_cflo TYPE qisrtspecial_param,
      lt_currency   TYPE qisrtspecial_param,
      lt_taxcateg   TYPE qisrtspecial_param,
      lt_sortkey    TYPE qisrtspecial_param,
      lt_fldstatus  TYPE qisrtspecial_param,
      lt_planlevel  TYPE qisrtspecial_param,
      lt_planrelv   TYPE qisrtspecial_param,
      lt_fintrans   TYPE qisrtspecial_param,    "commitment item
      lt_citem_post TYPE qisrtspecial_param,    "commitment item can be posted?
      lt_citem_cat  TYPE qisrtspecial_param,    "commitment item category
      lt_scitem_cat TYPE qisrtspecial_param,    "superior commitment item category
      lt_citem_var  TYPE qisrtspecial_param,    "commitment item variant
      lt_celem_cat  TYPE qisrtspecial_param,    "cost element category
      lt_fsv_hier   TYPE qisrtspecial_param,    "FSV TAFE Hierarchy
      lt_fsv_hier2  TYPE qisrtspecial_param.    "FSV TAFE Alternate Hierarchy

    "! <p class="shorttext synchronized" lang="en">Read Business Unit</p>
    CLASS-METHODS get_bus_unit
      IMPORTING
        iv_fieldvalue   TYPE qisrdfieldvalue
      RETURNING
        VALUE(r_result) TYPE qisrdfieldvalue.

    "! <p class="shorttext synchronized" lang="en">Read Domain Fixed Values</p>
    CLASS-METHODS get_domain_values
      IMPORTING !iv_name         TYPE csequence
      RETURNING VALUE(rt_result) TYPE hrasr00help_dataset_tab.

    "! <p class="shorttext synchronized" lang="en">Read Domain FIVOR</p>
    CLASS-METHODS get_fintrans
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Domain KATEG</p>
    CLASS-METHODS get_comitem_post
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Domain POTYP</p>
    CLASS-METHODS get_comitem_categ
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Superior Commitment Item Category</p>
    CLASS-METHODS get_supcomitem_categ
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Commitment Item Variant</p>
    CLASS-METHODS get_comitem_var
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Company Codes</p>
    CLASS-METHODS get_comp_codes
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Account Groups</p>
    CLASS-METHODS get_acct_group
      IMPORTING iv_ktopl         TYPE ktopl
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read TOES Account</p>
    CLASS-METHODS get_toes
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Financial Statements</p>
    CLASS-METHODS get_finstat
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Financial Statement Notes</p>
    CLASS-METHODS get_finstnote
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read CashFlow</p>
    CLASS-METHODS get_cflow
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read NCVER Disclosure</p>
    CLASS-METHODS get_ncver_dsc
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read NCVER Act</p>
    CLASS-METHODS get_ncver_act
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read NCVER Cashflow</p>
    CLASS-METHODS get_ncver_cflo
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Currencies</p>
    CLASS-METHODS get_currency
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Tax Categories</p>
    CLASS-METHODS get_taxcategory
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Sort Keys</p>
    CLASS-METHODS get_sortkey
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Field Statuses</p>
    CLASS-METHODS get_fieldstatus
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Planning Level</p>
    CLASS-METHODS get_plan_level
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Planning Relevance</p>
    CLASS-METHODS get_plan_relv
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read Cost Element Category</p>
    CLASS-METHODS get_costelem_categ
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param.

    "! <p class="shorttext synchronized" lang="en">Read FSV Hierarchy</p>
    CLASS-METHODS get_fsv_hierarchy
      IMPORTING VALUE(iv_versn)  TYPE versn_011 DEFAULT tafe_fsv-tafe_fsv
      RETURNING VALUE(rt_result) TYPE qisrtspecial_param
      RAISING   RESUMABLE(zcx_fi_general).

ENDCLASS.



CLASS zcl_im_fi_qisr1_gl_maint IMPLEMENTATION.




  METHOD check_md_exist.

    TRY.
        DATA(lo_gl_factory) = me->get_md_factory( is_keys ).

        CHECK lo_gl_factory IS BOUND.

        lo_gl_factory->zif_masterdata_factory~is_exist(    "Params are OK, do not panic! debug single step.
               iv_key1  = is_keys-saknr
               iv_key2  = is_keys-ktopl
               iv_key3  = is_keys-bukrs
      ).

        CHECK iv_ref IS INITIAL.        "Exists for New MD Object -> we have a problem with that

        IF is_keys-ktopl IS NOT INITIAL.
          MESSAGE e009 WITH is_keys-saknr is_keys-ktopl INTO DATA(dummy).
        ELSEIF is_keys-bukrs IS NOT INITIAL.
          MESSAGE e019 WITH is_keys-saknr is_keys-bukrs INTO dummy.
        ELSEIF is_keys-fipex IS NOT INITIAL.
          MESSAGE e021 WITH |{ is_keys-fipex ALPHA = OUT }| is_keys-fikrs INTO dummy.
        ELSEIF is_keys-kstar IS NOT INITIAL.
          MESSAGE e040 WITH |{ is_keys-kstar ALPHA = OUT }| tafe_kokrs INTO dummy.
        ELSEIF is_keys-versn IS NOT INITIAL.
          MESSAGE e041 WITH |{ is_keys-saknr ALPHA = OUT }| tafe_fsv-tafe_fsv INTO dummy.
        ENDIF.

        _mac_add_msg me->lt_messages data(return).
        me->clear_fields( |NEW| ).

      CATCH BEFORE UNWIND zcx_fi_general INTO DATA(lo_exception).
        IF lo_exception->is_resumable IS NOT INITIAL AND iv_ref IS NOT INITIAL.
          "MD Object does not exist for REF MD Object value entered
          me->set_messages( lo_exception->get_messages( ) ).
          me->clear_fields( ).
        ENDIF.
        "other non-resumable errors -> we have a problem with those as well
        CHECK lo_exception->is_resumable IS INITIAL.
        me->set_messages( lo_exception->get_messages( ) ).
        me->clear_fields( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_additional.
    rt_result = me->lt_additional.
  ENDMETHOD.


  METHOD get_bus_unit.

    IF l_bus_unit IS INITIAL.
      zcl_fcom_isr_tool=>get_business_unit_frm_position(
         EXPORTING
           iv_obj_id   = CONV #( iv_fieldvalue )
         IMPORTING
           ev_bu_descr = l_bus_unit
       ).
    ENDIF.

    r_result = l_bus_unit.

  ENDMETHOD.


  METHOD get_command.
    r_result = me->l_command.
  ENDMETHOD.


  METHOD get_comp_codes.

    IF lt_comp_codes IS INITIAL.

      zcl_swf_common=>get_company_codes(
        IMPORTING
          et_helpvalues = DATA(lt_helpvalues)
      ).

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = 'NEW_COMP_CODE_KEY'
          iv_fieldlabel      = 'NEW_COMP_CODE_LABEL'
        CHANGING
          et_additional_data = lt_comp_codes
      ).
    ENDIF.

    rt_result = lt_comp_codes.

  ENDMETHOD.


  METHOD get_fldvalue.
    DATA(lt_data) = COND #( WHEN it_data IS SUPPLIED THEN it_data ELSE me->get_special( ) ).
    r_result = VALUE #( lt_data[ fieldname = iv_name ]-fieldvalue OPTIONAL ).
  ENDMETHOD.


  METHOD get_md_factory.

    TRY.
        ro_md_factory = zcl_fico_masterdata_factory=>create(
                          iv_saknr      = is_keys-saknr
                          iv_costelem   = is_keys-kstar
                          iv_comitem    = is_keys-fipex
                          iv_ktopl      = is_keys-ktopl
                          iv_bukrs      = is_keys-bukrs
                          iv_fikrs      = is_keys-fikrs
                          iv_versn      = is_keys-versn
                          iv_ergsl      = is_keys-ergsl
                          is_coa_data   = me->get_coa_data( )
                          is_ccode_data = me->get_ccode_data( )
                          is_acct_name  = me->get_acct_names( )
                          iv_coarea     = tafe_kokrs                "create setters & getters when a new CO Area is introduced
                          is_citemdata  = me->get_commitment( )
                          is_costinput  = me->get_costelement( )
*                      iv_keydate    = SY-DATUM
*                      iv_coelclass  = '1'
                          iv_mode       = me->get_api_mode( )
                          iv_simulate   = me->get_simulate( )
                          iv_commit     = abap_false
                        ).

      CATCH zcx_fi_general.
        "this is necessary for a better user (UI) experience - Do Not remove or re-engineer!
        DATA(raise) = COND #( WHEN is_keys-saknr IS NOT INITIAL OR is_keys-fipex IS NOT INITIAL
                        THEN abap_true
                        ELSE THROW RESUMABLE zcx_fi_general(
                                              textid    = zcx_fi_general=>incorrect_params
                                              gv_string = |Incorrect Parameters|
                                                           )
                            ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_messages.
    CHECK me->lt_messages IS NOT INITIAL.
    SORT me->lt_messages. "BY number.
    DELETE ADJACENT DUPLICATES FROM me->lt_messages COMPARING ALL FIELDS.
    rt_result = me->lt_messages.
  ENDMETHOD.


  METHOD get_mode.
    r_result = me->l_mode.
  ENDMETHOD.


  METHOD get_special.
    rt_result = me->lt_special.
  ENDMETHOD.


  METHOD get_view.
    r_result = me->l_view.
  ENDMETHOD.


  METHOD if_ex_qisr1~int_service_request_check.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->set_mode( mode ).
    me->set_command( user_command ).
    me->set_view( form_view ).
    me->set_special( special_data ).
    me->set_additional( additional_data ).
    me->set_control( CHANGING ct_special_data = special_data ).

    CHECK   me->get_view( )     = |ISR_REQUEST|                            AND
            NOT me->get_fldvalue( |DRAFT| )                                AND
          ( me->get_mode( )     = |CREATE| OR me->get_mode( ) = |CHANGE| ) AND
          ( me->get_command( )  = |CHECK|  OR me->get_command( ) = |START| ).

    TRY.
        me->check_mandatory( ).
        me->simulate( ).

      CATCH BEFORE UNWIND zcx_fi_general INTO DATA(lo_exception).
        IF lo_exception->is_resumable IS NOT INITIAL.
          "that's OK - we continue with rest of the updates until a "hard" exception is raised
          RESUME.
        ELSE.
          me->set_messages( lo_exception->get_messages( ) ).
          message_list = me->get_messages( ).
          me->clear_fields( ).                                    "do we want to clear after check?
          special_data = me->get_special( ).
          return = VALUE #( message_list[ 1 ] OPTIONAL ).         "required for this interface method
          RETURN.
        ENDIF.
    ENDTRY.

    message_list = me->get_messages( ).
    return = VALUE #( message_list[ 1 ] OPTIONAL ).
    special_data = me->get_special( ).

  ENDMETHOD.


  METHOD if_ex_qisr1~int_service_request_init.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->set_usercomm( ).
    me->set_groups( ).

    special_data[ fieldname = |REQUEST_TYPE| ]-fieldvalue = |1|.  "Defaulting to 1 at start

  ENDMETHOD.

  METHOD if_ex_qisr1~scenario_process_user_command.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA gl_data_group TYPE /eby/pdmdgl_sbapi_data.
    DATA gl_data_oper  TYPE /eby/pdmdgl_sbapi_data.
    DATA gl_data_ccode TYPE /eby/pdmdgl_sbapi_data.
    DATA comitem_data  TYPE zfi_s_commitment_data.
    DATA costelem_data TYPE bapi1030_ceoutputlist.
    DATA fsv_data      TYPE fagl_011zc.

* method names are self explanatory without additional comments
    me->set_mode( mode ).
    me->set_command( user_command ).
    me->set_view( form_view ).
    me->set_special( special_data ).
    me->set_additional( additional_data ).
    me->set_control( CHANGING ct_special_data = special_data ).
    me->set_info( field_info ).
    me->update_fieldinfo( ).
    me->handle_commands( ).
    me->check_approvers( CHANGING cs_return = return ).
    me->refresh( ).

    CHECK ( me->get_mode( ) = |CREATE| OR me->get_mode( ) = |CHANGE| ) AND
            me->get_view( ) = |ISR_REQUEST|.

    me->set_fldvalue( VALUE #( LET indx = 1 IN fieldindex = indx
         ( fieldname = |NEW_GLACCT2|     fieldvalue = me->get_fldvalue( |NEW_GLACCT| ) )
         ( fieldname = |NEW_GLACCT4|     fieldvalue = me->get_fldvalue( |NEW_GLACCT| ) )
         ( fieldname = |NEW_COMITEM|     fieldvalue = me->get_fldvalue( |NEW_GLACCT| ) )
         ( fieldname = |NEW_CCODE_FIPEX| fieldvalue = me->get_fldvalue( |NEW_GLACCT| ) )
         ( fieldname = |NEW_GLFSV|       fieldvalue = me->get_fldvalue( |NEW_GLACCT| ) )
         ( fieldname = |NEW_COSTELEM|    fieldvalue = COND #( WHEN me->is_costelem_required( )   "Is Cost Element required at all for this GL account?
                                                              THEN me->get_fldvalue( |NEW_GLACCT| )
                                                              ELSE space
                                                            ) )
         ( fieldname = |NEW_BILKT|       fieldvalue = me->get_fldvalue( |NEW_GLACCT| ) )

       "the below block has to be commented out when all Sections are required visible on the Form
         ( fieldname = |REF_GLACCT2|    fieldvalue = me->get_fldvalue( |REF_GLACCT| ) )
         ( fieldname = |REF_GLACCT4|    fieldvalue = me->get_fldvalue( |REF_GLACCT| ) )
         ( fieldname = |REF_COMITEM|    fieldvalue = me->get_fldvalue( |REF_GLACCT| ) )
         ( fieldname = |REF_GLFSV|      fieldvalue = SWITCH #( me->get_fldvalue( |REQUEST_TYPE| )
                                                        WHEN 5
                                                        THEN me->get_fldvalue( |REF_GLFSV| )
                                                        ELSE me->get_fldvalue( |REF_GLACCT| )
                                                             ) )
         ( fieldname = |REF_COSTELEM|   fieldvalue = me->get_fldvalue( |REF_GLACCT| ) )
                             )
                    ).

    TRY.
        me->validate_data( ).
        me->set_api_mode( action-read ).

*      validation OK -> instantiate for Read
        me->lo_gl_factory = me->get_md_factory( VALUE #(
                                 saknr = CONV #( me->get_fldvalue( |REF_GLACCT| ) )
                                 ktopl = CONV #( me->get_fldvalue( |NEW_COA| ) )
                                                      )
                                             ).

        IF me->lo_gl_factory IS BOUND.
          "get data for Section 1 (GL Account in COA 2000)
          me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = gl_data_group ).

          me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
               ( fieldname = |REF_COA|        fieldvalue  = coa_group_tafe )
               ( fieldname = |REF_KTOKS|      fieldvalue  = gl_data_group-coa_data-ktoks )
               ( fieldname = |REF_XBILK|      fieldvalue  = gl_data_group-coa_data-xbilk )
               ( fieldname = |REF_GVTYP|      fieldvalue  = gl_data_group-coa_data-gvtyp )
               ( fieldname = |REF_PROP_DESC|  fieldvalue  = gl_data_group-names[ 1 ]-data-txt20 )
               ( fieldname = |REF_LONG_DESC|  fieldvalue  = gl_data_group-names[ 1 ]-data-txt50 )
               ( fieldname = |REF_TOES|       fieldvalue  = gl_data_group-coa_data-zzs_ncver_toes-zz_toes_acct )
               ( fieldname = |REF_FSTMT|      fieldvalue  = gl_data_group-coa_data-zzs_ncver_toes-zz_finstat_code )
               ( fieldname = |REF_FSTMT_NOTE| fieldvalue  = gl_data_group-coa_data-zzs_ncver_toes-zz_finstat_note )
               ( fieldname = |REF_CFLOW|      fieldvalue  = gl_data_group-coa_data-zzs_ncver_toes-zz_cf_classf )
               ( fieldname = |REF_NCVER_DSC|  fieldvalue  = gl_data_group-coa_data-zzs_ncver_toes-zz_fs_dsclsr )
               ( fieldname = |REF_NCVER_ACT|  fieldvalue  = gl_data_group-coa_data-zzs_ncver_toes-zz_actvt_code )
               ( fieldname = |REF_NCVER_CFLO| fieldvalue  = gl_data_group-coa_data-zzs_ncver_toes-zz_cflo_code )
                                   )
                          ).
        ENDIF.

        "get data for Section 2 (GL Account in COA 1000)
        me->lo_gl_factory = me->get_md_factory( VALUE #(
                                   saknr = CONV #( me->get_fldvalue( |REF_GLACCT2| ) )
                                   ktopl = CONV #( me->get_fldvalue( |NEW_COA2| ) )
                                                            )
                                                   ).

        IF me->lo_gl_factory IS BOUND.
          me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = gl_data_oper ).

          me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
               ( fieldname = |REF_COA2|       fieldvalue  = coa_oper_tafe )
               ( fieldname = |REF_KTOKS2|     fieldvalue  = gl_data_oper-coa_data-ktoks )
               ( fieldname = |REF_XBILK2|     fieldvalue  = gl_data_oper-coa_data-xbilk )
               ( fieldname = |REF_BILKT|      fieldvalue  = |{ gl_data_oper-coa_data-bilkt ALPHA = OUT }| )
               ( fieldname = |REF_GVTYP2|     fieldvalue  = gl_data_oper-coa_data-gvtyp )
               ( fieldname = |REF_PROP_DESC2| fieldvalue  = gl_data_oper-names[ 1 ]-data-txt20 )
               ( fieldname = |REF_LONG_DESC2| fieldvalue  = gl_data_oper-names[ 1 ]-data-txt50 )
                                   )
                          ).
        ENDIF.

        "get data for Section 4 (GL Account in Company Code 1020)
        me->lo_gl_factory = me->get_md_factory( VALUE #(
                                    saknr = CONV #( me->get_fldvalue( |REF_GLACCT4| ) )
                                    bukrs = CONV #( me->get_fldvalue( |NEW_COMPCODE4| ) )
                                              )
                            ).

        IF me->lo_gl_factory IS BOUND.
          me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = gl_data_ccode ).

          me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
               ( fieldname = |REF_COMPCODE4|   fieldvalue = tafe_compcode )
               ( fieldname = |REF_CURR|        fieldvalue = gl_data_ccode-ccode_data-waers )
               ( fieldname = |REF_BALLC|       fieldvalue = gl_data_ccode-ccode_data-xsalh )
               ( fieldname = |REF_TAXCATEG|    fieldvalue = gl_data_ccode-ccode_data-mwskz )
               ( fieldname = |REF_POSTNOTAX|   fieldvalue = gl_data_ccode-ccode_data-xmwno )
               ( fieldname = |REF_ALTACCT|     fieldvalue = |{ gl_data_ccode-ccode_data-altkt ALPHA = OUT }| )
               ( fieldname = |REF_OPENITEM|    fieldvalue = gl_data_ccode-ccode_data-xopvw )
               ( fieldname = |REF_LITEM_DISP|  fieldvalue = gl_data_ccode-ccode_data-xkres )
               ( fieldname = |REF_SORTKEY|     fieldvalue = gl_data_ccode-ccode_data-zuawa )
               ( fieldname = |REF_FSTAG|       fieldvalue = gl_data_ccode-ccode_data-fstag )
               ( fieldname = |REF_AUTOPOST|    fieldvalue = gl_data_ccode-ccode_data-xintb )
               ( fieldname = |REF_PLANLVL|     fieldvalue = gl_data_ccode-ccode_data-fdlev )
               ( fieldname = |REF_CFLOW_RELV|  fieldvalue = gl_data_ccode-ccode_data-xgkon )
               ( fieldname = |REF_CCODE_FIPEX| fieldvalue = gl_data_ccode-ccode_data-fipos )
               ( fieldname = |REF_PLANRELV|    fieldvalue = gl_data_ccode-ccode_data-zz_plan_relv )
               ( fieldname = |REF_ASSINFO|     fieldvalue = gl_data_ccode-ccode_data-zz_glacct_ccode_text )
                                   )
                          ).
        ENDIF.

        "get data for Section 3 (Commitment Item Details)
        me->lo_gl_factory = me->get_md_factory( VALUE #(
                                    fikrs = tafe_fmarea
                                    fipex = CONV #( me->get_fldvalue( |REF_COMITEM| ) ) ) ).

        IF me->lo_gl_factory IS BOUND.
          me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = comitem_data ).

          me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
                ( fieldname = |REF_FM|                fieldvalue = comitem_data-fmci-fikrs )
                ( fieldname = |REF_CITEM_NAME|        fieldvalue = comitem_data-fmcit-bezei )
                ( fieldname = |REF_CITEM_DESC|        fieldvalue = comitem_data-fmcit-text1 )
                ( fieldname = |REF_CITEM_POST|        fieldvalue = comitem_data-fmci-kateg )
                ( fieldname = |REF_CITEM_TRTYP|       fieldvalue = comitem_data-fmci-fivor )
                ( fieldname = |REF_CITEM_ITCATEG|     fieldvalue = comitem_data-fmci-potyp )
                ( fieldname = |REF_CITEM_SUP_ITCATEG| fieldvalue = comitem_data-fmci-fipup )
                ( fieldname = |REF_CITEM_VARIANT|     fieldvalue = comitem_data-fmci-stvar )
                                   )
                          ).
        ENDIF.

        "get data for Section 5 (GL account in FSV in TAFE FSV)
        me->lo_gl_factory = me->get_md_factory( VALUE #(
                                    saknr = CONV #( me->get_fldvalue( |REF_GLFSV| ) )
                                    versn = tafe_fsv-tafe_fsv
                                                       )
                                              ).

        IF me->lo_gl_factory IS BOUND.
          me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = fsv_data ).

          me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
                 ( fieldname = |REF_FSVCOA|            fieldvalue = fsv_data-ktopl )
                 ( fieldname = |REF_FSV|               fieldvalue = fsv_data-versn )
                 ( fieldname = |REF_FSVHIER|           fieldvalue = fsv_data-ergsl )
                                    )
                           ).
        ENDIF.

        "get data for Section 5 (GL account in FSV in TAFE Alternate FSV Hierarchy)
        me->lo_gl_factory = me->get_md_factory( VALUE #(
                                    saknr = CONV #( me->get_fldvalue( |REF_GLFSV| ) )
                                    versn = tafe_fsv-tafe_alt_fsv
                                                       )
                                              ).

        IF me->lo_gl_factory IS BOUND.
          me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = fsv_data ).

          me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
                 ( fieldname = |REF_ALTFSV|               fieldvalue = fsv_data-versn )
                 ( fieldname = |REF_ALTFSVHIER|           fieldvalue = fsv_data-ergsl )
                                    )
                           ).
        ENDIF.

        "get data for Section 6 (Cost Element)
        IF me->is_costelem_required( ).         "Is Cost Element required at all for this GL account?
          me->lo_gl_factory = me->get_md_factory( VALUE #( kstar = CONV #( me->get_fldvalue( |REF_COSTELEM| ) ) ) ).

          IF me->lo_gl_factory IS BOUND.
            me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = costelem_data ).

            me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
                   ( fieldname = |REF_KOKRS|           fieldvalue = tafe_kokrs )
                   ( fieldname = |REF_CLEM_BEGDA|      fieldvalue = costelem_data-valid_from )
                   ( fieldname = |REF_CLEM_ENDDA|      fieldvalue = costelem_data-valid_to )
                   ( fieldname = |REF_CELEM_NAME|      fieldvalue = costelem_data-name )
                   ( fieldname = |REF_CELEM_DESC|      fieldvalue = costelem_data-descript )
                   ( fieldname = |REF_CELEM_CATEG|     fieldvalue = costelem_data-celem_category )
                                     )
                           ).
          ENDIF.
        ELSE.     "Cost Element not required -> clear data for this Section 6 -> @TODO check config if hidden
          me->set_command( |RESET6| ).
          me->handle_commands( ).
        ENDIF.

        "Section Lock/Unlock GL account in Operating Chart of Account
        me->lo_gl_factory = me->get_md_factory( VALUE #(
                                    saknr = CONV #( me->get_fldvalue( |REF_ACCTLOCK| ) )
                                    ktopl = CONV #( me->get_fldvalue( |NEW_COALOCK| ) )
                                              )
                            ).

        IF me->lo_gl_factory IS BOUND.
          me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = gl_data_oper ).

          me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
                 ( fieldname = |REF_COALOCK|  fieldvalue = me->get_fldvalue( |NEW_COALOCK| ) )
                 ( fieldname = |REF_LOCK|     fieldvalue = gl_data_oper-coa_data-xspeb )
                                   )
                          ).
        ENDIF.

        "Section Set Delete Flag in Operating Chart of Account
        me->lo_gl_factory = me->get_md_factory( VALUE #(
                                    saknr = CONV #( me->get_fldvalue( |REF_ACCTDEL| ) )
                                    ktopl = CONV #( me->get_fldvalue( |NEW_COADEL| ) )
                                              )
                            ).

        IF me->lo_gl_factory IS BOUND.
          me->lo_gl_factory->zif_masterdata_factory~get_data( IMPORTING es_data = gl_data_oper ).

          me->set_fldvalue( VALUE #( LET index = 1 IN fieldindex = index
                 ( fieldname = |REF_COADEL|  fieldvalue = me->get_fldvalue( |NEW_COADEL| ) )
                 ( fieldname = |REF_DEL|     fieldvalue = gl_data_oper-coa_data-xloev )
                                   )
                          ).
        ENDIF.

      CATCH BEFORE UNWIND zcx_fi_general INTO DATA(lo_exception).
        IF lo_exception->is_resumable IS NOT INITIAL.
          "that's OK - we continue to load rest of the form until a "hard" exception is raised
          CLEAR fsv_data.
          RESUME.
        ELSE.
          me->set_messages( lo_exception->get_messages( ) ).
          message_list = me->get_messages( ).
          me->clear_fields( ).
          special_data = me->get_special( ).
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE e001 WITH flt_val INTO DATA(dummy).     "Customising error
        _mac_add_msg me->lt_messages return.
    ENDTRY.

* set static data - we need to know if REF accounts were changed from the last time
    ls_refaccts = VALUE #( glcoa_grp   = special_data[ fieldname = |REF_GLACCT| ]-fieldvalue
                           glcoa_oper  = special_data[ fieldname = |REF_GLACCT2| ]-fieldvalue
                           comitem     = special_data[ fieldname = |REF_COMITEM| ]-fieldvalue
                           glccode     = special_data[ fieldname = |REF_GLACCT4| ]-fieldvalue
                           glfsv       = special_data[ fieldname = |REF_GLFSV| ]-fieldvalue
                           costelem    = special_data[ fieldname = |REF_COSTELEM| ]-fieldvalue
                         ).

* Copy values in NEW column from values in REF column
    me->copy_ref( is_coa_group = gl_data_group
                  is_coa_oper  = gl_data_oper
                  is_ccode     = gl_data_ccode
                  is_citemdata = comitem_data
                  is_fsvdata   = fsv_data
                  is_celemdata = costelem_data
                ).

    "update Approval Status when Form is Withdrawn
    " - this is set for BEFORE_SUBMIT & AFTER_SUBMIT methods to pick up
    me->lt_special[ fieldname = |APPRV_STATUS|
                  ]-fieldvalue = SWITCH #( user_command
                                  WHEN `CLOSE`
                                  THEN |CLOSED|
                                  ELSE me->get_fldvalue( |APPRV_STATUS| )
                                ).

    special_data = me->get_special( ).

    CHECK user_command = |START| OR user_command = |CLOSE|.       "Requestor amends/updates/withdraws Form from Draft/Revision WF step etc.

    me->if_ex_qisr1~int_service_request_check(
      EXPORTING
        flt_val         = flt_val
        field_info      = field_info
        mode            = mode
        form_view       = form_view
        user_command    = user_command
      IMPORTING
        return          = return
      CHANGING
        general_data    = general_data
        special_data    = special_data
        additional_data = additional_data
        message_list    = message_list
    ).

    CHECK zcl_com_bapireturn_services=>check_itab_for_errors( message_list ) <> 0.

    CHECK l_notif IS NOT INITIAL.

    general_data-header-notif_no = l_notif.
    user_command = |SAVE|.                      "<-- system persists the Notification

  ENDMETHOD.


  METHOD if_ex_qisr1~scenario_set_additional_values.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->set_mode( mode ).
    me->set_view( form_view ).
    me->set_special( special_data ).
    me->set_additional( additional_data ).

    l_notif = COND #( WHEN general_data-header-notif_no IS NOT INITIAL
                      THEN general_data-header-notif_no
                      ELSE l_notif
                    ).

    me->set_dropdowns( ).

    additional_data = me->get_additional( ).

    "not sure if concurrent and vacant positions required to be checked
    zcl_fcom_isr_tool=>fill_form_req_position_data(
      EXPORTING
        iv_user_id               = SWITCH #( general_data-initiated_by-user_id
                                    WHEN space THEN sy-uname
                                    ELSE general_data-initiated_by-user_id
                                           )
      "IMPORTING
      "  ev_no_position_found     = DATA(lv_no_position_found)
      "  ev_no_concurnt_pos_found = DATA(lv_no_concurnt_pos_found)
      CHANGING
        et_additional_data       = additional_data
     ).

    TRY.
        special_data[ fieldname = |POSITION_NAME| ]-fieldvalue  = VALUE #( additional_data[ fieldname = |POSITION_NAME_KEY| ]-fieldvalue OPTIONAL ).
        special_data[ fieldname = |POSITION_DESC| ]-fieldvalue  = VALUE #( additional_data[ fieldname = |POSITION_NAME_LABEL| ]-fieldvalue OPTIONAL ).
        special_data[ fieldname = |BUSINESS_UNIT| ]-fieldvalue  = me->get_bus_unit( me->get_fldvalue( it_data = additional_data iv_name = |POSITION_NAME_KEY| ) ).
        special_data[ fieldname = |ISR_REQUEST_NO| ]-fieldvalue = l_notif.
        special_data[ fieldname = |NEW_COA| ]-fieldvalue        = coa_group_tafe.
        special_data[ fieldname = |NEW_COA2| ]-fieldvalue       = coa_oper_tafe.
        special_data[ fieldname = |NEW_COMPCODE4 | ]-fieldvalue = tafe_compcode.
        special_data[ fieldname = |NEW_FM| ]-fieldvalue         = tafe_fmarea .
        special_data[ fieldname = |NEW_FSVCOA| ]-fieldvalue     = coa_oper_tafe.
        special_data[ fieldname = |NEW_FSV| ]-fieldvalue        = fsv_versn-tafe_fsv.
        special_data[ fieldname = |NEW_ALTFSV| ]-fieldvalue     = fsv_versn-tafe_alt_fsv.
        special_data[ fieldname = |NEW_KOKRS| ]-fieldvalue      = tafe_kokrs.
        special_data[ fieldname = |NEW_CLEM_BEGDA| ]-fieldvalue = me->get_fy_start( ).
        special_data[ fieldname = |NEW_COALOCK| ]-fieldvalue    = coa_oper_tafe.
        special_data[ fieldname = |NEW_COADEL| ]-fieldvalue     = coa_oper_tafe.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE e001 WITH flt_val INTO DATA(dummy).     "Customising error
        _mac_add_msg me->lt_messages data(return).
        RETURN.
    ENDTRY.

    "initialise Agree flag on Revision request here as method REQUEST_INIT is not called on CHANGE mode
    CHECK me->get_mode( ) = |CHANGE| AND me->get_view( ) = |ISR_REQUEST| AND
          me->get_fldvalue( |ISR_EVENT| ) = |BACKTO|                     AND
          me->get_fldvalue( |ISR_REQUEST_NO| ).        "Notification persists

    " Requester must Agree every time on Revision request
    special_data[ fieldname = |AGREE| ]-fieldvalue = space.

  ENDMETHOD.


  METHOD validate_data.

* constants have been passed instead of reading from special_data because they get cleared when exceptions are raised.
    me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |NEW_GLACCT| )   ktopl = coa_group_tafe ) iv_ref = '' ).    "Section 1 New account
    me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |NEW_GLACCT2| )  ktopl = coa_oper_tafe ) iv_ref = '' ).     "Section 2 Reference account
    me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |REF_GLACCT| )   ktopl = coa_group_tafe ) ).                "Section 1 Reference account
    me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |REF_GLACCT2| )  ktopl = coa_oper_tafe ) ).                 "Section 2 Reference account
    me->check_md_exist( is_keys = VALUE #( fipex = me->get_fldvalue( |NEW_COMITEM| )  fikrs = tafe_fmarea ) iv_ref = '' ).       "Section 3 Commitment Item
    me->check_md_exist( is_keys = VALUE #( fipex = me->get_fldvalue( |REF_COMITEM| )  fikrs = tafe_fmarea ) ).                   "Section 3 Reference Commitment Item
    me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |NEW_GLACCT4| )  bukrs = tafe_compcode ) iv_ref = '' ).     "Section 4 New account
    me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |REF_GLACCT4| )  bukrs = tafe_compcode ) ).                 "Section 4 Reference account
    me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |REF_ACCTLOCK| ) ktopl = coa_oper_tafe ) ).                 "Section Lock/Unlock
    me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |REF_ACCTDEL| )  ktopl = coa_oper_tafe ) ).                 "Section Set Delete Flag

    IF me->is_fsv_required( ).      "makes sense to check if FSV is relevant for this update
      me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |REF_GLFSV| )  versn = tafe_fsv-tafe_fsv ) ).             "Section 5 Reference account
      me->check_md_exist( is_keys = VALUE #( saknr = me->get_fldvalue( |NEW_GLFSV| )  versn = tafe_fsv-tafe_fsv ) iv_ref = '' ). "Section 5 New account
    ENDIF.

    "passing NEW_GLACCT instead of NEW_COSTELEM because NEW_COSTELEM could not be set for technical reason
    me->check_md_exist( is_keys = VALUE #( kstar = me->get_fldvalue( |NEW_GLACCT| ) ) iv_ref = '' ).                             "Section 6 New account
    IF me->is_costelem_required( ). "makes sense to check if the CostElem is relevant for this update
      me->check_md_exist( is_keys = VALUE #( kstar = me->get_fldvalue( |REF_COSTELEM| ) ) ).                                     "Section 6 Reference account
    ENDIF.

    CHECK zcl_com_bapireturn_services=>check_itab_for_errors( me->get_messages( ) ) = 0.

    RAISE EXCEPTION TYPE zcx_fi_general.

  ENDMETHOD.

  METHOD check_mandatory.

    "only check when Check button is clicked
    CHECK ( me->get_command( ) = |CHECK| OR me->get_command( ) = |START| ) AND
            NOT me->get_fldvalue( |DRAFT| ).

    "Reason must be entered for all Request Types
    _mac_check_field `REASON` e020 `` ``.
    _mac_check_field `AGREE`  e062 `` ``.

    "Section Change Description, Lock/Unlock, Set Delete Flag, Reassign FSV
    CASE me->get_fldvalue( |REQUEST_TYPE| ).
      WHEN 1.
        "IF config-sectionnum-hide = abap_true. -> if this is the only Section invisible  "plan to bring in UI display configuration
        _mac_check_field `REF_GLACCT`       e050 coa_group_tafe ``.
        _mac_check_field `NEW_PROP_DESC`    e007 coa_group_tafe ``.
        _mac_check_field `NEW_LONG_DESC`    e008 coa_group_tafe ``.
        _mac_check_field `NEW_GLACCT`       e004 coa_group_tafe ``.
        _mac_check_field `NEW_KTOKS`        e010 coa_group_tafe ``.

**// I have kept the source code below so it can be uncommented when other Sections need to be visible on the Form

*    _mac_check_field `NEW_KTOKS2`               e010 coa_oper_tafe ``.
*    _mac_check_field `NEW_BILKT`                e018 `` ``.
*    _mac_check_field `NEW_TOES`                 e011 `` ``.
*    _mac_check_field `NEW_FSTMT`                e012 `` ``.
*    _mac_check_field `NEW_FSTMT_NOTE`           e013 `` ``.
*    _mac_check_field `NEW_CFLOW`                e014 `` ``.
*    _mac_check_field `NEW_NCVER_DSC`            e015 `` ``.
*    _mac_check_field `NEW_NCVER_ACT`            e016 `` ``.
*    _mac_check_field `NEW_NCVER_CFLO`           e017 `` ``.
*    _mac_check_field `NEW_CITEM_POST`           e024 `` ``.
*    _mac_check_field `NEW_CITEM_TRTYP`          e027 `` ``.
*    _mac_check_field `NEW_CITEM_ITCATEG`        e028 `` ``.
*    _mac_check_field `NEW_CITEM_SUP_ITCATEG`    e029 `` ``.
*    _mac_check_field `NEW_CURR`                 e031 `` ``.
*    _mac_check_field `NEW_FSTAG`                e035 `` ``.
*    _mac_check_field `NEW_PLANRELV`             e038 `` ``.
*    _mac_check_field `NEW_FSVHIER`              e042 `` ``.

*    IF NOT me->get_fldvalue( |NEW_XBILK| ) AND NOT me->get_fldvalue( |NEW_GVTYP| ).
*      _mac_check_field `NEW_XBILK` e005 coa_group_tafe ``.
*    ENDIF.
*
*    IF me->get_fldvalue( |NEW_XBILK| ) AND me->get_fldvalue( |NEW_GVTYP| ).
*      MESSAGE e006 WITH coa_group_tafe INTO me->l_dummy.
*      _mac_add_msg me->lt_messages me->ls_return.
*    ENDIF.
*
*    IF NOT me->get_fldvalue( |NEW_XBILK2| ) AND NOT me->get_fldvalue( |NEW_GVTYP2| ).
*      _mac_check_field `NEW_XBILK2`  e005 coa_oper_tafe ``.
*    ENDIF.
*
*    IF me->get_fldvalue( |NEW_XBILK2| ) AND me->get_fldvalue( |NEW_GVTYP2| ).
*      MESSAGE e006 WITH coa_oper_tafe INTO me->l_dummy.
*      _mac_add_msg me->lt_messages me->ls_return.
*    ENDIF.
*
*    "Section 6 -> Conditional
*    CHECK me->get_fldvalue( |NEW_COSTELEM| ).
*
*    _mac_check_field `NEW_CLEM_BEGDA`     e044 `` ``.
*    _mac_check_field `NEW_CLEM_ENDDA`     e045 `` ``.
*    _mac_check_field `NEW_CELEM_NAME`     e046 `` ``.
*    _mac_check_field `NEW_CELEM_DESC`     e047 `` ``.
*    _mac_check_field `NEW_CELEM_CATEG`    e048 `` ``.
*    _mac_check_field `NEW_PROP_DESC2`   e007 coa_oper_tafe ``.
*    _mac_check_field `NEW_LONG_DESC2`   e008 coa_oper_tafe ``.
*     _mac_check_field `NEW_CITEM_NAME`   e022 `` ``.
*    _mac_check_field `NEW_CITEM_DESC`   e023 `` ``.

      WHEN 2.
        _mac_check_field `REF_GLACCT`       e050 coa_group_tafe ``.
        _mac_check_field `NEW_PROP_DESC`    e007 coa_group_tafe ``.
        _mac_check_field `NEW_LONG_DESC`    e008 coa_group_tafe ``.
*    _mac_check_field `REF_GLACCT2`   e050 coa_oper_tafe  ``.
*    _mac_check_field `REF_COMITEM` e051 tafe_fmarea ``.
*    _mac_check_field `NEW_PROP_DESC2`   e007 coa_oper_tafe ``.
*    _mac_check_field `NEW_LONG_DESC2`   e008 coa_oper_tafe ``.
*    _mac_check_field `NEW_CITEM_NAME`   e022 `` ``.
*    _mac_check_field `NEW_CITEM_DESC`   e023 `` ``.

      WHEN 3.
        _mac_check_field  `REF_ACCTLOCK` e049 `` ``.
      WHEN 4.
        _mac_check_field  `REF_ACCTDEL`  e049 `` ``.
      WHEN 5.
        _mac_check_field  `REF_GLFSV`    e049 `` ``.
        _mac_check_field  `NEW_FSVHIER`  e042 `` ``.
      WHEN OTHERS.
        "continue with the checks below
    ENDCASE.

    me->check_changes( ).   "check deltas for Texts and other important fields

    CHECK zcl_com_bapireturn_services=>check_itab_for_errors( me->get_messages( ) ) = 0.

    RAISE EXCEPTION TYPE zcx_fi_general.

  ENDMETHOD.

  METHOD if_ex_qisr1~scenario_set_form_view.
  ENDMETHOD.

  METHOD if_ex_qisr1~post_notif_position.
  ENDMETHOD.


  METHOD if_ex_qisr1~reverse_notif_external.
  ENDMETHOD.

  METHOD if_ex_qisr1~scenario_after_submit.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->set_mode( mode ).
    me->set_command( user_command ).
    me->set_view( form_view ).
    me->set_special( special_data ).
    me->set_objectkey( object_key ).

    CHECK ( me->get_mode( ) = |CHANGE| AND me->get_view( ) = |ISR_APPROVE| )                        "during Approval process
      OR  ( me->get_view( ) = |ISR_REQUEST| AND me->get_fldvalue( |APPRV_STATUS| ) = |WITHDRAWN| ). "during Withdrawal process

    message_list = me->get_messages( ).
    return = VALUE #( message_list[ 1 ] OPTIONAL ).
    CHECK zcl_com_bapireturn_services=>check_itab_for_errors( message_list ) <> 0.

    message_list = VALUE #( BASE message_list ( LINES OF me->set_approval_status( ) ) ).
    return = VALUE #( message_list[ 1 ] OPTIONAL ).

  ENDMETHOD.


  METHOD if_ex_qisr1~scenario_final_before_submit.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->set_mode( mode ).
    me->set_command( user_command ).
    me->set_view( form_view ).
    me->set_special( special_data ).
    me->set_control( CHANGING ct_special_data = special_data ).

    CHECK me->get_view( ) = |ISR_REQUEST| AND    "first condition -> must be from a Request view
          ( me->get_mode( )    = |CREATE| OR me->get_mode( ) = |CHANGE| )  AND
          ( me->get_command( ) = |SEND|   OR me->get_command( ) = |SAVE| ).


    me->lt_special[ fieldname = |APPRV_STATUS| ]-fieldvalue
            = COND #( WHEN me->get_fldvalue( |DRAFT| )          "save as Draft checkbox is ON
                      THEN `DRAFT`
                      ELSE SWITCH #( me->get_fldvalue( |APPRV_STATUS| )
                            WHEN `CLOSED`
                            THEN |WITHDRAWN|
                            ELSE |SUBMIT|                       "save as Draft checkbox is OFF
                           )
                    ).

    special_data = me->get_special( ).

  ENDMETHOD.

  METHOD set_additional.
    me->lt_additional = it_aditional.
  ENDMETHOD.


  METHOD set_command.
    me->l_command = iv_ucomm.
  ENDMETHOD.


  METHOD set_messages.

    "this has to behave like a "collect messages" due to the design of the ISR framework
    me->lt_messages = VALUE #( BASE me->lt_messages
                        FOR message IN it_messages
                        ( message )
                             ).
  ENDMETHOD.


  METHOD set_mode.
    me->l_mode = iv_mode.
  ENDMETHOD.


  METHOD set_special.
    me->lt_special = it_special.
  ENDMETHOD.


  METHOD set_view.
    me->l_view = iv_view.
  ENDMETHOD.


  METHOD clear_fields.

    LOOP AT me->lt_special ASSIGNING FIELD-SYMBOL(<special>)
         WHERE fieldname(3) = iv_type.
      CHECK <special>-fieldname NS |{ iv_type }_GLACCT|.
      CHECK <special>-fieldname NS |{ iv_type }_COMITEM|.
      CHECK <special>-fieldname NS |{ iv_type }_COSTELEM|.
      CHECK <special>-fieldname NS |{ iv_type }_GLFSV|.
      CHECK <special>-fieldname NE |{ iv_type }_ACCTLOCK|.
      CHECK <special>-fieldname NE |{ iv_type }_ACCTDEL|.
      CHECK <special>-fieldname NE |REQUEST_TYPE|.
      CLEAR <special>-fieldvalue.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_fldvalue.

    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<field>).
      TRY.
          IF iv_additional IS INITIAL.
            me->lt_special[ fieldname = <field>-fieldname ]-fieldvalue = <field>-fieldvalue.
          ELSE.
            me->lt_additional[ fieldname = <field>-fieldname ]-fieldvalue = <field>-fieldvalue.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_acct_group.

    DATA(lt_helpvalues) = zcl_swf_common=>get_account_groups( iv_ktopl ).

    zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
      EXPORTING
        it_helpvalues  = lt_helpvalues
        iv_fieldkey    = SWITCH #( iv_ktopl
                          WHEN coa_group_tafe
                          THEN 'NEW_KTOKS_KEY'
                          WHEN coa_oper_tafe
                          THEN 'NEW_KTOKS2_KEY'
                                 )
        iv_fieldlabel  = SWITCH #( iv_ktopl
                          WHEN coa_group_tafe
                          THEN 'NEW_KTOKS_LABEL'
                          WHEN coa_oper_tafe
                          THEN 'NEW_KTOKS2_LABEL'
                                 )
      CHANGING
        et_additional_data = rt_result "lt_acct_group
    ).

  ENDMETHOD.

  METHOD get_toes.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_gl_toes IS INITIAL.

      SELECT toes_acct toes_acct_text
        FROM zfi_gl_toes
        INTO TABLE lt_helpvalues.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_TOES_KEY|
          iv_fieldlabel      = |NEW_TOES_LABEL|
        CHANGING
          et_additional_data = lt_gl_toes
      ).
    ENDIF.

    rt_result = lt_gl_toes.

  ENDMETHOD.

  METHOD get_finstat.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_finstat IS INITIAL.

      SELECT finstat_code,finstat_text
        FROM zfi_gl_finstat
        INTO TABLE @lt_helpvalues.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_FSTMT_KEY|
          iv_fieldlabel      = |NEW_FSTMT_LABEL|
        CHANGING
          et_additional_data = lt_finstat
      ).
    ENDIF.

    rt_result = lt_finstat.

  ENDMETHOD.

  METHOD get_finstnote.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_finstnote IS INITIAL.

      SELECT finstat_note,finstat_note_text
        FROM zfi_gl_finstnote
        INTO TABLE @lt_helpvalues.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_FSTMT_NOTE_KEY|
          iv_fieldlabel      = |NEW_FSTMT_NOTE_LABEL|
        CHANGING
          et_additional_data = lt_finstnote
      ).
    ENDIF.

    rt_result = lt_finstnote.

  ENDMETHOD.

  METHOD get_cflow.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_gl_cflow IS INITIAL.

      SELECT cf_classf,cf_classf_text
        FROM zfi_gl_cashflow
        INTO TABLE @lt_helpvalues.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_CFLOW_KEY|
          iv_fieldlabel      = |NEW_CFLOW_LABEL|
        CHANGING
          et_additional_data = lt_gl_cflow
      ).
    ENDIF.

    rt_result = lt_gl_cflow.

  ENDMETHOD.

  METHOD get_ncver_dsc.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_ncver_dsc IS INITIAL.

      SELECT fs_dsclsr,fs_dsclsr_text
        FROM zfi_gl_ncver_dsc
        INTO TABLE @lt_helpvalues.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_NCVER_DSC_KEY|
          iv_fieldlabel      = |NEW_NCVER_DSC_LABEL|
        CHANGING
          et_additional_data = lt_ncver_dsc
      ).
    ENDIF.

    rt_result = lt_ncver_dsc.

  ENDMETHOD.

  METHOD get_ncver_act.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_ncver_act IS INITIAL.

      SELECT actvt_code,actvt_text
        FROM zfi_gl_ncver_act
        INTO TABLE @lt_helpvalues.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_NCVER_ACT_KEY|
          iv_fieldlabel      = |NEW_NCVER_ACT_LABEL|
        CHANGING
          et_additional_data = lt_ncver_act
      ).
    ENDIF.

    rt_result = lt_ncver_act.

  ENDMETHOD.

  METHOD get_ncver_cflo.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_ncver_cflo IS INITIAL.

      SELECT cflo_code,cflo_text
        FROM zfi_gl_ncver_cfl
        INTO TABLE @lt_helpvalues.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_NCVER_CFLO_KEY|
          iv_fieldlabel      = |NEW_NCVER_CFLO_LABEL|
        CHANGING
          et_additional_data = lt_ncver_cflo
      ).
    ENDIF.

    rt_result = lt_ncver_cflo.

  ENDMETHOD.

  METHOD get_currency.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_currency IS INITIAL.
      SELECT waers,ltext
       FROM tcurt
       INTO TABLE @lt_helpvalues
       WHERE spras = @sy-langu.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
           EXPORTING
             it_helpvalues      = lt_helpvalues
             iv_fieldkey        = |NEW_CURRENCY_KEY|
             iv_fieldlabel      = |NEW_CURRENCY_LABEL|
           CHANGING
             et_additional_data = lt_currency
         ).
    ENDIF.

    rt_result = lt_currency.

  ENDMETHOD.

  METHOD get_taxcategory.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.
    DATA lt_t007s      TYPE STANDARD TABLE OF t007s.
    DATA l_kalsm       TYPE kalsm_d.

    IF lt_taxcateg IS INITIAL.

      SELECT SINGLE kalsm INTO l_kalsm
       FROM t005 WHERE land1 = zif_fi_global_constants=>country_au.

      "FYI they are hard coded in the standard system as well (you can build a table if you like)
      lt_helpvalues = VALUE #( BASE lt_helpvalues
                        ( help_key = |-| help_value  = |Only input tax allowed| )
                        ( help_key = |+| help_value  = |Only output tax allowed| )
                        ( help_key = |*| help_value  = |All tax types allowed| )
                        ( help_key = |<| help_value  = |Input Tax Account| )
                        ( help_key = |>| help_value  = |Output Tax Account| )
                        ( help_key = |-B| help_value = |Input tax - down payments managed gross| )
                        ( help_key = |+B| help_value = |Output tax - down payments managed gross| )
                             ).

      CALL FUNCTION 'WSRS_O_T007_X_DB_READ'
        EXPORTING
          pi_kalsm   = l_kalsm
        TABLES
          pe_t_t007s = lt_t007s.

      CHECK lt_t007s IS NOT INITIAL.

      lt_helpvalues = VALUE #( BASE lt_helpvalues
                        FOR t007s IN lt_t007s
                        ( help_key   = t007s-mwskz
                          help_value = t007s-text1 )
                             ).

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
           EXPORTING
             it_helpvalues      = lt_helpvalues
             iv_fieldkey        = |NEW_TAXCATEG_KEY|
             iv_fieldlabel      = |NEW_TAXCATEG_LABEL|
           CHANGING
             et_additional_data = lt_taxcateg
         ).

    ENDIF.

    rt_result = lt_taxcateg.

  ENDMETHOD.


  METHOD set_dropdowns.

    DATA(additional_data) = me->get_additional( ).

    TRY.
        additional_data = VALUE #( BASE additional_data ( LINES OF me->get_comp_codes( ) ) ).
        additional_data = VALUE #( BASE additional_data ( LINES OF me->get_acct_group( CONV #( me->get_fldvalue( |NEW_COA| ) ) ) ) ).

        "I'm keeping the source code - please uncomment if/when business decides to make other Sections visible on the Form
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_acct_group( CONV #( me->get_fldvalue( |NEW_COA2| ) ) ) ) ).
        "additional_data = VALUE #( BASE additional_data ( LINES OF me->get_toes( ) ) ).
        "additional_data = VALUE #( BASE additional_data ( LINES OF me->get_finstat( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_finstnote( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_cflow( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_ncver_act( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_ncver_cflo( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_ncver_dsc(  ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_currency( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_taxcategory( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_sortkey( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_fieldstatus( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_plan_level( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_plan_relv( ) ) ).
        "additional_data = VALUE #( BASE additional_data ( LINES OF me->get_fintrans( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_comitem_categ( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_comitem_post( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_supcomitem_categ( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_comitem_var( ) ) ).
        " additional_data = VALUE #( BASE additional_data ( LINES OF me->get_costelem_categ( ) ) ).
        additional_data = VALUE #( BASE additional_data ( LINES OF me->get_fsv_hierarchy( ) ) ).
        additional_data = VALUE #( BASE additional_data ( LINES OF me->get_fsv_hierarchy( tafe_fsv-tafe_alt_fsv ) ) ).

      CATCH BEFORE UNWIND zcx_fi_general.
        RESUME.     "make sure to check attribute IS_RESUMABLE of the exception object if raising hard exceptions in future
    ENDTRY.

    me->set_additional( additional_data ).

  ENDMETHOD.

  METHOD get_coa_data.
    rs_result = me->ls_coa_data.
  ENDMETHOD.

  METHOD set_coa_data.
    me->ls_coa_data = is_coa_data.
  ENDMETHOD.


  METHOD coa_mapper.

    IF me->get_api_mode( ) = action-insert.
      rs_result = SWITCH #( iv_ktopl
                   WHEN coa_group_tafe
                   THEN VALUE #(
                           ktoks                          = me->get_fldvalue( |NEW_KTOKS| )
                           xbilk                          = me->get_fldvalue( |NEW_XBILK| )
                           gvtyp                          = me->get_fldvalue( |NEW_GVTYP| )
                           zzs_ncver_toes-zz_toes_acct    = me->get_fldvalue( |NEW_TOES| )
                           zzs_ncver_toes-zz_finstat_code = me->get_fldvalue( |NEW_FSTMT| )
                           zzs_ncver_toes-zz_finstat_note = me->get_fldvalue( |NEW_FSTMT_NOTE| )
                           zzs_ncver_toes-zz_cf_classf    = me->get_fldvalue( |NEW_CFLOW| )
                           zzs_ncver_toes-zz_fs_dsclsr    = me->get_fldvalue( |NEW_NCVER_DSC| )
                           zzs_ncver_toes-zz_actvt_code   = me->get_fldvalue( |NEW_NCVER_ACT| )
                           zzs_ncver_toes-zz_cflo_code    = me->get_fldvalue( |NEW_NCVER_CFLO| )
                        )
                    WHEN coa_oper_tafe
                    THEN VALUE #(
                           ktoks                          = me->get_fldvalue( |NEW_KTOKS2| )
                           xbilk                          = me->get_fldvalue( |NEW_XBILK2| )
                           gvtyp                          = me->get_fldvalue( |NEW_GVTYP2| )
                           bilkt                          = COND #( WHEN me->get_simulate( )
                                                                    THEN me->get_fldvalue( |REF_GLACCT2| )
                                                                    ELSE me->get_fldvalue( |NEW_BILKT| )
                                                                  )
                          )
                        ).
    ENDIF.

  ENDMETHOD.

  METHOD get_acct_names.
    rs_result = me->ls_acct_names.
  ENDMETHOD.

  METHOD set_acct_names.
    me->ls_acct_names = is_acct_names.
  ENDMETHOD.


  METHOD acctname_mapper.

    rs_result = VALUE #( txt20 = me->get_fldvalue( SWITCH #( iv_ktopl
                                                    WHEN coa_group_tafe
                                                    THEN |NEW_PROP_DESC|
                                                    WHEN coa_oper_tafe
                                                    THEN |NEW_PROP_DESC2|
                                                           )
                                                 )
                         txt50 = me->get_fldvalue( SWITCH #( iv_ktopl
                                                    WHEN coa_group_tafe
                                                    THEN |NEW_LONG_DESC|
                                                    WHEN coa_oper_tafe
                                                    THEN |NEW_LONG_DESC2|
                                                           )
                                                 )
                       ).
  ENDMETHOD.

  METHOD get_api_mode.
    r_result = me->lv_mode.
  ENDMETHOD.

  METHOD set_api_mode.
    me->lv_mode = iv_mode.
  ENDMETHOD.

  METHOD get_ccode_data.
    rs_result = me->ls_ccode_data.
  ENDMETHOD.

  METHOD set_ccode_data.
    me->ls_ccode_data = is_ccode_data.
  ENDMETHOD.


  METHOD ccode_mapper.

    rs_result = VALUE #( waers = me->get_fldvalue( |NEW_CURR| )
                         xsalh = me->get_fldvalue( |NEW_BALLC| )
                         mwskz = me->get_fldvalue( |NEW_TAXCATEG| )
                         xmwno = me->get_fldvalue( |NEW_POSTNOTAX| )
                         altkt = me->get_fldvalue( |NEW_ALTACCT| )
                         xopvw = me->get_fldvalue( |NEW_OPENITEM| )
                         xkres = me->get_fldvalue( |NEW_LITEM_DISP| )
                         zuawa = me->get_fldvalue( |NEW_SORTKEY| )
                         fstag = me->get_fldvalue( |NEW_FSTAG| )
                         xintb = me->get_fldvalue( |NEW_AUTOPOST| )
                         fdlev = me->get_fldvalue( |NEW_PLANLVL| )
                         xgkon = me->get_fldvalue( |NEW_CFLOW_RELV| )
                         fipos = COND #( WHEN NOT me->get_simulate( )
                                         THEN |{ me->get_fldvalue( |NEW_CCODE_FIPEX| ) ALPHA = OUT }|
                                         ELSE space
                                       )
                         zz_plan_relv = me->get_fldvalue( |NEW_PLANRELV| )
                         zz_glacct_ccode_text = me->get_fldvalue( |NEW_ASSINFO| )
                      ).

  ENDMETHOD.

  METHOD read_glmast.

    DATA(lo_reader) = NEW zcl_fi_glmast_api_extend( ).

    lo_reader->get_detail(
      EXPORTING
        ic_saknr     = is_keys-saknr                 " G/L Account Number
        ic_bukrs     = is_keys-bukrs                 " Company Code
        ic_ktopl     = is_keys-ktopl                 " Chart of Accounts
      IMPORTING
        es_bapi_data = rs_result                     " GL Account BAPI data
    ).

  ENDMETHOD.

  METHOD get_simulate.
    r_result = me->lv_simulate.
  ENDMETHOD.

  METHOD set_simulate.
    me->lv_simulate = iv_simulate.
  ENDMETHOD.


  METHOD refresh.

    CHECK me->get_command( ) = |REFRESH|.

    DATA general_data TYPE qisrsgeneral_param.
    DATA(special_data) = me->get_special( ).

    LOOP AT me->get_groups( ) ASSIGNING FIELD-SYMBOL(<ls_group>).
      special_data[ fieldname = <ls_group>-fieldname ]-fieldvalue = space.
    ENDLOOP.

    me->set_special( special_data ).

  ENDMETHOD.


  METHOD copy_ref.

    CHECK me->get_command( ) <> |CHECK|.

    DATA(field_info) = me->get_info( ).

* Copy values to NEW column from values in REF column - Section 1
    IF me->get_fldvalue( |REF_GLACCT| ) AND
       VALUE #( field_info[ fieldname = |REF_GLACCT| ]-changed OPTIONAL ) = abap_true.
      me->set_fldvalue( VALUE #( LET group   = is_coa_group-coa_data
                                     grnames = is_coa_group-names
                                     index   = 1 IN fieldindex = index
                         ( fieldname = |NEW_KTOKS|             fieldvalue  = group-ktoks )
                         ( fieldname = |NEW_XBILK|             fieldvalue  = group-xbilk )
                         ( fieldname = |NEW_GVTYP|             fieldvalue  = group-gvtyp )
                         ( fieldname = |NEW_TOES|              fieldvalue  = group-zzs_ncver_toes-zz_toes_acct )
                         ( fieldname = |NEW_FSTMT|             fieldvalue  = group-zzs_ncver_toes-zz_finstat_code )
                         ( fieldname = |NEW_FSTMT_NOTE|        fieldvalue  = group-zzs_ncver_toes-zz_finstat_note )
                         ( fieldname = |NEW_CFLOW|             fieldvalue  = group-zzs_ncver_toes-zz_cf_classf )
                         ( fieldname = |NEW_NCVER_DSC|         fieldvalue  = group-zzs_ncver_toes-zz_fs_dsclsr )
                         ( fieldname = |NEW_NCVER_ACT|         fieldvalue  = group-zzs_ncver_toes-zz_actvt_code )
                         ( fieldname = |NEW_NCVER_CFLO|        fieldvalue  = group-zzs_ncver_toes-zz_cflo_code )
                         ( fieldname = |NEW_PROP_DESC|         fieldvalue  = VALUE #( grnames[ 1 ]-data-txt20 OPTIONAL ) )
                         ( fieldname = |NEW_LONG_DESC|         fieldvalue  = VALUE #( grnames[ 1 ]-data-txt50 OPTIONAL ) )
                                 )
                      ).
    ENDIF.

* Section 2
    IF me->get_fldvalue( |REF_GLACCT2| ) AND
       VALUE #( field_info[ fieldname = |REF_GLACCT2| ]-changed OPTIONAL ) = abap_true.
      me->set_fldvalue( VALUE #( LET oper    = is_coa_oper-coa_data
                                     opnames = is_coa_oper-names
                                     newglac = me->get_fldvalue( |NEW_GLACCT| )
                                     index   = 1 IN fieldindex = index
*                         ( fieldname = |NEW_PROP_DESC2|        fieldvalue  = VALUE #( opnames[ 1 ]-data-txt20 OPTIONAL ) )
*                         ( fieldname = |NEW_LONG_DESC2|        fieldvalue  = VALUE #( opnames[ 1 ]-data-txt50 OPTIONAL ) )
                         ( fieldname = |NEW_PROP_DESC2|        fieldvalue  = me->get_fldvalue( |NEW_PROP_DESC| ) )
                         ( fieldname = |NEW_LONG_DESC2|        fieldvalue  = me->get_fldvalue( |NEW_LONG_DESC| ) )
                         ( fieldname = |NEW_KTOKS2|            fieldvalue  = oper-ktoks )
                         ( fieldname = |NEW_XBILK2|            fieldvalue  = oper-xbilk )
                         ( fieldname = |NEW_GVTYP2|            fieldvalue  = oper-gvtyp )
                               )
                    ).
    ENDIF.

* Section 4
    IF me->get_fldvalue( |REF_GLACCT4| ) AND
       VALUE #( field_info[ fieldname = |REF_GLACCT4| ]-changed OPTIONAL ) = abap_true.
      me->set_fldvalue( VALUE #( LET ccode   = is_ccode-ccode_data
                                     index   = 1 IN fieldindex = index
                         ( fieldname = |NEW_CURR|              fieldvalue  = ccode-waers )
                         ( fieldname = |NEW_BALLC|             fieldvalue  = ccode-xsalh )
                         ( fieldname = |NEW_TAXCATEG|          fieldvalue  = ccode-mwskz )
                         ( fieldname = |NEW_POSTNOTAX|         fieldvalue  = ccode-xmwno )
                         ( fieldname = |NEW_ALTACCT|           fieldvalue  = |{ ccode-altkt ALPHA = OUT }| )
                         ( fieldname = |NEW_OPENITEM|          fieldvalue  = ccode-xopvw )
                         ( fieldname = |NEW_LITEM_DISP|        fieldvalue  = ccode-xkres )
                         ( fieldname = |NEW_SORTKEY|           fieldvalue  = ccode-zuawa )
                         ( fieldname = |NEW_FSTAG|             fieldvalue  = ccode-fstag )
                         ( fieldname = |NEW_AUTOPOST|          fieldvalue  = ccode-xintb )
                         ( fieldname = |NEW_PLANLVL|           fieldvalue  = ccode-fdlev )
                         ( fieldname = |NEW_CFLOW_RELV|        fieldvalue  = ccode-xgkon )
                         ( fieldname = |NEW_PLANRELV|          fieldvalue  = ccode-zz_plan_relv )
                         ( fieldname = |NEW_ASSINFO|           fieldvalue  = ccode-zz_glacct_ccode_text )
                                 )
                     ).
    ENDIF.

* Section 3
    IF me->get_fldvalue( |REF_COMITEM| ) AND
       VALUE #( field_info[ fieldname = |REF_COMITEM| ]-changed OPTIONAL ) = abap_true.
      me->set_fldvalue( VALUE #( LET comitem = is_citemdata
                                     index   = 1 IN fieldindex = index
                         ( fieldname = |NEW_FM|                fieldvalue  = comitem-fmci-fikrs )
                         ( fieldname = |NEW_CITEM_NAME|        fieldvalue  = me->get_fldvalue( |NEW_PROP_DESC| ) ) "comitem-fmcit-bezei )
                         ( fieldname = |NEW_CITEM_DESC|        fieldvalue  = me->get_fldvalue( |NEW_LONG_DESC| ) ) "comitem-fmcit-text1 )
                         ( fieldname = |NEW_CITEM_POST|        fieldvalue  = comitem-fmci-kateg )
                         ( fieldname = |NEW_CITEM_TRTYP|       fieldvalue  = COND #( WHEN comitem-fmci-fivor IS INITIAL
                                                                                     THEN space
                                                                                     ELSE comitem-fmci-fivor
                                                                                   ) )
                         ( fieldname = |NEW_CITEM_ITCATEG|     fieldvalue  = COND #( WHEN comitem-fmci-potyp IS INITIAL
                                                                                     THEN space
                                                                                     ELSE comitem-fmci-potyp
                                                                                   ) )
                         ( fieldname = |NEW_CITEM_SUP_ITCATEG| fieldvalue  = COND #( WHEN comitem-fmci-fipup IS INITIAL
                                                                                     THEN space
                                                                                     ELSE comitem-fmci-fipup
                                                                                   ) )
                         ( fieldname = |NEW_CITEM_VARIANT|     fieldvalue  = comitem-fmci-stvar )
                                 )
                     ).
    ENDIF.

* Section 5
    IF me->get_fldvalue( |REF_GLFSV| ) AND
       VALUE #( field_info[ fieldname = |REF_GLFSV| ]-changed OPTIONAL ) = abap_true.
      me->set_fldvalue( VALUE #( LET fsvdata  = is_fsvdata
                                     index    = 1 IN fieldindex = index
                         ( fieldname = |NEW_FSVHIER|    fieldvalue = me->get_fldvalue( |REF_FSVHIER| ) )
                         ( fieldname = |NEW_ALTFSVHIER| fieldvalue = me->get_fldvalue( |REF_ALTFSVHIER| ) )
                                )
                      ).
    ENDIF.

* Section 6
    IF me->get_fldvalue( |REF_COSTELEM| ) AND
       VALUE #( field_info[ fieldname = |REF_COSTELEM| ]-changed OPTIONAL ) = abap_true.
      me->set_fldvalue( VALUE #( LET costelem = is_celemdata
                                     index    = 1 IN fieldindex = index
                         ( fieldname = |NEW_CLEM_BEGDA|   fieldvalue = costelem-valid_from )
                         ( fieldname = |NEW_CLEM_ENDDA|   fieldvalue = costelem-valid_to )
                         ( fieldname = |NEW_CELEM_NAME|   fieldvalue = me->get_fldvalue( |NEW_PROP_DESC| ) ) "costelem-name )
                         ( fieldname = |NEW_CELEM_DESC|   fieldvalue = me->get_fldvalue( |NEW_LONG_DESC| ) ) "costelem-descript )
                         ( fieldname = |NEW_CELEM_CATEG|  fieldvalue = costelem-celem_category )
                                )
                      ).
    ENDIF.

    "@TODO check Config here -> is Section 1 is the only section visible? -> If yes, copy over all texts
    "move this to ELSE block for every Section coding above
    me->set_fldvalue( VALUE #( LET short =  me->get_fldvalue( |NEW_PROP_DESC| )
                                   long  =  me->get_fldvalue( |NEW_LONG_DESC| )
                                   req   =  me->is_costelem_required( )
                               IN
                       ( fieldname = |NEW_PROP_DESC2|        fieldvalue  = short )
                       ( fieldname = |NEW_LONG_DESC2|        fieldvalue  = long  )
                       ( fieldname = |NEW_CITEM_NAME|        fieldvalue  = short )
                       ( fieldname = |NEW_CITEM_DESC|        fieldvalue  = long  )
                       ( fieldname = |NEW_CELEM_NAME|        fieldvalue  = COND #( WHEN req = abap_true THEN short ) )
                       ( fieldname = |NEW_CELEM_DESC|        fieldvalue  = COND #( WHEN req = abap_true THEN long  ) )
                              )
                    ).

    "special condition for Section 2 copying Group Account Number
    CHECK me->get_fldvalue( |REQUEST_TYPE| ) = 2.
    me->set_fldvalue( VALUE #( ( fieldname = |NEW_BILKT|  fieldvalue  = me->get_fldvalue( |REF_BILKT| ) ) ) ).

  ENDMETHOD.

  METHOD get_commitment.
    rs_result = me->ls_commitment.
  ENDMETHOD.

  METHOD set_commitment.
    me->ls_commitment = is_commitment.
  ENDMETHOD.

  METHOD commitment_mapper.

    rs_result = VALUE #( LET api_mode = me->get_api_mode( )
                 IN
                  fmci-fipex  = SWITCH #( api_mode
                                 WHEN action-insert
                                 THEN me->get_fldvalue( |NEW_COMITEM| )
                                 ELSE me->get_fldvalue( |REF_COMITEM| )
                                        )
                  fmci-fipos  = rs_result-fmci-fipex
                  fmci-fikrs  = me->get_fldvalue( |NEW_FM| )
                  fmci-kateg  = me->get_fldvalue( |NEW_CITEM_POST| )
                  fmci-fivor  = me->get_fldvalue( |NEW_CITEM_TRTYP| )
                  fmci-potyp  = me->get_fldvalue( |NEW_CITEM_ITCATEG| )
                  fmci-fipup  = me->get_fldvalue( |NEW_CITEM_SUP_ITCATEG| )
                  fmci-stvar  = me->get_fldvalue( |NEW_CITEM_VARIANT| )
                  fmcit-spras = sy-langu
                  fmcit-fipex = rs_result-fmci-fipex
                  fmcit-fikrs = me->get_fldvalue( |NEW_FM| )
                  fmcit-bezei = me->get_fldvalue( |NEW_CITEM_NAME| )
                  fmcit-text1 = me->get_fldvalue( |NEW_CITEM_DESC| )
                       ).

  ENDMETHOD.


  METHOD get_fy_start.
    r_result = COND #( WHEN sy-datum+4(2) BETWEEN |01| AND |06|
                       THEN |{ sy-datum(4) - 1 }0701|
                       ELSE |{ sy-datum(4) }0701|
                     ).
  ENDMETHOD.

  METHOD get_domain_values.

    cl_reca_ddic_doma=>get_values(
      EXPORTING
        id_name   =  iv_name
      IMPORTING
        et_values = DATA(lt_values)
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2
    ).

    CHECK sy-subrc = 0.

    rt_result = VALUE #( FOR ls_value IN lt_values
                    ( help_key   = ls_value-domvalue_l
                      help_value = ls_value-ddtext )
                       ).

  ENDMETHOD.

  METHOD get_fintrans.

    IF lt_fintrans IS INITIAL.

      DATA(lt_helpvalues) = get_domain_values( |FM_FIVOR| ).

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_CITEM_TRTYP_KEY|
          iv_fieldlabel      = |NEW_CITEM_TRTYP_LABEL|
        CHANGING
          et_additional_data = lt_fintrans
      ).

    ENDIF.

    rt_result = lt_fintrans.

  ENDMETHOD.

  METHOD get_comitem_categ.

    IF lt_citem_cat IS INITIAL.

      DATA(lt_helpvalues) = get_domain_values( |FM_POTYP| ).

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_CITEM_ITCATEG_KEY|
          iv_fieldlabel      = |NEW_CITEM_ITCATEG_LABEL|
        CHANGING
          et_additional_data = lt_citem_cat
      ).

    ENDIF.

    rt_result = lt_citem_cat.

  ENDMETHOD.

  METHOD get_supcomitem_categ.

    IF lt_scitem_cat IS INITIAL.

      zcl_fcom_isr_tool=>fill_help_value_without_key(
        EXPORTING
           it_helpvalues     = VALUE hrasr00help_dataset_tab(
                                FOR ls_fipup IN zcl_fi_commitment_factory=>get_all( )
                                ( help_key = ls_fipup-fipup ) )
          iv_fieldkey        = |NEW_CITEM_SUP_ITCATEG_KEY|
          iv_fieldlabel      = |NEW_CITEM_SUP_ITCATEG_LABEL|
        CHANGING
          et_additional_data = lt_scitem_cat
      ).

    ENDIF.

    rt_result = lt_scitem_cat.

  ENDMETHOD.

  METHOD get_comitem_var.

    IF lt_citem_var IS INITIAL.

      DATA(lt_helpvalues) = get_domain_values( |XFELD| ).

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_CITEM_VARIANT_KEY|
          iv_fieldlabel      = |NEW_CITEM_VARIANT_LABEL|
        CHANGING
          et_additional_data = lt_citem_var
      ).

    ENDIF.

    rt_result = lt_citem_var.

  ENDMETHOD.

  METHOD get_sortkey.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_sortkey IS INITIAL.

      SELECT zuawa, ttext
        FROM tzunt
        INTO TABLE @lt_helpvalues
        WHERE spras = @sy-langu.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_SORTKEY_KEY|
          iv_fieldlabel      = |NEW_SORTKEY_LABEL|
        CHANGING
          et_additional_data = lt_finstat
      ).

    ENDIF.

    rt_result = lt_sortkey.


  ENDMETHOD.

  METHOD get_fieldstatus.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_fldstatus IS INITIAL.

      SELECT fstag, fsttx
        FROM t004g
        INTO TABLE @lt_helpvalues
        WHERE spras = @sy-langu
        AND   bukrs = @tafe_kokrs.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_FSTAG_KEY|
          iv_fieldlabel      = |NEW_FSTAG_LABEL|
        CHANGING
          et_additional_data = lt_fldstatus
      ).

    ENDIF.

    rt_result = lt_fldstatus.

  ENDMETHOD.

  METHOD get_plan_level.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_planlevel IS INITIAL.

      SELECT ebene, ltext
        FROM t036t
        INTO TABLE @lt_helpvalues
        WHERE spras = @sy-langu.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_PLANLVL_KEY|
          iv_fieldlabel      = |NEW_PLANLVL_LABEL|
        CHANGING
          et_additional_data = lt_planlevel
      ).

    ENDIF.

    rt_result = lt_planlevel.

  ENDMETHOD.

  METHOD get_plan_relv.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.

    IF lt_planrelv IS INITIAL.

      SELECT plan_relv, plan_relv_text
        FROM zfi_gl_planning
        INTO TABLE @lt_helpvalues.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_PLANRELV_KEY|
          iv_fieldlabel      = |NEW_PLANRELV_LABEL|
        CHANGING
          et_additional_data = lt_planrelv
      ).

    ENDIF.

    rt_result = lt_planrelv.

  ENDMETHOD.

  METHOD get_comitem_post.

    IF lt_citem_post IS INITIAL.

      DATA(lt_helpvalues) = get_domain_values( |FM_KATEG| ).

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_CITEM_KATEG_KEY|
          iv_fieldlabel      = |NEW_CITEM_KATEG_LABEL|
        CHANGING
          et_additional_data = lt_citem_post
      ).

    ENDIF.

    rt_result = lt_citem_post.

  ENDMETHOD.

  METHOD get_info.
    rt_result = me->lt_info.
  ENDMETHOD.

  METHOD set_info.

    IF it_info IS NOT INITIAL.
      me->lt_info = it_info.
    ELSE.
      "FIELD_INFO table is not sent at PROCESS_USER_COMMAND from Portal Tasklist (Draft mode)
      me->lt_info = VALUE #( FOR info IN me->get_special( )
                      ( fieldname = info-fieldname )
                           ).
    ENDIF.
  ENDMETHOD.


  METHOD update_fieldinfo.

    " what has changed and what has not?

    CHECK me->get_info( ).

    DATA(field_info) = me->get_info( ).

    field_info[ fieldname = |REF_GLACCT| ]-changed   = xsdbool( ls_refaccts-glcoa_grp  <> me->get_fldvalue( |REF_GLACCT| ) OR
                                                          "checking Request Type has not been changed as a result NEW values are blank
                                                                ( NOT me->get_fldvalue( |NEW_XBILK| ) AND NOT me->get_fldvalue( |NEW_GVTYP| ) ) ).

    "the below 2 blocks have to be swapped over (from comment to uncomment and vice versa) when all Sections are required on the Form

*    field_info[ fieldname = |REF_GLACCT2| ]-changed  = xsdbool( ls_refaccts-glcoa_oper <> me->get_fldvalue( |REF_GLACCT2| ) ).
*    field_info[ fieldname = |REF_COMITEM| ]-changed  = xsdbool( ls_refaccts-comitem    <> me->get_fldvalue( |REF_COMITEM| ) ).
*    field_info[ fieldname = |REF_GLACCT4| ]-changed  = xsdbool( ls_refaccts-glccode    <> me->get_fldvalue( |REF_GLACCT4| ) ).
*    field_info[ fieldname = |REF_GLFSV| ]-changed    = xsdbool( ls_refaccts-glfsv      <> me->get_fldvalue( |REF_GLFSV| ) ).
*    field_info[ fieldname = |REF_COSTELEM| ]-changed = xsdbool( ls_refaccts-costelem   <> me->get_fldvalue( |REF_COSTELEM| ) ).

    field_info[ fieldname = |REF_GLACCT2| ]-changed  = xsdbool( ls_refaccts-glcoa_oper <> me->get_fldvalue( |REF_GLACCT| ) ).
    field_info[ fieldname = |REF_COMITEM| ]-changed  = xsdbool( ls_refaccts-comitem    <> me->get_fldvalue( |REF_GLACCT| ) ).
    field_info[ fieldname = |REF_GLACCT4| ]-changed  = xsdbool( ls_refaccts-glccode    <> me->get_fldvalue( |REF_GLACCT| ) ).
    field_info[ fieldname = |REF_GLFSV| ]-changed    = xsdbool( ls_refaccts-glfsv      <> COND #( WHEN me->get_fldvalue( |REQUEST_TYPE| ) = 1
                                                                                                  THEN me->get_fldvalue( |REF_GLACCT| )
                                                                                                  ELSE me->get_fldvalue( |REF_GLFSV| )   "because Section 5 also appears in Reassign
                                                                                                ) ).
    field_info[ fieldname = |REF_COSTELEM| ]-changed = xsdbool( ls_refaccts-costelem   <> me->get_fldvalue( |REF_GLACCT| ) ).

    me->set_info( field_info ).

  ENDMETHOD.

  METHOD get_costelement.
    rs_result = me->ls_costelement.
  ENDMETHOD.

  METHOD set_costelement.
    me->ls_costelement = is_costelement.
  ENDMETHOD.


  METHOD set_groups.

* we need to clear section specific data
*   - not possible to read fields from customising (QISRSCENARIO) as they are not Section specific

    CHECK lt_groups IS INITIAL.

    "Section 1
    lt_groups = VALUE #( LET num = 1 IN sectionnum = num
                 ( fieldname = |REF_GLACCT| )
                 ( fieldname = |REF_COA| )
                 ( fieldname = |NEW_KTOKS| )
                 ( fieldname = |REF_KTOKS| )
                 ( fieldname = |NEW_XBILK| )
                 ( fieldname = |REF_XBILK| )
                 ( fieldname = |NEW_GVTYP| )
                 ( fieldname = |REF_GVTYP| )
                 ( fieldname = |NEW_PROP_DESC| )
                 ( fieldname = |REF_PROP_DESC| )
                 ( fieldname = |NEW_LONG_DESC| )
                 ( fieldname = |REF_LONG_DESC| )
                 ( fieldname = |NEW_TOES| )
                 ( fieldname = |REF_TOES| )
                 ( fieldname = |NEW_FSTMT| )
                 ( fieldname = |REF_FSTMT| )
                 ( fieldname = |NEW_FSTMT_NOTE| )
                 ( fieldname = |REF_FSTMT_NOTE| )
                 ( fieldname = |NEW_CFLOW| )
                 ( fieldname = |REF_CFLOW| )
                 ( fieldname = |NEW_NCVER_DSC| )
                 ( fieldname = |REF_NCVER_DSC| )
                 ( fieldname = |NEW_NCVER_ACT| )
                 ( fieldname = |REF_NCVER_ACT| )
                 ( fieldname = |NEW_NCVER_CFLO| )
                 ( fieldname = |REF_NCVER_CFLO| )
                ).

    "Section 2
    lt_groups = VALUE #( LET num = 2 IN
                BASE lt_groups
                  ( sectionnum = num fieldname = |REF_GLACCT2| )
                  ( sectionnum = num fieldname = |REF_COA2| )
                  ( sectionnum = num fieldname = |NEW_KTOKS2| )
                  ( sectionnum = num fieldname = |REF_KTOKS2| )
                  ( sectionnum = num fieldname = |NEW_XBILK2| )
                  ( sectionnum = num fieldname = |REF_XBILK2| )
                  ( sectionnum = num fieldname = |NEW_GVTYP2| )
                  ( sectionnum = num fieldname = |REF_GVTYP2| )
                  ( sectionnum = num fieldname = |NEW_PROP_DESC2| )
                  ( sectionnum = num fieldname = |REF_PROP_DESC2| )
                  ( sectionnum = num fieldname = |NEW_LONG_DESC2| )
                  ( sectionnum = num fieldname = |REF_LONG_DESC2| )
                  ( sectionnum = num fieldname = |NEW_BILKT| )
                  ( sectionnum = num fieldname = |REF_BILKT| )
                       ).

    "Section 3
    lt_groups = VALUE #( LET num = 3 IN
            BASE lt_groups
              ( sectionnum = num fieldname = |REF_COMITEM| )
              ( sectionnum = num fieldname = |REF_FM| )
              ( sectionnum = num fieldname = |NEW_CITEM_NAME| )
              ( sectionnum = num fieldname = |REF_CITEM_NAME| )
              ( sectionnum = num fieldname = |NEW_CITEM_DESC| )
              ( sectionnum = num fieldname = |REF_CITEM_DESC| )
              ( sectionnum = num fieldname = |NEW_CITEM_POST| )
              ( sectionnum = num fieldname = |REF_CITEM_POST| )
              ( sectionnum = num fieldname = |NEW_CITEM_TRTYP| )
              ( sectionnum = num fieldname = |REF_CITEM_TRTYP| )
              ( sectionnum = num fieldname = |NEW_CITEM_ITCATEG| )
              ( sectionnum = num fieldname = |REF_CITEM_ITCATEG| )
              ( sectionnum = num fieldname = |NEW_CITEM_SUP_ITCATEG| )
              ( sectionnum = num fieldname = |REF_CITEM_SUP_ITCATEG| )
              ( sectionnum = num fieldname = |NEW_CITEM_VARIANT| )
              ( sectionnum = num fieldname = |REF_CITEM_VARIANT| )
                   ).

    "Section 4
    lt_groups = VALUE #( LET num = 4 IN
                BASE lt_groups
                  ( sectionnum = num fieldname = |REF_GLACCT4| )
                  ( sectionnum = num fieldname = |REF_COMPCODE4| )
                  ( sectionnum = num fieldname = |NEW_CURR| )
                  ( sectionnum = num fieldname = |REF_CURR| )
                  ( sectionnum = num fieldname = |NEW_BALLC| )
                  ( sectionnum = num fieldname = |REF_BALLC| )
                  ( sectionnum = num fieldname = |NEW_TAXCATEG| )
                  ( sectionnum = num fieldname = |REF_TAXCATEG| )
                  ( sectionnum = num fieldname = |NEW_POSTNOTAX| )
                  ( sectionnum = num fieldname = |REF_POSTNOTAX| )
                  ( sectionnum = num fieldname = |NEW_ALTACCT| )
                  ( sectionnum = num fieldname = |REF_ALTACCT| )
                  ( sectionnum = num fieldname = |NEW_OPENITEM| )
                  ( sectionnum = num fieldname = |REF_OPENITEM| )
                  ( sectionnum = num fieldname = |NEW_LITEM_DISP| )
                  ( sectionnum = num fieldname = |REF_LITEM_DISP| )
                  ( sectionnum = num fieldname = |NEW_SORTKEY| )
                  ( sectionnum = num fieldname = |REF_SORTKEY| )
                  ( sectionnum = num fieldname = |NEW_FSTAG| )
                  ( sectionnum = num fieldname = |REF_FSTAG| )
                  ( sectionnum = num fieldname = |NEW_AUTOPOST| )
                  ( sectionnum = num fieldname = |REF_AUTOPOST| )
                  ( sectionnum = num fieldname = |NEW_PLANLVL| )
                  ( sectionnum = num fieldname = |REF_PLANLVL| )
                  ( sectionnum = num fieldname = |NEW_CFLOW_RELV| )
                  ( sectionnum = num fieldname = |REF_CFLOW_RELV| )
                  ( sectionnum = num fieldname = |REF_CCODE_FIPEX| )
                  ( sectionnum = num fieldname = |NEW_PLANRELV| )
                  ( sectionnum = num fieldname = |REF_PLANRELV| )
                       ).

    "Section 5
    lt_groups = VALUE #( LET num = 5 IN
            BASE lt_groups
              ( sectionnum = num fieldname = |REF_GLFSV| )
              ( sectionnum = num fieldname = |REF_FSVCOA| )
              ( sectionnum = num fieldname = |NEW_FSVHIER| )
              ( sectionnum = num fieldname = |REF_FSVHIER| )
              ( sectionnum = num fieldname = |REF_ALTFSV| )
              ( sectionnum = num fieldname = |NEW_ALTFSVHIER| )
              ( sectionnum = num fieldname = |REF_ALTFSVHIER| )
                   ).

    "Section 6
    lt_groups = VALUE #( LET num = 6 IN
            BASE lt_groups
              ( sectionnum = num fieldname = |REF_COSTELEM| )
              ( sectionnum = num fieldname = |REF_KOKRS| )
              ( sectionnum = num fieldname = |REF_CLEM_BEGDA| )
              ( sectionnum = num fieldname = |NEW_CLEM_ENDDA| )
              ( sectionnum = num fieldname = |REF_CLEM_ENDDA| )
              ( sectionnum = num fieldname = |NEW_CELEM_NAME| )
              ( sectionnum = num fieldname = |REF_CELEM_NAME| )
              ( sectionnum = num fieldname = |NEW_CELEM_DESC| )
              ( sectionnum = num fieldname = |REF_CELEM_DESC| )
              ( sectionnum = num fieldname = |NEW_CELEM_CATEG| )
              ( sectionnum = num fieldname = |REF_CELEM_CATEG| )
                   ).

    "Include Reason Text, New IDs, Sections Lock/Unlock and Set Deletion Flag for button Reset Form
    lt_groups = VALUE #( LET num = 0 IN
            BASE lt_groups
             ( sectionnum = num fieldname = |REASON| )
             ( sectionnum = num fieldname = |NEW_GLACCT| )
             ( sectionnum = num fieldname = |NEW_GLACCT2| )
             ( sectionnum = num fieldname = |NEW_GLACCT4| )
             ( sectionnum = num fieldname = |NEW_COMITEM| )
             ( sectionnum = num fieldname = |NEW_GLFSV| )
             ( sectionnum = num fieldname = |NEW_COSTELEM| )
             ( sectionnum = num fieldname = |REF_ACCTLOCK| )
             ( sectionnum = num fieldname = |REF_COALOCK| )
             ( sectionnum = num fieldname = |REF_LOCK| )
             ( sectionnum = num fieldname = |NEW_LOCK| )
             ( sectionnum = num fieldname = |REF_ACCTDEL| )
             ( sectionnum = num fieldname = |REF_COADEL| )
             ( sectionnum = num fieldname = |REF_DEL| )
             ( sectionnum = num fieldname = |NEW_DEL| )
                       ).

  ENDMETHOD.


  METHOD handle_commands.

    me->set_usercomm( ).
    me->set_groups( ).

    CHECK me->get_usercomm( ) AND me->get_command( ) IN me->get_usercomm( ).

    DATA(special_data) = me->get_special( ).

    DATA(l_secnum) = CONV i( LET l_str = me->get_command( ) IN
                        substring( val = l_str off = strlen( l_str ) - 1 len = 1 )
                           ).

    DATA(where) = COND #( WHEN me->get_command( ) <> `RESET0` THEN |sectionnum = l_secnum| ).

    LOOP AT me->get_groups( ) ASSIGNING FIELD-SYMBOL(<ls_group>)
         WHERE (where).
      special_data[ fieldname = <ls_group>-fieldname ]-fieldvalue = space.
    ENDLOOP.

    me->set_special( special_data ).

  ENDMETHOD.

  METHOD get_costelem_categ.

    IF lt_celem_cat IS INITIAL.

      DATA(lt_helpvalues) = get_domain_values( |KATYP| ).

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
        EXPORTING
          it_helpvalues      = lt_helpvalues
          iv_fieldkey        = |NEW_CELEM_CATEG_KEY|
          iv_fieldlabel      = |NEW_CELEM_CATEG_LABEL|
        CHANGING
          et_additional_data = lt_celem_cat
      ).

    ENDIF.

    rt_result = lt_celem_cat.

  ENDMETHOD.

  METHOD get_usercomm.
    rt_result = lt_usercomm.
  ENDMETHOD.

  METHOD set_usercomm.

    CHECK lt_usercomm IS INITIAL.

    lt_usercomm = VALUE #(
                    LET  s = rsmds_c_sign-including
                         o = rsmds_c_option-equal
                    IN sign   = s
                       option = o
                    ( low =  |RESET1| )
                    ( low =  |RESET2| )
                    ( low =  |RESET3| )
                    ( low =  |RESET4| )
                    ( low =  |RESET5| )
                    ( low =  |RESET6| )
                    ( low =  |RESET0| )
                         ).

  ENDMETHOD.

  METHOD get_groups.
    rt_result = lt_groups.
  ENDMETHOD.

  METHOD get_fsv_hierarchy.

    DATA lt_helpvalues TYPE hrasr00help_dataset_tab.
    DATA lt_additional TYPE qisrtspecial_param.

    IF SWITCH #( iv_versn
        WHEN tafe_fsv-tafe_fsv
        THEN lt_fsv_hier
        ELSE lt_fsv_hier2 ) IS INITIAL.

      CALL FUNCTION 'ZFI_GL_READ_FSV'
        EXPORTING
          iv_versn        = iv_versn          " Financial Statement Version
        IMPORTING
          rt_result       = lt_helpvalues
        EXCEPTIONS
          ex_no_authority = 1
          ex_load_error   = 2
          ex_read_error   = 3
          OTHERS          = 4.

      zcl_fcom_isr_tool=>fill_add_data_frm_help_value(
         EXPORTING
           it_helpvalues      = lt_helpvalues
           iv_fieldkey        = SWITCH #( iv_versn
                                 WHEN tafe_fsv-tafe_fsv
                                 THEN |NEW_FSVHIER_KEY|
                                 ELSE |NEW_ALTFSVHIER_KEY|
                                        )
           iv_fieldlabel      = SWITCH #( iv_versn
                                 WHEN tafe_fsv-tafe_fsv
                                 THEN |NEW_FSVHIER_LABEL|
                                 ELSE |NEW_ALTFSVHIER_LABEL|
                                        )
         CHANGING
           et_additional_data = lt_additional
       ).

      CASE iv_versn.
        WHEN tafe_fsv-tafe_fsv.
          lt_fsv_hier = lt_additional.
        WHEN tafe_fsv-tafe_alt_fsv.
          lt_fsv_hier2 = lt_additional.
        WHEN OTHERS.
          RAISE RESUMABLE EXCEPTION TYPE zcx_fi_general
            EXPORTING
              textid    = zcx_fi_general=>incorrect_params
              gv_string = |Parameter { iv_versn } is unknown!|.
      ENDCASE.

    ENDIF.

    rt_result = SWITCH #( iv_versn
                 WHEN tafe_fsv-tafe_fsv
                 THEN lt_fsv_hier
                 WHEN tafe_fsv-tafe_alt_fsv
                 THEN lt_fsv_hier2
                 ELSE THROW zcx_fi_general(
                              textid    = zcx_fi_general=>incorrect_params
                              gv_string = |Parameter { iv_versn } is unknown!|
                                          )
                       ).

  ENDMETHOD.


  METHOD costelem_mapper.

    rs_result = VALUE #(
                 LET prefix = SWITCH char3( me->get_api_mode( )
                               WHEN action-insert THEN |NEW| ELSE |REF| )
                 IN
                  cost_elem      = me->get_fldvalue( |{ prefix }_COSTELEM| )        "Key
                  celem_category = me->get_fldvalue( |{ prefix }_CELEM_CATEG| )     "readOnly
                  valid_from     = me->get_fldvalue( |{ prefix }_CLEM_BEGDA| )      "readOnly
                  valid_to       = me->get_fldvalue( |{ prefix }_CLEM_ENDDA| )      "readOnly
                  descript       = me->get_fldvalue( |NEW_CELEM_DESC| )             "updateable
                  name           = me->get_fldvalue( |NEW_CELEM_NAME| )             "updateable
                       ).

  ENDMETHOD.


  METHOD simulate.

    me->set_simulate( abap_true ).
    me->set_api_mode( SWITCH #( me->get_fldvalue( |REQUEST_TYPE| )
                       WHEN 1
                       THEN action-insert
                       ELSE action-update
                               )
                     ).

    "Simulate Maintain account in COA 2000 - Section 1
    me->set_coa_data( SWITCH #( me->get_api_mode( )
                      WHEN action-insert
                      THEN me->coa_mapper( )
                      ELSE me->read_glmast( VALUE #(
                                             saknr = me->get_fldvalue( |REF_GLACCT| )
                                             ktopl = me->get_fldvalue( |REF_COA| )
                                                   )
                                          )-coa_data
                             )
                   ).

    me->set_acct_names( me->acctname_mapper( ) ).

    me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                             saknr = CONV #( me->get_fldvalue(
                                                              SWITCH #( api_mode
                                                               WHEN action-insert
                                                               THEN |NEW_GLACCT|
                                                               ELSE |REF_GLACCT|
                                                                      )
                                                                             )
                                                           )
                                             ktopl = CONV #( me->get_fldvalue(
                                                              SWITCH #( api_mode
                                                               WHEN action-insert
                                                               THEN |NEW_COA|
                                                               ELSE |REF_COA|
                                                                      )
                                                                             )
                                                           )
                                                   )
                                           ).

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.

    "Simulate Maintain account in COA 1000 - Section 2
    me->set_coa_data( SWITCH #( me->get_api_mode( )
                       WHEN action-insert
                       THEN me->coa_mapper( coa_oper_tafe )
                       ELSE me->read_glmast( VALUE #(
                                              saknr = me->get_fldvalue( |REF_GLACCT2| )
                                              ktopl = me->get_fldvalue( |REF_COA2| )
                                                    )
                                           )-coa_data
                              )
                    ).

    me->set_acct_names( me->acctname_mapper( coa_oper_tafe ) ).

    me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                             saknr = CONV #( me->get_fldvalue(
                                                              SWITCH #( api_mode
                                                               WHEN action-insert
                                                               THEN |NEW_GLACCT2|
                                                               ELSE |REF_GLACCT2|
                                                                      )
                                                                             )
                                                           )
                                             ktopl = CONV #( me->get_fldvalue(
                                                              SWITCH #( api_mode
                                                               WHEN action-insert
                                                               THEN |NEW_COA2|
                                                               ELSE |REF_COA2|
                                                                      )
                                                                             )
                                                           )
                                                   )
                                          ).

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.

    "Simulate Commitment Item - Section 3
    me->set_commitment( me->commitment_mapper( ) ).
    me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                              fikrs = CONV #( me->get_fldvalue(
                                                               SWITCH #( api_mode
                                                                WHEN action-insert
                                                                THEN |NEW_FM|
                                                                ELSE |REF_FM|
                                                                       )
                                                                              )
                                                            )
                                              fipex = CONV #( me->get_fldvalue(
                                                                SWITCH #( api_mode
                                                                 WHEN action-insert
                                                                 THEN |NEW_COMITEM|
                                                                 ELSE |REF_COMITEM|
                                                                         )
                                                                      )
                                                            )
                                                   )
                                          ).

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.

    "Simulate Maintain account in CCODE 1020 - Section 4
    me->set_ccode_data( me->ccode_mapper( ) ).
    me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                             saknr = CONV #( me->get_fldvalue(
                                                              SWITCH #( api_mode
                                                               WHEN action-insert
                                                               THEN |NEW_GLACCT4|
                                                               ELSE |REF_GLACCT4|
                                                                      )
                                                                             )
                                                           )
                                             bukrs = CONV #( me->get_fldvalue(
                                                              SWITCH #( api_mode
                                                               WHEN action-insert
                                                               THEN |NEW_COMPCODE4|
                                                               ELSE |REF_COMPCODE4|
                                                                      )
                                                                             )
                                                           )
                                                   )
                                          ).

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.

    "Simulate Section Account Lock/Unlock
    IF me->get_fldvalue( |REQUEST_TYPE| ) = 3.
      me->lo_gl_factory =   NEW zcl_fi_glmast_factory(
                              iv_saknr      = CONV #( me->get_fldvalue( |REF_ACCTLOCK| ) )
                              iv_ktopl      = CONV #( me->get_fldvalue( |REF_COALOCK| ) )
                              iv_mode       = me->get_api_mode( )
                              iv_commit     = xsdbool( me->get_simulate( ) IS INITIAL )
                              iv_simulate   = me->get_simulate( )
                           ).

      IF me->lo_gl_factory IS BOUND.
        CAST zcl_fi_glmast_factory( me->lo_gl_factory )->set_coa_block( CONV #( me->get_fldvalue( |NEW_LOCK| ) ) ).
      ENDIF.
    ENDIF.

    "Simulate Section Account Set/Clear Deletion Flag
    IF me->get_fldvalue( |REQUEST_TYPE| ) = 4.
      me->lo_gl_factory =  NEW zcl_fi_glmast_factory(
                            iv_saknr      = CONV #( me->get_fldvalue( |REF_ACCTDEL| ) )
                            iv_ktopl      = CONV #( me->get_fldvalue( |REF_COADEL| ) )
                            iv_mode       = me->get_api_mode( )
                            iv_commit     = xsdbool( me->get_simulate( ) IS INITIAL )
                            iv_simulate   = me->get_simulate( )
                          ).

      IF me->lo_gl_factory IS BOUND.
        CAST zcl_fi_glmast_factory( me->lo_gl_factory )->set_coa_delete( CONV #( me->get_fldvalue( |NEW_DEL| ) ) ).
      ENDIF.
    ENDIF.


    "Simulation is tricky for the below MD objects so we avoid simulating them
    " for e.g. GL account needs to exist before it can be assigned to the FSV (for create mode)
    " GL account must exist before its Cost Element can be simulated etc. (for create mode)
    " could not think of a way to simulate Reassignment of FSV (no test_flag available)
    " you could do the simulation of Cost Element for Update mode if maintain/update Cost Element
    "is causing issues

    "Assign TAFE FSV - Section 5
*   IF me->is_fsv_required( ).
*    me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
*                              saknr = CONV #( me->get_fldvalue(
*                                               SWITCH #( api_mode
*                                                WHEN action-insert
*                                                THEN |NEW_GLFSV|
*                                                ELSE |REF_GLFSV|
*                                                       )
*                                                              )
*                                            )
*                              ktopl = coa_oper_tafe
*                              versn = CONV #( me->get_fldvalue( |NEW_FSV| ) )
*                              ergsl = CONV #( me->get_fldvalue( |NEW_FSVHIER| ) )
*                                   )
*                          ).
*
*    IF me->lo_gl_factory IS BOUND.
*      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
*    ENDIF.

    "Assign TAFE Alternate FSV - Section 5
*    me->lo_gl_factory =  me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
*                              saknr = CONV #( me->get_fldvalue(
*                                               SWITCH #( api_mode
*                                                WHEN action-insert
*                                                THEN |NEW_GLFSV|
*                                                ELSE |REF_GLFSV|
*                                                       )
*                                                              )
*                                            )
*                              ktopl = coa_oper_tafe
*                              versn = CONV #( me->get_fldvalue( |NEW_ALTFSV| ) )
*                              ergsl = CONV #( me->get_fldvalue( |NEW_ALTFSVHIER| ) )
*                                   )
*                          ).
*
*    IF me->lo_gl_factory IS BOUND AND me->is_alt_fsv( ).
*      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
*    ENDIF.
*   ENDIF.

    "Cost Element - Section 6
*    CHECK me->is_costelem_required( ).
*    me->set_costelement( me->costelem_mapper( ) ).
*    me->lo_gl_factory =  me->get_md_factory( VALUE #(
*                              kstar = CONV #( me->get_fldvalue( |NEW_COSTELEM| ) )
*                                                    )
*                                           ).
*
*    IF me->lo_gl_factory IS BOUND.
*      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
*    ENDIF.

  ENDMETHOD.


  METHOD check_changes.

* only applicable for Request Type 2
    IF me->get_fldvalue( |REQUEST_TYPE| ) = 2.

* check short text changes in COA 2000
      IF me->get_fldvalue( |REF_PROP_DESC| ) AND
         me->get_fldvalue( |REF_PROP_DESC| ) = me->get_fldvalue( |NEW_PROP_DESC| ).
        MESSAGE e052 WITH coa_group_tafe INTO me->l_dummy.
        _mac_add_msg me->lt_messages data(return).
      ENDIF.

* check long text changes in COA 2000
      IF me->get_fldvalue( |REF_LONG_DESC| ) AND
          me->get_fldvalue( |REF_LONG_DESC| ) = me->get_fldvalue( |NEW_LONG_DESC| ).
        MESSAGE e053 WITH coa_group_tafe INTO me->l_dummy.
        _mac_add_msg me->lt_messages return.
      ENDIF.


      "I have kept the source code below - please uncomment it when other Sections of the Form are required to be visible
      "At the moment, business decision is to hide Sections 2-6 on the Form


* check short text changes in COA 1000
*      IF me->get_fldvalue( |REF_PROP_DESC2| ) AND
*          me->get_fldvalue( |REF_PROP_DESC2| ) = me->get_fldvalue( |NEW_PROP_DESC2| ).
*        MESSAGE e052 WITH coa_oper_tafe INTO me->l_dummy.
*        _mac_add_msg me->lt_messages return.
*      ENDIF.

** check long text changes in COA 1000
*      IF me->get_fldvalue( |REF_LONG_DESC2| ) AND
*          me->get_fldvalue( |REF_LONG_DESC2| ) = me->get_fldvalue( |NEW_LONG_DESC2| ).
*        MESSAGE e053 WITH coa_oper_tafe INTO me->l_dummy.
*        _mac_add_msg me->lt_messages return.
*      ENDIF.

** check name changes in Commitment Items
*      IF me->get_fldvalue( |REF_CITEM_NAME| ) AND
*          me->get_fldvalue( |REF_CITEM_NAME| ) = me->get_fldvalue( |NEW_CITEM_NAME| ).
*        MESSAGE e054 WITH tafe_fmarea INTO me->l_dummy.
*        _mac_add_msg me->lt_messages return.
*      ENDIF.
*
** check description changes in Commitment Items
*      IF me->get_fldvalue( |REF_CITEM_DESC| ) AND
*          me->get_fldvalue( |REF_CITEM_DESC| ) = me->get_fldvalue( |NEW_CITEM_DESC| ).
*        MESSAGE e055 WITH tafe_fmarea INTO me->l_dummy.
*        _mac_add_msg me->lt_messages return.
*      ENDIF.
    ENDIF.

* only applicable for Request Type 3 - Lock/Unlock
    IF me->get_fldvalue( |REQUEST_TYPE| ) = 3.

* check lock indicator delta
      IF me->get_fldvalue( |REF_LOCK| ) = me->get_fldvalue( |NEW_LOCK| ).
        MESSAGE e056 WITH  |{ me->get_fldvalue( |REF_ACCTLOCK| ) ALPHA = OUT }| coa_oper_tafe INTO me->l_dummy.
        _mac_add_msg me->lt_messages return.
      ENDIF.
    ENDIF.

* only applicable for Request Type 4 - Set DeletionFlag
    IF me->get_fldvalue( |REQUEST_TYPE| ) = 4.

* check deletion flag delta
      IF me->get_fldvalue( |REF_DEL| ) = me->get_fldvalue( |NEW_DEL| ).
        MESSAGE e057 WITH  |{ me->get_fldvalue( |REF_ACCTDEL| ) ALPHA = OUT }| coa_oper_tafe INTO me->l_dummy.
        _mac_add_msg me->lt_messages return.
      ENDIF.
    ENDIF.

* only applicable for Request Type 5 - Reassign FSV
    IF me->get_fldvalue( |REQUEST_TYPE| ) = 5.

* check hierarchy node delta
      IF me->get_fldvalue( |REF_FSVHIER| ) = me->get_fldvalue( |NEW_FSVHIER| ).
        MESSAGE e058 WITH fsv_versn-tafe_fsv INTO me->l_dummy.
        _mac_add_msg me->lt_messages return.
      ENDIF.
* check alternate hierarchy values
      IF NOT me->get_fldvalue( |REF_ALTFSVHIER| ) AND me->get_fldvalue( |NEW_ALTFSVHIER| ).
        MESSAGE e059 WITH |{ me->get_fldvalue( |REF_GLFSV| ) ALPHA = OUT }| fsv_versn-tafe_alt_fsv INTO me->l_dummy.
        _mac_add_msg me->lt_messages return.
      ELSEIF NOT me->get_fldvalue( |NEW_ALTFSVHIER| ) AND me->get_fldvalue( |REF_ALTFSVHIER| ) OR
        me->get_fldvalue( |NEW_ALTFSVHIER| ) = me->get_fldvalue( |REF_ALTFSVHIER| ).
        MESSAGE i061 WITH |{ me->get_fldvalue( |REF_GLFSV| ) ALPHA = OUT }| fsv_versn-tafe_alt_fsv INTO me->l_dummy.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_alt_fsv.

    IF me->get_fldvalue( |REQUEST_TYPE| ) = 1.
      r_result = xsdbool( me->get_fldvalue( |NEW_ALTFSVHIER| ) IS NOT INITIAL ).
    ELSE.
      CHECK me->get_fldvalue( |REQUEST_TYPE| ) = 5.
      CHECK me->get_fldvalue( |REF_ALTFSVHIER| ).
      r_result = xsdbool( me->get_fldvalue( |REF_ALTFSVHIER| ) <> me->get_fldvalue( |NEW_ALTFSVHIER| ) AND
                          me->get_fldvalue( |NEW_ALTFSVHIER| )
                        ).
    ENDIF.

  ENDMETHOD.


  METHOD check_fsv_locks.

    CHECK me->is_fsv_required( ).

    CALL FUNCTION 'FI_BS_ENQUEUE'
      EXPORTING
        versn          = fsv_versn-tafe_fsv
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE e060 WITH fsv_versn-tafe_fsv INTO me->l_dummy.
      _mac_add_msg me->lt_messages data(return).
      RAISE RESUMABLE EXCEPTION TYPE zcx_fi_general
        EXPORTING
          gv_symsg = CORRESPONDING #( sy ).
    ELSE.
      CALL FUNCTION 'FI_BS_DEQUEUE'
        EXPORTING
          versn = fsv_versn-tafe_fsv.
    ENDIF.

    CALL FUNCTION 'FI_BS_ENQUEUE'
      EXPORTING
        versn          = fsv_versn-tafe_alt_fsv
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE e060 WITH fsv_versn-tafe_alt_fsv INTO me->l_dummy.
      _mac_add_msg me->lt_messages return.
      RAISE RESUMABLE EXCEPTION TYPE zcx_fi_general
        EXPORTING
          gv_symsg = CORRESPONDING #( sy ).
    ELSE.
      CALL FUNCTION 'FI_BS_DEQUEUE'
        EXPORTING
          versn = fsv_versn-tafe_alt_fsv.
    ENDIF.

  ENDMETHOD.


  METHOD is_costelem_required.

    TRY.
        DATA(lt_tvarvc) = VALUE /iwbep/t_cod_select_options( ( LINES OF zcl_com_fi_utilities=>get_tvarvc( |ZFI_GLFORM_COSTELEM| ) ) ).

        r_result = xsdbool( ( me->get_fldvalue( |NEW_GLACCT| ) IN lt_tvarvc OR
                            ( me->get_fldvalue( |REQUEST_TYPE| ) = 2 AND
                              me->get_fldvalue( |REF_GLACCT| ) IN lt_tvarvc ) )
                                                AND
                            NEW zcl_fi_glmast_factory( iv_saknr = CONV #( me->get_fldvalue( |REF_GLACCT| ) )
                                                       iv_ktopl = CONV #( me->get_fldvalue( |NEW_COA2| ) )
                                    )->read_glmast( )-coa_data-gvtyp IS NOT INITIAL
                          ).

      CATCH BEFORE UNWIND zcx_fi_general INTO me->lo_exception.
        CHECK me->lo_exception->is_resumable IS NOT INITIAL.
        lt_tvarvc = VALUE #(
                       ( sign   = rsmds_c_sign-including
                         option = rsmds_c_option-between
                         low    = |0000400000|
                         high   = |0000599999| )
                           ).
        RESUME.
    ENDTRY.

  ENDMETHOD.


  METHOD check_locks.

    CHECK me->get_mode( ) = |DISPLAY| AND me->get_view( ) = |ISR_APPROVE|.

    me->check_fsv_locks( ).
    me->check_costelem_locks( ).

  ENDMETHOD.


  METHOD check_costelem_locks.

* only valid for a cost element scenario and when Request Type is "Change Description"
    CHECK me->is_costelem_required( ) AND me->get_fldvalue( |REQUEST_TYPE| ) = 2.

    CALL FUNCTION 'ENQUEUE_EKCSKAE'
      EXPORTING
        ktopl          = coa_oper_tafe
        kstar          = CONV kstar( me->get_fldvalue( |NEW_COSTELEM| ) )
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE e065 WITH me->get_fldvalue( |NEW_COSTELEM| ) INTO me->l_dummy.
      _mac_add_msg me->lt_messages data(return).
      RAISE RESUMABLE EXCEPTION TYPE zcx_fi_general
        EXPORTING
          gv_symsg = CORRESPONDING #( sy ).
    ELSE.
      CALL FUNCTION 'DEQUEUE_EKCSKAE'
        EXPORTING
          ktopl = coa_oper_tafe
          kstar = CONV kstar( me->get_fldvalue( |NEW_COSTELEM| ) ).
    ENDIF.

  ENDMETHOD.

  METHOD maintain_masterdata.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

* To be able to maintain from outside, we ensure that:
    " - ISR Notification exists
    " - ISR Notification is not completed before update of master data objects; and

    DATA(lo_notification) = zcl_swf_glmast=>get_instance( iv_notification ).

    IF lo_notification->is_complete( ).
      MESSAGE e066 WITH iv_notification INTO DATA(dummy).
      DATA(lo_exception) = NEW zcx_fi_general( gv_symsg = CORRESPONDING #( sy ) ).
      lo_exception->add_sy_message( ).
      RAISE EXCEPTION lo_exception.
    ENDIF.

    me->set_special( lo_notification->get_value_all( ) ).
    me->set_mode( iv_mode ).
    me->set_view( iv_view ).
    me->set_command( iv_command ).

    " - ISR Notification is at the correct stage of the process before master data updates
    CHECK:
      me->get_mode( )    = me->get_fldvalue( |ISR_MODE| )      AND
      me->get_view( )    = me->get_fldvalue( |ISR_FORM_VIEW| ) AND
      me->get_command( ) = me->get_fldvalue( |ISR_EVENT| ).

    rs_result = me->maintain( ).

  ENDMETHOD.


  METHOD maintain.

    me->set_api_mode( SWITCH #( me->get_fldvalue( |REQUEST_TYPE| )
                       WHEN 1
                       THEN action-insert
                       ELSE action-update
                               )
                     ).

    "Maintain GL account in COA 2000 - Section 1
    me->set_coa_data( SWITCH #( me->get_api_mode( )
                      WHEN action-insert
                      THEN me->coa_mapper( )
                      ELSE me->read_glmast( VALUE #(
                                             saknr = me->get_fldvalue( |REF_GLACCT| )
                                             ktopl = me->get_fldvalue( |REF_COA| )
                                                   )
                                          )-coa_data
                             )
                   ).

    me->set_acct_names( me->acctname_mapper( ) ).

    TRY.
        me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                                 saknr = CONV #( me->get_fldvalue(
                                                                  SWITCH #( api_mode
                                                                   WHEN action-insert
                                                                   THEN |NEW_GLACCT|
                                                                   ELSE |REF_GLACCT|
                                                                          )
                                                                                 )
                                                               )
                                                 ktopl = CONV #( me->get_fldvalue(
                                                                  SWITCH #( api_mode
                                                                   WHEN action-insert
                                                                   THEN |NEW_COA|
                                                                   ELSE |REF_COA|
                                                                          )
                                                                                 )
                                                               )
                                                       )
                                               ).

      CATCH BEFORE UNWIND zcx_fi_general INTO DATA(lo_exception).
        _mac_resume_if_resumable lo_exception->is_resumable.
    ENDTRY.

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.


    "Maintain GL account in COA 1000 - Section 2
    me->set_coa_data( SWITCH #( me->get_api_mode( )
                       WHEN action-insert
                       THEN me->coa_mapper( coa_oper_tafe )
                       ELSE me->read_glmast( VALUE #(
                                              saknr = me->get_fldvalue( |REF_GLACCT2| )
                                              ktopl = me->get_fldvalue( |REF_COA2| )
                                                    )
                                           )-coa_data
                              )
                    ).

    me->set_acct_names( me->acctname_mapper( coa_oper_tafe ) ).

    TRY.
        me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                                 saknr = CONV #( me->get_fldvalue(
                                                                  SWITCH #( api_mode
                                                                   WHEN action-insert
                                                                   THEN |NEW_GLACCT2|
                                                                   ELSE |REF_GLACCT2|
                                                                          )
                                                                                 )
                                                               )
                                                 ktopl = CONV #( me->get_fldvalue(
                                                                  SWITCH #( api_mode
                                                                   WHEN action-insert
                                                                   THEN |NEW_COA2|
                                                                   ELSE |REF_COA2|
                                                                          )
                                                                                 )
                                                               )
                                                       )
                                              ).

      CATCH BEFORE UNWIND zcx_fi_general INTO lo_exception.
        _mac_resume_if_resumable lo_exception->is_resumable.
    ENDTRY.

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.

    "Maintain Commitment Item - Section 3
    me->set_commitment( me->commitment_mapper( ) ).

    TRY.
        me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                                  fikrs = CONV #( me->get_fldvalue(
                                                                   SWITCH #( api_mode
                                                                    WHEN action-insert
                                                                    THEN |NEW_FM|
                                                                    ELSE |REF_FM|
                                                                           )
                                                                                  )
                                                                )
                                                  fipex = CONV #( me->get_fldvalue(
                                                                    SWITCH #( api_mode
                                                                     WHEN action-insert
                                                                     THEN |NEW_COMITEM|
                                                                     ELSE |REF_COMITEM|
                                                                             )
                                                                          )
                                                                )
                                                       )
                                              ).

      CATCH BEFORE UNWIND zcx_fi_general INTO lo_exception.
        _mac_resume_if_resumable lo_exception->is_resumable.
    ENDTRY.

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.

    "Maintain GL account in CCODE 1020 - Section 4
    me->set_ccode_data( me->ccode_mapper( ) ).

    TRY.
        me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                                 saknr = CONV #( me->get_fldvalue(
                                                                  SWITCH #( api_mode
                                                                   WHEN action-insert
                                                                   THEN |NEW_GLACCT4|
                                                                   ELSE |REF_GLACCT4|
                                                                          )
                                                                                 )
                                                               )
                                                 bukrs = CONV #( me->get_fldvalue(
                                                                  SWITCH #( api_mode
                                                                   WHEN action-insert
                                                                   THEN |NEW_COMPCODE4|
                                                                   ELSE |REF_COMPCODE4|
                                                                          )
                                                                                 )
                                                               )
                                                       )
                                              ).

      CATCH BEFORE UNWIND zcx_fi_general INTO lo_exception.
        _mac_resume_if_resumable lo_exception->is_resumable.
    ENDTRY.

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.

    "Maintain Section Account Lock/Unlock
    IF me->get_fldvalue( |REQUEST_TYPE| ) = 3.
      me->lo_gl_factory =   NEW zcl_fi_glmast_factory(
                              iv_saknr      = CONV #( me->get_fldvalue( |REF_ACCTLOCK| ) )
                              iv_ktopl      = CONV #( me->get_fldvalue( |REF_COALOCK| ) )
                              iv_mode       = me->get_api_mode( )
                              iv_commit     = abap_false
                              iv_simulate   = me->get_simulate( )
                           ).

      IF me->lo_gl_factory IS BOUND.
        CAST zcl_fi_glmast_factory( me->lo_gl_factory )->set_coa_block( CONV #( me->get_fldvalue( |NEW_LOCK| ) ) ).
      ENDIF.
    ENDIF.

    "Maintain Section Account Set/Clear Deletion Flag
    IF me->get_fldvalue( |REQUEST_TYPE| ) = 4.
      me->lo_gl_factory =  NEW zcl_fi_glmast_factory(
                            iv_saknr      = CONV #( me->get_fldvalue( |REF_ACCTDEL| ) )
                            iv_ktopl      = CONV #( me->get_fldvalue( |REF_COADEL| ) )
                            iv_mode       = me->get_api_mode( )
                            iv_commit     = abap_false
                            iv_simulate   = me->get_simulate( )
                          ).

      IF me->lo_gl_factory IS BOUND.
        CAST zcl_fi_glmast_factory( me->lo_gl_factory )->set_coa_delete( CONV #( me->get_fldvalue( |NEW_DEL| ) ) ).
      ENDIF.
    ENDIF.

    "Check FSV & Cost Element can be locked before updates
    me->check_locks( ).

    "Assign TAFE FSV - Section 5
    IF me->is_fsv_required( ).

      TRY.
          me->lo_gl_factory = me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                    saknr = CONV #( me->get_fldvalue(
                                                     SWITCH #( api_mode
                                                      WHEN action-insert
                                                      THEN |NEW_GLFSV|
                                                      ELSE |REF_GLFSV|
                                                             )
                                                                    )
                                                  )
                                    ktopl = coa_oper_tafe
                                    versn = CONV #( me->get_fldvalue( |NEW_FSV| ) )
                                    ergsl = CONV #( me->get_fldvalue( |NEW_FSVHIER| ) )
                                         )
                                ).

        CATCH BEFORE UNWIND zcx_fi_general INTO lo_exception.
          _mac_resume_if_resumable lo_exception->is_resumable.
      ENDTRY.

      IF me->lo_gl_factory IS BOUND.
        me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
      ENDIF.

      "Assign TAFE Alternate FSV - Section 5
      TRY.
          me->lo_gl_factory =  me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                    saknr = CONV #( me->get_fldvalue(
                                                     SWITCH #( api_mode
                                                      WHEN action-insert
                                                      THEN |NEW_GLFSV|
                                                      ELSE |REF_GLFSV|
                                                             )
                                                                    )
                                                  )
                                    ktopl = coa_oper_tafe
                                    versn = CONV #( me->get_fldvalue( |NEW_ALTFSV| ) )
                                    ergsl = CONV #( me->get_fldvalue( |NEW_ALTFSVHIER| ) )
                                         )
                                ).

        CATCH BEFORE UNWIND zcx_fi_general INTO lo_exception.
          _mac_resume_if_resumable lo_exception->is_resumable.
      ENDTRY.

      IF me->lo_gl_factory IS BOUND AND me->is_alt_fsv( ).
        me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
      ENDIF.
    ENDIF.

    "Maintain Cost Element - Section 6
    CHECK me->is_costelem_required( ).
    me->set_costelement( me->costelem_mapper( ) ).

    TRY.
        me->lo_gl_factory =  me->get_md_factory( VALUE #( LET api_mode = me->get_api_mode( ) IN
                                  kstar = CONV #( me->get_fldvalue(
                                                   SWITCH #( api_mode
                                                   WHEN action-insert
                                                   THEN |NEW_COSTELEM|
                                                   ELSE |REF_COSTELEM|
                                                           )
                                                                  )
                                                )
                                                        )
                                               ).

      CATCH BEFORE UNWIND zcx_fi_general INTO lo_exception.
        _mac_resume_if_resumable lo_exception->is_resumable.
    ENDTRY.

    IF me->lo_gl_factory IS BOUND.
      me->lo_gl_factory->zif_masterdata_factory~maintain_data( ).
    ENDIF.

    "Maintain new GL account number in table ZFIC_EBS_GLACCNT for EBS
    me->persist_ebs_glmast( ).

    "if you are here then it is a good day today - so let us update return with the good news!
    MESSAGE s099 INTO me->l_dummy.
    zcl_com_bapireturn_services=>add_msg_to_str( CHANGING cs_return = rs_result ).

  ENDMETHOD.

  METHOD simulate_masterdata.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

* To be able to Simulate from outside, we ensure that:
    " - ISR Notification exists

    me->set_special( zcl_swf_glmast=>get_instance( iv_notification )->get_value_all( ) ).

    TRY.
        me->simulate( ).

      CATCH BEFORE UNWIND zcx_fi_general INTO DATA(lo_exception).
        IF lo_exception->is_resumable IS NOT INITIAL.
          "that's OK - we continue with rest of the updates until a "hard" exception is raised
          RESUME.
        ELSE.
          me->set_messages( lo_exception->get_messages( ) ).
          rt_return = me->get_messages( ).
          RETURN.
        ENDIF.
    ENDTRY.

    rt_return = me->get_messages( ).

  ENDMETHOD.

  METHOD is_fsv_required.
    r_result = xsdbool(  me->get_fldvalue( |REQUEST_TYPE| ) = 1 OR
                         me->get_fldvalue( |REQUEST_TYPE| ) = 5 ).
  ENDMETHOD.


  METHOD set_approval_status.

* Do not set/update any other fields here apart from what is updated below (only STATUS CONTROL fields)

    DATA(lt_special) = me->get_special( ).

    me->set_control( CHANGING ct_special_data = lt_special ).

    IF me->get_command( ) = |APPROVE|.          "Approve button was clicked by the Approver
      lt_special[ fieldname = |APPRV_STATUS| ]-fieldvalue =
        SWITCH #( me->get_fldvalue( |APPRV_STATUS| )
         WHEN `SUBMIT`
         THEN |APPRVD_L1|
         WHEN `APPRVD_L1`
         THEN |APPRVD_L2|
         WHEN `APPRVD_L2`
         THEN |APPRVD_L3|
         ELSE space         "N/A
               ).
    ELSEIF me->get_command( ) = |REJECT|.       "Reject button was clicked by the Approver
      lt_special[ fieldname = |APPRV_STATUS| ]-fieldvalue = |REJECT|.
    ELSEIF me->get_command( ) = |BACKTO|.       "Back to Requestor button was clicked by the Approver
      lt_special[ fieldname = |APPRV_STATUS| ]-fieldvalue = |SUBMIT|.
    ELSEIF me->get_fldvalue( |APPRV_STATUS| ) = |WITHDRAWN|. "Withdraw button was clicked by Requestor
      "overwriting ISR Mode and ISR Event in case form was Withdrawn by the Requestor
      lt_special[ fieldname = |ISR_MODE| ]-fieldvalue  = |CLOSE|.
      lt_special[ fieldname = |ISR_EVENT| ]-fieldvalue = |WITHDRAWN|.
    ELSE.
      RETURN.
    ENDIF.

    rt_result = me->set_special_isr( lt_special ).

  ENDMETHOD.


  METHOD set_control.

    "if it short-dumps here then ISR customising needs to be addressed to add the Characteristics below
    ct_special_data[ fieldname = |ISR_MODE|       ]-fieldvalue = me->get_mode( ).
    ct_special_data[ fieldname = |ISR_EVENT|      ]-fieldvalue = me->get_command( ).
    ct_special_data[ fieldname = |ISR_FORM_VIEW|  ]-fieldvalue = me->get_view( ).
    ct_special_data[ fieldname = |ISR_REQUEST_NO|
                   ]-fieldvalue = COND #( WHEN NOT me->get_objectkey( )
                                          THEN l_notif
                                          ELSE me->get_objectkey( )
                                        ).

  ENDMETHOD.

  METHOD get_objectkey.
    r_result = me->l_objectkey.
  ENDMETHOD.

  METHOD set_objectkey.
    me->l_objectkey = iv_objectkey.
  ENDMETHOD.


  METHOD set_special_isr.

    CALL FUNCTION 'ISR_SPECIAL_DATA_SET'
      EXPORTING
        notification_no               = me->get_objectkey( )
        special_data                  = COND #( WHEN it_special IS INITIAL
                                                THEN me->get_special( )
                                                ELSE it_special
                                              )
      IMPORTING
        et_return                     = rt_result
      EXCEPTIONS
        no_internal_service_request   = 1
        invalid_notif_number          = 2
        int_service_request_not_found = 3
        OTHERS                        = 4.

  ENDMETHOD.


  METHOD persist_ebs_glmast.

    CHECK me->get_fldvalue( |REQUEST_TYPE| ) = 1.       "only applicable for Create new GL

    "maintain new GL account number in table ZFIC_EBS_GLACCNT for EBS
    zcl_com_tabl_upd_utility=>update_dbtab(
      EXPORTING
        iv_tabnm                    = |ZFIC_EBS_GLACCNT|
        it_data                     = VALUE tty_ebs_gl(
                                        ( company_code = tafe_compcode
                                          gl_account   = me->get_fldvalue( |NEW_GLACCT| )
                                          inactive     = abap_false
                                          date_update  = sy-datum
                                          time_update  = sy-uzeit
                                          user_update  = sy-uname )
                                                      )
      IMPORTING
        ev_dbcnt                    = DATA(dbcnt)
      EXCEPTIONS
        ex_dbtabl_not_found         = 1
        ex_data_tab_empty           = 2
        ex_tabl_upd_no_support      = 3
        ex_data_tab_wrong_format    = 4
        ex_dbtabl_not_locked        = 5
        ex_dbtabl_not_updated       = 6
        ex_exceeds_max_rows_allowed = 7
        OTHERS                      = 8
    ).

    CHECK sy-subrc <> 0 OR dbcnt = 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO me->l_dummy.

    RAISE EXCEPTION TYPE zcx_fi_general EXPORTING gv_symsg = CORRESPONDING #( sy ).

  ENDMETHOD.


  METHOD check_approvers.

    CHECK me->get_mode( ) = |CHANGE| AND me->get_view( ) = |ISR_APPROVE|.       "logic only for Approvers

    IF  me->get_fldvalue( |APPRV_STATUS| ) =  |APPRVD_L1| OR   "only Level 2 & 3 approvers have a checklist
        me->get_fldvalue( |APPRV_STATUS| ) =  |APPRVD_L2|.

* validate all checkboxes of the Approver Checklist
      CASE me->get_fldvalue( |APPRV_STATUS| ).
        WHEN `APPRVD_L1`.
          "Person viewing is Level 2 approver
          CASE me->get_fldvalue( |REQUEST_TYPE| ).
            WHEN 1.
              IF me->get_command( ) = |APPROVE|.     "checking only when Approve button is clicked
                _mac_check_field `CBOX1` e067 1 ``.
                _mac_check_field `CBOX2` e067 2 ``.
                _mac_check_field `CBOX3` e067 3 ``.
                _mac_check_field `CBOX4` e067 4 ``.
                _mac_check_field `CBOX5` e067 5 ``.
              ENDIF.

            WHEN 2.
              IF me->get_command( ) = |APPROVE|.     "checking only when Approve button is clicked
                _mac_check_field `CBOX6` e067 1 ``.
                _mac_check_field `CBOX7` e067 2 ``.
              ENDIF.

            WHEN 3.
              IF me->get_fldvalue( |NEW_LOCK| ).
                IF me->get_command( ) = |APPROVE|.     "checking only when Approve button is clicked
                  _mac_check_field `CBOX8`  e067 1 ``.
                  _mac_check_field `CBOX9`  e067 2 ``.
                  _mac_check_field `CBOX10` e067 3 ``.
                  _mac_check_field `CBOX11` e067 4 ``.
                ENDIF.
              ELSE.
                IF me->get_command( ) = |APPROVE|.     "checking only when Approve button is clicked
                  _mac_check_field `CBOX12` e067 1 ``.
                  _mac_check_field `CBOX13` e067 2 ``.
                  _mac_check_field `CBOX14` e067 3 ``.
                  _mac_check_field `CBOX15` e067 4 ``.
                ENDIF.
              ENDIF.
            WHEN 4.
              IF me->get_command( ) = |APPROVE|.     "checking only when Approve button is clicked
                _mac_check_field `CBOX16` e067 1 ``.
                _mac_check_field `CBOX17` e067 2 ``.
                _mac_check_field `CBOX18` e067 3 ``.
                _mac_check_field `CBOX19` e067 4 ``.
              ENDIF.

            WHEN 5.
              IF me->get_command( ) = |APPROVE|.     "checking only when Approve button is clicked
                _mac_check_field `CBOX20` e067 1 ``.
                _mac_check_field `CBOX21` e067 2 ``.
              ENDIF.

            WHEN OTHERS.
          ENDCASE.

          "Level 2 approver has to enter a comment when:
          "Reject Submission   - user_command = 'REJECT'
          "Revised Submissions - user_command = 'BACKTO'
          IF me->get_command( )  = |REJECT| OR me->get_command( ) = |BACKTO|.
            "validation for Comments for Level 1 approver
            DATA(string) = SWITCH #( me->get_command( )
                             WHEN `REJECT` THEN |Rejection|
                             ELSE |Revision| ).
            _mac_check_field `REJ_COMMENTS` e069 string  ``.
          ENDIF.

        WHEN `APPRVD_L2`.
          "Person viewing is Level 3 Approver
          IF me->get_command( ) = |APPROVE|.     "checking only when Approve button is clicked
            _mac_check_field `CBOX25` e068 `` ``.
          ENDIF.

          "Level 3 approver has to enter a comment when:
          "Reject Submission   - user_command = 'REJECT'
          "Revised Submissions - user_command = 'BACKTO'
          IF me->get_command( )  = |REJECT| OR me->get_command( ) = |BACKTO|.
            "validation for Comments for Level 1 approver
            string = SWITCH #( me->get_command( )
                      WHEN `REJECT` THEN |Rejection|
                      ELSE |Revision| ).
            _mac_check_field `REJ_COMMENTS` e069 string  ``.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.

      "Level 1 approver has to enter a comment when:
      "Reject Submission   - user_command = 'REJECT'
      "Revised Submissions - user_command = 'BACKTO'
    ELSEIF me->get_fldvalue( |APPRV_STATUS| ) =  |SUBMIT| OR    "Confirms a Level 1 approver
           me->get_fldvalue( |APPRV_STATUS| ) =  |BACKTO|.
      IF me->get_command( )  = |REJECT| OR me->get_command( ) = |BACKTO|.
        "validation for Comments for Level 1 approver
        string = SWITCH #( me->get_command( )
                   WHEN `REJECT` THEN |Rejection|
                   ELSE |Revision| ).
        _mac_check_field `REJ_COMMENTS` e069 string  ``.
      ENDIF.
    ENDIF.

    DATA(messages) = me->get_messages( ).
    cs_return = VALUE #( messages[ 1 ] OPTIONAL ).

  ENDMETHOD.

ENDCLASS.
