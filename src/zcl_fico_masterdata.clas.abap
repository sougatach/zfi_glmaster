CLASS zcl_fico_masterdata DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_fi_global_constants .
    INTERFACES zif_masterdata
      ALL METHODS ABSTRACT .

    ALIASES tafe_fm_area
      FOR zif_fi_global_constants~fm_area_1000 .
    ALIASES tafe_fsv
      FOR zif_fi_global_constants~co_fsv_versn .

    "! <p class="shorttext synchronized">Singleton</p>
    "! @parameter iv_saknr | <p class="shorttext synchronized">GL Account</p>
    "! @parameter iv_costelem | <p class="shorttext synchronized">Cost Element</p>
    "! @parameter iv_comitem | <p class="shorttext synchronized">Commitment Item</p>
    "! @parameter iv_versn | <p class="shorttext synchronized">FSV Version</p>
    "! @parameter ro_md_api | <p class="shorttext synchronized">Instance of Master data Subclass</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
    CLASS-METHODS get_instance
      IMPORTING
        !iv_saknr        TYPE any OPTIONAL
        !iv_costelem     TYPE any OPTIONAL
        !iv_comitem      TYPE any OPTIONAL
        !iv_versn        TYPE any OPTIONAL
      RETURNING
        VALUE(ro_md_api) TYPE REF TO zcl_fico_masterdata
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Set Simulation mode</p>
    METHODS set_simulate
      IMPORTING
        !iv_simulate TYPE boole_d .
    "! <p class="shorttext synchronized">Set API Simulation Mode</p>
    METHODS set_mode
      IMPORTING
        !iv_mode TYPE glaccount_action .
    "! <p class="shorttext synchronized">Set Wait parameter for Commit</p>
    METHODS set_wait
      IMPORTING
        !iv_wait TYPE boole_d .
    "! <p class="shorttext synchronized">Set Commit Flag</p>
    METHODS set_commit
      IMPORTING
        iv_commit TYPE boole_d .
    "! <p class="shorttext synchronized">Clear Attributes</p>
    METHODS clear.

  PROTECTED SECTION.

    ALIASES action
      FOR zif_fi_global_constants~co_action .
    ALIASES dummy
      FOR zif_masterdata~lv_dummy .
    ALIASES lo_exception
      FOR zif_masterdata~lo_exception .
    ALIASES return
      FOR zif_masterdata~ls_return .

    DATA lv_simulate TYPE boole_d .
    DATA lv_mode TYPE glaccount_action .
    DATA lv_wait TYPE boole_d .
    DATA lv_commit TYPE boole_d .

    "! <p class="shorttext synchronized">Get Simulation mode</p>
    METHODS get_simulate
      RETURNING
        VALUE(r_result) TYPE boole_d .
    "! <p class="shorttext synchronized">Get API Run mode</p>
    METHODS get_mode
      RETURNING
        VALUE(r_result) TYPE glaccount_action .
    "! <p class="shorttext synchronized">To Commit or not to</p>
    METHODS _commit .
    "! <p class="shorttext synchronized">Get Wait parameter for Commit</p>
    METHODS get_wait
      RETURNING
        VALUE(r_result) TYPE boole_d .
    "! <p class="shorttext synchronized">Get Commit Flag</p>
    METHODS get_commit
      RETURNING
        VALUE(rv_commit) TYPE boole_d .
    "! <p class="shorttext synchronized">Check API Mode</p>
    METHODS check_mode
          ABSTRACT
      RAISING
        RESUMABLE(zcx_fi_general) .
  PRIVATE SECTION.

    CLASS-DATA lo_md_api TYPE REF TO zcl_fico_masterdata .
    CLASS-DATA lt_obj_refs TYPE tty_obj_refs.
ENDCLASS.



CLASS zcl_fico_masterdata IMPLEMENTATION.


  METHOD get_commit.
    rv_commit = me->lv_commit.
  ENDMETHOD.


  METHOD get_instance.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    "Design Pattern -> Singleton
    TRY.
        DATA(ls_ref) = lt_obj_refs[ saknr = |{ iv_saknr    ALPHA = IN }|
                                    kstar = |{ iv_costelem ALPHA = IN }|
                                    fipex = |{ iv_comitem  ALPHA = OUT }|
                                    versn = |{ iv_versn    ALPHA = IN }|
                                  ].

        ro_md_api = ls_ref-ref.

      CATCH cx_sy_itab_line_not_found.
        lt_obj_refs = VALUE #( BASE lt_obj_refs
                                 ( saknr = |{ iv_saknr    ALPHA = IN }|
                                   kstar = |{ iv_costelem ALPHA = IN }|
                                   fipex = |{ iv_comitem  ALPHA = OUT }|
                                   versn = |{ iv_versn    ALPHA = IN }|
                                   ref   = COND #( WHEN iv_saknr IS NOT INITIAL
                                                   THEN SWITCH #( iv_versn
                                                         WHEN space
                                                         THEN NEW zcl_fi_glmast_api( iv_saknr )
                                                         ELSE NEW zcl_fi_fsv_api( iv_saknr )
                                                                )
                                                   WHEN iv_costelem IS NOT INITIAL
                                                   THEN NEW zcl_fi_costelem_api( iv_costelem )
                                                   WHEN iv_comitem IS NOT INITIAL
                                                   THEN NEW zcl_fi_commitment_api( iv_comitem )
                                                   ELSE THROW zcx_fi_general(
                                                         textid = zcx_fi_general=>incorrect_params
                                                                            )
                                                 )
                                  )
                               ).

        ro_md_api = lt_obj_refs[ saknr = |{ iv_saknr    ALPHA = IN }|
                                 kstar = |{ iv_costelem ALPHA = IN }|
                                 fipex = |{ iv_comitem  ALPHA = OUT }|
                                 versn = |{ iv_versn    ALPHA = IN }|
                               ]-ref.
    ENDTRY.

  ENDMETHOD.


  METHOD get_mode.
    r_result = me->lv_mode.
  ENDMETHOD.


  METHOD get_simulate.
    r_result = me->lv_simulate.
  ENDMETHOD.


  METHOD get_wait.
    r_result = me->lv_wait.
  ENDMETHOD.


  METHOD set_commit.
    me->lv_commit = iv_commit.
  ENDMETHOD.


  METHOD set_mode.
    me->lv_mode = iv_mode.
  ENDMETHOD.


  METHOD set_simulate.
    me->lv_simulate = iv_simulate.
  ENDMETHOD.


  METHOD set_wait.
    me->lv_wait = iv_wait.
  ENDMETHOD.


  METHOD _commit.

    CHECK NOT me->get_simulate( ).
    CHECK me->get_commit( ).

    IF NOT me->get_wait( ).
      cl_soap_commit_rollback=>commit( ).
    ELSEIF me->get_wait( ).
      cl_soap_commit_rollback=>commit_and_wait( ).
    ELSE.
      ASSERT 1 = 2.
    ENDIF.

  ENDMETHOD.

  METHOD clear.
    CLEAR:
     me->lo_exception,
     me->lv_commit,
     me->lv_mode,
     me->lv_simulate,
     me->lv_wait.
  ENDMETHOD.

ENDCLASS.
