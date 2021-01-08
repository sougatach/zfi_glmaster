"! <p class="shorttext synchronized">Abstract Class for FI-CO Masterdata Factory</p>
CLASS zcl_fico_masterdata_factory DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_fi_global_constants .
    INTERFACES zif_masterdata_factory
      ALL METHODS ABSTRACT .

    ALIASES gl_action
      FOR zif_fi_global_constants~co_action .
    ALIASES tafe_comp_code
      FOR zif_fi_global_constants~comp_code_tafe .
    ALIASES tafe_co_area
      FOR zif_fi_global_constants~co_area_1000 .
    ALIASES tafe_fm_area
      FOR zif_fi_global_constants~fm_area_1000 .
    ALIASES tafe_fsv
      FOR zif_fi_global_constants~co_fsv_versn .
    ALIASES tafe_group_coa
      FOR zif_fi_global_constants~coa_group_tafe .

    "! <p class="shorttext synchronized">Returns the Factory Subclass</p>
    "! <strong>Either GL Account or Cost Element has to be passed as parameter</strong>
    "!
    "! @parameter iv_saknr | <p class="shorttext synchronized">GL Account</p>
    "! @parameter iv_costelem | <p class="shorttext synchronized">Cost Element</p>
    "! @parameter iv_comitem | <p class="shorttext synchronized">Commitment Item</p>
    "! @parameter iv_ktopl | <p class="shorttext synchronized">Chart of Accounts</p>
    "! @parameter iv_bukrs | <p class="shorttext synchronized">Company Code</p>
    "! @parameter is_coa_data | <p class="shorttext synchronized">Data for COA</p>
    "! @parameter is_ccode_data | <p class="shorttext synchronized">Data for Company Code</p>
    "! @parameter is_acct_name | <p class="shorttext synchronized">Data for Account Name</p>
    "! @parameter is_citemdata | <p class="shorttext synchronized">Commitment Data</p>
    "! @parameter iv_keyword | <p class="shorttext synchronized">Data for Keyword</p>
    "! @parameter iv_coarea | <p class="shorttext synchronized">Controlling Area</p>
    "! @parameter iv_keydate | <p class="shorttext synchronized">Keydate</p>
    "! @parameter iv_coelclass | <p class="shorttext synchronized">Cost Element Class</p>
    "! @parameter is_costinput | <p class="shorttext synchronized">Data for Cost Element</p>
    "! @parameter iv_fikrs | <p class="shorttext synchronized">FM Area</p>
    "! @parameter iv_ergsl | <p class="shorttext synchronized">FSV Hierarchy Node</p>
    "! @parameter iv_versn | <p class="shorttext synchronized">FSV Version</p>
    "! @parameter iv_mode | <p class="shorttext synchronized">Factory Run mode</p>
    "! @parameter iv_commit | <p class="shorttext synchronized">Commit Flag</p>
    "! @parameter iv_simulate | <p class="shorttext synchronized">Simulation mode</p>
    "! @parameter iv_wait | <p class="shorttext synchronized">Wait or not for Commit Work</p>
    "! @parameter ro_result | <p class="shorttext synchronized">Factory Subclass as an Instance</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
    CLASS-METHODS create
      IMPORTING
        VALUE(iv_saknr)    TYPE saknr OPTIONAL
        VALUE(iv_costelem) TYPE kstar OPTIONAL
        VALUE(iv_comitem)  TYPE fm_fipex OPTIONAL
        !iv_ktopl          TYPE ktopl DEFAULT tafe_group_coa
        !iv_bukrs          TYPE bukrs DEFAULT tafe_comp_code
        !is_coa_data       TYPE glaccount_coa_data OPTIONAL
        !is_ccode_data     TYPE glaccount_ccode_data OPTIONAL
        !is_acct_name      TYPE glaccount_name_data OPTIONAL
        !is_citemdata      TYPE zfi_s_commitment_data OPTIONAL
        !iv_keyword        TYPE schlw OPTIONAL
        !iv_coarea         TYPE kokrs DEFAULT tafe_co_area
        !iv_keydate        TYPE allg_datum DEFAULT sy-datum
        !iv_coelclass      TYPE co_kaint DEFAULT '1'
        !is_costinput      TYPE bapi1030_ceinputlist OPTIONAL
        !iv_fikrs          TYPE fikrs DEFAULT tafe_fm_area
        !iv_ergsl          TYPE ergsl OPTIONAL
        !iv_versn          TYPE versn_011 OPTIONAL
        !iv_mode           TYPE glaccount_action DEFAULT gl_action-insert
        !iv_commit         TYPE boole_d DEFAULT abap_true
        !iv_simulate       TYPE boole_d DEFAULT abap_false
        !iv_wait           TYPE boole_d DEFAULT abap_true
      RETURNING
        VALUE(ro_result)   TYPE REF TO zcl_fico_masterdata_factory
      RAISING
        zcx_fi_general .

    "! <p class="shorttext synchronized" lang="en">Object Constructor</p>
    METHODS constructor.

  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">Get Simulation mode</p>
    METHODS get_simulate
      RETURNING
        VALUE(r_result) TYPE boole_d .
    "! <p class="shorttext synchronized" lang="en">Set Simulation mode</p>
    METHODS set_simulate
      IMPORTING
        !iv_simulate TYPE boole_d .
    "! <p class="shorttext synchronized" lang="en">Get API Run mode</p>
    METHODS get_mode
      RETURNING
        VALUE(r_result) TYPE glaccount_action .
    "! <p class="shorttext synchronized" lang="en">Set API Run mode</p>
    METHODS set_mode
      IMPORTING
        !iv_mode TYPE glaccount_action .
    "! <p class="shorttext synchronized" lang="en">Get Wait parameter for Commit</p>
    METHODS get_wait
      RETURNING
        VALUE(r_result) TYPE boole_d .
    "! <p class="shorttext synchronized" lang="en">Set Wait parameter for Commit</p>
    METHODS set_wait
      IMPORTING
        !iv_wait TYPE boole_d .
    "! <p class="shorttext synchronized" lang="en">Get Commit Flag</p>
    METHODS get_commit
      RETURNING
        VALUE(rv_commit) TYPE boole_d .
    "! <p class="shorttext synchronized" lang="en">Set Commit Flag</p>
    METHODS set_commit
      IMPORTING
         iv_commit TYPE boole_d .
    "! <p class="shorttext synchronized" lang="en">Clear Attributes</p>
    METHODS clear.

  PRIVATE SECTION.

    DATA lv_simulate TYPE boole_d .
    DATA lv_mode     TYPE glaccount_action .
    DATA lv_wait     TYPE boole_d .
    DATA lv_commit   TYPE boole_d .

ENDCLASS.



CLASS zcl_fico_masterdata_factory IMPLEMENTATION.

  METHOD constructor.
    me->clear( ).
  ENDMETHOD.


  METHOD create.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    ro_result = COND #( WHEN iv_saknr IS NOT INITIAL
                        THEN NEW zcl_fi_glmast_factory(
                                   iv_saknr       = iv_saknr
                                   iv_bukrs       = iv_bukrs
                                   is_acct_name   = is_acct_name
                                   is_ccode_data  = is_ccode_data
                                   is_coa_data    = is_coa_data
                                   iv_keyword     = iv_keyword
                                   iv_ktopl       = iv_ktopl
                                   iv_versn       = iv_versn
                                   iv_ergsl       = iv_ergsl
                                   iv_mode        = iv_mode
                                   iv_simulate    = iv_simulate
                                   iv_commit      = iv_commit
                                   iv_wait        = iv_wait
                                )
                        WHEN iv_costelem IS NOT INITIAL
                        THEN NEW zcl_fi_costelem_factory(
                                   iv_costelem   = iv_costelem
                                   iv_mode       = iv_mode
                                   iv_simulate   = iv_simulate
                                   iv_commit     = iv_commit
                                   iv_wait       = iv_wait
                                   iv_coarea     = iv_coarea
                                   iv_keydate    = iv_keydate
                                   iv_coelclass  = iv_coelclass
                                   is_costinput  = is_costinput
                                )
                        WHEN iv_comitem IS NOT INITIAL
                        THEN NEW zcl_fi_commitment_factory(
                                  iv_fikrs     = iv_fikrs
                                  iv_fipex     = iv_comitem
                                  is_citemdata = is_citemdata
                                  iv_mode      = iv_mode
                                  iv_commit    = iv_commit
                                  iv_simulate  = iv_simulate
                                  iv_wait      = iv_wait
                                )
                        ELSE THROW zcx_fi_general(
                                    textid  = zcx_fi_general=>incorrect_params
                                                 )
                    ).

  ENDMETHOD.


  METHOD get_commit.
    rv_commit = me->lv_commit.
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

  METHOD clear.
    CLEAR:
     me->lv_commit,
     me->lv_mode,
     me->lv_simulate,
     me->lv_wait.
  ENDMETHOD.

ENDCLASS.
