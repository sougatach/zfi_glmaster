CLASS zcl_fi_glmast_factory DEFINITION
  PUBLIC
  INHERITING FROM zcl_fico_masterdata_factory
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized">Set Deletion Flag in Chart of Accounts</p>
    METHODS set_coa_delete
      IMPORTING !iv_del         TYPE xloev DEFAULT abap_true
      RETURNING
                VALUE(r_result) TYPE boole_d
      RAISING
                zcx_fi_general .
    "! <p class="shorttext synchronized">Set Posting Block in Chart of Accounts</p>
    METHODS set_coa_block
      IMPORTING !iv_block       TYPE xspeb DEFAULT abap_true
      RETURNING
                VALUE(r_result) TYPE boole_d
      RAISING
                zcx_fi_general .
    "! <p class="shorttext synchronized">Read GL Account Master Data (All attributes)</p>
    METHODS read_glmast
      RETURNING VALUE(rs_result) TYPE /eby/pdmdgl_sbapi_data
      RAISING   zcx_fi_general.
    "! <p class="shorttext synchronized">Read GL Account Blocked Indicators</p>
    METHODS read_gl_blocks
      RETURNING
        VALUE(rs_result) TYPE /eby/pdmdgl_sbapi_blocks
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Assign GL Account to FSV</p>
    METHODS assign_fsv
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general .


    "! <p class="shorttext synchronized">Factory Object Constructor</p>
    METHODS constructor
      IMPORTING
        iv_saknr      TYPE saknr
        iv_ktopl      TYPE ktopl                DEFAULT tafe_group_coa
        iv_bukrs      TYPE bukrs                OPTIONAL
        iv_versn      TYPE versn_011            OPTIONAL
        iv_ergsl      TYPE ergsl                OPTIONAL
        is_coa_data   TYPE glaccount_coa_data   OPTIONAL
        is_ccode_data TYPE glaccount_ccode_data OPTIONAL
        is_acct_name  TYPE glaccount_name_data  OPTIONAL
        iv_keyword    TYPE schlw                OPTIONAL
        iv_mode       TYPE glaccount_action     DEFAULT gl_action-insert
        iv_commit     TYPE boole_d              DEFAULT abap_true
        iv_simulate   TYPE boole_d              DEFAULT abap_false
        iv_wait       TYPE boole_d              DEFAULT abap_true .

    METHODS zif_masterdata_factory~is_exist      REDEFINITION.
    METHODS zif_masterdata_factory~get_data      REDEFINITION.
    METHODS zif_masterdata_factory~maintain_data REDEFINITION.

  PROTECTED SECTION.


  PRIVATE SECTION.

    "! <p class="shorttext synchronized">Get GL Account</p>
    METHODS get_saknr RETURNING VALUE(r_result) TYPE saknr.
    "! <p class="shorttext synchronized">Set GL Account</p>
    METHODS set_saknr IMPORTING iv_saknr TYPE saknr.
    "! <p class="shorttext synchronized">Set Chart of Accounts</p>
    METHODS set_ktopl IMPORTING !iv_ktopl TYPE ktopl .
    "! <p class="shorttext synchronized">Get Chart of Accounts</p>
    METHODS get_ktopl RETURNING VALUE(r_result) TYPE ktopl .
    "! <p class="shorttext synchronized">Get Company Code</p>
    METHODS get_bukrs RETURNING VALUE(r_result) TYPE bukrs .
    "! <p class="shorttext synchronized">Set Company Code</p>
    METHODS set_bukrs IMPORTING !iv_bukrs TYPE bukrs .
    "! <p class="shorttext synchronized">Get Chart of Accounts data</p>
    METHODS get_coa_data RETURNING VALUE(r_result) TYPE glaccount_coa_data.
    "! <p class="shorttext synchronized">Set Chart of Accounts data</p>
    METHODS set_coa_data IMPORTING is_coa_data TYPE glaccount_coa_data.
    "! <p class="shorttext synchronized">Get Company Code data</p>
    METHODS get_ccode_data RETURNING VALUE(r_result) TYPE glaccount_ccode_data.
    "! <p class="shorttext synchronized">Set Company Code data</p>
    METHODS set_ccode_data IMPORTING is_ccode_data TYPE glaccount_ccode_data.
    "! <p class="shorttext synchronized">Get GL Account Name</p>
    METHODS get_acct_name RETURNING VALUE(r_result) TYPE glaccount_name_data.
    "! <p class="shorttext synchronized">Set GL Account Name</p>
    METHODS set_acct_name IMPORTING is_acct_name TYPE glaccount_name_data.
    "! <p class="shorttext synchronized">Get Keyword</p>
    METHODS get_keyword RETURNING VALUE(r_result) TYPE schlw.
    "! <p class="shorttext synchronized">Set Keyword</p>
    METHODS set_keyword IMPORTING iv_keyword TYPE schlw.
    "! <p class="shorttext synchronized">Set API Attributes</p>
    METHODS set_api IMPORTING io_api  TYPE REF TO zcl_fi_glmast_api
                              iv_mode TYPE glaccount_action OPTIONAL.

    METHODS: "! <p class="shorttext synchronized" lang="en">Get FS Version</p>
      get_versn RETURNING VALUE(r_result) TYPE versn_011,
      "! <p class="shorttext synchronized" lang="en">Set FSV Version</p>
      set_versn IMPORTING iv_versn TYPE versn_011,
      "! <p class="shorttext synchronized" lang="en">Get FSV Item</p>
      get_ergsl RETURNING VALUE(r_result) TYPE ergsl,
      "! <p class="shorttext synchronized" lang="en">Set FSV Item</p>
      set_ergsl IMPORTING iv_ergsl TYPE ergsl.

    DATA:
      ls_coa_data   TYPE glaccount_coa_data,
      ls_ccode_data TYPE glaccount_ccode_data,
      ls_acct_name  TYPE glaccount_name_data,
      lv_saknr      TYPE saknr,
      lv_ktopl      TYPE ktopl,
      lv_versn      TYPE versn_011,
      lv_ergsl      TYPE ergsl,
      lv_bukrs      TYPE bukrs,
      lv_keyword    TYPE schlw.

ENDCLASS.



CLASS zcl_fi_glmast_factory IMPLEMENTATION.


  METHOD assign_fsv.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_fsv_api) = CAST zcl_fi_fsv_api( zcl_fico_masterdata=>get_instance(
                                iv_saknr = me->get_saknr( )
                                iv_versn = me->get_versn( )
                                )
                       ).
    me->set_api( iv_mode = gl_action-update io_api = lo_fsv_api ).
    lo_fsv_api->set_ergsl( me->get_ergsl( ) ).
    lo_fsv_api->set_versn( me->get_versn( ) ).
    lo_fsv_api->set_ktopl( me->get_ktopl( ) ).

    r_result = lo_fsv_api->zif_masterdata~maintain( ).

  ENDMETHOD.


  METHOD constructor.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    super->constructor( ).

    me->set_mode( iv_mode ).
    me->set_commit( iv_commit ).
    me->set_simulate( iv_simulate ).
    me->set_wait( iv_wait ).
    me->set_saknr( iv_saknr ).
    me->set_ktopl( iv_ktopl ).
    me->set_bukrs( iv_bukrs ).
    me->set_versn( iv_versn ).
    me->set_ergsl( iv_ergsl ).
    me->set_ccode_data( is_ccode_data ).
    me->set_coa_data( is_coa_data ).
    me->set_acct_name( is_acct_name ).
    me->set_keyword( iv_keyword ).

  ENDMETHOD.


  METHOD get_acct_name.
    r_result = me->ls_acct_name.
  ENDMETHOD.


  METHOD get_bukrs.
    r_result = me->lv_bukrs.
  ENDMETHOD.


  METHOD get_ccode_data.
    r_result = me->ls_ccode_data.
  ENDMETHOD.


  METHOD get_coa_data.
    r_result = me->ls_coa_data.
  ENDMETHOD.


  METHOD get_keyword.
    r_result = me->lv_keyword.
  ENDMETHOD.


  METHOD get_ktopl.
    r_result = me->lv_ktopl.
  ENDMETHOD.


  METHOD get_saknr.
    r_result = me->lv_saknr.
  ENDMETHOD.


  METHOD read_gl_blocks.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_glmast_api(
                    zcl_fico_masterdata=>get_instance(
                        iv_saknr = me->get_saknr( )
                        )
                  ).

    me->set_api( io_api = lo_api iv_mode = gl_action-read ).

    rs_result = lo_api->read_gl_blocks( ).

  ENDMETHOD.


  METHOD set_acct_name.
    me->ls_acct_name = is_acct_name.
  ENDMETHOD.


  METHOD set_api.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    io_api->clear( ).
    io_api->set_mode( COND #( WHEN iv_mode IS SUPPLIED
                              THEN iv_mode
                              ELSE me->get_mode( )
                            )
                    ).

    io_api->set_simulate( me->get_simulate( ) ).
    io_api->set_commit( me->get_commit( ) ).
    io_api->set_wait( me->get_wait( ) ).
    io_api->set_ktopl( me->get_ktopl( ) ).
    io_api->set_bukrs( me->get_bukrs( ) ).
    io_api->set_versn( me->get_versn( ) ).
    io_api->set_ergsl( me->get_ergsl( ) ).
    io_api->set_coa_data( me->get_coa_data( ) ).
    io_api->set_ccode_data( me->get_ccode_data( ) ).
    io_api->set_acct_name( me->get_acct_name( ) ).
    io_api->set_keyword( me->get_keyword( ) ).

  ENDMETHOD.


  METHOD set_bukrs.
    me->lv_bukrs = iv_bukrs.
  ENDMETHOD.


  METHOD set_ccode_data.
    me->ls_ccode_data = is_ccode_data.
  ENDMETHOD.


  METHOD set_coa_block.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_glmast_api(
                    zcl_fico_masterdata=>get_instance(
                        iv_saknr = me->get_saknr( )
                        )
                  ).

    me->set_api( io_api = lo_api ).

    r_result = lo_api->set_coa_block( iv_block ).

  ENDMETHOD.


  METHOD set_coa_data.
    me->ls_coa_data = is_coa_data.
  ENDMETHOD.


  METHOD set_coa_delete.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_glmast_api(
                    zcl_fico_masterdata=>get_instance(
                        iv_saknr = me->get_saknr( )
                       )
                  ).

    me->set_api( io_api = lo_api ).

    r_result = lo_api->set_coa_delete( iv_del ).

  ENDMETHOD.


  METHOD set_keyword.
    me->lv_keyword = iv_keyword.
  ENDMETHOD.


  METHOD set_ktopl.
    me->lv_ktopl = iv_ktopl.
  ENDMETHOD.


  METHOD set_saknr.
    me->lv_saknr = iv_saknr.
  ENDMETHOD.


  METHOD zif_masterdata_factory~get_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_glmast_api(
                    zcl_fico_masterdata=>get_instance(
                        iv_saknr = me->get_saknr( )
                        iv_versn = me->get_versn( )
                        )
                  ).

    me->set_api( io_api = lo_api iv_mode = me->get_mode( ) ). "gl_action-read ).

    lo_api->zif_masterdata~read_data(
      IMPORTING
        es_data = es_data
    ).

  ENDMETHOD.


  METHOD zif_masterdata_factory~maintain_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_glmast_api(
                    zcl_fico_masterdata=>get_instance(
                        iv_saknr = me->get_saknr( )
                        iv_versn = me->get_versn( )
                       )
                  ).

    me->set_api( io_api = lo_api iv_mode = me->get_mode( ) ).

    r_result = lo_api->zif_masterdata~maintain( CONV saknr( iv_ref ) ).

  ENDMETHOD.

  METHOD zif_masterdata_factory~is_exist.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_glmast_api(
                    zcl_fico_masterdata=>get_instance(
                        iv_saknr = me->get_saknr( )
                        iv_versn = me->get_versn( )
                       )
                  ).

    me->set_api( lo_api ).

    r_result = lo_api->zif_masterdata~existencecheck(
                        iv_key1 = me->get_saknr( )
                        iv_key2 = me->get_ktopl( )
                        iv_key3 = me->get_bukrs( )
               ).

  ENDMETHOD.

  METHOD get_versn.
    r_result = me->lv_versn.
  ENDMETHOD.

  METHOD set_versn.
    me->lv_versn = iv_versn.
  ENDMETHOD.

  METHOD get_ergsl.
    r_result = me->lv_ergsl.
  ENDMETHOD.

  METHOD set_ergsl.
    me->lv_ergsl = iv_ergsl.
  ENDMETHOD.

  METHOD read_glmast.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   October 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_glmast_api(
                    zcl_fico_masterdata=>get_instance(
                        iv_saknr = me->get_saknr( )
                        )
                  ).

    me->set_api( io_api = lo_api iv_mode = gl_action-read ).

    lo_api->zif_masterdata~read_data(
      IMPORTING
        es_data = rs_result
    ).

  ENDMETHOD.

ENDCLASS.
