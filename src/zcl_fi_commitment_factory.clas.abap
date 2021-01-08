CLASS zcl_fi_commitment_factory DEFINITION
  PUBLIC
  INHERITING FROM zcl_fico_masterdata_factory
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      zif_masterdata_factory~is_exist       REDEFINITION,
      zif_masterdata_factory~get_data       REDEFINITION,
      zif_masterdata_factory~maintain_data  REDEFINITION.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get All Commitment Items for an FM Area</p>
      "!
      "! @parameter iv_fikrs | <p class="shorttext synchronized" lang="en">FM Area for TAFE (Default 1000)</p>
      "! @parameter rt_result | <p class="shorttext synchronized" lang="en">Table of all Commitment Items in the FM Area Passed</p>
      get_all IMPORTING iv_fikrs         TYPE fikrs DEFAULT tafe_fm_area
              RETURNING VALUE(rt_result) TYPE fmci_t.

    METHODS constructor
      IMPORTING
        iv_fikrs     TYPE fikrs DEFAULT tafe_fm_area
        iv_gjahr     TYPE gjahr OPTIONAL
        iv_fipex     TYPE fm_fipex
        is_citemdata TYPE zfi_s_commitment_data OPTIONAL
        iv_mode      TYPE glaccount_action DEFAULT gl_action-insert
        iv_commit    TYPE boole_d DEFAULT abap_true
        iv_simulate  TYPE boole_d DEFAULT abap_false
        iv_wait      TYPE boole_d DEFAULT abap_true.

  PROTECTED SECTION.



  PRIVATE SECTION.

    DATA:
      lv_fikrs     TYPE fikrs,
      lv_fipex     TYPE fipex,
      lv_gjahr     TYPE gjahr,
      ls_citemdata TYPE zfi_s_commitment_data.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get FM Area</p>
      get_fikrs RETURNING VALUE(r_result) TYPE fikrs,
      "! <p class="shorttext synchronized" lang="en">Set FM Area</p>
      set_fikrs IMPORTING iv_fikrs TYPE fikrs,
      "! <p class="shorttext synchronized" lang="en">Get Commitment Item</p>
      get_fipex RETURNING VALUE(r_result) TYPE fipex,
      "! <p class="shorttext synchronized" lang="en">Set Commitment Item</p>
      set_fipex IMPORTING iv_fipex TYPE fipex,
      "! <p class="shorttext synchronized" lang="en">Get Commitment Data</p>
      get_data  RETURNING VALUE(rs_result) TYPE zfi_s_commitment_data,
      "! <p class="shorttext synchronized" lang="en">set Commitment Data</p>
      set_data  IMPORTING is_citemdata TYPE zfi_s_commitment_data,
      "! <p class="shorttext synchronized" lang="en">Set API Methods</p>
      set_api   IMPORTING io_api  TYPE REF TO zcl_fi_commitment_api
                          iv_mode TYPE glaccount_action OPTIONAL.

ENDCLASS.



CLASS zcl_fi_commitment_factory IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    me->set_mode( iv_mode ).
    me->set_commit( iv_commit ).
    me->set_simulate( iv_simulate ).
    me->set_wait( iv_wait ).
    me->set_fikrs( iv_fikrs ).
    me->set_fipex( iv_fipex ).
    me->set_data( is_citemdata ).

  ENDMETHOD.

  METHOD zif_masterdata_factory~get_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   September 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_commitment_api(
                    zcl_fico_masterdata=>get_instance(
                      iv_comitem = me->get_fipex( )
                                             )
                  ).

    me->set_api( io_api = lo_api iv_mode = me->get_mode( ) ).

    lo_api->zif_masterdata~read_data(
      IMPORTING
        es_data = es_data
    ).

  ENDMETHOD.

  METHOD zif_masterdata_factory~is_exist.

    DATA(lo_api) = CAST zcl_fi_commitment_api(
                    zcl_fico_masterdata=>get_instance(
                      iv_comitem = me->get_fipex( )
                                           )
                  ).

    me->set_api( lo_api ).

    r_result = lo_api->zif_masterdata~existencecheck( ).

  ENDMETHOD.

  METHOD zif_masterdata_factory~maintain_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   September 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_commitment_api(
                    zcl_fico_masterdata=>get_instance(
                      iv_comitem = me->get_fipex( )
                                           )
                  ).

    me->set_api( io_api = lo_api iv_mode = me->get_mode( ) ).

    r_result = lo_api->zif_masterdata~maintain( iv_ref ).

  ENDMETHOD.

  METHOD get_fikrs.
    r_result = me->lv_fikrs.
  ENDMETHOD.

  METHOD set_fikrs.
    me->lv_fikrs = iv_fikrs.
  ENDMETHOD.

  METHOD get_fipex.
    r_result = me->lv_fipex.
  ENDMETHOD.

  METHOD set_fipex.
    me->lv_fipex = iv_fipex.
  ENDMETHOD.

  METHOD get_data.
    rs_result = me->ls_citemdata.
  ENDMETHOD.

  METHOD set_data.
    me->ls_citemdata = is_citemdata.
  ENDMETHOD.

  METHOD set_api.

    io_api->clear( ).
    io_api->set_mode( COND #( WHEN iv_mode IS SUPPLIED
                              THEN iv_mode
                              ELSE me->get_mode( )
                            )
                    ).

    io_api->set_simulate( me->get_simulate( ) ).
    io_api->set_commit( me->get_commit( ) ).
    io_api->set_wait( me->get_wait( ) ).
    io_api->set_fikrs( me->get_fikrs( ) ).
    io_api->set_fipex( me->get_fipex( ) ).
    io_api->set_data( me->get_data( ) ).

  ENDMETHOD.

  METHOD get_all.

    SELECT fipex fipup FROM fmci
      INTO CORRESPONDING FIELDS OF TABLE rt_result
      WHERE fikrs = iv_fikrs
      AND   gjahr = '000'.

  ENDMETHOD.

ENDCLASS.
