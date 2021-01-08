CLASS zcl_fi_costelem_factory DEFINITION
  PUBLIC
  INHERITING FROM zcl_fico_masterdata_factory
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized">Object Constructor</p>
    METHODS constructor
      IMPORTING
        !iv_costelem  TYPE kstar
        !iv_mode      TYPE glaccount_action DEFAULT gl_action-insert
        !iv_commit    TYPE boole_d DEFAULT abap_true
        !iv_simulate  TYPE boole_d DEFAULT abap_false
        !iv_wait      TYPE boole_d DEFAULT abap_true
        !iv_coarea    TYPE kokrs DEFAULT tafe_co_area
        !iv_keydate   TYPE allg_datum DEFAULT sy-datum
        !iv_coelclass TYPE co_kaint DEFAULT '1'
        !is_costinput TYPE bapi1030_ceinputlist OPTIONAL .

    METHODS zif_masterdata_factory~get_data
        REDEFINITION .
    METHODS zif_masterdata_factory~is_exist
        REDEFINITION .
    METHODS zif_masterdata_factory~maintain_data
        REDEFINITION .

  PROTECTED SECTION.


  PRIVATE SECTION.

    METHODS:
      "! <p class="shorttext synchronized">Get Cost Element</p>
      get_costelem RETURNING VALUE(r_result) TYPE kstar,
      "! <p class="shorttext synchronized">Set Cost Element</p>
      set_costelem IMPORTING iv_costelem TYPE kstar,
      "! <p class="shorttext synchronized">Get Key date</p>
      get_keydate  RETURNING VALUE(r_result) TYPE allg_datum,
      "! <p class="shorttext synchronized">Set Key date</p>
      set_keydate  IMPORTING iv_keydate TYPE allg_datum.
    "! <p class="shorttext synchronized">Get Controlling Area</p>
    METHODS get_coarea RETURNING VALUE(r_result) TYPE kokrs .
    "! <p class="shorttext synchronized">Set Controlling Area</p>
    METHODS set_coarea IMPORTING VALUE(iv_coarea) TYPE kokrs .

    METHODS:
      "! <p class="shorttext synchronized">Get Cost Element input</p>
      get_costinput RETURNING VALUE(r_result) TYPE bapi1030_ceinputlist,
      "! <p class="shorttext synchronized">Set Cost Element</p>
      set_costinput IMPORTING is_costinput    TYPE bapi1030_ceinputlist,
      "! <p class="shorttext synchronized">Set API specific attributes</p>
      set_api       IMPORTING io_api  TYPE REF TO zcl_fi_costelem_api
                              iv_mode TYPE glaccount_action OPTIONAL,
      "! <p class="shorttext synchronized">Get C Cost Element Class</p>
      get_coelclass RETURNING VALUE(r_result) TYPE co_kaint,
      "! <p class="shorttext synchronized">Set Cost Element Class</p>
      set_coelclass IMPORTING iv_coelclass TYPE co_kaint.

    DATA:
      lv_coarea    TYPE kokrs,
      lv_costelem  TYPE kstar,
      lv_keydate   TYPE allg_datum,
      lv_coelclass TYPE co_kaint,
      ls_costinput TYPE bapi1030_ceinputlist.

ENDCLASS.



CLASS zcl_fi_costelem_factory IMPLEMENTATION.


  METHOD constructor.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    super->constructor( ).

    me->set_mode( iv_mode ).
    me->set_commit( iv_commit ).
    me->set_simulate( iv_simulate ).
    me->set_wait( iv_wait ).
    me->set_coarea( iv_coarea ).
    me->set_costelem( |{ iv_costelem ALPHA = IN }| ).
    me->set_keydate( iv_keydate ).
    me->set_coelclass( iv_coelclass ).
    me->set_costinput( is_costinput ).

  ENDMETHOD.


  METHOD get_coarea.
    r_result = me->lv_coarea.
  ENDMETHOD.


  METHOD get_coelclass.
    r_result = me->lv_coelclass.
  ENDMETHOD.


  METHOD get_costelem.
    r_result = me->lv_costelem.
  ENDMETHOD.


  METHOD get_costinput.
    r_result = me->ls_costinput.
  ENDMETHOD.


  METHOD get_keydate.
    r_result = me->lv_keydate.
  ENDMETHOD.


  METHOD set_api.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
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
    io_api->set_coarea( me->get_coarea( ) ).
    io_api->set_keydate( me->get_keydate( ) ).
    io_api->set_coelclass( me->get_coelclass( ) ).
    io_api->set_costinput( me->get_costinput( ) ).

  ENDMETHOD.


  METHOD set_coarea.
    me->lv_coarea = iv_coarea.
  ENDMETHOD.


  METHOD set_coelclass.
    me->lv_coelclass = iv_coelclass.
  ENDMETHOD.


  METHOD set_costelem.
    me->lv_costelem = iv_costelem.
  ENDMETHOD.


  METHOD set_costinput.
    me->ls_costinput = is_costinput.
  ENDMETHOD.


  METHOD set_keydate.
    me->lv_keydate = iv_keydate.
  ENDMETHOD.


  METHOD zif_masterdata_factory~get_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_costelem_api(
                    zcl_fico_masterdata=>get_instance(
                      iv_costelem = me->get_costelem( )
                                            )
                  ).

    me->set_api( io_api = lo_api iv_mode = me->get_mode( ) ).

    lo_api->zif_masterdata~read_data(
      IMPORTING
        es_data = es_data
    ).

  ENDMETHOD.


  METHOD zif_masterdata_factory~is_exist.

    DATA(lo_api) = CAST zcl_fi_costelem_api(
                     zcl_fico_masterdata=>get_instance(
                       iv_costelem = me->get_costelem( )
                                           )
                  ).

    me->set_api( lo_api ).

    r_result = lo_api->zif_masterdata~existencecheck( ).

  ENDMETHOD.


  METHOD zif_masterdata_factory~maintain_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(lo_api) = CAST zcl_fi_costelem_api(
                    zcl_fico_masterdata=>get_instance(
                      iv_costelem = me->get_costelem( )
                                           )
                  ).

    me->set_api( io_api = lo_api iv_mode = me->get_mode( ) ).

    r_result = lo_api->zif_masterdata~maintain( iv_ref ).

  ENDMETHOD.

ENDCLASS.
