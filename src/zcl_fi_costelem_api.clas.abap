"! <p class="shorttext synchronized" lang="en">API for FI-CO Cost Element Master Data</p>
CLASS zcl_fi_costelem_api DEFINITION
  PUBLIC
  INHERITING FROM zcl_fico_masterdata
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_fico_masterdata .

  PUBLIC SECTION.

    ALIASES tafe_co_area
      FOR zif_fi_global_constants~co_area_1000 .

    METHODS zif_masterdata~maintain
        REDEFINITION .
    METHODS zif_masterdata~read_data
        REDEFINITION .
    "! <p class="shorttext synchronized">Set Controlling Area</p>
    METHODS set_coarea
      IMPORTING
        VALUE(iv_coarea) TYPE kokrs .
    "! <p class="shorttext synchronized">Set Cost Element Class</p>
    METHODS set_coelclass
      IMPORTING
        iv_class TYPE co_kaint .
    "! <p class="shorttext synchronized">Set Cost Element details</p>
    METHODS set_costinput
      IMPORTING
        is_costinput TYPE bapi1030_ceinputlist .
    "! <p class="shorttext synchronized">Set Key date</p>
    METHODS set_keydate
      IMPORTING
        iv_keydate TYPE allg_datum .

    METHODS: zif_masterdata~existencecheck REDEFINITION.



  PROTECTED SECTION.
    METHODS: check_mode REDEFINITION.

  PRIVATE SECTION.

    DATA:
      lv_coarea    TYPE kokrs,
      lv_costelem  TYPE kstar,
      lv_ref       TYPE kstar,
      lv_keydate   TYPE allg_datum,
      ls_costinput TYPE bapi1030_ceinputlist,
      lv_coelclass TYPE co_kaint.

    "! <p class="shorttext synchronized">Get Controlling Area</p>
    METHODS get_coarea RETURNING VALUE(r_result) TYPE kokrs .
    "! <p class="shorttext synchronized">Read a Cost Element</p>
    "!
    "! @parameter rs_result | <p class="shorttext synchronized">Cost Element deatils</p>
    "! @raising zcx_fi_general |
    METHODS read_costelem RETURNING VALUE(rs_result) TYPE bapi1030_ceoutputlist
                          RAISING   RESUMABLE(zcx_fi_general) .
    "! <p class="shorttext synchronized">Maintain Cost Element (Create,Update)</p>
    "!
    "! @parameter iv_ref | <p class="shorttext synchronized">Create with Reference</p>
    "! @parameter r_result | <p class="shorttext synchronized">True = Successfully maintained</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
    METHODS maintain_costelem
      IMPORTING
        !iv_ref         TYPE any OPTIONAL
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general.

    "! <p class="shorttext synchronized">Create a Cost Element</p>
    METHODS create_costelem
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general .

    "! <p class="shorttext synchronized">Update a Cost Element</p>
    METHODS update_costelem
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general .

    METHODS:
      "! <p class="shorttext synchronized">Get Cost Element</p>
      get_costelem RETURNING VALUE(r_result) TYPE kstar,
      "! <p class="shorttext synchronized">Set Cost Element</p>
      set_costelem IMPORTING iv_costelem TYPE kstar,
      "! <p class="shorttext synchronized">Get Key date</p>
      get_keydate  RETURNING VALUE(r_result) TYPE allg_datum.
    "! <p class="shorttext synchronized">Get Cost Element details</p>
    METHODS get_costinput
      RETURNING
        VALUE(rs_result) TYPE bapi1030_ceinputlist .
    "! <p class="shorttext synchronized">Get Cost Element Class</p>
    METHODS get_coelclass
      RETURNING
        VALUE(r_result) TYPE co_kaint .
    "! <p class="shorttext synchronized">Object Constructor</p>
    "!
    "! @parameter iv_costelem | <p class="shorttext synchronized">Cost Element</p>
    METHODS constructor
      IMPORTING
        iv_costelem TYPE kstar .
    METHODS: "! <p class="shorttext synchronized" lang="en">Get Reference Account</p>
      get_ref RETURNING VALUE(r_result) TYPE kstar,
      "! <p class="shorttext synchronized" lang="en">Set Reference Account</p>
      set_ref IMPORTING iv_ref TYPE any.

ENDCLASS.


CLASS zcl_fi_costelem_api IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    me->set_costelem( |{ iv_costelem ALPHA = IN }| ).

  ENDMETHOD.


  METHOD create_costelem.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA lt_return TYPE bapiret2_t.
    DATA(lt_costelem) = VALUE tty_bapi1030_ceinputlist(
                         LET costelem = me->get_costinput( ) IN
                          ( LINES OF COND #( WHEN costelem IS NOT INITIAL
                                             THEN VALUE #( ( CORRESPONDING #( costelem ) ) )
                                             ELSE VALUE #( )
                                           )
                          )
                                                      ).

    CALL FUNCTION 'BAPI_COSTELEM_CREATEMULTIPLE'
      EXPORTING
        coarea          = me->get_coarea( )
        costelemclass   = me->get_coelclass( )
        testrun         = me->get_simulate( )
      TABLES
        costelementlist = lt_costelem
        return          = lt_return.

    _mac_raise_multi lt_return.

    r_result = abap_true.

    me->_commit( ).

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
    rs_result = me->ls_costinput.
  ENDMETHOD.


  METHOD get_keydate.
    r_result = me->lv_keydate.
  ENDMETHOD.


  METHOD maintain_costelem.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->dummy = SWITCH #( me->get_mode( )
                   WHEN action-insert OR action-update
                   THEN abap_true
                   ELSE THROW zcx_fi_general(
                           textid    = zcx_fi_general=>wrong_mode
                           gv_mode   = me->get_mode( )
                           gv_string = |Maintain Cost Element|
                                            )
                         ).

    me->set_costinput( COND #( WHEN me->get_ref( )
                               THEN LET costelem = me->get_costelem( ) IN
                                CORRESPONDING #( BASE ( costelem ) me->read_costelem( ) EXCEPT cost_elem )
                               ELSE SWITCH #( me->get_mode( )
                                     WHEN action-insert OR action-update
                                     THEN me->get_costinput( )
                                     WHEN action-read OR space
                                     THEN me->read_costelem( )
                                     ELSE THROW zcx_fi_general(
                                             gv_mode   = me->get_mode( )
                                             textid    = zcx_fi_general=>wrong_mode
                                             gv_string = |Maintain Cost Element|
                                                             )
                                            )
                             )
                    ).

    r_result = SWITCH #( me->get_mode( )
                WHEN action-insert
                THEN create_costelem( )
                ELSE update_costelem( )
                       ).

  ENDMETHOD.


  METHOD read_costelem.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA lt_return TYPE bapiret2_t.

    CALL FUNCTION 'BAPI_COSTELEM_GETDETAIL'
      EXPORTING
        controllingarea   = me->get_coarea( )
        costelement       = COND #( WHEN me->get_ref( )
                                    THEN me->get_ref( )
                                    ELSE me->get_costelem( )
                                  )
        keydate           = me->get_keydate( )
      IMPORTING
        costelementdetail = rs_result
      TABLES
        return            = lt_return.

    _mac_raise_multi lt_return.

  ENDMETHOD.


  METHOD set_coarea.
    me->lv_coarea = iv_coarea.
  ENDMETHOD.


  METHOD set_coelclass.
    me->lv_coelclass = iv_class.
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


  METHOD update_costelem.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA lt_return TYPE bapiret2_t.

    DATA(lt_costelem) = VALUE tty_bapi1030_ceinputlist(
                         LET costinput = me->get_costinput( ) IN
                          ( LINES OF COND #( WHEN costinput IS NOT INITIAL
                                             THEN VALUE #( ( CORRESPONDING #( costinput ) ) )
                                             ELSE VALUE #( )
                                           )
                          )
                                                      ).

    CALL FUNCTION 'BAPI_COSTELEM_CHANGEMULTIPLE'
      EXPORTING
        coarea          = me->get_coarea( )
        testrun         = me->get_simulate( )
      TABLES
        costelementlist = lt_costelem
        return          = lt_return.

    _mac_raise_multi lt_return.

    r_result = abap_true.

    me->_commit( ).

  ENDMETHOD.


  METHOD zif_masterdata~maintain.
    me->set_ref( iv_ref ).
    r_result = me->maintain_costelem( iv_ref ).
  ENDMETHOD.


  METHOD zif_masterdata~read_data.
    es_data = me->read_costelem( ).
  ENDMETHOD.

  METHOD check_mode.
    ##NEEDED
  ENDMETHOD.

  METHOD zif_masterdata~existencecheck.
    r_result = xsdbool( me->read_costelem( ) ).
    "or call function 'K_COSTELEMENT_SELECT_SINGLE'
  ENDMETHOD.

  METHOD get_ref.
    r_result = me->lv_ref.
  ENDMETHOD.

  METHOD set_ref.
    me->lv_ref = |{ iv_ref ALPHA = IN }|.
  ENDMETHOD.

ENDCLASS.
