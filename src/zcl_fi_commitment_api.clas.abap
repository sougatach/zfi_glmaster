CLASS zcl_fi_commitment_api DEFINITION
  PUBLIC
  INHERITING FROM zcl_fico_masterdata
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_fico_masterdata .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized">Set Financial Management Area</p>
    METHODS set_fikrs IMPORTING !iv_fikrs TYPE fikrs DEFAULT tafe_fm_area .
    "! <p class="shorttext synchronized">Set Commitment Item Master Data</p>
    METHODS set_data IMPORTING !is_data TYPE zfi_s_commitment_data .
    "! <p class="shorttext synchronized" lang="en">Set Commitment Item</p>
    METHODS set_fipex IMPORTING !iv_fipex TYPE fm_fipex.

    METHODS zif_masterdata~maintain       REDEFINITION.
    METHODS zif_masterdata~read_data      REDEFINITION.
    METHODS zif_masterdata~existencecheck REDEFINITION.



  PROTECTED SECTION.

    METHODS: check_mode REDEFINITION.

  PRIVATE SECTION.

    ALIASES auth_actvt FOR zif_fi_global_constants~co_auth_actvt.

    DATA lv_fikrs   TYPE fikrs.
    DATA lv_fipex   TYPE fm_fipex.
    DATA lv_ref     TYPE fm_fipex.
    DATA ls_data    TYPE zfi_s_commitment_data.

    "! <p class="shorttext synchronized">Check Commitment Item Authorisations</p>
    METHODS check_auth RETURNING VALUE(r_result) TYPE boole_d RAISING zcx_fi_general.
    "! <p class="shorttext synchronized">Read Commitment Item details</p>
    METHODS read RETURNING VALUE(rs_result) TYPE zfi_s_commitment_data
                 RAISING   zcx_fi_general.
    "! <p class="shorttext synchronized">Get Financial Management Area</p>
    METHODS get_fikrs RETURNING VALUE(r_result)     TYPE fikrs.
    "! <p class="shorttext synchronized">Get Commitment Item Master Data</p>
    METHODS get_data RETURNING VALUE(rs_result)     TYPE zfi_s_commitment_data.

    "! <p class="shorttext synchronized" lang="en">Check data before Maintenance</p>
    METHODS check_maintain RAISING RESUMABLE(zcx_fi_general).
    "! <p class="shorttext synchronized">Save Commitment Item Master Data</p>
    METHODS _save RETURNING VALUE(r_result) TYPE boole_d RAISING zcx_fi_general.
    "! <p class="shorttext synchronized">Object Constructor</p>
    METHODS constructor IMPORTING  !iv_fipex TYPE fm_fipex.
    "! <p class="shorttext synchronized" lang="en">Get Commitment Item</p>
    METHODS get_fipex RETURNING VALUE(r_result) TYPE fm_fipex.
    METHODS: "! <p class="shorttext synchronized" lang="en">Get Reference Commitment Item</p>
      get_ref RETURNING VALUE(r_result) TYPE fm_fipex,
      "! <p class="shorttext synchronized" lang="en">Set Reference Commitment Item</p>
      set_ref IMPORTING iv_ref TYPE any.

ENDCLASS.



CLASS zcl_fi_commitment_api IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    me->set_fipex( iv_fipex ).

  ENDMETHOD.


  METHOD check_auth.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(l_actvt) = auth_actvt-update.

    DATA l_flg_auth TYPE xfeld.

    CALL FUNCTION 'FM_AUTH_CHECK_FM_AREA'
      EXPORTING
        i_fikrs       = me->get_fikrs( )
        i_actvt       = l_actvt
        i_msgty       = if_hrbas_message_handler=>error
      IMPORTING
        e_flg_auth    = l_flg_auth
      EXCEPTIONS
        error_message = 1.

    IF l_flg_auth IS INITIAL OR sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        INTO me->dummy.
      _mac_raise.
    ENDIF.

* check authority CI group
    l_actvt = SWITCH #( me->get_mode( )
                WHEN action-insert
                THEN auth_actvt-create
                WHEN action-update
                THEN auth_actvt-update
                ELSE auth_actvt-read
                      ).

    CALL FUNCTION 'FM_AUTH_CHECK_GRP_COM_ITEM'
      EXPORTING
        i_fikrs         = me->get_fikrs( )
        i_fipex         = CONV fm_fipex( |{ me->get_fipex( ) ALPHA = OUT }| )
        i_authgrp_fipex = me->get_data( )-fmci-augrp
        i_actvt         = l_actvt
        i_msgty         = if_hrbas_message_handler=>error
      IMPORTING
        e_flg_auth      = l_flg_auth
      EXCEPTIONS
        error_message   = 1.

    CHECK l_flg_auth IS INITIAL OR sy-subrc IS NOT INITIAL.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO me->dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD check_maintain.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    CHECK me->get_mode( ) = action-update. "AND me->get_ref( ).

    MESSAGE e008(zfi) WITH |Maintain with Reference is only valid for 'Insert' Mode| INTO me->dummy.
    _mac_raise.

  ENDMETHOD.


  METHOD get_data.
    rs_result = me->ls_data.
  ENDMETHOD.


  METHOD get_fikrs.
    r_result = me->lv_fikrs.
  ENDMETHOD.


  METHOD read.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    ASSERT me->get_fikrs( ).

    CALL FUNCTION 'FM_COM_ITEM_READ_SINGLE_DATA'
      EXPORTING
        i_fikrs                  = me->get_fikrs( )
*       i_varnt                  = '000'
        i_gjahr                  = CONV gjahr( '' )
        i_fipex                  = COND #( WHEN me->get_ref( )
                                           THEN me->get_ref( )
                                           ELSE me->get_fipex( )
                                         )
        i_date                   = sy-datum
        i_flg_text               = abap_true
        i_flg_hierarchy          = abap_true
      IMPORTING
        e_f_fmci                 = rs_result-fmci
        e_f_fmcit                = rs_result-fmcit
        e_f_fmhici               = rs_result-fmhici
      EXCEPTIONS
        master_data_not_found    = 1
        hierarchy_data_not_found = 2
        input_error              = 3
        cmmt_item_not_valid      = 4
        OTHERS                   = 5.

    CHECK sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO me->dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD set_data.
    me->ls_data = is_data.
  ENDMETHOD.


  METHOD set_fikrs.
    me->lv_fikrs = iv_fikrs.
  ENDMETHOD.


  METHOD zif_masterdata~maintain.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->set_ref( iv_ref ).

    me->dummy = SWITCH #( me->get_mode( )
                  WHEN action-insert OR action-update
                  THEN me->check_auth( )
                  ELSE THROW zcx_fi_general(
                              textid    = zcx_fi_general=>wrong_mode
                              gv_mode   = me->get_mode( )
                              gv_string = |Maintain Commitment Item|
                                           )
                       ).

    me->set_data( COND #( WHEN me->get_ref( )
                          THEN LET l_subs = VALUE fmci( fipex = me->get_fipex( )
                                                        fipos = me->get_fipex( )
                                                      )
                                   l_fmci  = me->read( )-fmci
                                   ls_fmci = CORRESPONDING fmci( BASE ( l_subs ) l_fmci EXCEPT fipex fipos )
                          IN
                           CORRESPONDING #( BASE ( ls_fmci ) me->read( ) EXCEPT fmci )
                          ELSE SWITCH #( me->get_mode( )
                                WHEN action-insert OR action-update
                                THEN me->get_data( )
                                WHEN action-read OR space
                                THEN me->read( )
                                ELSE THROW zcx_fi_general(
                                             gv_mode   = me->get_mode( )
                                             textid    = zcx_fi_general=>wrong_mode
                                             gv_string = |Maintain Commitment Item|
                                                         )
                                       )
                        )
                ).

    r_result = me->_save( ).

  ENDMETHOD.


  METHOD zif_masterdata~read_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->dummy = SWITCH #( me->get_mode( )
                  WHEN action-read
                  THEN me->check_auth( )
                  ELSE THROW zcx_fi_general(
                              textid  = zcx_fi_general=>wrong_mode
                              gv_mode = me->get_mode( )
                                           )
                       ).

    es_data = me->read( ).

  ENDMETHOD.


  METHOD _save.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA:
      lt_fmci    TYPE fmmd_t_fmci,
      lt_fmcit   TYPE fmmd_t_fmcit,
      lt_fmhici  TYPE fmmd_t_fmhici,
      lt_fmzubsp TYPE fmmd_t_fmzubsp,
      l_subrc    TYPE sysubrc.

    DATA:
      l_status_md        TYPE fmdy-xfeld,
      l_status_hierarchy TYPE fmdy-xfeld.

    DATA(ls_fmci)    = VALUE fmmd_fmci( LET l_fipex = me->get_data( )-fmci-fipex IN
                         BASE CORRESPONDING #( me->get_data( )-fmci )
                                fikrs  = me->get_fikrs( )
                                fipex  = |{ l_fipex ALPHA = OUT }|
                                fipos  = |{ l_fipex ALPHA = OUT }|
                                action = me->get_mode( )
                                      ).

    DATA(ls_fmcit)   = VALUE fmmd_fmcit( LET l_fipex = me->get_data( )-fmci-fipex IN
                         BASE CORRESPONDING #( me->get_data( )-fmcit )
                                fikrs  = me->get_fikrs( )
                                fipex  = |{ l_fipex ALPHA = OUT }|
                                spras  = sy-langu
                                action = me->get_mode( )
                                       ).

    IF me->get_data( )-fmzubsp IS NOT INITIAL.
      DATA(ls_fmzubsp) = VALUE fmmd_fmzubsp( LET l_fipex = me->get_data( )-fmci-fipex IN
                          BASE CORRESPONDING #( me->get_data( )-fmzubsp )
                                sfipex  = |{ l_fipex ALPHA = OUT }|
                                action = me->get_mode( )
                                           ).
    ENDIF.

    DATA(lt_hier) = VALUE fmmd_t_fmhici_small( ( VALUE #( BASE CORRESPONDING #( me->get_data( )-fmci )
                                                            varnt  = CONV #( |000| )
                                                            action = me->get_mode( )
                                                        )
                                               )
                                             ).

    PERFORM single_item_fill_structures
      IN PROGRAM sapmfmci IF FOUND
            TABLES   lt_fmci
                     lt_fmcit
                     lt_fmhici
                     lt_fmzubsp
            USING    space
                     ls_fmci
                     ls_fmcit
                     lt_hier
                     ls_fmzubsp
           CHANGING  l_subrc.

    IF NOT me->get_simulate( ).
      "update mode
      CALL FUNCTION 'ENQUEUE_EFMCI'
        EXPORTING
          fikrs          = me->get_fikrs( )
          gjahr          = CONV gjahr( '' )
          fipex          = me->get_data( )-fmci-fipex
        EXCEPTIONS
          foreign_lock   = 1                " Object already locked
          system_failure = 2                " Internal error from enqueue server
          OTHERS         = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO me->dummy.
        _mac_raise.
      ENDIF.

      CALL FUNCTION 'FM_COMITEM_UPDATE'
        EXPORTING
          i_fikrs        = me->get_fikrs( )
          i_gjahr        = CONV gjahr( '' )
          i_flg_commit   = me->get_commit( )
        TABLES
          t_fmmd_fmci    = lt_fmci
          t_fmmd_fmcit   = lt_fmcit
          t_fmmd_fmhici  = lt_fmhici
          t_fmmd_fmzubsp = lt_fmzubsp
        EXCEPTIONS
          error_occurred = 1
          OTHERS         = 2.

      r_result = xsdbool( sy-subrc = 0 ).

      CALL FUNCTION 'DEQUEUE_EFMCI'
        EXPORTING
          fikrs = me->get_fikrs( )
          gjahr = CONV gjahr( '' )
          fipex = me->get_data( )-fmci-fipex.

    ELSE.
      "simulation mode
      CALL FUNCTION 'FM_COM_ITEM_NO_SCREEN_CREATE'
        EXPORTING
          i_f_fmci           = CORRESPONDING fmci( ls_fmci )
          i_f_fmcit          = CORRESPONDING fmcit( ls_fmcit )
          i_f_fmzubsp        = CORRESPONDING fmzubsp( ls_fmzubsp )
          i_fipup            = ls_fmci-fipup
          i_varnt            = VALUE #( lt_hier[ 1 ]-varnt DEFAULT CONV #( |000| ) )
          i_flg_test         = abap_true
          i_flg_no_enqueue   = abap_true
          i_flg_commit       = abap_false
        IMPORTING
          e_status_md        = l_status_md
          e_status_hierarchy = l_status_hierarchy
        EXCEPTIONS
          error_occured      = 1
          OTHERS             = 2.

      r_result = xsdbool( sy-subrc = 0 ).

    ENDIF.

    CHECK r_result IS INITIAL.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO me->dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD zif_masterdata~existencecheck.

    DATA(l_fikrs) = me->get_fikrs( ).
    DATA(l_fipex) = me->get_fipex( ).
    DATA(l_gjahr) = me->get_data( )-fmci-gjahr.

    SELECT COUNT(*) FROM fmci
      WHERE fikrs = l_fikrs
      AND   gjahr = l_gjahr
      AND   fipex = l_fipex.

    r_result = xsdbool( sy-dbcnt > 0 ).

    CHECK r_result IS INITIAL.

    MESSAGE e094 WITH me->get_fipex( ) INTO me->dummy.

    _mac_raise.

  ENDMETHOD.

  METHOD get_fipex.
    r_result = me->lv_fipex.
  ENDMETHOD.

  METHOD set_fipex.
    me->lv_fipex = |{ iv_fipex ALPHA = OUT }|.
  ENDMETHOD.

  METHOD check_mode.

  ENDMETHOD.

  METHOD get_ref.
    r_result = me->lv_ref.
  ENDMETHOD.

  METHOD set_ref.
    me->lv_ref = |{ iv_ref ALPHA = OUT }|.
  ENDMETHOD.

ENDCLASS.
