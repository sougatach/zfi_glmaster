CLASS zcl_fi_fsv_api DEFINITION
  PUBLIC
  INHERITING FROM zcl_fi_glmast_api
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_fico_masterdata.

  PUBLIC SECTION.

    METHODS zif_masterdata~existencecheck REDEFINITION.
    METHODS zif_masterdata~maintain       REDEFINITION.
    METHODS zif_masterdata~read_data      REDEFINITION.

  PROTECTED SECTION.

    METHODS _save REDEFINITION.

  PRIVATE SECTION.

    METHODS:
      "! <p class="shorttext synchronized">Get FSV Version</p>
      get_versn RETURNING VALUE(r_result) TYPE versn_011,
      "! <p class="shorttext synchronized">Get FSV Position</p>
      get_ergsl RETURNING VALUE(r_result) TYPE ergsl.

ENDCLASS.



CLASS zcl_fi_fsv_api IMPLEMENTATION.


  METHOD get_ergsl.
    r_result = me->lv_ergsl.
  ENDMETHOD.


  METHOD get_versn.
    r_result = me->lv_versn.
  ENDMETHOD.


  METHOD zif_masterdata~maintain.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->dummy = SWITCH #( me->get_mode( )
                  WHEN action-insert OR action-update
                  THEN me->check_auth_glmast( )
                  ELSE THROW zcx_fi_general(
                              textid  = zcx_fi_general=>wrong_mode_fsv
                              gv_mode = me->get_mode( )
                                           )
                       ).

    me->check_data( ).

    r_result = me->_save( ).

  ENDMETHOD.


  METHOD zif_masterdata~read_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(l_versn) = me->get_versn( ).
    DATA(l_saknr) = me->get_saknr( ).

    SELECT * FROM fagl_011zc
      UP TO 1 ROWS
        INTO es_data
        WHERE versn = l_versn
        AND   vonkt <= l_saknr
        AND   biskt >= l_saknr.
    ENDSELECT.

    CHECK sy-subrc <> 0.

    MESSAGE e095 WITH |{ l_saknr ALPHA = OUT }| l_versn INTO me->dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD _save.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    ASSERT me->get_ktopl( ) AND me->get_ergsl( ).

    DATA ls_data TYPE fagl_011zc.

* check if the GL account is assigned or not to the FSV
    TRY.
        me->zif_masterdata~read_data(
          IMPORTING
            es_data = ls_data
        ).

      CATCH BEFORE UNWIND zcx_fi_general INTO DATA(lo_exception).
        DATA(is_assign) = xsdbool( ls_data IS INITIAL ).
        CHECK lo_exception->is_resumable = abap_true.
        RESUME.
    ENDTRY.


    IF is_assign IS NOT INITIAL.
      "not assigned to the FSV yet -> Assign to the FSV
      CALL FUNCTION 'ZFI_GL_ASSIGN_FSV'
        EXPORTING
          iv_versn        = me->get_versn( )
          iv_ktopl        = me->get_ktopl( )
          iv_saknr        = me->get_saknr( )
          iv_ergsl        = me->get_ergsl( )
          iv_commit       = me->get_commit( )
        IMPORTING
          ev_ok           = r_result
        EXCEPTIONS
          ex_no_authority = 1
          ex_gl_no_exist  = 2
          ex_load_error   = 3
          ex_read_error   = 4
          ex_lock_error   = 5
          ex_save_error   = 6
          OTHERS          = 7.

    ELSE.           "already assigned to the FSV
      "check the hierarchy node reassignment is different from the existing node
      CHECK ls_data-ergsl <> me->get_ergsl( ).

      " Now Reassign to a different reporting Hierarchy node of the FSV
      CALL FUNCTION 'ZFI_GL_REASSIGN_FSV'
        EXPORTING
          iv_versn        = me->get_versn( )
          iv_ktopl        = me->get_ktopl( )
          iv_saknr        = me->get_saknr( )
          iv_ergsl        = me->get_ergsl( )
          iv_commit       = me->get_commit( )
        IMPORTING
          ev_ok           = r_result
        EXCEPTIONS
          ex_no_authority = 1
          ex_gl_no_exist  = 2
          ex_gl_no_assign = 3
          ex_load_error   = 4
          ex_read_error   = 5
          ex_lock_error   = 6
          ex_move_error   = 7
          ex_save_error   = 8
          OTHERS          = 9.

    ENDIF.

    CHECK sy-subrc <> 0 OR r_result IS INITIAL.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO me->dummy.

    _mac_raise.

  ENDMETHOD.

  METHOD zif_masterdata~existencecheck.

    DATA ls_data TYPE fagl_011zc.

    super->zif_masterdata~existencecheck( ).

    me->zif_masterdata~read_data( IMPORTING es_data = ls_data ).


  ENDMETHOD.

ENDCLASS.
