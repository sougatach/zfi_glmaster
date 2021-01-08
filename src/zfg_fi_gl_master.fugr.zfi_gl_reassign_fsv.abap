FUNCTION ZFI_GL_REASSIGN_FSV.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VERSN) TYPE  T011-VERSN DEFAULT '1020'
*"     VALUE(IV_KTOPL) TYPE  SKA1-KTOPL DEFAULT '1000'
*"     VALUE(IV_SAKNR) TYPE  SKA1-SAKNR
*"     VALUE(IV_ERGSL) TYPE  RF011P-ERGSL
*"     VALUE(IV_COMMIT) TYPE  BAPI_COMMIT_WORK-COMMIT_WORK DEFAULT
*"       ABAP_TRUE
*"     VALUE(IV_WAIT) TYPE  BAPITA-WAIT DEFAULT ABAP_FALSE
*"  EXPORTING
*"     REFERENCE(EV_OK) TYPE  BOOLE-BOOLE
*"  EXCEPTIONS
*"      EX_NO_AUTHORITY
*"      EX_GL_NO_EXIST
*"      EX_GL_NO_ASSIGN
*"      EX_LOAD_ERROR
*"      EX_READ_ERROR
*"      EX_LOCK_ERROR
*"      EX_MOVE_ERROR
*"      EX_SAVE_ERROR
*"----------------------------------------------------------------------
*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   September 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

  FREE:
   lt_node_acct,
   lt_node_acct_info,
   del_node_acct_info_tab,
   lt_fagl_011qt,
   lt_fagl_011tc.

  CLEAR:
    flg_sylangu_not_found,
    maint_langu,
    l_intersection,
    l_refresh,
    l_posid,
    ls_pos_info.

* check auth
  AUTHORITY-CHECK OBJECT 'F_T011'
        ID 'VERSN' FIELD iv_versn
        ID 'ACTVT' FIELD '02'.

  IF sy-subrc <> 0.
    MESSAGE ID 'FE' TYPE 'E' NUMBER 030
     WITH iv_versn RAISING ex_no_authority.
  ENDIF.

  DATA(l_saknr) = CONV saknr( |{ iv_saknr ALPHA = IN }| ).

* check GL account passed exists in order to proceed to next step
  CALL FUNCTION 'READ_SKA1'
    EXPORTING
      xktopl         = iv_ktopl
      xsaknr         = l_saknr
    EXCEPTIONS
      key_incomplete = 1
      not_authorized = 2
      not_found      = 3
      OTHERS         = 4.

  CASE sy-subrc.
    WHEN 0.
    WHEN 2.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         RAISING ex_no_authority.
    WHEN OTHERS.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
       RAISING ex_gl_no_exist.
  ENDCASE.

* check GL account is already assigned in FSV to be able to reassign it
  SELECT * FROM fagl_011zc
    INTO @DATA(ls_011zc)
    UP TO 1 ROWS
    WHERE versn = @iv_versn
    AND   ktopl = @iv_ktopl
    AND   vonkt <= @l_saknr
    AND   biskt >= @l_saknr.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE e059(zfi_glmast) WITH iv_saknr iv_versn
     RAISING ex_gl_no_assign.
  ENDIF.

  EXPORT l_mem TO MEMORY ID 'TAFE_GL'.      "See Enhancement ZENH_IMPL_BS_SAVE

* load the FAGL Balance Sheet "Object" (formerly known as Function Groups in SAP)
  CALL FUNCTION 'FAGL_FI_BS_LOAD'
    EXPORTING
      version         = iv_versn
    EXCEPTIONS
      not_found       = 1
      pos_not_found   = 2
      langu_not_found = 3
      OTHERS          = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
     RAISING ex_load_error.
  ENDIF.

  CALL FUNCTION 'FAGL_FI_BS_LOAD_LANGU'
    EXPORTING
      flg_langu_maint       = flg_langu_maint
    IMPORTING
      flg_sylangu_not_found = flg_sylangu_not_found
      maint_langu           = maint_langu
    EXCEPTIONS
      OTHERS                = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
     RAISING ex_load_error.
  ENDIF.

  DATA l_posid_from TYPE fibs_acct_id.

* convert the Source FS Item (ERGSL) to Source FS Position ID (POS_ID) complying with FG SAPLFIBS
  PERFORM ergsl_to_pos_id IN PROGRAM saplfibs IF FOUND
            USING    ls_011zc-ergsl
            CHANGING l_posid_from.

* convert the Target FS Item (ERGSL) to Target FS Position ID (POS_ID) complying with FG SAPLFIBS
  PERFORM ergsl_to_pos_id IN PROGRAM saplfibs IF FOUND
            USING    iv_ergsl
            CHANGING l_posid.

* prepare the NODE_ID source and target structures before moving into the next step
  DATA(node_id)    = VALUE fibs_bs_node_id( type = |P| pos_id = l_posid_from ). "P = Parent
  DATA(node_id_to) = VALUE fibs_bs_node_id( type = |P| pos_id = l_posid ).

* get the children of the Source Node ID in the FSV
  CALL FUNCTION 'FI_BS_NODE_GET_CHILDREN'
    EXPORTING
      node              = node_id
      with_gl_accounts  = ' '
    TABLES
      children_node_tab = lt_node_acct
    EXCEPTIONS
      node_not_found    = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING ex_read_error.
  ENDIF.

* build a table with more information like GL account details from the LT_NODE_ACCT retrieved in the previous step
  LOOP AT lt_node_acct INTO DATA(ls_node_acct).
    CALL FUNCTION 'FI_BS_ACCT_GET_INFO'
      EXPORTING
        node_id        = ls_node_acct
      IMPORTING
        acct_info      = lt_node_acct_info-info
      EXCEPTIONS
        node_not_found = 01
        wrong_type     = 02.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        RAISING ex_read_error.
    ENDIF.

    lt_node_acct_info-node = ls_node_acct.
    APPEND lt_node_acct_info.

    AT LAST.
      CLEAR lt_node_acct_info-node.
      lt_node_acct_info-info-ktopl     = iv_ktopl.
      lt_node_acct_info-info-acct_from = lt_node_acct_info-info-acct_to = |{ iv_saknr ALPHA = IN }|.
      lt_node_acct_info-info-debit     = lt_node_acct_info-info-credit  = abap_true.
      APPEND lt_node_acct_info.
    ENDAT.
  ENDLOOP.

* now move/reassign the GL account within the FSV from the source Node to the target node
  CALL FUNCTION 'FI_BS_NODE_MOVE'
    EXPORTING
      from_node           = VALUE fibs_bs_node_id(
                              type    = |A|
                              acct_id = VALUE #( lt_node_acct_info[ info-acct_from = l_saknr ]-node-acct_id OPTIONAL )
                                                 )
      to_node             = node_id_to
      as_child            = abap_true
    EXCEPTIONS
      from_node_not_found = 1
      to_node_not_found   = 2
      wrong_type          = 3
      OTHERS              = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING ex_move_error.
  ENDIF.

* lock the FS Version
  CALL FUNCTION 'FI_BS_ENQUEUE'
    EXPORTING
      versn          = iv_versn
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         RAISING ex_lock_error.
  ENDIF.

* update the FSV Version
  CALL FUNCTION 'FAGL_FI_BS_SAVE'
    EXCEPTIONS
      no_save_intersection     = 1
      auto_transport_cancelled = 2
      auto_transport_failed    = 3
      OTHERS                   = 4.

  DATA(l_subrc) = sy-subrc.

* unlock the FS Version
  CALL FUNCTION 'FI_BS_DEQUEUE'
    EXPORTING
      versn = iv_versn.

  IF l_subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING ex_save_error.
  ENDIF.

  ev_ok = abap_true.

  CHECK iv_commit = abap_true.

* commit if required
  IF iv_wait IS INITIAL.
    cl_soap_commit_rollback=>commit( ).
  ELSE.
    cl_soap_commit_rollback=>commit_and_wait( ).
  ENDIF.


ENDFUNCTION.
