FUNCTION ZFI_GL_ASSIGN_FSV.
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
*"      EX_LOAD_ERROR
*"      EX_READ_ERROR
*"      EX_LOCK_ERROR
*"      EX_SAVE_ERROR
*"----------------------------------------------------------------------
*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
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

  AUTHORITY-CHECK OBJECT 'F_T011'
        ID 'VERSN' FIELD iv_versn
        ID 'ACTVT' FIELD '02'.

  IF sy-subrc <> 0.
    MESSAGE ID 'FE' TYPE 'E' NUMBER 030
     WITH iv_versn RAISING ex_no_authority.
  ENDIF.

  DATA(l_saknr) = CONV saknr( |{ iv_saknr ALPHA = IN }| ).

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

  EXPORT l_mem TO MEMORY ID 'TAFE_GL'.              "See Enhancement ZENH_IMPL_BS_SAVE

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

  PERFORM ergsl_to_pos_id IN PROGRAM saplfibs IF FOUND
            USING    iv_ergsl
            CHANGING l_posid.

  DATA(node_id) = VALUE fibs_bs_node_id( type = |P| pos_id = l_posid ). "P = Parent

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

  PERFORM delete_update_add_accounts IN PROGRAM rfgsbstr IF FOUND
            TABLES del_node_acct_info_tab
                   lt_node_acct_info
            USING  node_id.

  PERFORM check_intersection_all IN PROGRAM saplfibs IF FOUND
            USING l_intersection.

  IF l_intersection = abap_true.
    MESSAGE e723(fe) RAISING ex_save_error.
  ENDIF.

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

  CALL FUNCTION 'FAGL_FI_BS_SAVE'
    EXCEPTIONS
      no_save_intersection     = 1
      auto_transport_cancelled = 2
      auto_transport_failed    = 3
      OTHERS                   = 4.

  DATA(l_subrc) = sy-subrc.

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

  IF iv_wait IS INITIAL.
    cl_soap_commit_rollback=>commit( ).
  ELSE.
    cl_soap_commit_rollback=>commit_and_wait( ).
  ENDIF.



ENDFUNCTION.
