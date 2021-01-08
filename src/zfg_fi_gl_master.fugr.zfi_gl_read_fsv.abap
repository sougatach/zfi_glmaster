FUNCTION ZFI_GL_READ_FSV.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_VERSN) TYPE  VERSN_011 DEFAULT '1020'
*"  EXPORTING
*"     REFERENCE(RT_RESULT) TYPE  HRASR00HELP_DATASET_TAB
*"  EXCEPTIONS
*"      EX_NO_AUTHORITY
*"      EX_LOAD_ERROR
*"      EX_READ_ERROR
*"----------------------------------------------------------------------

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   September 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

  CLEAR:
    flg_sylangu_not_found,
    maint_langu.

  FREE node_tab.

  AUTHORITY-CHECK OBJECT 'F_T011'
        ID 'VERSN' FIELD iv_versn
        ID 'ACTVT' FIELD '03'.

  IF sy-subrc <> 0.
    MESSAGE ID 'FE' TYPE 'E' NUMBER 030
     WITH iv_versn RAISING ex_no_authority.
  ENDIF.

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

  PERFORM create_node_tab IN PROGRAM rfgsbstr IF FOUND
      TABLES node_tab.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
    TABLES
      nodetab            = node_tab
    EXCEPTIONS
      tree_failure       = 1                " Unable to generate hierarchy
      id_not_found       = 2
      wrong_relationship = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING ex_read_error.
  ENDIF.

* we only care for the P's
  rt_result = VALUE #( FOR node IN node_tab
                 WHERE ( type = |P| )
               ( help_key   = node-name
                 help_value = node-text )
                     ).

ENDFUNCTION.
