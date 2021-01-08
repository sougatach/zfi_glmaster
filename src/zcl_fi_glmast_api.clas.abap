"! <p class="shorttext synchronized">API for FI GL Master Data</p>
CLASS zcl_fi_glmast_api DEFINITION
  PUBLIC
  INHERITING FROM zcl_fico_masterdata
  CREATE PROTECTED

  GLOBAL FRIENDS zcl_fico_masterdata .

  PUBLIC SECTION.

    ALIASES auth_actvt
      FOR zif_fi_global_constants~co_auth_actvt .
    ALIASES tafe_ccode
      FOR zif_fi_global_constants~comp_code_tafe .
    ALIASES tafe_co_area
      FOR zif_fi_global_constants~co_area_1000 .
    ALIASES tafe_grp_coa
      FOR zif_fi_global_constants~coa_group_tafe .
    ALIASES tafe_oper_coa
      FOR zif_fi_global_constants~coa_operating_tafe .

    "! <p class="shorttext synchronized">Set Deletion Flag in Chart of Accounts</p>
    "!
    "! @parameter iv_del | <p class="shorttext synchronized">X = Set delete; Space = Clear delete flag</p>
    "! @parameter r_result | <p class="shorttext synchronized">True = Set delete successful</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
    METHODS set_coa_delete
      IMPORTING !iv_del         TYPE xloev DEFAULT abap_true
      RETURNING
                VALUE(r_result) TYPE boole_d
      RAISING
                zcx_fi_general .
    "! <p class="shorttext synchronized">Set Posting Block in Chart of Accounts</p>
    "!
    "! @parameter iv_block | <p class="shorttext synchronized">True = X = Block; Space = Unblock</p>
    "! @parameter r_result | <p class="shorttext synchronized">True = Set Posting Block successful</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
    METHODS set_coa_block
      IMPORTING !iv_block       TYPE xspeb DEFAULT abap_true
      RETURNING
                VALUE(r_result) TYPE boole_d
      RAISING
                zcx_fi_general .
    "! <p class="shorttext synchronized">Read GL Account Blocked Indicators</p>
    "!
    "! @parameter rs_result | <p class="shorttext synchronized">G/L Account blocks</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
    METHODS read_gl_blocks
      RETURNING
        VALUE(rs_result) TYPE /eby/pdmdgl_sbapi_blocks
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Set Chart of Accounts</p>
    METHODS set_ktopl
      IMPORTING
        iv_ktopl TYPE ktopl .
    "! <p class="shorttext synchronized">Set Company Code</p>
    METHODS set_bukrs
      IMPORTING
        iv_bukrs TYPE bukrs .
    "! <p class="shorttext synchronized">Set COA data</p>
    METHODS set_coa_data
      IMPORTING
        is_coa_data TYPE glaccount_coa_data .
    "! <p class="shorttext synchronized">Set Company Code data</p>
    METHODS set_ccode_data
      IMPORTING
        is_ccode_data TYPE glaccount_ccode_data .
    "! <p class="shorttext synchronized">Set GL Account Name</p>
    METHODS set_acct_name
      IMPORTING
        is_acct_name TYPE glaccount_name_data .
    "! <p class="shorttext synchronized">Set GL Account Keyword</p>
    METHODS set_keyword
      IMPORTING
        iv_keyword TYPE schlw .
    METHODS: set_versn IMPORTING iv_versn TYPE versn_011.
    METHODS: set_ergsl IMPORTING iv_ergsl TYPE ergsl.

    METHODS zif_masterdata~existencecheck REDEFINITION.
    METHODS zif_masterdata~maintain       REDEFINITION .
    METHODS zif_masterdata~read_data      REDEFINITION .



  PROTECTED SECTION.

    CLASS-DATA lv_ref TYPE saknr .

    DATA lv_saknr TYPE saknr .
    DATA lv_ktopl TYPE ktopl .
    DATA lv_bukrs TYPE bukrs .
    DATA lv_versn TYPE versn_011.
    DATA lv_ergsl TYPE ergsl.

    "! <p class="shorttext synchronized">Set GL Account</p>
    METHODS set_saknr
      IMPORTING
        iv_saknr TYPE saknr .
    "! <p class="shorttext synchronized">Check Read/Write Authorisation</p>
    METHODS check_auth_glmast
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Get GL Account</p>
    METHODS get_saknr
      RETURNING
        VALUE(r_result) TYPE saknr .
    "! <p class="shorttext synchronized">Get Chart of Accounts</p>
    METHODS get_ktopl
      RETURNING
        VALUE(r_result) TYPE ktopl .
    "! <p class="shorttext synchronized">Get Company Code</p>
    METHODS get_bukrs
      RETURNING
        VALUE(r_result) TYPE bukrs .
    "! <p class="shorttext synchronized">Check the data passed from outside</p>
    METHODS check_data
      RAISING
        RESUMABLE(zcx_fi_general).
    "! <p class="shorttext synchronized">Check GL Account exist</p>
    METHODS check_gl
      IMPORTING
        iv_saknr        TYPE saknr OPTIONAL
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        RESUMABLE(zcx_fi_general).
    "! <p class="shorttext synchronized">Check Chart of Accounts</p>
    METHODS check_ktopl
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Check Company Code</p>
    METHODS check_bukrs
      RAISING
        zcx_fi_general.
    "! <p class="shorttext synchronized">Check Keys for Read</p>
    METHODS check_read
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general.
    "! <p class="shorttext synchronized">Check call to Maintain</p>
    METHODS check_maintain
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Set Reference GL Account</p>
    METHODS set_ref
      IMPORTING
        iv_ref TYPE any .
    "! <p class="shorttext synchronized">Insert/Update/Delete of Master Data object</p>
    "! @parameter r_result | <p class="shorttext synchronized">True = No Errors</p>
    METHODS _save
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Get Reference GL Account</p>
    METHODS get_ref
      RETURNING
        VALUE(r_result) TYPE saknr.
    "! <p class="shorttext synchronized">GL API Object Constructor</p>
    "! @parameter iv_saknr | <p class="shorttext synchronized">GL Account</p>
    METHODS constructor
      IMPORTING
        iv_saknr TYPE saknr .

    METHODS: check_mode REDEFINITION.

  PRIVATE SECTION.

    DATA ls_coa_refacct TYPE glaccount_coa_key .
    DATA ls_ccode_refacct TYPE glaccount_ccode_key .
    DATA ls_coa_data TYPE glaccount_coa_data .
    DATA ls_ccode_data TYPE glaccount_ccode_data .
    DATA ls_acct_name TYPE glaccount_name_data .
    DATA ls_coa_mapped TYPE glaccount_coa .
    DATA lt_ccode_mapped TYPE glaccount_ccode_table .
    DATA lt_kw_mapped TYPE glaccount_keyword_table .
    DATA lt_name_mapped TYPE glaccount_name_table .
    DATA lv_keyword TYPE schlw .

    "! <p class="shorttext synchronized">Reads table SKA1 with its Key</p>
    METHODS read_ska1
      IMPORTING
        !is_coa_key      TYPE glaccount_coa_key OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ska1 .
    "! <p class="shorttext synchronized">Reads table SKB1 with its Key</p>
    METHODS read_skb1
      IMPORTING
        !is_ccode_key    TYPE glaccount_ccode_key OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE skb1 .
    "! <p class="shorttext synchronized">Check GL Account exist in the COA</p>
    METHODS check_gl_coa
      IMPORTING
        !is_coa_key     TYPE glaccount_coa_key OPTIONAL
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        RESUMABLE(zcx_fi_general).
    "! <p class="shorttext synchronized">Check GL Account exist in the Company Code</p>
    METHODS check_gl_ccode
      IMPORTING
        !is_ccode_key   TYPE glaccount_ccode_key OPTIONAL
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        RESUMABLE(zcx_fi_general).
    "! <p class="shorttext synchronized">Get COA Reference GL Account</p>
    METHODS get_coa_refacct
      RETURNING
        VALUE(rs_result) TYPE glaccount_coa_key .
    "! <p class="shorttext synchronized">Get Company Code Reference GL Account</p>
    METHODS get_ccode_refacct
      RETURNING
        VALUE(rs_result) TYPE glaccount_ccode_key .
    "! <p class="shorttext synchronized">Get GL Account Name</p>
    METHODS get_acct_name
      RETURNING
        VALUE(rs_result) TYPE glaccount_name_data .
    "! <p class="shorttext synchronized">Get Reference GL Account Name</p>
    METHODS get_refacct_name
      RETURNING
        VALUE(rs_result) TYPE glaccount_name_data .
    "! <p class="shorttext synchronized">Get GL Account Keyword</p>
    METHODS get_keyword
      RETURNING
        VALUE(r_result) TYPE schlw .
    "! <p class="shorttext synchronized">Get Reference GL Account Keyword</p>
    METHODS get_refkeyword
      RETURNING
        VALUE(r_result) TYPE schlw .
    "! <p class="shorttext synchronized">Get COA data</p>
    METHODS get_coa_data
      RETURNING
        VALUE(rs_result) TYPE glaccount_coa_data .
    "! <p class="shorttext synchronized">Get Company Code data</p>
    METHODS get_ccode_data
      RETURNING
        VALUE(rs_result) TYPE glaccount_ccode_data .
    "! <p class="shorttext synchronized">Get COA from Company Code</p>
    METHODS get_ktopl_ccode
      IMPORTING
        !iv_bukrs       TYPE bukrs
      RETURNING
        VALUE(r_result) TYPE ktopl .
    "! <p class="shorttext synchronized">Read GL Master Data for single COA and/or Comp Code</p>
    METHODS read_glmast
      RETURNING
        VALUE(rs_result) TYPE /eby/pdmdgl_sbapi_data
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Maintain Single GL Account Master Data</p>
    "!
    "! @parameter r_result | <p class="shorttext synchronized">True = Success</p>
    "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
    METHODS maintain
      IMPORTING
        !iv_ref         TYPE saknr OPTIONAL
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Set COA Reference GL Account</p>
    METHODS set_coa_refacct
      IMPORTING
        !is_coa_refacct TYPE glaccount_coa_key OPTIONAL.
    "! <p class="shorttext synchronized">Set Company Code Reference GL Account</p>
    METHODS set_ccode_refacct
      IMPORTING
        !is_ccode_refacct TYPE glaccount_ccode_key OPTIONAL.
    "! <p class="shorttext synchronized">Check GL Account in both COA and Company Code</p>
    METHODS check_gl_coa_ccode RETURNING VALUE(r_result) TYPE boole_d
                               RAISING   RESUMABLE(zcx_fi_general).
    "! <p class="shorttext synchronized">Lock GL Account for Update</p>
    METHODS enqueue
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">UnLock GL Account</p>
    METHODS dequeue .
    "! <p class="shorttext synchronized">Handle Maintenance</p>
    METHODS _maintain
      RETURNING
        VALUE(r_result) TYPE boole_d
      RAISING
        zcx_fi_general .
    "! <p class="shorttext synchronized">Is created by COA Reference?</p>
    METHODS _is_coa_byref
      RETURNING
        VALUE(r_result) TYPE boole_d .
    "! <p class="shorttext synchronized">Is created by Company Code Reference?</p>
    METHODS _is_ccode_byref
      RETURNING
        VALUE(r_result) TYPE boole_d .
    "! <p class="shorttext synchronized">Set COA Mappings</p>
    METHODS _set_map_coa
      IMPORTING
        !is_coa_data TYPE glaccount_coa .
    "! <p class="shorttext synchronized">Get COA Mappings</p>
    METHODS _get_map_coa
      RETURNING
        VALUE(rs_result) TYPE glaccount_coa .
    "! <p class="shorttext synchronized">Get GL Account Keyword</p>
    METHODS _get_keyword
      RETURNING
        VALUE(rt_result) TYPE glaccount_keyword_table .
    "! <p class="shorttext synchronized">Set GL Account Keyword</p>
    METHODS _set_keyword
      IMPORTING
        !it_keyword TYPE glaccount_keyword_table .
    "! <p class="shorttext synchronized">Mapping for COA</p>
    METHODS _coa_mapper
      RETURNING
        VALUE(rs_result) TYPE glaccount_coa .
    "! <p class="shorttext synchronized">Mapping for Keyword</p>
    METHODS _keyword_mapper
      RETURNING
        VALUE(rt_result) TYPE glaccount_keyword_table .
    "! <p class="shorttext synchronized">Get GL Account Name</p>
    METHODS _get_acct_name
      RETURNING
        VALUE(rt_result) TYPE glaccount_name_table .
    "! <p class="shorttext synchronized">Set GL Account Name</p>
    METHODS _set_acct_name
      IMPORTING
        !it_acct_name TYPE glaccount_name_table .
    "! <p class="shorttext synchronized">Mapping for Account Name</p>
    METHODS _acct_name_mapper
      RETURNING
        VALUE(rt_result) TYPE glaccount_name_table .
    "! <p class="shorttext synchronized">Get Company Code Mappings</p>
    METHODS _get_map_ccode
      RETURNING
        VALUE(rt_result) TYPE glaccount_ccode_table .
    "! <p class="shorttext synchronized">Set Company Code Mappings</p>
    METHODS _set_map_ccode
      IMPORTING
        !it_ccode_data TYPE glaccount_ccode_table .
    "! <p class="shorttext synchronized">Mapping for Company Code</p>
    METHODS _ccode_mapper
      RETURNING
        VALUE(rt_result) TYPE glaccount_ccode_table .
    METHODS _save_glenhancements RAISING zcx_fi_general.
    METHODS _get_glenh_text
      IMPORTING
        iv_tabname       TYPE tabname
        iv_key           TYPE name_feld
        iv_value         TYPE any
      EXPORTING
        VALUE(es_result) TYPE any.

ENDCLASS.


CLASS zcl_fi_glmast_api IMPLEMENTATION.


  METHOD check_auth_glmast.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    IF me->get_mode( ) = action-insert OR me->get_mode( ) = action-update.
      CHECK NOT me->get_simulate( ).
    ENDIF.

    CALL FUNCTION 'GL_ACCT_UTIL_CHECK_AUTHORITY'
      EXPORTING
        x_chart_of_accounts = CONV xfeld( xsdbool( me->get_ktopl( ) OR me->_is_coa_byref( ) ) )
        x_company_code      = CONV xfeld( xsdbool( me->get_bukrs( ) OR me->_is_ccode_byref( ) ) )
        chart_of_accounts   = COND #( WHEN me->get_ktopl( )
                                      THEN me->get_ktopl( )
                                      ELSE me->get_coa_refacct( )-ktopl
                                    )
        company_code        = COND #( WHEN me->get_bukrs( )
                                      THEN me->get_bukrs( )
                                      ELSE me->get_ccode_refacct( )-bukrs
                                    )
        authorization_group = me->read_skb1(
                                VALUE #( bukrs = COND #(
                                                  WHEN me->get_bukrs( )
                                                  THEN me->get_bukrs( )
                                                  ELSE me->get_ccode_refacct( )-bukrs
                                                       )
                                         saknr = me->get_saknr( )
                                       )
                                           )-begru
        activity            = SWITCH #( me->get_mode( )
                                WHEN action-read OR space
                                THEN auth_actvt-read
                                ELSE auth_actvt-update
                                      )
      EXCEPTIONS
        no_authority        = 1
        OTHERS              = 2.

    r_result = xsdbool( sy-subrc = 0 ).

    CHECK sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO me->dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD check_bukrs.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    CHECK me->get_bukrs( ).

    TRY.
        NEW cl_company_code( me->get_bukrs( ) ).

      CATCH cx_company_code.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO dummy.

        _mac_raise.
    ENDTRY.

  ENDMETHOD.


  METHOD check_data.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->check_mode( ).

    TRY.
        me->dummy = SWITCH boole_d( me->get_mode( )
                      WHEN action-insert
                      THEN COND #( WHEN me->zif_masterdata~existencecheck( )
                                   THEN THROW zcx_fi_general(
                                                textid    = zcx_fi_general=>gl_exists
                                                gv_saknr  = me->get_saknr( )
                                                             )
                                   ELSE abap_false
                                 )
                      WHEN action-update OR action-delete OR action-block
                      THEN me->zif_masterdata~existencecheck( )
                      WHEN action-read
                      THEN me->zif_masterdata~existencecheck( )
                      ELSE THROW zcx_fi_general(
                                  textid    = zcx_fi_general=>wrong_mode
                                  gv_mode   = me->get_mode( )
                                               )
                                 ).

      CATCH BEFORE UNWIND zcx_fi_general INTO DATA(lr_exception).
        IF lr_exception->is_resumable = abap_false.
          RAISE EXCEPTION TYPE zcx_fi_general
            EXPORTING
              textid   = SWITCH #( me->get_mode( )
                          WHEN action-insert
                          THEN zcx_fi_general=>gl_exists
                          ELSE zcx_fi_general=>gl_not_exist
                                 )
              gv_saknr = lr_exception->gv_saknr.
        ELSE.       "Note that Resumable exception is raised here
          IF me->get_mode( ) = action-insert.
            RESUME.
          ELSE.
            _mac_raise.
          ENDIF.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD check_gl.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(l_saknr) = COND #( WHEN iv_saknr IS INITIAL
                            THEN me->get_saknr( )
                            ELSE |{ iv_saknr ALPHA = IN }|
                          ).

    SELECT COUNT(*)
        FROM ska1 BYPASSING BUFFER
        WHERE saknr = l_saknr.

    r_result = xsdbool( sy-dbcnt > 0 ).

    CHECK r_result = abap_false.

    SELECT COUNT(*)
       FROM skb1 BYPASSING BUFFER
       WHERE saknr = l_saknr.

    r_result = xsdbool( sy-dbcnt > 0 ).

    CHECK r_result = abap_false.

    MESSAGE e089 WITH me->get_saknr( ) INTO DATA(var).

    _mac_raise.

  ENDMETHOD.


  METHOD check_gl_ccode.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    CHECK is_ccode_key IS NOT INITIAL.

    r_result = xsdbool( me->read_skb1( is_ccode_key ) ).

    CHECK r_result IS INITIAL.

    MESSAGE e003(zfi_glmast) WITH |{ is_ccode_key-saknr ALPHA = OUT }| is_ccode_key-bukrs  INTO dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD check_gl_coa.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    CHECK is_coa_key IS NOT INITIAL.

    r_result = xsdbool( me->read_ska1( is_coa_key ) ).

    CHECK r_result IS INITIAL.

    MESSAGE e002(zfi_glmast) WITH |{ is_coa_key-saknr ALPHA = OUT }| is_coa_key-ktopl INTO dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD check_ktopl.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    CHECK me->get_ktopl( ).

    CALL FUNCTION 'FI_GL_READ_CHART_OF_ACCOUNT'
      EXPORTING
        i_ktopl                    = me->get_ktopl( )
      EXCEPTIONS
        chart_of_account_not_found = 1
        text_not_found             = 2
        OTHERS                     = 3.

    CHECK sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD check_maintain.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    IF NOT me->get_ktopl( ) AND NOT me->get_bukrs( ).
      MESSAGE e008 WITH |Either Chart of Accounts or Company Code is required to Maintain GL Master| INTO dummy.
      _mac_raise.
    ENDIF.

    CHECK me->get_mode( ) = action-update AND me->get_ref( ).

    MESSAGE e008 WITH |Maintain with Reference is only valid for 'Insert' Mode| INTO dummy.
    _mac_raise.

  ENDMETHOD.


  METHOD check_read.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    me->check_gl_coa( COND #( WHEN me->get_ktopl( )
                              THEN |{ me->get_ktopl( ) }{ me->get_saknr( ) }|
                            )
                    ).

    me->check_gl_ccode( COND #( WHEN me->get_bukrs( )
                                THEN |{ me->get_bukrs( ) }{ me->get_saknr( ) }|
                              )
                      ).

    r_result = abap_true.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    me->set_saknr( iv_saknr ).

  ENDMETHOD.


  METHOD dequeue.

    CHECK NOT me->get_simulate( ).

    CALL FUNCTION 'GL_ACCT_UTIL_DEQUEUE_ALL'.

  ENDMETHOD.


  METHOD enqueue.

    CHECK NOT me->get_simulate( ).

    CALL FUNCTION 'GL_ACCT_UTIL_ENQUEUE'
      EXPORTING
        x_chart_of_accounts = abap_true
        x_names             = abap_true
        x_company_code      = abap_true
        x_cost_element      = abap_true
        gl_account_number   = me->get_saknr( )
        chart_of_accounts   = me->get_ktopl( )
        company_code        = me->get_bukrs( )
      EXCEPTIONS
        foreign_lock        = 1
        system_fail         = 2
        OTHERS              = 3.

    CHECK sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO dummy.

    _mac_raise.

  ENDMETHOD.


  METHOD get_acct_name.
    rs_result = me->ls_acct_name.
  ENDMETHOD.


  METHOD get_bukrs.
    r_result = me->lv_bukrs.
  ENDMETHOD.


  METHOD get_ccode_data.
    rs_result = me->ls_ccode_data.
  ENDMETHOD.


  METHOD get_ccode_refacct.
    rs_result = me->ls_ccode_refacct.
  ENDMETHOD.


  METHOD get_coa_data.
    rs_result = me->ls_coa_data.
  ENDMETHOD.


  METHOD get_coa_refacct.
    rs_result = me->ls_coa_refacct.
  ENDMETHOD.


  METHOD get_keyword.
    r_result = me->lv_keyword.
  ENDMETHOD.


  METHOD get_ktopl.
    r_result = me->lv_ktopl.
  ENDMETHOD.


  METHOD get_ktopl_ccode.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    TRY.
        r_result = COND #( WHEN me->get_ktopl( )
                           THEN me->get_ktopl( )
                           ELSE NEW cl_company_code( iv_bukrs )->get_chart_of_accounts( )
                         ).
      CATCH cx_company_code.
        r_result = me->get_ktopl( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_ref.
    r_result = me->lv_ref.
  ENDMETHOD.


  METHOD get_refacct_name.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(ls_acct_name) = VALUE glaccount_name(
                            keyy = VALUE #( BASE CORRESPONDING #( me->get_coa_refacct( ) )
                                             spras = sy-langu )
                                             ).

    CALL FUNCTION 'GL_ACCT_MASTER_GET_NAME'
      CHANGING
        account_name = ls_acct_name
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.

    CHECK sy-subrc = 0.

    rs_result = CORRESPONDING #( ls_acct_name-data ).

  ENDMETHOD.


  METHOD get_refkeyword.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA(ls_keyword) = VALUE glaccount_keyword(
                        BASE CORRESPONDING #(
                          me->get_coa_refacct( ) EXCEPT spras )
                          spras = sy-langu ).

    CALL FUNCTION 'GL_ACCT_MASTER_GET_KEYWORD'
      CHANGING
        account_keyword = ls_keyword
      EXCEPTIONS
        not_existing    = 1
        OTHERS          = 2.

    r_result = COND #( WHEN sy-subrc = 0 THEN ls_keyword-schlw ).

  ENDMETHOD.


  METHOD get_saknr.
    r_result = |{ me->lv_saknr ALPHA = IN }|.
  ENDMETHOD.


  METHOD maintain.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

* check data passed by the caller
    me->check_data( ).
* set reference account
    me->set_ref( iv_ref ).
* check reference account
    DATA(x) = COND #( WHEN me->get_ref( ) THEN me->check_gl( me->get_ref( ) ) ).
* check chart of accounts
    me->check_ktopl( ).
* check company code
    me->check_bukrs( ).
* check parameter basic syntax
    me->check_maintain( ).
* set reference account in company code
    me->set_ccode_refacct( ).
* check reference account in company code
    x =  COND #( WHEN me->get_ref( ) THEN me->check_gl_ccode( me->get_ccode_refacct( ) ) ).
* set reference account in chart of accounts
    me->set_coa_refacct( ).
* check reference account in chart of accounts
    x = COND #( WHEN me->get_ref( ) THEN me->check_gl_coa(  me->get_coa_refacct( ) ) ).
* check authorization
    me->check_auth_glmast( ).

* maintain (create/update)
    r_result = SWITCH #( me->get_mode( )
                WHEN action-insert OR action-update OR action-delete OR action-block
                THEN me->_maintain( )
                ELSE THROW zcx_fi_general(
                            textid    = zcx_fi_general=>wrong_mode
                            gv_mode   = me->get_mode( )
                            gv_string = |Maintain GL Master Data|
                                         )
                       ).

  ENDMETHOD.


  METHOD read_glmast.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    ASSERT me->get_mode( ).

    me->check_data( ).

    me->dummy = COND #( WHEN me->check_read( )
                        THEN me->check_auth_glmast( )
                        ELSE THROW zcx_fi_general(
                                      textid   = zcx_fi_general=>gl_not_exist
                                      gv_saknr = me->get_saknr( )
                                                 )
                      ).

    NEW zcl_fi_glmast_api_extend(
       )->get_detail(
            EXPORTING
              ic_saknr     = me->get_saknr( )
              ic_bukrs     = me->get_bukrs( )
              ic_ktopl     = me->get_ktopl( )
            IMPORTING
              es_bapi_data = rs_result
              es_return    = DATA(ls_return)
            ).


    DATA(lt_return) = VALUE bapiret2_t(
                        ( LINES OF COND #( WHEN ls_return IS NOT INITIAL
                                           THEN VALUE #( ( ls_return ) )
                                           ELSE VALUE #( )
                                         )
                        )
                                        ).

    _mac_raise_multi lt_return.


  ENDMETHOD.


  METHOD read_gl_blocks.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    ASSERT me->get_ktopl( ) AND me->get_bukrs( ).

    me->check_ktopl( ).
    me->check_bukrs( ).
    me->check_data( ).

    me->dummy = COND #( WHEN me->check_read( )
                        THEN me->check_auth_glmast( )
                        ELSE THROW zcx_fi_general(
                                      textid   = zcx_fi_general=>gl_not_exist
                                      gv_saknr = me->get_saknr( )
                                                 )
                      ).

    rs_result =  NEW /eby/cl_pdmdgl_bapi(
                     )->get_glacct_block_flags(
                         ic_saknr  = me->get_saknr( )
                         ic_bukrs  = me->get_bukrs( )
                         ic_ktopl  = me->get_ktopl( )
                 ).

  ENDMETHOD.


  METHOD read_ska1.

    DATA(l_saknr) = COND #( WHEN is_coa_key-saknr IS INITIAL
                            THEN me->get_saknr( )
                            ELSE |{ is_coa_key-saknr ALPHA = IN }|
                          ).

    DATA(l_ktopl) = COND #( WHEN is_coa_key-ktopl IS INITIAL
                            THEN me->get_ktopl( )
                            ELSE is_coa_key-ktopl
                          ).

    SELECT SINGLE * FROM ska1
      INTO rs_result BYPASSING BUFFER
      WHERE ktopl = l_ktopl
      AND   saknr = l_saknr.

  ENDMETHOD.


  METHOD read_skb1.

    DATA(l_saknr) = COND #( WHEN is_ccode_key-saknr IS INITIAL
                            THEN me->get_saknr( )
                            ELSE |{ is_ccode_key-saknr ALPHA = IN }|
                          ).

    DATA(l_bukrs) = COND #( WHEN is_ccode_key-bukrs IS INITIAL
                            THEN me->get_bukrs( )
                            ELSE is_ccode_key-bukrs
                          ).

    SELECT SINGLE * FROM skb1
     INTO rs_result BYPASSING BUFFER
     WHERE bukrs = l_bukrs
     AND   saknr = l_saknr.

  ENDMETHOD.


  METHOD set_acct_name.
    me->ls_acct_name = is_acct_name.
  ENDMETHOD.


  METHOD set_bukrs.
    me->lv_bukrs = iv_bukrs.
  ENDMETHOD.


  METHOD set_ccode_data.
    me->ls_ccode_data = is_ccode_data.
  ENDMETHOD.


  METHOD set_ccode_refacct.

    CHECK me->get_ref( ) AND me->get_bukrs( ).

    me->ls_ccode_refacct = COND #( WHEN is_ccode_refacct IS SUPPLIED
                                   THEN is_ccode_refacct
                                   ELSE |{ me->get_bukrs( ) }{ me->get_ref( ) }|
                                 ).
  ENDMETHOD.


  METHOD set_coa_block.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    ASSERT me->get_ktopl( ).

    me->check_ktopl( ).
    me->check_data( ).

    me->dummy = SWITCH #( me->get_mode( )
                 WHEN action-update
                 THEN me->check_auth_glmast( )
                 ELSE THROW zcx_fi_general(
                             textid  = zcx_fi_general=>wrong_mode_del
                             gv_mode = me->get_mode( )
                                          )
                        ).

    me->set_coa_data( COND #( WHEN NOT me->get_coa_data( )
                              THEN VALUE #( BASE CORRESPONDING #(
                      me->read_ska1( VALUE #(
                            ktopl = me->get_ktopl( )
                            saknr = me->get_saknr( )
                                            )
                                   )
                                                 )
                      xspeb = iv_block
                                          )
                           )
                   ).

    r_result = me->_maintain( ).


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

    ASSERT me->get_ktopl( ).

    me->check_ktopl( ).
    me->check_data( ).

    me->dummy = SWITCH #( me->get_mode( )
                 WHEN action-update
                 THEN me->check_auth_glmast( )
                 ELSE THROW zcx_fi_general(
                             textid  = zcx_fi_general=>wrong_mode_del
                             gv_mode = me->get_mode( )
                                          )
                        ).

    me->set_coa_data( COND #( WHEN NOT me->get_coa_data( )
                              THEN VALUE #( BASE CORRESPONDING #(
                      me->read_ska1( VALUE #(
                            ktopl = me->get_ktopl( )
                            saknr = me->get_saknr( )
                                            )
                                   )
                                                 )
                      xloev = iv_del
                                          )
                           )
                   ).

    r_result = me->_maintain( ).

  ENDMETHOD.


  METHOD set_coa_refacct.

    CHECK me->get_ref( ) AND me->get_ktopl( ).

    me->ls_coa_refacct = COND #( WHEN is_coa_refacct IS SUPPLIED
                                 THEN is_coa_refacct
                                 ELSE |{ me->get_ktopl( ) }{ me->get_ref( ) }|
                               ).
  ENDMETHOD.


  METHOD set_keyword.
    me->lv_keyword = iv_keyword.
  ENDMETHOD.


  METHOD set_ktopl.
    me->lv_ktopl = iv_ktopl.
  ENDMETHOD.


  METHOD set_ref.
    me->lv_ref = |{ iv_ref ALPHA = IN }|.
  ENDMETHOD.


  METHOD set_saknr.
    me->lv_saknr = |{ iv_saknr ALPHA = IN }|.
  ENDMETHOD.


  METHOD zif_masterdata~maintain.
    r_result = me->maintain( iv_ref ).
  ENDMETHOD.


  METHOD zif_masterdata~read_data.
    es_data = me->read_glmast( ).
  ENDMETHOD.


  METHOD _acct_name_mapper.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    rt_result = VALUE #(
                 ( keyy = VALUE #( ktopl = me->get_ktopl_ccode( me->get_bukrs( ) )
                                   saknr = me->get_saknr( )
                                   spras = sy-langu
                                 )
                   data = COND #( LET ls_refacct_name = me->get_refacct_name( ) IN
                                  WHEN me->_is_coa_byref( ) AND NOT me->get_acct_name( )
                                  THEN CORRESPONDING #( ls_refacct_name )
                                  ELSE CORRESPONDING #( me->get_acct_name( ) )
                                )
                   action = COND #( WHEN me->get_ktopl( ) AND NOT me->get_bukrs( )
                                    THEN me->get_mode( )
                                    WHEN me->get_bukrs( ) AND NOT me->get_ktopl( )
                                    THEN space           "initialising when maintained in CCode (and not COA)
                                    ELSE me->get_mode( )
                                 )
                 )
                       ).

    DELETE rt_result WHERE data IS INITIAL.

  ENDMETHOD.


  METHOD _ccode_mapper.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    CHECK me->get_bukrs( ).

    rt_result = VALUE #(                                        ##ENH_OK
                 ( keyy = VALUE #( bukrs = me->get_bukrs( )
                                   saknr = me->get_saknr( ) )
                   data = COND #( WHEN me->_is_ccode_byref( ) AND NOT me->get_ccode_data( )
                                  THEN CORRESPONDING #( me->read_skb1( CORRESPONDING #( me->get_ccode_refacct( ) ) ) )
                                  ELSE me->get_ccode_data( )
                                )
                   info  = VALUE #( erdat = sy-datum
                                    ernam = sy-uname )
                   action = me->get_mode( )
                 )
                       ).

  ENDMETHOD.


  METHOD _coa_mapper.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    rs_result = VALUE #(                                                            ##ENH_OK
                  keyy = VALUE #( ktopl = me->get_ktopl_ccode( me->get_bukrs( ) )
                                  saknr = me->get_saknr( )
                                )
                  data = COND #( WHEN me->_is_coa_byref( ) AND NOT me->get_coa_data( )
                                 THEN CORRESPONDING #( me->read_ska1( CORRESPONDING #( me->get_coa_refacct( ) ) ) )
                                 ELSE me->get_coa_data( )
                               )
                  info  = VALUE #( erdat = sy-datum
                                   ernam = sy-uname
                                   sakan = me->get_saknr( )
                                 )
                  action = COND #( WHEN me->get_ktopl( ) AND NOT me->get_bukrs( )
                                   THEN me->get_mode( )
                                   WHEN me->get_bukrs( ) AND NOT me->get_ktopl( )
                                   THEN COND #( WHEN NOT me->get_simulate( ) THEN space ELSE me->get_mode( ) )
                                   ELSE me->get_mode( )
                                 )
                       ).

  ENDMETHOD.


  METHOD _get_acct_name.
    rt_result = me->lt_name_mapped.
  ENDMETHOD.


  METHOD _get_keyword.
    rt_result = me->lt_kw_mapped.
  ENDMETHOD.


  METHOD _get_map_ccode.
    rt_result = me->lt_ccode_mapped.
  ENDMETHOD.


  METHOD _get_map_coa.
    rs_result = me->ls_coa_mapped.
  ENDMETHOD.


  METHOD _is_ccode_byref.
    r_result = xsdbool( me->get_ccode_refacct( ) ).
  ENDMETHOD.


  METHOD _is_coa_byref.
    r_result = xsdbool( me->get_coa_refacct( ) ).
  ENDMETHOD.


  METHOD _keyword_mapper.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    rt_result = VALUE #(
                  ( ktopl = me->get_ktopl( )
                    saknr = me->get_saknr( )
                    spras = sy-langu
                    schlw = COND #( WHEN me->_is_coa_byref( ) AND NOT me->get_keyword( )
                                    THEN me->get_refkeyword( )
                                    ELSE me->get_keyword( )
                                  )
                    action = COND #( WHEN me->get_ktopl( ) AND NOT me->get_bukrs( )
                                     THEN me->get_mode( )
                                     WHEN me->get_bukrs( ) AND NOT me->get_ktopl( )
                                     THEN space           "initialising when maintained in CCode (and not COA)
                                     ELSE me->get_mode( )
                                  )
                  )
                        ).

    DELETE rt_result WHERE schlw IS INITIAL.

  ENDMETHOD.


  METHOD _maintain.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    "initial checks done - now set mappings
    me->_set_map_coa( me->_coa_mapper( ) ).
    me->_set_map_ccode( me->_ccode_mapper( ) ).
    me->_set_keyword( me->_keyword_mapper( ) ).
    me->_set_acct_name( me->_acct_name_mapper( ) ).

    "and save
    r_result = me->_save( ).
    me->_commit( ).

  ENDMETHOD.


  METHOD _save.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   July 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    DATA lt_return TYPE bapireturn_t.

    DATA(lt_keyword)    = me->_get_keyword( ).

    DATA(lt_acct_names) = me->_get_acct_name( ).

    DATA(ls_acct_coa)   = me->_get_map_coa( ).

    DATA(lt_ccodes)     = me->_get_map_ccode( ).

    me->enqueue( ).

    CALL FUNCTION 'GL_ACCT_MASTER_SAVE'
      EXPORTING
        testmode           = me->get_simulate( )      " Test mode?
        no_authority_check = me->get_simulate( )      " No Auth Checks in Sim mode
      TABLES
        account_names      = lt_acct_names            " Descriptions
        account_keywords   = lt_keyword               " Keywords
        account_ccodes     = lt_ccodes                " Company Code Data
        return             = lt_return                " Messages
      CHANGING
        account_coa        = ls_acct_coa.             " Chart of Accounts Data

    IF zcl_com_bapireturn_services=>check_itab_for_success( lt_return ) = 0.
      me->_save_glenhancements(  ).
    ENDIF.

    me->dequeue( ).

    r_result = xsdbool( zcl_com_bapireturn_services=>check_itab_for_errors( lt_return ) <> 0 ).

    CHECK r_result IS INITIAL.

    _mac_raise_multi lt_return.

  ENDMETHOD.


  METHOD _set_acct_name.
    me->lt_name_mapped = it_acct_name.
  ENDMETHOD.


  METHOD _set_keyword.
    me->lt_kw_mapped = it_keyword.
  ENDMETHOD.


  METHOD _set_map_ccode.
    me->lt_ccode_mapped = it_ccode_data.
  ENDMETHOD.


  METHOD _set_map_coa.
    me->ls_coa_mapped = is_coa_data.
  ENDMETHOD.

  METHOD check_mode.

    DATA(lt_mode) = VALUE ra_mode(
                      LET s = rsmds_c_sign-including
                          o = rsmds_c_option-equal
                      IN sign  = s
                        option = o
                     ( low = action-block )
                     ( low = action-delete )
                     ( low = action-insert )
                     ( low = action-read )
                     ( low = action-update )
                                 ).

    CHECK me->get_mode( ) NOT IN lt_mode.

    MESSAGE e008 WITH |Mode '{ me->get_mode( ) }' is not allowed as a value.| INTO me->dummy.

    _mac_raise.

  ENDMETHOD.

  METHOD zif_masterdata~existencecheck.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    r_result = COND #( LET l_ktopl = me->get_ktopl( )
                           l_bukrs = me->get_bukrs( ) IN
                       WHEN l_ktopl IS INITIAL AND l_bukrs IS INITIAL
                       THEN me->check_gl( )
                       WHEN l_ktopl IS NOT INITIAL AND l_bukrs IS NOT INITIAL
                       THEN me->check_gl_coa_ccode(  )
                       WHEN l_ktopl IS NOT INITIAL
                       THEN me->check_gl_coa( VALUE #( saknr = me->get_saknr( ) ktopl = l_ktopl ) )
                       WHEN l_bukrs IS NOT INITIAL
                       THEN me->check_gl_ccode( VALUE #( saknr = me->get_saknr( ) bukrs = l_bukrs ) )
                       ELSE THROW RESUMABLE zcx_fi_general(     "Resumable because sometimes the caller may not care about the existence
                                              textid    = zcx_fi_general=>incorrect_params
                                                          )
                    ).

  ENDMETHOD.


  METHOD check_gl_coa_ccode.

    r_result = me->check_gl_coa( VALUE #( saknr = me->get_saknr( ) ktopl = me->get_ktopl( ) ) ).
    r_result = me->check_gl_ccode( VALUE #( saknr = me->get_saknr( ) bukrs = me->get_bukrs( ) ) ).

  ENDMETHOD.


  METHOD _save_glenhancements.

    CHECK NOT me->get_simulate( ).

    DATA:
      ls_gl_toes      TYPE zfi_gl_toes,
      ls_ncver_act    TYPE zfi_gl_ncver_act,
      ls_ncver_cflo   TYPE zfi_gl_ncver_cfl,
      ls_cflow_classf TYPE zfi_gl_cashflow,
      ls_finstat      TYPE zfi_gl_finstat,
      ls_finstat_note TYPE zfi_gl_finstnote,
      ls_ncver_dsc    TYPE zfi_gl_ncver_dsc.

    IF me->get_ktopl( ) = tafe_grp_coa.

      me->_get_glenh_text(
         EXPORTING
          iv_tabname = |ZFI_GL_TOES|
          iv_key     = |TOES_ACCT|
          iv_value   = me->_get_map_coa( )-data-zzs_ncver_toes-zz_toes_acct
        IMPORTING
          es_result  = ls_gl_toes
     ).

      me->_get_glenh_text(
          EXPORTING
           iv_tabname = |ZFI_GL_NCVER_ACT|
           iv_key     = |ACTVT_CODE|
           iv_value   = me->_get_map_coa( )-data-zzs_ncver_toes-zz_actvt_code
         IMPORTING
           es_result  = ls_ncver_act
      ).

      me->_get_glenh_text(
          EXPORTING
           iv_tabname = |ZFI_GL_NCVER_CFL|
           iv_key     = |CFLO_CODE|
           iv_value   = me->_get_map_coa( )-data-zzs_ncver_toes-zz_cflo_code
         IMPORTING
           es_result  = ls_ncver_cflo
      ).

      me->_get_glenh_text(
          EXPORTING
           iv_tabname = |ZFI_GL_CASHFLOW|
           iv_key     = |CF_CLASSF|
           iv_value   = me->_get_map_coa( )-data-zzs_ncver_toes-zz_cf_classf
         IMPORTING
           es_result  = ls_cflow_classf
      ).

      me->_get_glenh_text(
          EXPORTING
           iv_tabname = |ZFI_GL_FINSTAT|
           iv_key     = |FINSTAT_CODE|
           iv_value   = me->_get_map_coa( )-data-zzs_ncver_toes-zz_finstat_code
         IMPORTING
           es_result  = ls_finstat
      ).

      me->_get_glenh_text(
          EXPORTING
           iv_tabname = |ZFI_GL_FINSTNOTE|
           iv_key     = |FINSTAT_NOTE|
           iv_value   = me->_get_map_coa( )-data-zzs_ncver_toes-zz_finstat_note
         IMPORTING
           es_result  = ls_finstat_note
      ).

      me->_get_glenh_text(
          EXPORTING
           iv_tabname = |ZFI_GL_NCVER_DSC|
           iv_key     = |FS_DSCLSR|
           iv_value   = me->_get_map_coa( )-data-zzs_ncver_toes-zz_fs_dsclsr
         IMPORTING
           es_result  = ls_ncver_dsc
      ).

      CALL FUNCTION 'ZFI_UPDATE_DEC_GLMAST_SKA1'
        EXPORTING
          it_dec_ska1     = VALUE ztt_fi_gl_dec_ska1(
                             ( ktopl             = me->_get_map_coa( )-keyy-ktopl
                               saknr             = me->_get_map_coa( )-keyy-saknr
                               toes_acct         = me->_get_map_coa( )-data-zzs_ncver_toes-zz_toes_acct
                               toes_acct_text    = ls_gl_toes-toes_acct_text
                               actvt_code        = me->_get_map_coa( )-data-zzs_ncver_toes-zz_actvt_code
                               actvt_text        = ls_ncver_act-actvt_text
                               cflo_code         = me->_get_map_coa( )-data-zzs_ncver_toes-zz_cflo_code
                               cflo_text         = ls_ncver_cflo-cflo_text
                               cf_classf         = me->_get_map_coa( )-data-zzs_ncver_toes-zz_cf_classf
                               cf_classf_text    = ls_cflow_classf-cf_classf_text
                               finstat_code      = me->_get_map_coa( )-data-zzs_ncver_toes-zz_finstat_code
                               finstat_text      = ls_finstat-finstat_text
                               finstat_note      = me->_get_map_coa( )-data-zzs_ncver_toes-zz_finstat_note
                               finstat_note_text = ls_finstat_note-finstat_note_text
                               fs_dsclsr         = me->_get_map_coa( )-data-zzs_ncver_toes-zz_fs_dsclsr
                               fs_dsclsr_text    = ls_ncver_dsc-fs_dsclsr_text
                             )
                                                   )
        EXCEPTIONS
          ex_update_error = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
        me->dequeue( ).
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO dummy.
        _mac_raise.
      ENDIF.
    ENDIF.

    CHECK me->get_bukrs( ) = tafe_ccode.

    DATA(lt_ccode) = me->_get_map_ccode( ).
    DATA ls_plan_relv TYPE zfi_gl_planning.

    me->_get_glenh_text(
         EXPORTING
          iv_tabname = |ZFI_GL_PLANNING|
          iv_key     = |PLAN_RELV|
          iv_value   = lt_ccode[ 1 ]-data-zz_plan_relv
        IMPORTING
          es_result  = ls_plan_relv
     ).

    CALL FUNCTION 'ZFI_UPDATE_DEC_GLMAST_SKB1'
      EXPORTING
        it_dec_skb1     = VALUE ztt_fi_gl_dec_skb1(
                            ( bukrs          = lt_ccode[ 1 ]-keyy-bukrs
                              saknr          = lt_ccode[ 1 ]-keyy-saknr
                              plan_relv      = lt_ccode[ 1 ]-data-zz_plan_relv
                              plan_relv_text = ls_plan_relv-plan_relv_text )
                                                  )
      EXCEPTIONS
        ex_update_error = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      me->dequeue( ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO dummy.
      _mac_raise.
    ENDIF.

  ENDMETHOD.


  METHOD _get_glenh_text.

    zcl_com_tabl_upd_utility=>buffer_any_table_for_read(
    EXPORTING
      iv_table    = iv_tabname
      iv_key1     = iv_key
      iv_value1   = iv_value
    IMPORTING
      es_workarea = es_result
    EXCEPTIONS
      read_error  = 1
      OTHERS      = 2
  ).

  ENDMETHOD.

  METHOD set_versn.
    me->lv_versn = iv_versn.
  ENDMETHOD.

  METHOD set_ergsl.
    me->lv_ergsl = iv_ergsl.
  ENDMETHOD.

ENDCLASS.
