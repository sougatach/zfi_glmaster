*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_test DEFINITION DEFERRED.
CLASS zcl_fi_glmast_api DEFINITION LOCAL FRIENDS lcl_unit_test.

CLASS lcl_unit_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Unit_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_FI_GLMAST_FACTORY
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_fi_glmast_api.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    "METHODS: read_comitem FOR TESTING.
    "METHODS: maintain_comitem FOR TESTING.
    METHODS maintain_costelem FOR TESTING.
    METHODS fsv FOR TESTING.
ENDCLASS.       "lcl_Unit_Test


CLASS lcl_unit_test IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.

    DATA iv_saknr TYPE saknr VALUE '569999'.
    DATA iv_mode TYPE glaccount_action VALUE 'U'. "IS INITIAL.
    DATA iv_ktopl TYPE ktopl VALUE '1020'.

    TRY.
        CREATE OBJECT f_cut
          EXPORTING
            iv_saknr = iv_saknr.
        "iv_ktopl = iv_ktopl
*            iv_mode  = iv_mode.
      CATCH zcx_fi_general INTO DATA(lx_exception). " General Exception Object for FI
        cl_abap_unit_assert=>abort(
              msg    = lx_exception->get_text( )          " Description
          ).
    ENDTRY.

    "for testing only -- to test create cost element with an existing GL
    "f_cut->set_mode( f_cut->action-insert ).

  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


*  METHOD read_comitem.
*
*    DATA iv_fikrs TYPE fikrs.
*    DATA iv_comitem TYPE fm_fipex.
*    DATA es_text TYPE fmcit.
*    DATA es_hier TYPE fmhici.
*    DATA rs_result TYPE fmci.
*
*    TRY.
*        rs_result = f_cut->read_comitem(
**     EXPORTING
**       IV_FIKRS = iv_Fikrs
**       IV_COMITEM = iv_Comitem
*         IMPORTING
*           es_text = es_text
*           es_hier = es_hier
*        ).
*      CATCH zcx_fi_general INTO DATA(lx_exception).
*        "handle exception
*        cl_abap_unit_assert=>abort(
*            msg    = lx_exception->get_text( )          " Description
*        ).
*    ENDTRY.
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = xsdbool( es_text IS NOT INITIAL AND es_hier IS NOT INITIAL AND rs_result IS NOT INITIAL )
*      exp   = abap_true "et_Texts          "<--- please adapt expected value
*    " msg   = 'Testing value et_Texts'
**     level =
*    ).
*
*  ENDMETHOD.




*  METHOD maintain_comitem.
*
*    TRY.
*        f_cut->maintain_comitem(
*          EXPORTING
**         iv_fikrs    = tafe_fm_area     " FM Area
*            iv_ref      = '560007' "'562999'                 " Create with Reference
*            iv_simulate = abap_false        " Test mode (Create only)
**         is_fmci     =                  " Commitment Item Master Data
**         is_fmcit    =                  " Commitment Item Text
**         is_fmzubsp  =                  " Assign Budget Structure Element
*          RECEIVING
*            r_result    = DATA(ok)                 " True = Successfully maintained
*        ).
*      CATCH zcx_fi_general INTO DATA(lx_exception). " General Exception Object for FI
*        cl_abap_unit_assert=>abort(
*              msg    = lx_exception->get_text( )          " Description
*          ).
*    ENDTRY.
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = ok
*      exp   = abap_true "et_Texts          "<--- please adapt expected value
*    " msg   = 'Testing value et_Texts'
**     level =
*    ).
*
*  ENDMETHOD.

  METHOD maintain_costelem.

*    TRY.
*        f_cut->maintain_costelem(
*       EXPORTING
**         iv_coarea   = tafe_co_area     " Controlling Area
**         iv_class    = '1'              " Cost Element Classification (Primary/Secondary)
*         iv_simulate = abap_false        " Test mode
*         iv_ref      = '520718'                 " Create with Reference
**         is_costelem =                  " Cost Element details
*          RECEIVING
*            r_result    = DATA(ok)                 " True = Successfully maintained
*        ).
*      CATCH zcx_fi_general INTO DATA(lx_exception). " General Exception Object for FI
*        cl_abap_unit_assert=>abort(
*              msg    = lx_exception->get_text( )          " Description
*          ).
*    ENDTRY.
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = ok
*      exp   = abap_true "et_Texts          "<--- please adapt expected value
*    " msg   = 'Testing value et_Texts'
**     level =
*    ).
  ENDMETHOD.

  METHOD fsv.

    DATA l_version TYPE versn_011 VALUE '1020'.
    DATA l_ergsl   TYPE ergsl VALUE '137'.
    DATA l_ok TYPE c.
break 11034616.
*    TRY.
*      zcl_fi_glmast_factory=>create(
*        EXPORTING
*          iv_saknr      = |569999|
*          iv_ktopl      = '1000' "tafe_group_coa
**          iv_bukrs      = tafe_comp_code
**          is_coa_data   =
**          is_ccode_data =
**          is_acct_name  =
**          iv_keyword    =
**          iv_mode       = |U| "gl_action-insert
**          iv_simulate   = abap_false
**          iv_wait       = abap_true
**        RECEIVING
**          r_result      =
*      )->assign_fsv(
*        EXPORTING
**          iv_versn = tafe_fsv-tafe_fsv
*          iv_ergsl = l_ergsl
*        RECEIVING
*          r_result = l_ok
*      ).
**      CATCH zcx_fi_general. " General Exception Object for FI.
*      CATCH zcx_fi_general INTO DATA(lx_exception). " General Exception Object for FI
*        cl_abap_unit_assert=>abort(
*          msg    = lx_exception->get_text( )          " Description
*      ).
*    ENDTRY.
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = l_ok
*      exp   = abap_true "et_Texts          "<--- please adapt expected value
*    " msg   = 'Testing value et_Texts'
**     level =
*    ).
  ENDMETHOD.

ENDCLASS.
