*"* use this source file for your ABAP unit test classes


class lcl_Unit_Test definition for testing
  duration short
     inheriting from ZCL_FICO_MASTERDATA_FACTORY  risk level harmless
.
  PUBLIC SECTION.
    METHODS: zif_masterdata_factory~is_exist REDEFINITION,
             zif_masterdata_factory~get_data REDEFINITION,
             zif_masterdata_factory~maintain_data REDEFINITION.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Unit_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_FI_COMMITMENT_FACTORY
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
  private section.
    data:
      f_Cut type ref to zcl_Fi_Commitment_Factory.  "class under test

    class-methods: class_Setup.
    class-methods: class_Teardown.
    methods: setup.
    methods: teardown.
    methods: maintain_Data for testing.

endclass.       "lcl_Unit_Test


class lcl_Unit_Test implementation.

  method class_Setup.



  endmethod.


  method class_Teardown.



  endmethod.


  method setup.

    data iv_Fikrs type fikrs VALUE '1000'.
    data iv_Gjahr type gjahr VALUE '000'.
    data iv_Fipex type fm_Fipex VALUE '567875'.
    data is_Citemdata type zfi_S_Commitment_Data.
    data iv_Mode type glaccount_Action VALUE 'I'.
    data iv_Commit type boole_D VALUE ''.
    data iv_Simulate type boole_D VALUE 'X'.
    data iv_Wait type boole_D.

    create object f_Cut
      EXPORTING
       IV_FIKRS = iv_Fikrs
       IV_GJAHR = iv_Gjahr
        IV_FIPEX = iv_Fipex
*       IS_CITEMDATA = is_Citemdata
       IV_MODE = iv_Mode
       IV_COMMIT = iv_Commit
       IV_SIMULATE = iv_Simulate
*       IV_WAIT = iv_Wait
   .
  endmethod.


  method teardown.



  endmethod.


  method maintain_Data.

    data iv_Ref type fm_fipex VALUE '567879'.
    data r_Result type boole_D.

  try.
    r_Result = f_Cut->zif_Masterdata_Factory~maintain_Data( iv_Ref ).
  CATCH zcx_fi_general INTO DATA(lo_exception).
  ENDTRY.

    cl_Abap_Unit_Assert=>assert_Equals(
      act   = r_Result
      exp   = r_Result          "<--- please adapt expected value
    " msg   = 'Testing value r_Result'
*     level =
    ).
  endmethod.




  method zif_masterdata_factory~get_data.

  endmethod.

  method zif_masterdata_factory~is_exist.

  endmethod.


  method zif_masterdata_factory~maintain_data.

  endmethod.

endclass.
