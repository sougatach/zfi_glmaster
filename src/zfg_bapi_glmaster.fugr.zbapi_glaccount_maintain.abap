FUNCTION ZBAPI_GLACCOUNT_MAINTAIN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SAKNR) TYPE  SKA1-SAKNR
*"     VALUE(IV_WITH_REF) TYPE  SKA1-SAKNR OPTIONAL
*"     VALUE(IV_KTOPL) TYPE  SKA1-KTOPL DEFAULT '2000'
*"     VALUE(IV_BUKRS) TYPE  SKB1-BUKRS DEFAULT '1020'
*"     VALUE(IS_COA_DATA) TYPE  ZBAPI_S_COA_DATA OPTIONAL
*"     VALUE(IS_CCODE_DATA) TYPE  ZBAPI_S_CCODE_DATA OPTIONAL
*"     VALUE(IS_ACCT_NAME) TYPE  GLACCOUNT_NAME_DATA OPTIONAL
*"     VALUE(IV_KEYWORD) TYPE  SKAS-SCHLW OPTIONAL
*"     VALUE(IV_MODE) TYPE  ZBAPI_S_ACTION-ACTION DEFAULT 'I'
*"     VALUE(IV_SIMULATE) TYPE  BAPI1030_GEN-TESTRUN DEFAULT ABAP_TRUE
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

  TRY.
      DATA(ok) = zcl_fico_masterdata_factory=>create(
                   iv_saknr      = iv_saknr
                   iv_ktopl      = iv_ktopl
                   iv_bukrs      = iv_bukrs
                   is_coa_data   = CORRESPONDING #( is_coa_data )
                   is_ccode_data = CORRESPONDING #( is_ccode_data )
                   is_acct_name  = is_acct_name
                   iv_keyword    = iv_keyword
                   iv_mode       = iv_mode
                   iv_commit     = abap_false           "BAPI must not commit in itself
                   iv_simulate   = iv_simulate
                       )->zif_masterdata_factory~maintain_data(
                            iv_ref = iv_with_ref
                 ).

    CATCH zcx_fi_general INTO DATA(lo_exception).
 "handle exceptions, fill bapireturn etc.
      return[] = lo_exception->get_messages( ).
      CHECK return[] IS INITIAL.
      MESSAGE e008 WITH CONV bapi_msg( lo_exception->get_text( ) ) INTO DATA(dummy).
      _mac_add_return return[].
      RETURN.
  ENDTRY.

    CHECK ok = abap_true.
    MESSAGE s512(fh) INTO dummy.
    _mac_add_sy_return return[].

ENDFUNCTION.
