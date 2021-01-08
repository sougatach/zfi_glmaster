CLASS zcl_fi_glmast_api_extend DEFINITION
  PUBLIC
  INHERITING FROM /eby/cl_pdmdgl_bapi
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_detail REDEFINITION .

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      lv_saknr TYPE saknr,
      lv_ktopl TYPE ktopl,
      lv_bukrs TYPE bukrs.

    METHODS:
    "! <p class="shorttext synchronized" lang="en">Get Additional Data for Chart of Account</p>
      get_coa_additional_data   CHANGING cs_bapi_data TYPE /eby/pdmdgl_sbapi_data,
    "! <p class="shorttext synchronized" lang="en">Get Additional Data for Company Code</p>
      get_ccode_additional_data CHANGING cs_bapi_data TYPE /eby/pdmdgl_sbapi_data,
    "! <p class="shorttext synchronized" lang="en">Get GL Account</p>
      get_saknr RETURNING VALUE(r_result) TYPE saknr,
    "! <p class="shorttext synchronized" lang="en">Set GL Account</p>
      set_saknr IMPORTING i_saknr TYPE saknr,
    "! <p class="shorttext synchronized" lang="en">Get Chart of Account</p>
      get_ktopl RETURNING VALUE(r_result) TYPE ktopl,
    "! <p class="shorttext synchronized" lang="en">GSet Chart of Account</p>
      set_ktopl IMPORTING i_ktopl TYPE ktopl,
    "! <p class="shorttext synchronized" lang="en">Get Company Code</p>
      get_bukrs RETURNING VALUE(r_result) TYPE bukrs,
    "! <p class="shorttext synchronized" lang="en">Set Company Code</p>
      set_bukrs IMPORTING i_bukrs TYPE bukrs.

ENDCLASS.



CLASS zcl_fi_glmast_api_extend IMPLEMENTATION.


  METHOD get_detail.

*----------------------------------------------------------------------
* Author: Sougata Chatterjee (UserID 11034616)
* Date:   August 2020
* Reason: TAFE_CIME 144 - GL Account Maintenance Form and Workflow
*----------------------------------------------------------------------

    super->get_detail(
      EXPORTING
        ic_saknr     = ic_saknr
        ic_bukrs     = ic_bukrs
        ic_ktopl     = ic_ktopl
      IMPORTING
        es_bapi_data = es_bapi_data
        es_return    = es_return
   ).

    CHECK es_bapi_data IS NOT INITIAL AND
          es_return-type NA zcl_com_bapireturn_services=>gc_error_msgtypes.

    me->set_saknr( ic_saknr ).
    me->set_bukrs( ic_bukrs ).
    me->set_ktopl( ic_ktopl ).

    me->get_coa_additional_data( CHANGING cs_bapi_data = es_bapi_data ).

    me->get_ccode_additional_data( CHANGING cs_bapi_data = es_bapi_data ).


  ENDMETHOD.

  METHOD get_ccode_additional_data.

    DATA text_lines TYPE STANDARD TABLE OF tline.

    DATA(l_saknr) = me->get_saknr( ).
    DATA(l_bukrs) = me->get_bukrs( ).

    CHECK me->get_bukrs( ).

    SELECT SINGLE plan_relv
        FROM zfi_dec_skb1_pr
        INTO cs_bapi_data-ccode_data-zz_plan_relv
        WHERE bukrs = l_bukrs
        AND   saknr = l_saknr.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = CONV tdid( |0001| )
        language                = sy-langu
        name                    = CONV tdobname( |{ l_saknr ALPHA = IN }{ l_bukrs }| )
        object                  = CONV tdobject( |SKB1| )
      TABLES
        lines                   = text_lines
      EXCEPTIONS
        id                      = 1                " Text ID invalid
        language                = 2                " Invalid language
        name                    = 3                " Invalid text name
        not_found               = 4                " Text not found
        object                  = 5                " Invalid text object
        reference_check         = 6                " Reference chain interrupted
        wrong_access_to_archive = 7                " Archive handle invalid for access
        OTHERS                  = 8.

    CHECK sy-subrc = 0.

    cs_bapi_data-ccode_data-zz_glacct_ccode_text = VALUE #( text_lines[ 1 ]-tdline OPTIONAL ).

  ENDMETHOD.

  METHOD get_coa_additional_data.

    DATA(l_saknr) = me->get_saknr( ).
    DATA(l_ktopl) = me->get_ktopl( ).

    CHECK me->get_ktopl( ).

    SELECT SINGLE
        toes_acct
        finstat_code
        finstat_note
        cf_classf
        fs_dsclsr
        actvt_code
        cflo_code
    FROM zfi_dec_ska1_ncv
    INTO cs_bapi_data-coa_data-zzs_ncver_toes
    WHERE ktopl = l_ktopl
    AND   saknr = l_saknr.

  ENDMETHOD.

  METHOD get_saknr.
    r_result = me->lv_saknr.
  ENDMETHOD.

  METHOD set_saknr.
    me->lv_saknr = i_saknr.
  ENDMETHOD.

  METHOD get_ktopl.
    r_result = me->lv_ktopl.
  ENDMETHOD.

  METHOD set_ktopl.
    me->lv_ktopl = i_ktopl.
  ENDMETHOD.

  METHOD get_bukrs.
    r_result = me->lv_bukrs.
  ENDMETHOD.

  METHOD set_bukrs.
    me->lv_bukrs = i_bukrs.
  ENDMETHOD.

ENDCLASS.
