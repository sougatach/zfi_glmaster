"! <p class="shorttext synchronized">Master data Factory Interface</p>
INTERFACE zif_masterdata_factory
  PUBLIC .

  "! <p class="shorttext synchronized">Check Existence of a Master data object</p>
  "!
  "! @parameter iv_key1 | <p class="shorttext synchronized">Key 1 of a Master data object (Optional)</p>
  "! @parameter iv_key2 | <p class="shorttext synchronized">Key 2 of a Master data object (Optional)</p>
  "! @parameter iv_key3 | <p class="shorttext synchronized">Key 3 of a Master data object (Optional)</p>
  "! @parameter r_result | <p class="shorttext synchronized">Exist = True</p>
  "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object (Resumable)</p>
  METHODS is_exist       IMPORTING !iv_key1        TYPE any OPTIONAL
                                   !iv_key2        TYPE any OPTIONAL
                                   !iv_key3        TYPE any OPTIONAL
                         RETURNING VALUE(r_result) TYPE boole_d
                         RAISING   RESUMABLE(zcx_fi_general).

  "! <p class="shorttext synchronized">Get Master data Object</p>
  "!
  "! @parameter es_data | <p class="shorttext synchronized">Master data</p>
  "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
  METHODS get_data EXPORTING !es_data TYPE any
                   RAISING   RESUMABLE(zcx_fi_general).

  "! <p class="shorttext synchronized">Maintain Master data (CRUD)</p>
  "!
  "! @parameter iv_ref | <p class="shorttext synchronized">Reference object</p>
  "! @parameter r_result | <p class="shorttext synchronized">Result: True = Success</p>
  "! @raising zcx_fi_general | <p class="shorttext synchronized">Exception Object</p>
  METHODS maintain_data IMPORTING !iv_ref         TYPE any OPTIONAL
                        RETURNING VALUE(r_result) TYPE boole_d
                        RAISING   zcx_fi_general.
ENDINTERFACE.
