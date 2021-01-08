*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF ty_obj_refs,
    saknr TYPE saknr,
    kstar TYPE kstar,
    fipex type fm_fipex,
    versn type versn_011,
    ref   TYPE REF TO zcl_fico_masterdata,
  END OF ty_obj_refs,
  tty_obj_refs TYPE STANDARD TABLE OF ty_obj_refs.
