*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF ty_refaccts,
    glcoa_grp  TYPE saknr,
    glcoa_oper TYPE saknr,
    comitem    TYPE fm_fipex,
    glccode    TYPE saknr,
    glfsv      TYPE saknr,
    costelem   TYPE kstar,
  END OF ty_refaccts,

  BEGIN OF ty_isr_groups,
    sectionnum TYPE qisrdfieldindex,
    fieldname  TYPE qisrdfieldname,
  END OF ty_isr_groups,
  tty_isr_group TYPE SORTED TABLE OF ty_isr_groups WITH UNIQUE KEY sectionnum fieldname,
  tty_ucomm     TYPE RANGE OF syucomm,
  tty_ebs_gl    TYPE STANDARD TABLE OF zfic_ebs_glaccnt WITH DEFAULT KEY.
