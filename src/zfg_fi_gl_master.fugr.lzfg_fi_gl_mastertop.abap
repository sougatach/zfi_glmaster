FUNCTION-POOL ZFG_FI_GL_MASTER.             "MESSAGE-ID ..

* INCLUDE LZFG_FI_GL_MASTERD...              " Local class definition

  TYPES:
    BEGIN OF t_node_acct_info,
      node    TYPE fibs_bs_node_id,
      info    TYPE rfaccinf,
      flg_mod TYPE c,
    END OF t_node_acct_info,
    t_node_acct_info_tab TYPE t_node_acct_info OCCURS 50.

  DATA l_refresh         TYPE c.
  DATA l_posid           TYPE fibs_acct_id.
  DATA ls_pos_info       TYPE fibs_pos_info.
  DATA node_tab          TYPE STANDARD TABLE OF snodetext WITH HEADER LINE.
  DATA lt_node_acct      TYPE STANDARD TABLE OF  fibs_bs_node_id WITH HEADER LINE.
  DATA lt_node_acct_info TYPE t_node_acct_info_tab WITH HEADER LINE.
  DATA del_node_acct_info_tab TYPE t_node_acct_info_tab WITH HEADER LINE.
  DATA lt_fagl_011qt     TYPE STANDARD TABLE OF fagl_011qt.
  DATA lt_fagl_011tc     TYPE STANDARD TABLE OF fagl_011tc.
  DATA l_intersection    TYPE c.
  DATA:
    flg_langu_maint       TYPE c VALUE ' ',
    flg_sylangu_not_found TYPE c,
    maint_langu           TYPE sylangu,
    l_mem                 TYPE string VALUE 'TAFE_GL'.
