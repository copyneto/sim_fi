FUNCTION-POOL zfgdms_link_rbkp.             "MESSAGE-ID ..

* INCLUDE LZFGDMS_LINK_RBKPD...              " Local class definition

TABLES:   rbkp.

TYPES: BEGIN OF ty_rbkp_dms.
TYPES:   belnr TYPE rbkp-belnr,
         gjahr TYPE rbkp-gjahr.
         INCLUDE STRUCTURE dms_drad_badi_work.

TYPES: END OF ty_rbkp_dms,
ty_t_rbkp_dms TYPE TABLE OF ty_rbkp_dms.

DATA: gt_rbkp_dms  TYPE ty_t_rbkp_dms,
      gs_link_rbkp TYPE ty_rbkp_dms.
DATA:     gv_link_rbkp_copied.           "copy flag

CONTROLS: tc_link_rbkp2 TYPE TABLEVIEW USING SCREEN 9000.

DATA: gv_val           TYPE char10 VALUE 'RBKP',
      gv_activity_1500 TYPE i,
      gv_tab_mark      TYPE flag,
      gv_ucomm         TYPE sy-ucomm.
