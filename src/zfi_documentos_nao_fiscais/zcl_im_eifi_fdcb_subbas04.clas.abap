class ZCL_IM_EIFI_FDCB_SUBBAS04 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_FDCB_SUBBAS04 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_EIFI_FDCB_SUBBAS04 IMPLEMENTATION.


  METHOD if_ex_badi_fdcb_subbas04~get_data_from_screen_object.
*   fill export parameters from interface attributes
    ex_invfo = me->if_ex_badi_fdcb_subbas04~invfo.
  ENDMETHOD.


  METHOD if_ex_badi_fdcb_subbas04~put_data_to_screen_object.
*   fill interface attributes from importing paramters
    me->if_ex_badi_fdcb_subbas04~invfo = im_invfo.
  ENDMETHOD.
ENDCLASS.
