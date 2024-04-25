FUNCTION zfmfi_bombeio_ajustar_solicita.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_KEY) TYPE  FDM_T_BSEG_KEY
*"     VALUE(IS_POPUP) TYPE  ZI_FI_BOMBEIO_POPUP_AJUSTE_SOL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  DATA(lo_event) = zclfi_cockpit_bombeio_event=>get_instance( ).

  lo_event->event_ajustar_solicitacao( EXPORTING it_key    = it_key
                                                 is_popup  = is_popup
                                       IMPORTING et_return = et_return ).

ENDFUNCTION.
