FUNCTION zfmfi_bombeio_estorna_solicita.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_KEY) TYPE  FDM_T_BSEG_KEY
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  DATA(lo_event) = zclfi_cockpit_bombeio_event=>get_instance( ).

  lo_event->event_estornar_solicitacao( EXPORTING it_key    = it_key
                                        IMPORTING et_return = et_return ).

ENDFUNCTION.
