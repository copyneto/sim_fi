*&---------------------------------------------------------------------*
*& Include zfii_group_reporting_process
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  DATA(lt_message) = NEW ZCLFI_group_reporting(      iv_rldnr  = p_rldnr
                                                                                        iv_rdimen = p_rdimen
                                                                                        iv_gjahr = p_gjahr
                                                                                        iv_rvers  = p_rvers
                                                                                        iv_poper = p_poper
                                                                                        iv_ritclg   = p_ritclg
                                                                                        iv_rbunit  = p_rbunit )->execute( IMPORTING et_acdocu = gt_acdocu ).
