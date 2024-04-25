@EndUserText.label: 'View basic - Client'
@AccessControl.authorizationCheck: #NOT_REQUIRED

define table function ZI_FI_GROUP_REPORTING_2
returns
{
  rclnt   : mandt;
  rldnr   : rldnr;
  rdimen  : fc_dimen;
  ryear   : gjahr;
  docnr   : belnr_d;
  docln   : docln6;
  rrcty   : fc_rrcty;
  rvers   : fc_rvers;
  rtcur   : rtcur;
  rhcur   : lcurr;
  rkcur   : gcurr;
  runit   : meins;
  poper   : poper;
  docct   : docct;
  rcomp   : rcomp_d;
  rbunit  : fc_bunit;
  ritclg  : fc_itclg;
  ritem   : fc_item;
  rbuptr  : fc_buptr;
  rcongr  : fc_congr;
  robukrs : obukr;
  sityp   : fc_sityp;
  subit   : fc_sitem;
  plevl   : fc_plevl;
  rpflg   : fc_rpflg;
  rtflg   : fc_rtflg;
  docty   : fc_docty;
  yracq   : fc_ryacq;
  pracq   : rpacq;
  coicu   : fc_coicu;
  uppcu   : fc_uppcu;
  tsl     : vtcur12;
  hsl     : vlcur12;
  ksl     : vgcur12;
  msl     : fincs_quan;
  sgtxt   : sgtxt;
}
implemented by method
  ZCLFI_GROUPING_AMDP=>GET_DATA;
