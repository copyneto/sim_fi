CLASS zclfi_grouping_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb.

    CLASS-METHODS get_data FOR TABLE FUNCTION zi_fi_group_reporting_2.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclfi_grouping_amdp IMPLEMENTATION.
  METHOD get_data
  BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING acdocu ztca_param_val ztca_param_mod ztca_param_par.

    lt_tab1 = select val.client, val.low, val.high from ztca_param_mod as mod
    inner join ztca_param_par as par
    on par.id_mod = mod.id_mod
    and par.chave1 = 'GROUP_REPORTING'
    AND par.chave2 = 'MANDANTES'
    INNER JOIN ztca_param_val AS val
    ON val.id_par = par.id_par
    where modulo = 'FI';

*    lt_tab2 = select t1.high as rclnt, rldnr, rdimen, ryear from acdocu as ac
*    inner join :lt_tab1 as t1 ON t1.low = ac.rclnt;

    return select
 t1.high as rclnt,
  rldnr ,
  rdimen,
  ryear,
  docnr,
  docln ,
  rrcty  ,
  rvers ,
  rtcur ,
  rhcur  ,
  rkcur ,
  runit   ,
  poper,
  docct ,
  rcomp,
  rbunit ,
  ritclg  ,
  ritem ,
  rbuptr  ,
  rcongr ,
  robukrs,
  sityp  ,
  subit ,
  plevl  ,
  rpflg   ,
  rtflg  ,
  docty ,
  yracq  ,
  pracq ,
  coicu ,
  uppcu ,
  tsl   ,
  hsl    ,
  ksl    ,
  msl   ,
  sgtxt  from acdocu as ac
    inner join :lt_tab1 as t1 ON t1.low = ac.rclnt;

  endmethod.

ENDCLASS.
