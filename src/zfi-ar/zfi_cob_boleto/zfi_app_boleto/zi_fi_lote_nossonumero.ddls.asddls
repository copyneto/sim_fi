@AbapCatalog.sqlViewName: 'ZVFINOSSONUMERO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca Nosso Numero'
define view zi_fi_lote_nossonumero
  as select from pcec
{

  key zbukr                        as Zbukr,
  key hbkid                        as Hbkid,
      hktid                        as Hktid,
      stapl                        as Stapl,
      checf                        as Checf,
      chect                        as Chect,
      fstap                        as Fstap,
      checl                        as Checl,
      stapi                        as Stapi,
      xarcp                        as Xarcp,
      xarct                        as Xarct,
      xchch                        as Xchch,
      purdt                        as Purdt,
      dummy_chkbookmng_incl_eew_ps as DummyChkbookmngInclEewPs,
      zwels                        as Zwels,
      minialertnum                 as Minialertnum,
      purchaseby                   as Purchaseby,
      createby                     as Createby,
      createdate                   as Createdate,
      createtime                   as Createtime,
      checktype                    as Checktype,
      case when ( pcec.checl < pcec.chect )
              then 'X'
              else ' ' end         as Valido

}
