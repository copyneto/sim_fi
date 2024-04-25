@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - FIDC'
define root view entity ZI_FI_FIDC
  as select from    ztfi_fidc_contr          as _main

    left outer join ZI_FI_FIDC_FATURAS_DADOS as _Rem on  _main.bukrs = _Rem.bukrs
                                                     and _main.belnr = _Rem.belnr
                                                     and _main.gjahr = _Rem.gjahr
                                                     and _main.buzei = _Rem.buzei

  association [1..1] to I_Customer      as _customer on _customer.Customer = $projection.kunnr
  association [1..1] to I_CompanyCodeVH as _company  on _company.CompanyCode = $projection.Bukrs



{
  key _main.bukrs                                  as Bukrs,
  key _main.belnr                                  as Belnr,
  key _main.gjahr                                  as Gjahr,
  key _main.buzei                                  as Buzei,
      _Rem.kunnr,
      _company[ Language = $session.system_language ].CompanyCodeName,
      _main.nomearqremessa                         as NomeArqRem,
      _main.nossonumero                            as Nossonumero,
      _main.status                                 as Status,
      _main.data_remessa                           as DataRemessa,
      _main.hora_remessa                           as HoraRemessa,
      _main.data_retorno                           as DataRetorno,
      _main.hora_retorno                           as HoraRetorno,
      case when _main.titulo_aceito = 'X' then 'Aceito'
               else
                 ( case _main.motivo_recusa when '' then 'Aguardando retorno'
                        else 'Rejeitado' end ) end as Aceito,
      _main.titulo_cob                             as TituloCob,
      //      _main.titulo_buzei                           as ItemCob,
      _main.titulo_gjahr                           as GjahrCob,
      _main.titulo_comp                            as DocComp,
      _main.motivo_recusa                          as MotRecusa,

      _main.dt_recompra                            as DataRecompra,
      @Semantics.amount.currencyCode: 'Moeda'
      _main.valor_cessao                           as ValorCessao,
      @Semantics.amount.currencyCode: 'Moeda'
      _main.taxa_cessao                            as TaxaCessao,
      @Semantics.amount.currencyCode: 'Moeda'
      _main.valor_lanc_desc                        as ValorDesco,
      _main.titulo_bukrs                           as TituloBukrs,
      case when _main.tipo_status = 'E' then 1
                              else 3 end           as criticality,
      _main.tipo_status                            as TipoStatus,

      _Rem.kkber                                   as Areacredito,
      _Rem.gsber                                   as Divisao,
      _Rem.xblnr                                   as Nf,
      _Rem.Chave                                   as Chave,
      _Rem.waers                                   as Moeda,
      @Semantics.amount.currencyCode: 'Moeda'
      _Rem.wrbtr                                   as Valor,
      _Rem.budat                                   as DataEmissaoNF,
      cast( _Rem.CNPJEmissor as abap.char( 14 ) )  as CNPJEmissor,
      _Rem.NomeCliente,
      cast( _Rem.CNPJCliente as abap.char( 16 ) )  as CNPJCliente,
      _Rem.TELEFONE1,
      _Rem.TELEFONE2,
      _Rem.DocNum,
      _Rem.EndCliente,
      _Rem.CompEnd,
      _Rem.Bairro,
      _Rem.Cidade,
      _Rem.UF,
      _Rem.CEP,
      @Semantics.amount.currencyCode: 'Moeda'
      _Rem.ValorNF,
      _Rem.Vencimento,
      _Rem.Email1,
      _Rem.Email2


}
