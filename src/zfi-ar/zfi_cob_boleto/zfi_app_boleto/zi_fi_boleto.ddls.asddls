@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Buscar Faturas - Boleto'
define root view entity ZI_FI_BOLETO
  as select from    bsid_view                 as _Fat

    inner join      Fclm_Bseg_Basic           as _Venc       on  _Venc.bukrs = _Fat.bukrs
                                                             and _Venc.belnr = _Fat.belnr
                                                             and _Venc.buzei = _Fat.buzei
                                                             and _Venc.gjahr = _Fat.gjahr

    inner join      ZI_CA_PARAM_VAL           as _Fpgto      on  _Fpgto.Modulo = 'FI-AR'
                                                             and _Fpgto.Chave1 = 'BOLETO'
                                                             and _Fpgto.Chave2 = 'FORMAPGTO'
                                                             and _Fpgto.Low    = _Fat.zlsch

    inner join      ZI_FI_BOLETO_BANCOEMPRESA as _BancoEmp   on  _BancoEmp.Empresa   = _Fat.bukrs
                                                             and _BancoEmp.Documento = _Fat.belnr
                                                             and _BancoEmp.Parcela   = _Fat.buzei
                                                             and _BancoEmp.Exercicio = _Fat.gjahr


    inner join      ZI_CA_PARAM_VAL           as _BkEmpParam on  _BkEmpParam.Modulo = 'FI-AR'
                                                             and _BkEmpParam.Chave1 = 'BOLETO'
                                                             and _BkEmpParam.Chave2 = 'BANCOEMPRESA'
                                                             and _BkEmpParam.Low    = _BancoEmp.BancoEmpresaKey

    inner join      ZI_CA_PARAM_VAL           as _TpDoc      on  _TpDoc.Modulo = 'FI-AR'
                                                             and _TpDoc.Chave1 = 'BOLETO'
                                                             and _TpDoc.Chave2 = 'TIPODOC'
                                                             and _TpDoc.Low    = _Fat.blart

    left outer join balhdr                    as _Log        on  _Log.extnumber = _Fat.xref3
                                                             and _Log.object    = 'ZFI_BOLETO_B2B'

  association to I_CompanyCodeVH   as _Emp       on  _Emp.CompanyCode = _Fat.bukrs

  association to F_Mmim_Blart      as _TpDocDesc on  _TpDocDesc.AccountingDocumentType = _Fat.blart

  association to ZI_FI_BANCOEMP_VH as _BkEmpDesc on  _BkEmpDesc.bukrs = _Fat.bukrs
                                                 and _BkEmpDesc.hbkid = _Fat.hbkid
                                                 and _BkEmpDesc.hktid = _Fat.hktid

  association to kna1              as _CliDesc   on  _CliDesc.kunnr = _Fat.kunnr

  association to ZI_SD_NFE_DOCREF  as _DocRef    on  _DocRef.SubsequentDocument         = _Fat.vbeln
                                                 and _DocRef.PrecedingDocumentCategory  = 'C'
                                                 and _DocRef.SubsequentDocumentCategory = 'M'
{


  key _Fat.bukrs                            as Empresa,
  key _Fat.belnr                            as Documento,
  key _Fat.buzei                            as Parcela,
  key _Fat.gjahr                            as Exercicio,
      _Emp.CompanyCodeName                  as EmpresaDesc,
      _Fat.blart                            as TipoDoc,
      _TpDocDesc.AccountingDocumentTypeName as TipoDocDesc,
      _Fat.kunnr                            as Cliente,
      _CliDesc.name1                        as ClienteDesc,
      _Fat.xblnr                            as Referencia,
      _Fat.waers                            as Moeda,
      @Semantics.amount.currencyCode: 'Moeda'
      _Fat.dmbtr                            as Valor,
      _Fat.hbkid                            as BancoEmpresa,
      _BkEmpDesc.text1                      as BancoEmpresaDesc,
      _Fat.zlsch                            as FormaPagto,
      _Fat.bldat                            as DataDoc,
      _Fat.budat                            as DataLanc,
      _Fat.xref3                            as NossoNumero,
      _Fat.anfbn                            as SolicLC,
      _Venc.netdt                           as Vencimento,
      _Fat.vbeln                            as Fatura,
      _DocRef.PrecedingDocument             as SalesOrder,
      _DocRef.BR_NotaFiscal                 as BR_NotaFiscal,
      _DocRef._NFActive.authdate            as AuthDate,
      _DocRef._NFActive.authtime            as AuthTime,
      _DocRef._NFActive.branch              as BusinessPlace,
      _Log.log_handle                       as LogHandle,

      case
        when _Log.msg_cnt_s is not initial and  _Log.msg_cnt_e is initial then 'X'
        else ''
      end                                   as IsIntegrated,

      case when (_Fat.xref3 = ' ') then cast( 'Boleto Disponível' as abap.char( 20 ) )
                 else cast( 'Boleto Gerado' as abap.char( 20 ) )
                 end                        as Status


}
