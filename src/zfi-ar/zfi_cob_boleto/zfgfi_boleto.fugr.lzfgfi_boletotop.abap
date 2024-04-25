FUNCTION-POOL zfgfi_boleto.                 "MESSAGE-ID ..

* INCLUDE LZFGFI_BOLETOD...                  " Local class definition

CONSTANTS:
  "! Interface CPI
  BEGIN OF gc_cpi,
    processo    TYPE ze_processo   VALUE 'ZFI_B2B_INTEGRACAO_BOLETO' ##NO_TEXT,
    method_post TYPE ze_method_api VALUE 'POST' ##NO_TEXT,
  END OF gc_cpi,

  "! Classe de mensagem
  gc_message_id    TYPE t100-arbgb       VALUE 'ZFI_BOLETO_INTEG' ##NO_TEXT,
  "! ID do log (objeto)
  gc_log_object    TYPE balhdr-object    VALUE 'ZFI_BOLETO_B2B' ##NO_TEXT,
  "! ID do log (sub-objeto)
  gc_log_subobject TYPE balhdr-subobject VALUE 'INTERFACE' ##NO_TEXT.
