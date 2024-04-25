@EndUserText.label: 'Adiantam. Bombeio - Ajustar Solicitação'

define abstract entity ZI_FI_BOMBEIO_POPUP_AJUSTE_SOL
{

      @UI.lineItem  :        [{ position: 10 }]
      @UI.identification:    [{ position: 10 }]

      @Consumption.valueHelpDefinition: [{ entity: {name: 'ZI_FI_VH_BANK_ID', element: 'hbkid' } }]

      @EndUserText.label: 'Banco da Empresa'
  key HouseBank     : hbkid;

      @UI.lineItem  :        [{ position: 20 }]
      @UI.identification:    [{ position: 20 }]

      @Consumption.valueHelpDefinition: [{ entity: {name: 'ZI_FI_VH_PYMT_METH', element: 'zlsch' } }]

      @EndUserText.label: 'Forma de Pagamento'
  key PaymentMethod : acpi_zlsch;

      @UI.lineItem  :        [{ position: 30 }]
      @UI.identification:    [{ position: 30 }]

      @EndUserText.label: 'Novo Vencimento'
  key NetDueDateNew : acpi_zfbdt;

      @UI.lineItem  :        [{ position: 40 }]
      @UI.identification:    [{ position: 40 }]

      @EndUserText.label: 'Novo Montante'
  key AmountNew     : abap.dec(23,2);
}
