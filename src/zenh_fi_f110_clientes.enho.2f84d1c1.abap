"Name: \PR:SAPF110S\FO:BSID_VERARBEITEN\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_FI_F110_CLIENTES.

IF BSID-XREF3 IS INITIAL AND BSID-UMSKZ <> 'R' AND BSID-ZLSPR IS INITIAL.

  BSID-ZLSPR = 'X'.

ENDIF.

ENDENHANCEMENT.
