*&---------------------------------------------------------------------*
*& Include          ZFII_VAL_LANCMTNO
*&---------------------------------------------------------------------*

  CONSTANTS:
    BEGIN OF gc_bukrs,
      2000 TYPE bseg-bukrs VALUE '2000',
      2500 TYPE bseg-bukrs VALUE '2500',
      3000 TYPE bseg-bukrs VALUE '3000',
      3500 TYPE bseg-bukrs VALUE '3500',
      4000 TYPE bseg-bukrs VALUE '4000',
      4500 TYPE bseg-bukrs VALUE '4500',
      5000 TYPE bseg-bukrs VALUE '5000',
      5500 TYPE bseg-bukrs VALUE '5500',
    END OF gc_bukrs,

    BEGIN OF gc_val,
      2001 TYPE c LENGTH 4 VALUE '2001',
      2501 TYPE c LENGTH 4 VALUE '2501',
      3001 TYPE c LENGTH 4 VALUE '3001',
      3501 TYPE c LENGTH 4 VALUE '3501',
      4001 TYPE c LENGTH 4 VALUE '4001',
      4501 TYPE c LENGTH 4 VALUE '4501',
      5001 TYPE c LENGTH 4 VALUE '5001',
      5501 TYPE c LENGTH 4 VALUE '5501',
    END OF gc_val.

  IF bseg-gsber IS INITIAL AND bseg-koart = 'A' AND bseg-anln1(06) = 'INTERN'.

    IF bseg-bukrs = gc_bukrs-2000.
      bseg-gsber = gc_val-2001.
      bseg-bupla = gc_val-2001.
    ELSEIF bseg-bukrs = gc_bukrs-2500.
      bseg-gsber = gc_val-2501.
      bseg-bupla = gc_val-2501.
    ELSEIF bseg-bukrs = gc_bukrs-3000.
      bseg-gsber = gc_val-3001.
      bseg-bupla = gc_val-3001.
    ELSEIF bseg-bukrs = gc_bukrs-3500.
      bseg-gsber = gc_val-3501.
      bseg-bupla = gc_val-3501.
    ELSEIF bseg-bukrs = gc_bukrs-4000.
      bseg-gsber = gc_val-4001.
      bseg-bupla = gc_val-4001.
    ELSEIF bseg-bukrs = gc_bukrs-4500.
      bseg-gsber = gc_val-4501.
      bseg-bupla = gc_val-4501.
    ELSEIF bseg-bukrs = gc_bukrs-5000.
      bseg-gsber = gc_val-5001.
      bseg-bupla = gc_val-5001.
    ELSEIF bseg-bukrs = gc_bukrs-5500.
      bseg-gsber = gc_val-5501.
      bseg-bupla = gc_val-5501.
    ENDIF.

  ENDIF.
