*&---------------------------------------------------------------------*
*& Report zalv_example_basic
*&---------------------------------------------------------------------*
*& Objetivo: Ejemplos para entender de como utilizar la clase ZCL_CA_ALV
*& Descripción: Ejemplo cambiando el catalogo de campos del ALV
*&---------------------------------------------------------------------*
REPORT zalv_example_basic.

TYPES: BEGIN OF ts_user,
         bname    TYPE usr02-bname,
         gltgv    TYPE usr02-gltgv,
         gltgb    TYPE usr02-gltgb,
         uflag    TYPE usr02-uflag,
         count    TYPE i,
         navigate TYPE lvc_icon,
       END OF ts_user.
TYPES: tt_user TYPE STANDARD TABLE OF ts_user.

START-OF-SELECTION.

  SELECT bname, gltgv, gltgb, uflag, 1 AS count, @icon_businav_objects AS navigate
         FROM usr02
         INTO TABLE @DATA(mt_datos).


END-OF-SELECTION.

  DATA(mo_alv) = NEW zcl_ca_alv(  ).

  mo_alv->create_alv(
    EXPORTING
      iv_program        = sy-repid
    CHANGING
      ct_data           = mt_datos
    EXCEPTIONS
      error_create_alv  = 1
      OTHERS            = 2 ).

** Opciones de layout

* Columnas optimizadas
  mo_alv->set_optimized_cols( abap_true ).

** Catalogo de campos

* Texto en el campo COUNT y que además se sumarice por dicho campo
  mo_alv->set_field_properties(  iv_field = 'COUNT' iv_all_text = 'Cont.' iv_set_aggregation = if_salv_c_aggregation=>total  ).

* Texto campo campo NAVIGATE, indicar que es un icono y que es navegable
  mo_alv->set_field_properties(  iv_field = 'NAVIGATE' iv_all_text = 'Navigate' iv_symbol = abap_true iv_cell_type = if_salv_c_cell_type=>hotspot ).

  IF sy-subrc <> 0.
    WRITE:/ 'Error crear ALV'.
  ELSE.
    mo_alv->show_alv( ).
  ENDIF.
