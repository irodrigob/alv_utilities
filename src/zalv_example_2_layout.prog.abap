*&---------------------------------------------------------------------*
*& Report zalv_example_2_layout
*&---------------------------------------------------------------------*
*& Objetivo: Ejemplos para entender de como utilizar la clase ZCL_CA_ALV
*& Descripción: Ejemplo cambiando el layout del ALV
*&---------------------------------------------------------------------*
REPORT zalv_example_2_layout.

START-OF-SELECTION.

  SELECT bname, gltgv, gltgb, uflag
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

* Todas las funciones ALV por defecto
  mo_alv->set_alv_functions( abap_true ).

* Permitir la grabación de variantes
  mo_alv->set_manag_layout( ).


  IF sy-subrc <> 0.
    WRITE:/ 'Error crear ALV'.
  ELSE.
    mo_alv->show_alv( ).
  ENDIF.
