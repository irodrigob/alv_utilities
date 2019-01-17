*&---------------------------------------------------------------------*
*& Report zalv_example_3_fieldcatalog
*&---------------------------------------------------------------------*
*& Objetivo: Ejemplos para entender como utilizar la clase ZCL_CA_ALV
*& Descripción: Ejemplo cambiando el catalogo de campos del ALV
*&---------------------------------------------------------------------*
REPORT zalv_example_3_fieldcatalog.

TYPES: BEGIN OF ts_user,
         bname    TYPE usr02-bname,
         gltgv    TYPE usr02-gltgv,
         gltgb    TYPE usr02-gltgb,
         uflag    TYPE usr02-uflag,
         count    TYPE i,
         navigate TYPE lvc_s_icon,
         amount   TYPE p LENGTH 13 DECIMALS 2,
         currency TYPE waers,
         quantity TYPE p LENGTH 13 DECIMALS 4,
         unit     TYPE meins,
         color    TYPE lvc_t_scol,
         celltype TYPE salv_t_int4_column,
       END OF ts_user.
TYPES: tt_user TYPE STANDARD TABLE OF ts_user.

DATA mt_datos TYPE tt_user.

START-OF-SELECTION.

  SELECT bname, gltgv, gltgb, uflag, 1 AS count, @icon_businav_objects AS navigate
         FROM usr02
         INTO CORRESPONDING FIELDS OF TABLE @mt_datos.

  LOOP AT mt_datos ASSIGNING FIELD-SYMBOL(<ls_datos>).

    IF <ls_datos>-bname = 'DEVELOPER'.
      <ls_datos>-celltype = VALUE #( ( columnname = 'BNAME' value = if_salv_c_cell_type=>hotspot  ) ).
    ENDIF.

    <ls_datos>-amount = 100 * sy-tabix.
    <ls_datos>-quantity = 250 * sy-tabix.
    IF sy-tabix < 3.
      <ls_datos>-currency = 'EUR'.
      <ls_datos>-unit = 'ST'.
      IF sy-tabix = 1.
        APPEND VALUE #( fname = '' color-col = col_total ) TO <ls_datos>-color.
      ELSE.
        APPEND VALUE #( fname = 'BNAME' color-col = col_positive ) TO <ls_datos>-color.
      ENDIF.
    ELSE.
      APPEND VALUE #( fname = 'BNAME' color-col = col_negative ) TO <ls_datos>-color.
      <ls_datos>-currency = 'ESP'.
      <ls_datos>-unit = 'KG'.
    ENDIF.

  ENDLOOP.

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
  mo_alv->set_field_properties(  iv_field = 'COUNT' iv_all_text = 'Cont.' iv_set_aggregation = if_salv_c_aggregation=>total iv_position = 1 ).

* Texto campo campo NAVIGATE, indicar que es un icono y que es navegable
  mo_alv->set_field_properties(  iv_field = 'NAVIGATE' iv_all_text = 'Navigate' iv_symbol = abap_true iv_cell_type = if_salv_c_cell_type=>hotspot ).

* Texo campo de importe y su moneda
  mo_alv->set_field_properties(  iv_field = 'AMOUNT' iv_all_text = 'Amount' iv_currency_field = 'CURRENCY' ).

* Texo campo de cantidad y su unidad de medida
  mo_alv->set_field_properties(  iv_field = 'QUANTITY' iv_all_text = 'Quantity' iv_unit_field = 'UNIT' ).

* Color para el campo de la moneda
  mo_alv->set_field_properties(  iv_field = 'CURRENCY'  is_color = VALUE #( col = col_key ) ).

* Columna donde se define el color a nivel de registro y celda
  mo_alv->set_field_color( 'COLOR' ).

* Columna donde se indicara como serán los estilos de la celda
  mo_alv->set_celltype( 'CELLTYPE' ).

  IF sy-subrc <> 0.
    WRITE:/ 'Error crear ALV'.
  ELSE.
    mo_alv->show_alv( ).
  ENDIF.
