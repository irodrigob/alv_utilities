*&---------------------------------------------------------------------*
*& Report zalv_exmple_4_events
*&---------------------------------------------------------------------*
*& Objetivo: Ejemplos para entender como utilizar la clase ZCL_CA_ALV
*& Descripción: Ejemplo de uso de los eventos. Se toma como base el ZALV_EXAMPLE_3_FIELDCATALOG para uso con eventos
*&---------------------------------------------------------------------*
REPORT zalv_example_4_events.

CLASS lcl_alv_events DEFINITION.
  PUBLIC SECTION.

    METHODS: on_link_click FOR EVENT link_click OF zcl_ca_alv
      IMPORTING row column,
      on_user_command FOR EVENT added_function OF zcl_ca_alv
        IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_alv_events IMPLEMENTATION.

  METHOD on_link_click.
    CASE column.
      WHEN 'NAVIGATE'.
        CALL TRANSACTION 'SU01'.
    ENDCASE.

  ENDMETHOD.

  METHOD on_user_command.
    CASE e_salv_function.
      WHEN 'SE38'.
        CALL TRANSACTION 'SE38'.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

TYPES: BEGIN OF ts_user,
         bname    TYPE usr02-bname,
         gltgv    TYPE usr02-gltgv,
         gltgb    TYPE usr02-gltgb,
         uflag    TYPE usr02-uflag,
         navigate TYPE lvc_s_icon,
       END OF ts_user.
TYPES: tt_user TYPE STANDARD TABLE OF ts_user.

DATA mt_datos TYPE tt_user.
DATA mo_alv_events TYPE REF TO lcl_alv_events.



START-OF-SELECTION.

  SELECT bname, gltgv, gltgb, uflag, @icon_businav_objects AS navigate
         FROM usr02
         INTO CORRESPONDING FIELDS OF TABLE @mt_datos.

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

  IF sy-subrc <> 0.
    WRITE:/ 'Error crear ALV'.
  ELSE.
    "Opciones de layout

    "Columnas optimizadas
    mo_alv->set_optimized_cols( abap_true ).

    "Catalogo de campos

    "Texto campo campo NAVIGATE, indicar que es un icono y que es navegable
    mo_alv->set_field_properties(  iv_field = 'NAVIGATE' iv_all_text = 'Navigate' iv_symbol = abap_true iv_cell_type = if_salv_c_cell_type=>hotspot ).

    "PF-Status a medida. Copiaado del STANDARD del grupo de funciones SALV
    mo_alv->set_pfstatus( iv_pfstatus = 'STANDARD' ).

    "Eventos
    mo_alv_events = NEW lcl_alv_events( ).
    SET HANDLER mo_alv_events->on_link_click FOR mo_alv.
    SET HANDLER mo_alv_events->on_user_command FOR mo_alv.

  ENDIF.
