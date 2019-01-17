# alv_utilities
Clase que permite mostrar de una mánera y rápida un listado ALV. Simplificando las tareas para poder asignar el texto a una columna, cambiar de color, etc. haciendolo posible todo en una sola llamada.

# ¿De donde nace?
Los SALV son fáciles de implementar pero debido a su arquitectura orientada a objetos algo tan sencillo como cambiar el texto de una columna requiere de varios pasos. 
A raíz de eso me dije porque no hacer algo que me permita en un solo método cambiar el texto, poner color, alinear, etc.. Sin necesidad de tener que llamar a varios métodos la vez. 

# ¿Que permite?

- Permite crear un listado ALV en un report, dynpro o popup.
- Cambiar los atributos de una campo en un solo método
- Internamente se inicializa las estructuras que permiten de una manera simple habilitar la opcion de grabar variantes
- Permite cambiar los atributos de la disposición del ALV: modo selección, títulos, etc.
- En modo dynpro permite añadir funciones o indicar un PF-Status a medida
- Permite añadir ordenaciones
- Permite añadir cabecera y pie de página aunque de manera limitada
- Si algo no permite permite obtener la instancia de la clase SALV, llamar a los métodos no implementar y volver a pasar la clase SALV a la clase utilidad.

# Requisitos

- Esta clase se creo en la versión ABAP 7.0 y funciona en versiones superior. Cualquier cambio se evita usar ABAP in-line o las nuevas sentencias disponibles en la 7.4x para ser compatible con la mayoria de sistemas
- ABAPGit
