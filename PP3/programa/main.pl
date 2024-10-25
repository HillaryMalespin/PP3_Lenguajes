% Incluir las funciones
:- [funcs, auxiliares].

% Programa en Prolog para mostrar menús interactivos

% Predicado principal que inicia el menú principal
menu_principal :-
    write(' ____________________________'), nl,
    write('|                            |'), nl,
    write('|      Menu Principal        |'), nl,
    write('|____________________________|'), nl,
    nl,
    write('1. Opciones Operativas'), nl,
    write('2. Salir'), nl,
    read(Opcion),
    menu_principal(Opcion).

% Predicado que maneja las opciones del menú principal
menu_principal(1) :-
    menu_operativas.
menu_principal(2) :-
    write('Saliendo del programa...'), nl.
menu_principal(_) :-
    write('Opcion  invalida, intente de nuevo.'), nl,
    menu_principal.

% Predicado que muestra el submenú de Opciones Operativas
menu_operativas :-
    write(' ____________________________'), nl,
    write('|                            |'), nl,
    write('|   Opciones Operativas      |'), nl,
    write('|____________________________|'), nl,
    nl,
    write('1. Agregar hechos'), nl,
    write('2. Consulta destino'), nl,
    write('3. Actividades por tipo'), nl,
    write('4. Consulta por precio'), nl,
    write('5. Generar itinerario por monto'), nl,
    write('6. Generar itinerario por dias'), nl,
    write('7. Recomendar por frase'), nl,
    write('8. Estadisticas'), nl,
    write('9. Volver al menu principal'), nl,
    read(Opcion),
    menu_operativas(Opcion).

% Predicado que maneja las opciones del submenú de Opciones Operativas
menu_operativas(1) :-
    menu_agregar_hechos.
menu_operativas(2) :-
    write('Funcion para consultar destino.'), nl,
    menu_operativas.
menu_operativas(3) :-
    actividades_por_tipo_menu.
menu_operativas(4) :-
    consulta_por_precio,
    menu_operativas.
menu_operativas(5) :-
    write('Funcion para generar itinerario por monto.'), nl,
    menu_operativas.
menu_operativas(6) :-
    write('Funcion para generar itinerario por dias.'), nl,
    menu_operativas.
menu_operativas(7) :-
    write('Funcion para recomendar por frase.'), nl,
    menu_operativas.
menu_operativas(8) :-
    write('Funcion para mostrar estadisticas.'), nl,
    menu_operativas.
menu_operativas(9) :-
    menu_principal.
menu_operativas(_) :-
    write('Opcion invalida, intente de nuevo.'), nl,
    menu_operativas.

% Submenú Agregar Hechos
menu_agregar_hechos :-
    write(' ____________________________'), nl,
    write('|                            |'), nl,
    write('|       Agregar Hechos       |'), nl,
    write('|____________________________|'), nl,
    nl,
    write('1. Agregar destino'), nl,
    write('2. Agregar actividad'), nl,
    write('3. Agregar relacion destino_actividad'), nl,
    write('4. Volver al menu opciones operativas'), nl,
    read(Opcion),
    menu_agregar_hechos(Opcion).

% Manejo de opciones para el submenú Agregar Hechos
menu_agregar_hechos(1) :-
    solicitar_nombre_destino(Nombre),
    solicitar_descripcion_destino(Descripcion),
    registrar_destino(Nombre, Descripcion),
    menu_agregar_hechos.
menu_agregar_hechos(2) :-
    solicitar_nombre_actividad(Nombre),
    solicitar_costo(Costo),
    solicitar_duracion(Duracion),
    solicitar_descripcion_actividad(Descripcion),
    solicitar_tipos(Tipos),
    registrar_actividad(Nombre, Costo, Duracion, Descripcion, Tipos),
    menu_agregar_hechos.
menu_agregar_hechos(3) :-
    solicitar_nombre_destino(Destino),
    solicitar_nombre_actividad(Actividad),
    registrar_asociacion(Destino, Actividad),
    menu_agregar_hechos.
menu_agregar_hechos(4) :-
    menu_operativas.
menu_agregar_hechos(_) :-
    write('Opcion invalida, intente de nuevo.'), nl,
    menu_agregar_hechos.

% Menú para actividades por tipo
actividades_por_tipo_menu :-
    writeln('Ingrese el tipo de actividad:'),
    read(Tipo),
    actividades_por_tipo(Tipo),  % Llamar al predicado con el tipo ingresado
    menu_operativas.

:- menu_principal.