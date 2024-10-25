% main.pl

:- [base_conocimiento].  % Cargar la base de conocimiento
:- [funcs].             % Cargar las funciones

% Menú principal
menu :-
    writeln('Bienvenido al Asistente de Planificación de Viajes'),
    writeln('Seleccione una opción:'),
    writeln('1. Agregar hechos'),
    writeln('2. Consulta destino'),
    writeln('3. Actividades por tipo'),
    writeln('4. Consulta por precio'),
    writeln('5. Generar itinerario por monto'),
    writeln('6. Generar itinerario por días'),
    writeln('7. Recomendar por frase'),
    writeln('8. Estadísticas'),
    writeln('9. Salir'),
    read(Opcion),
    procesar_opcion(Opcion).

% Procesar la opción seleccionada
procesar_opcion(1) :- writeln('agregar_hechos.').
procesar_opcion(2) :- writeln('consulta_destino.').
procesar_opcion(3) :- actividades_por_tipo_menu.
procesar_opcion(4) :- writeln('consulta_por_precio.').
procesar_opcion(5) :- writeln(' generar_itinerario_por_monto.').
procesar_opcion(6) :- writeln(' generar_itinerario_por_dias.').
procesar_opcion(7) :- writeln(' recomendar_por_frase.').
procesar_opcion(8) :- writeln(' estadisticas.').
procesar_opcion(9) :- writeln('Saliendo del sistema.'), halt.
procesar_opcion(_) :- writeln('Opción no válida, intente de nuevo.'), menu.

% Menú para actividades por tipo
actividades_por_tipo_menu :-
    writeln('Ingrese el tipo de actividad:'),
    read(Tipo),
    actividades_por_tipo(Tipo).  % Llamar al predicado con el tipo ingresado

% Iniciar el menú
:- menu.
