% Incluir los hechos
:- [destino, actividad, destino_actividad].

% Registrar nuevo destino.
registrar_destino(Nombre, Descripcion) :-
    % Verificar si el destino ya existe.
    (   destino(Nombre, _) ->
        write('Error: El destino ya existe.'), nl
    ;   % Agregar el hecho en la base de conocimiento.
        assertz(destino(Nombre, Descripcion)),
        % Guardar el hecho en el archivo destino.pl.
        open('destino.pl', append, Stream),
        write(Stream, 'destino('), write(Stream, Nombre), write(Stream, ', '),
        write(Stream, '\''), write(Stream, Descripcion), write(Stream, '\''), write(Stream, ').\n'),
        close(Stream),
        write('Destino registrado con exito.'), nl
    ).

% Registrar nueva actividad.
registrar_actividad(Nombre, Costo, Duracion, Descripcion, Tipos) :-
    % Verificar si la actividad ya existe.
    (   actividad(Nombre, _, _, _, _) ->
        write('Error: La actividad ya existe.'), nl
    ;   % Agregar el hecho en la base de conocimiento.
        assertz(actividad(Nombre, Costo, Duracion, Descripcion, Tipos)),
        % Guardar el hecho en el archivo actividad.pl.
        open('actividad.pl', append, Stream),
        write(Stream, 'actividad('), write(Stream, Nombre), write(Stream, ', '),
        write(Stream, Costo), write(Stream, ', '),
        write(Stream, Duracion), write(Stream, ', '),
        write(Stream, '\''), write(Stream, Descripcion), write(Stream, '\''), write(Stream, ', '),
        write(Stream, Tipos), write(Stream, ').\n'),
        close(Stream),
        write('Actividad registrada con exito.'), nl
    ).

% Asociar actividad con destino.
registrar_asociacion(Destino, Actividad) :-
    % Verificar si la asociación ya existe.
    (   asociar_actividad(Destino, Actividad) ->
        write('Error: La asociacion ya existe.'), nl
    ;   % Verificar si tanto el destino como la actividad existen.
        destino(Destino, _),
        actividad(Actividad, _, _, _, _),
        % Agregar el hecho en la base de conocimiento.
        assertz(asociar_actividad(Destino, Actividad)),
        % Guardar el hecho en el archivo destino_actividad.pl.
        open('destino_actividad.pl', append, Stream),
        write(Stream, 'asociar_actividad('), write(Stream, Destino), write(Stream, ', '),
        write(Stream, Actividad), write(Stream, ').\n'),
        close(Stream),
        write('Asociacion registrada con exito.'), nl
    ;   write('Error: El destino o la actividad no existen.'), nl
    ).

% Consultar actividades de un destino con detalles, tiempo total y costo total.
consultar_destino(Destino) :-
    % Verificar si el destino existe.
    destino(Destino, Descripcion),
    % Encontrar todas las actividades asociadas al destino.
    findall(
        (NombreActividad, Costo, Duracion, DescripcionActividad, Tipos),
        (asociar_actividad(Destino, NombreActividad), actividad(NombreActividad, Costo, Duracion, DescripcionActividad, Tipos)),
        Actividades),
    % Mostrar información del destino.
    write('Destino: '), write(Destino), nl,
    write('Descripción: '), write(Descripcion), nl,
    write('Actividades disponibles:'), nl,
    % Mostrar detalles de cada actividad.
    mostrar_actividades(Actividades),
    % Calcular tiempo y costo total.
    calcular_totales(Actividades, TiempoTotal, CostoTotal),
    % Mostrar tiempo y costo total.
    write('Tiempo total: '), write(TiempoTotal), write(' horas'), nl,
    write('Costo total: $'), write(CostoTotal), nl.

% Mostrar los detalles de cada actividad.
mostrar_actividades([]).
mostrar_actividades([(NombreActividad, Costo, Duracion, DescripcionActividad, Tipos)|Resto]) :-
    write('  - Actividad: '), write(NombreActividad), nl,
    write('    Descripción: '), write(DescripcionActividad), nl,
    write('    Costo: $'), write(Costo), nl,
    write('    Duración: '), write(Duracion), write(' horas'), nl,
    write('    Tipos: '), write(Tipos), nl, nl,
    mostrar_actividades(Resto).

% Calcular el tiempo y costo total de todas las actividades.
calcular_totales([], 0, 0).
calcular_totales([(_, Costo, Duracion, _, _)|Resto], TiempoTotal, CostoTotal) :-
    calcular_totales(Resto, TiempoResto, CostoResto),
    TiempoTotal is TiempoResto + Duracion,
    CostoTotal is CostoResto + Costo.

% Devolver una lista con todos los destinos.
obtener_lista_destinos(Destinos) :-
    findall(Nombre, destino(Nombre, _), Destinos).

% Función para actividades por tipo
% actividades_por_tipo_menu/0
% Solicita al usuario que ingrese un tipo de actividad y muestra las actividades
% correspondientes o sugiere tipos similares si el tipo no existe.
%
% Entrada: Ninguna
% Salida: Muestra actividades por tipo o sugerencias si no se encuentra el tipo.
actividades_por_tipo_menu :-
    writeln('Ingrese el tipo de actividad:'),
    read(Tipo),  % Leer tipo de actividad ingresado por el usuario
    (   actividades_por_tipo(Tipo)  % Llamar a actividades_por_tipo con el tipo ingresado
    ->  true  % Si se encuentran actividades, no se hace nada más
    ;   sugerir_tipo(Tipo),  % Si no se encuentran, se sugieren tipos
        menu  % Regresar al menú después de sugerir
    ).

% Predicado para mostrar actividades por tipo
% actividades_por_tipo/1
% Busca y muestra todas las actividades de un tipo específico.
%
% Parámetro:
%   Tipo - El tipo de actividad que se desea consultar (átomo).
%
% Entrada: Tipo de actividad (ej. 'arte').
% Salida: Muestra actividades que coinciden con el tipo.
actividades_por_tipo(Tipo) :-
    findall((Actividad, Costo, Duracion, Descripcion, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, ListaTipos),
             member(Tipo, ListaTipos),  % Verifica si el tipo está en la lista de tipos
             asociar_actividad(Destino, Actividad)),  % Encuentra el destino asociado
            Resultados),  % Almacena resultados en la lista
    mostrar_resultados(Resultados).  % Muestra los resultados encontrados

% Predicado auxiliar para mostrar los resultados
% mostrar_resultados/1
% Muestra las actividades encontradas o un mensaje si no hay actividades.
%
% Parámetro:
%   Resultados - Lista de actividades encontradas (lista de tuplas).
%
% Entrada: Resultados de actividades.
% Salida: Mensajes que indican las actividades encontradas o que no se encontraron.
mostrar_resultados([]) :-  % Caso base: no hay resultados
    writeln('No se encontraron actividades de este tipo.').  % Mensaje cuando no hay actividades
mostrar_resultados(Resultados) :-  % Caso cuando hay resultados
    writeln('Actividades encontradas:'),  % Mensaje de actividades encontradas
    forall(member((Actividad, Costo, Duracion, Descripcion, Destino), Resultados),
           format('Actividad: ~w, Costo: ~d, Duración: ~d días, Descripción: ~s, Destino: ~w~n',
                  [Actividad, Costo, Duracion, Descripcion, Destino])).  % Formato de salida para cada actividad

% Sugerir tipo similar
% sugerir_tipo/1
% Sugerir tipos de actividad similares si el tipo ingresado no existe en la base de conocimiento.
%
% Parámetro:
%   Tipo - El tipo de actividad ingresado por el usuario (átomo).
%
% Entrada: Tipo de actividad (ej. 'ciencia').
% Salida: Mensaje que sugiere tipos disponibles si no se encuentra el tipo.
sugerir_tipo(Tipo) :-
    findall(T, (actividad(_, _, _, _, ListaTipos), member(T, ListaTipos)), Tipos),  % Encuentra todos los tipos
    sort(Tipos, TiposUnicos),  % Ordena y elimina duplicados
    sugerir_similar(Tipo, TiposUnicos).  % Llama a la función para sugerir tipos similares

% Función auxiliar para mostrar sugerencias
% sugerir_similar/2
% Muestra sugerencias de tipos similares basadas en la entrada del usuario.
%
% Parámetros:
%   Tipo - El tipo de actividad ingresado por el usuario (átomo).
%   Tipos - Lista de tipos de actividad disponibles (lista de átomos).
%
% Entrada: Tipo de actividad y lista de tipos disponibles.
% Salida: Mensaje que indica que el tipo no existe y muestra tipos similares.
sugerir_similar(Tipo, Tipos) :-
    (   member(Tipo, Tipos)  % Si el tipo es encontrado, no hacemos nada
    ->  true
    ;   writeln('Lo siento, este tipo no existe. Tal vez quisiste decir:'),
        mostrar_sugerencias(Tipos)  % Muestra los tipos disponibles
    ).

% Mostrar tipos sugeridos
% mostrar_sugerencias/1
% Muestra todos los tipos disponibles en la base de conocimiento.
%
% Parámetro:
%   Tipos - Lista de tipos de actividad disponibles (lista de átomos).
%
% Entrada: Lista de tipos disponibles.
% Salida: Mensaje que muestra todos los tipos sugeridos.
mostrar_sugerencias(Tipos) :-
    forall(member(Tipo, Tipos), format(' - ~w~n', [Tipo])).  % Imprime cada tipo en la lista

% Función para actividades por tipo
% actividades_por_tipo_menu/0
% Solicita al usuario que ingrese un tipo de actividad y muestra las actividades
% correspondientes o sugiere tipos similares si el tipo no existe.
%
% Entrada: Ninguna
% Salida: Muestra actividades por tipo o sugerencias si no se encuentra el tipo.
actividades_por_tipo_menu :-
    writeln('Ingrese el tipo de actividad:'),
    read(Tipo),  % Leer tipo de actividad ingresado por el usuario
    (   actividades_por_tipo(Tipo)  % Llamar a actividades_por_tipo con el tipo ingresado
    ->  true  % Si se encuentran actividades, no se hace nada más
    ;   sugerir_tipo(Tipo),  % Si no se encuentran, se sugieren tipos
        menu  % Regresar al menú después de sugerir
    ).

% Predicado para mostrar actividades por tipo
% actividades_por_tipo/1
% Busca y muestra todas las actividades de un tipo específico.
%
% Parámetro:
%   Tipo - El tipo de actividad que se desea consultar (átomo).
%
% Entrada: Tipo de actividad (ej. 'arte').
% Salida: Muestra actividades que coinciden con el tipo.
actividades_por_tipo(Tipo) :-
    findall((Actividad, Costo, Duracion, Descripcion, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, ListaTipos),
             member(Tipo, ListaTipos),  % Verifica si el tipo está en la lista de tipos
             asociar_actividad(Destino, Actividad)),  % Encuentra el destino asociado
            Resultados),  % Almacena resultados en la lista
    mostrar_resultados(Resultados).  % Muestra los resultados encontrados

% Predicado auxiliar para mostrar los resultados
% mostrar_resultados/1
% Muestra las actividades encontradas o un mensaje si no hay actividades.
%
% Parámetro:
%   Resultados - Lista de actividades encontradas (lista de tuplas).
%
% Entrada: Resultados de actividades.
% Salida: Mensajes que indican las actividades encontradas o que no se encontraron.
mostrar_resultados([]) :-  % Caso base: no hay resultados
    writeln('No se encontraron actividades de este tipo.').  % Mensaje cuando no hay actividades
mostrar_resultados(Resultados) :-  % Caso cuando hay resultados
    writeln('Actividades encontradas:'),  % Mensaje de actividades encontradas
    forall(member((Actividad, Costo, Duracion, Descripcion, Destino), Resultados),
           format('Actividad: ~w, Costo: ~d, Duración: ~d días, Descripción: ~s, Destino: ~w~n',
                  [Actividad, Costo, Duracion, Descripcion, Destino])).  % Formato de salida para cada actividad

% Sugerir tipo similar
% sugerir_tipo/1
% Sugerir tipos de actividad similares si el tipo ingresado no existe en la base de conocimiento.
%
% Parámetro:
%   Tipo - El tipo de actividad ingresado por el usuario (átomo).
%
% Entrada: Tipo de actividad (ej. 'ciencia').
% Salida: Mensaje que sugiere tipos disponibles si no se encuentra el tipo.
sugerir_tipo(Tipo) :-
    findall(T, (actividad(_, _, _, _, ListaTipos), member(T, ListaTipos)), Tipos),  % Encuentra todos los tipos
    sort(Tipos, TiposUnicos),  % Ordena y elimina duplicados
    sugerir_similar(Tipo, TiposUnicos).  % Llama a la función para sugerir tipos similares

% Función auxiliar para mostrar sugerencias
% sugerir_similar/2
% Muestra sugerencias de tipos similares basadas en la entrada del usuario.
%
% Parámetros:
%   Tipo - El tipo de actividad ingresado por el usuario (átomo).
%   Tipos - Lista de tipos de actividad disponibles (lista de átomos).
%
% Entrada: Tipo de actividad y lista de tipos disponibles.
% Salida: Mensaje que indica que el tipo no existe y muestra tipos similares.
sugerir_similar(Tipo, Tipos) :-
    (   member(Tipo, Tipos)  % Si el tipo es encontrado, no hacemos nada
    ->  true
    ;   writeln('Lo siento, este tipo no existe. Tal vez quisiste decir:'),
        mostrar_sugerencias(Tipos)  % Muestra los tipos disponibles
    ).

% Mostrar tipos sugeridos
% mostrar_sugerencias/1
% Muestra todos los tipos disponibles en la base de conocimiento.
%
% Parámetro:
%   Tipos - Lista de tipos de actividad disponibles (lista de átomos).
%
% Entrada: Lista de tipos disponibles.
% Salida: Mensaje que muestra todos los tipos sugeridos.
mostrar_sugerencias(Tipos) :-
    forall(member(Tipo, Tipos), format(' - ~w~n', [Tipo])).  % Imprime cada tipo en la lista

% Consulta por precio
% consulta_por_precio/0
% Permite al usuario consultar actividades en función de un monto.
%
% Entrada: Ninguna
% Salida: Muestra actividades más baratas o más caras que el monto especificado.
consulta_por_precio :-
    writeln('Ingrese el monto:'),
    read(Monto),  % Leer el monto ingresado por el usuario
    writeln('¿Desea consultar actividades más baratas o más caras?'),
    writeln('1. Más baratas'),
    writeln('2. Más caras'),
    read(Opcion),  % Leer la opción elegida por el usuario
    (   (Opcion = 1 -> mostrar_actividades_mas_baratas(Monto)  % Si elige más baratas
    ;   Opcion = 2 -> mostrar_actividades_mas_caras(Monto)  % Si elige más caras
    ;   writeln('Opción no válida, regresando al menú.'), menu)  % Manejo de opción no válida
    ).

% Mostrar actividades más baratas
% mostrar_actividades_mas_baratas/1
% Muestra las actividades cuyo costo es menor que el monto especificado.
%
% Parámetro:
%   Monto - Monto ingresado por el usuario (número).
%
% Entrada: Monto
% Salida: Muestra las actividades que son más baratas que el monto.
mostrar_actividades_mas_baratas(Monto) :-
    findall((Actividad, Costo, Duracion, Descripcion, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, _),
             Costo < Monto),  % Filtrar actividades más baratas
            Resultados),  % Almacena resultados en la lista
    mostrar_resultados(Resultados).  % Muestra los resultados encontrados

% Mostrar actividades más caras
% mostrar_actividades_mas_caras/1
% Muestra las actividades cuyo costo es mayor que el monto especificado.
%
% Parámetro:
%   Monto - Monto ingresado por el usuario (número).
%
% Entrada: Monto
% Salida: Muestra las actividades que son más caras que el monto.
mostrar_actividades_mas_caras(Monto) :-
    findall((Actividad, Costo, Duracion, Descripcion, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, _),
             Costo > Monto),  % Filtrar actividades más caras
            Resultados),  % Almacena resultados en la lista
    mostrar_resultados(Resultados).  % Muestra los resultados encontrados