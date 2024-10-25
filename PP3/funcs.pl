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
