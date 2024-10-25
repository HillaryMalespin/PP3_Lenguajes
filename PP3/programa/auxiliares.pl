% Solicitar y validar nombre (atomo)
solicitar_nombre_destino(Nombre) :-
    write('Ingrese el nombre del destino (sin comillas, como un atomo): '), nl,
    read(Input),
    (   validar_atomo(Input) ->
        Nombre = Input
    ;   write('Error: El nombre debe ser un atomo.'), nl,
        solicitar_nombre_destino(Nombre)
    ).

% Solicitar y validar descripción (string)
solicitar_descripcion_destino(Descripcion) :-
    write('Ingrese la descripcion del destino (como una cadena): '), nl,
    read(Input),
    (   validar_string(Input) ->
        Descripcion = Input
    ;   write('Error: La descripcion debe ser una cadena.'), nl,
        solicitar_descripcion_destino(Descripcion)
    ).

% Solicitar y validar el nomvbre de la actividad (string)
solicitar_nombre_actividad(Nombre) :-
    write('Ingrese el nombre de la actividad (como un atomo): '), nl,
    read(Input),
    (   validar_atomo(Input) ->
        Nombre = Input
    ;   write('Error: El nombre debe ser un atomo.'), nl,
        solicitar_nombre_actividad(Nombre)
    ).

% Solicitar y validar el costo de la actividad (string)
solicitar_costo(Costo) :-
    write('Ingrese el costo de la actividad (entero): '), nl,
    read(Input),
    (   validar_entero(Input) ->
        Costo = Input
    ;   write('Error: El costo debe ser un numero entero.'), nl,
        solicitar_costo(Costo)
    ).

% Solicitar y validar la duracion de la actividad (string)
solicitar_duracion(Duracion) :-
    write('Ingrese la duracion en dias (entero): '), nl,
    read(Input),
    (   validar_entero(Input) ->
        Duracion = Input
    ;   write('Error: La duracion debe ser un numero entero.'), nl,
        solicitar_duracion(Duracion)
    ).

% Solicitar y validar la descripcion de la actividad (string)
solicitar_descripcion_actividad(Descripcion) :-
    write('Ingrese la descripcion de la actividad (cadena): '), nl,
    read(Input),
    (   validar_string(Input) ->
        Descripcion = Input
    ;   write('Error: La descripcion debe ser una cadena.'), nl,
        solicitar_descripcion_actividad(Descripcion)
    ).

% Solicitar y validar los tipos de la actividad (string)
solicitar_tipos(Tipos) :-
    write('Ingrese la lista de tipos (como una lista de cadenas, por ejemplo, ["aventura", "cultural"]): '), nl,
    read(Input),
    (   validar_lista_strings(Input) ->
        Tipos = Input
    ;   write('Error: La lista debe contener solo cadenas.'), nl,
        solicitar_tipos(Tipos)
    ).

% Validaciones de tipos de datos
validar_atomo(Valor) :-
    atom(Valor).

validar_entero(Valor) :-
    integer(Valor).

validar_string(Valor) :-
    string(Valor).

validar_lista_strings(Lista) :-
    is_list(Lista),
    forall(member(Elemento, Lista), string(Elemento)).
