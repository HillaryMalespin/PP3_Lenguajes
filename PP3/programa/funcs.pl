% Incluir los hechos
:- [destino, actividad, destino_actividad, afinidad].

/*****Nombre****************************************
 * registrar_destino
 *****Descripción***********************************
 * Registra un destino turístico en la base de conocimiento.
 * Verifica si el destino ya existe; si es así, devuelve un mensaje de error.
 * Si el destino no existe, lo agrega como un hecho nuevo y lo guarda en el archivo `destino.pl`.
 *****Parámetros************************************
 * @Nombre: Nombre del destino a registrar.
 * @Descripcion: Descripción del destino.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor, pero imprime mensajes en consola 
 * indicando si el destino se registró exitosamente o si hubo un error.
 ***************************************************/
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

/*****Nombre****************************************
 * registrar_actividad
 *****Descripción***********************************
 * Registra una actividad turística en la base de conocimiento.
 * Verifica si la actividad ya existe; si es así, imprime un mensaje de error.
 * Si la actividad no existe, la agrega como un hecho nuevo y la guarda en el archivo `actividad.pl`.
 *****Parámetros************************************
 * @Nombre: Nombre de la actividad a registrar.
 * @Costo: Costo de la actividad.
 * @Duracion: Duración de la actividad.
 * @Descripcion: Descripción detallada de la actividad.
 * @Tipos: Lista de tipos o categorías de la actividad.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor, pero imprime mensajes en consola 
 * indicando si la actividad se registró exitosamente o si ya existe.
 ***************************************************/
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

/*****Nombre****************************************
 * registrar_asociacion
 *****Descripción***********************************
 * Registra una asociación entre un destino turístico y una actividad en la base de conocimiento.
 * Primero, verifica si la asociación ya existe; si es así, imprime un mensaje de error.
 * Si la asociación no existe, verifica que tanto el destino como la actividad estén registrados.
 * Si ambos existen, los asocia como un nuevo hecho y guarda esta asociación en el archivo `destino_actividad.pl`.
 *****Parámetros************************************
 * @Destino: Nombre del destino turístico.
 * @Actividad: Nombre de la actividad a asociar con el destino.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor, pero imprime mensajes en consola 
 * indicando si la asociación se registró exitosamente o si hubo un error 
 * debido a que la asociación ya existe o alguno de los elementos no está registrado.
 ***************************************************/
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

/*****Nombre****************************************
 * consultar_destino
 *****Descripción***********************************
 * Consulta la información de un destino turístico y sus actividades asociadas.
 * Verifica si el destino existe en la base de conocimiento. Luego, recupera todas las actividades
 * asociadas al destino, mostrando información detallada de cada actividad. Finalmente, calcula y muestra
 * el tiempo total y el costo total de todas las actividades disponibles para el destino.
 *****Parámetros************************************
 * @Destino: Nombre del destino turístico a consultar.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor, pero imprime en consola la descripción del destino, 
 * la lista de actividades con sus detalles, y los totales de tiempo y costo.
 ***************************************************/
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
    write('Descripcion: '), write(Descripcion), nl,
    write('Actividades disponibles:'), nl,
    % Mostrar detalles de cada actividad.
    mostrar_actividades(Actividades),
    % Calcular tiempo y costo total.
    calcular_totales(Actividades, TiempoTotal, CostoTotal),
    % Mostrar tiempo y costo total.
    write('Tiempo total: '), write(TiempoTotal), write(' dias'), nl,
    write('Costo total: $'), write(CostoTotal), nl.

/*****Nombre****************************************
 * mostrar_actividades
 *****Descripción***********************************
 * Muestra en consola la información detallada de una lista de actividades.
 * Recorre recursivamente la lista de actividades, imprimiendo el nombre, descripción, costo, duración y tipos
 * de cada actividad en un formato legible.
 *****Parámetros************************************
 * @Actividades: Lista de actividades donde cada elemento es una tupla 
 * (NombreActividad, Costo, Duracion, DescripcionActividad, Tipos).
 *****Retorno***************************************
 * Este predicado no retorna ningún valor, pero imprime en consola la información detallada 
 * de cada actividad en la lista.
 ***************************************************/
mostrar_actividades([]).
mostrar_actividades([(NombreActividad, Costo, Duracion, DescripcionActividad, Tipos)|Resto]) :-
    write('  - Actividad: '), write(NombreActividad), nl,
    write('    Descripcion: '), write(DescripcionActividad), nl,
    write('    Costo: $'), write(Costo), nl,
    write('    Duracion: '), write(Duracion), write(' dias'), nl,
    write('    Tipos: '), write(Tipos), nl, nl,
    mostrar_actividades(Resto).

/*****Nombre****************************************
 * calcular_totales
 *****Descripción***********************************
 * Calcula el tiempo total y el costo total de una lista de actividades.
 * Recorre recursivamente la lista de actividades, sumando la duración y el costo de cada actividad
 * para obtener los totales.
 *****Parámetros************************************
 * @Actividades: Lista de actividades donde cada elemento es una tupla 
 * (NombreActividad, Costo, Duracion, DescripcionActividad, Tipos).
 * @TiempoTotal: Variable de salida para el tiempo total calculado de todas las actividades en días.
 * @CostoTotal: Variable de salida para el costo total calculado de todas las actividades.
 *****Retorno***************************************
 * Este predicado retorna el tiempo total y el costo total de las actividades al asignarlos
 * a `TiempoTotal` y `CostoTotal`.
 ***************************************************/
calcular_totales([], 0, 0).
calcular_totales([(_, Costo, Duracion, _, _)|Resto], TiempoTotal, CostoTotal) :-
    calcular_totales(Resto, TiempoResto, CostoResto),
    TiempoTotal is TiempoResto + Duracion,
    CostoTotal is CostoResto + Costo.

/*-------------------------------------------------------------*/

generar_itinerario_dias :-
    solicitar_duracion(Duracion),
    solicitar_categoria(CategoriaPreferida),
    write('Duracion ingresada: '), write(Duracion), nl,
    write('Categoria preferida: '), write(CategoriaPreferida), nl.

/*-------------------------------------------------------------*/

/*****Nombre****************************************
 * estadistica
 *****Descripción***********************************
 * Ejecuta varias funciones estadísticas para mostrar información 
 * sobre ciudades, actividades y categorías en el sistema.
 *****Parámetros************************************
 * No tiene parámetros.
 *****Retorno***************************************
 * No retorna un valor; llama a otras funciones que muestran resultados en la salida estándar.
 ***************************************************/
estadistica :-
    estadistica_ciudades, nl,
    estadistica_actividad_costo, nl,
    estadistica_actividad_duracion, nl,
    estadistica_categoria, nl.

/*****Nombre****************************************
 * estadistica_ciudades
 *****Descripción***********************************
 * Obtiene y muestra las ciudades junto con la cantidad de actividades distintas que tienen.
 *****Parámetros************************************
 * No tiene parámetros.
 *****Retorno***************************************
 * No retorna un valor; muestra directamente los resultados en la salida estándar.
 ***************************************************/
estadistica_ciudades :-
    % Obtener todas las ciudades y sus actividades distintas.
    findall(Ciudad-Cantidad, (
        setof(Actividad, asociar_actividad(Ciudad, Actividad), Actividades),
        length(Actividades, Cantidad)  % Contar cuántas actividades hay.
    ), ListaCiudades),
    % Ordenar descendente
    predsort(comparar_cantidades, ListaCiudades, ListaOrdenada),  % Usar un comparador personalizado.
    % Mostrar solo los primeros 3 resultados.
    write('Top 3 ciudades con mas actividades:'), nl,
    mostrar_lista(ListaOrdenada, 3).

/*****Nombre****************************************
 * comparar_cantidades
 *****Descripción***********************************
 * Compara las cantidades de dos ciudades para determinar su orden.
 *****Parámetros************************************
 * @Orden: El resultado de la comparación, que será `>` si Cantidad1 es menor que Cantidad2, 
 *          o `<` si es mayor.
 * @Cantidad1: La cantidad de actividades de la primera ciudad.
 * @Cantidad2: La cantidad de actividades de la segunda ciudad.
 *****Retorno***************************************
 * No retorna un valor explícito; el parámetro `Orden` se unifica con el resultado de la comparación.
 ***************************************************/
comparar_cantidades(Orden, _-Cantidad1, _-Cantidad2) :-
    (Cantidad1 < Cantidad2 -> Orden = (>); Orden = (<)).

/*****Nombre****************************************
 * mostrar_lista
 *****Descripción***********************************
 * Muestra las ciudades y sus respectivas cantidades de actividades, 
 * limitándose a un número específico de resultados.
 *****Parámetros************************************
 * @Lista: La lista de pares Ciudad-Cantidad a mostrar.
 * @Limite: El número máximo de resultados a mostrar.
 *****Retorno***************************************
 * No retorna un valor; imprime directamente los resultados en la salida estándar.
 ***************************************************/
mostrar_lista(_, 0).  % Si el límite es 0, no hacer nada.
mostrar_lista([], _).  % Si la lista está vacía, no hacer nada.
mostrar_lista([Ciudad-Cantidad|Resto], Limite) :-
    format('       Ciudad: ~w, Cantidad de actividades: ~d~n', [Ciudad, Cantidad]),
    NuevoLimite is Limite - 1,  % Reducir el límite.
    mostrar_lista(Resto, NuevoLimite).  % Llamar recursivamente para el resto de las ciudades.

/*****Nombre****************************************
 * estadistica_actividad_cara
 *****Descripción***********************************
 * Obtiene y muestra la actividad más cara entre todas las actividades disponibles, 
 * junto con su costo.
 *****Parámetros************************************
 * No tiene parámetros.
 *****Retorno***************************************
 * No retorna un valor; muestra directamente los resultados en la salida estándar.
 ***************************************************/
estadistica_actividad_costo :-
    % Obtener todas las actividades y sus costos
    findall(Nombre-Costo, actividad(Nombre, Costo, _, _, _), ListaActividades),
    actividad_mas_cara(ListaActividades, NombreMasCaro, CostoMasCaro),
    write('La actividad mas cara es:'), nl,
    write('     Activida: '), write(NombreMasCaro), nl,
    write('     Costo: $'), write(CostoMasCaro), nl.

/*****Nombre****************************************
 * actividad_mas_cara
 *****Descripción***********************************
 * Determina la actividad más cara de una lista de actividades, 
 * comparando sus costos.
 *****Parámetros************************************
 * @Lista: La lista de pares Nombre-Costo de actividades.
 * @NombreMasCaro: El nombre de la actividad más cara encontrada.
 * @CostoMasCaro: El costo de la actividad más cara encontrada.
 *****Retorno***************************************
 * Retorna el nombre y el costo de la actividad más cara a través de la unificación 
 * de los parámetros `NombreMasCaro` y `CostoMasCaro`.
 ***************************************************/
actividad_mas_cara([], _, 0).
actividad_mas_cara([Nombre-Costo|Resto], NombreMasCaro, CostoMasCaro) :-
    actividad_mas_cara(Resto, NombreAux, CostoAux),
    (Costo > CostoAux ->
        (NombreMasCaro = Nombre, CostoMasCaro = Costo)
        ;
        (NombreMasCaro = NombreAux, CostoMasCaro = CostoAux)
    ).

/*****Nombre****************************************
 * estadistica_actividad_duracion
 *****Descripción***********************************
 * Obtiene y muestra la actividad de menor duración entre todas las actividades disponibles,
 * junto con su duración.
 *****Parámetros************************************
 * No tiene parámetros.
 *****Retorno***************************************
 * No retorna un valor; muestra directamente los resultados en la salida estándar.
 ***************************************************/
estadistica_actividad_duracion :-
    % Obtener todas las actividades y sus duraciones
    findall(Nombre-Duracion, actividad(Nombre, _, Duracion, _, _), ListaActividades),
    (   ListaActividades = [] ->
        write('No hay actividades disponibles.'), nl
    ;   actividad_menor_duracion(ListaActividades, NombreMenorDuracion, DuracionMenor),
        write('La actividad de menor duracion es:'), nl,
        write('     Actividad: '), write(NombreMenorDuracion), nl,
        write('     Duracion: '), write(DuracionMenor), write(' dias'), nl
    ).

/*****Nombre****************************************
 * actividad_menor_duracion
 *****Descripción***********************************
 * Determina la actividad de menor duración de una lista de actividades, 
 * comparando sus duraciones.
 *****Parámetros************************************
 * @Lista: La lista de pares Nombre-Duración de actividades.
 * @NombreMenorDuracion: El nombre de la actividad de menor duración encontrada.
 * @DuracionMenor: La duración de la actividad de menor duración encontrada.
 *****Retorno***************************************
 * Retorna el nombre y la duración de la actividad de menor duración a través de la unificación 
 * de los parámetros `NombreMenorDuracion` y `DuracionMenor`.
 ***************************************************/
actividad_menor_duracion([Nombre-Duracion], Nombre, Duracion).  % Caso base: solo una actividad.
actividad_menor_duracion([Nombre-Duracion|Resto], NombreMenorDuracion, DuracionMenor) :-
    actividad_menor_duracion(Resto, NombreAux, DuracionAux),
    (Duracion < DuracionAux ->
        (NombreMenorDuracion = Nombre, DuracionMenor = Duracion)
    ; 
        (NombreMenorDuracion = NombreAux, DuracionMenor = DuracionAux)
    ).

/*****Nombre****************************************
 * estadistica_categoria
 *****Descripción***********************************
 * Obtiene y muestra la categoría con más actividades disponibles.
 *****Parámetros************************************
 * No tiene parámetros.
 *****Retorno***************************************
 * No retorna un valor; muestra directamente los resultados en la salida estándar.
 ***************************************************/
estadistica_categoria :-
    % Obtener todas las categorías y contarlas
    findall(Categoria, (actividad(_, _, _, _, Categorias), member(Categoria, Categorias)), ListaCategorias),
    % Contar las ocurrencias de cada categoría
    contar_ocurrencias(ListaCategorias, ContadorCategorias),
    % Encontrar la categoría con mas actividades
    categoria_mas_frecuente(ContadorCategorias, CategoriaMasFrecuente, MaxCantidad),
    write('La categoria con mas actividades es:'), nl,
    write('     Categoria: '), write(CategoriaMasFrecuente), nl,
    write('     Cantidad de actividades: '), write(MaxCantidad), nl.

/*****Nombre****************************************
 * contar_ocurrencias
 *****Descripción***********************************
 * Cuenta cuántas veces aparece cada categoría en la lista.
 *****Parámetros************************************
 * @ListaCategorias: Lista de categorías a contar.
 * @ContadorCategorias: Lista de pares Categoría-Cantidad.
 *****Retorno***************************************
 * Retorna la lista de categorías y sus cantidades.
 ***************************************************/
contar_ocurrencias(Lista, Contador) :-
    setof(Categoria-Cantidad, (member(Categoria, Lista), contar_categoria(Categoria, Lista, Cantidad)), Contador).

contar_categoria(Categoria, Lista, Cantidad) :-
    include(==(Categoria), Lista, Filtrada),
    length(Filtrada, Cantidad).

/*****Nombre****************************************
 * categoria_mas_frecuente
 *****Descripción***********************************
 * Encuentra la categoría que tiene más actividades.
 *****Parámetros************************************
 * @ContadorCategorias: Lista de pares Categoría-Cantidad.
 * @CategoriaMasFrecuente: La categoría con más actividades.
 * @MaxCantidad: La cantidad de actividades de esa categoría.
 *****Retorno***************************************
 * Retorna la categoría más frecuente y su cantidad.
 ***************************************************/
categoria_mas_frecuente([], '', 0).  % Caso base: sin categorías.
categoria_mas_frecuente([Categoria-Cantidad], Categoria, Cantidad).  % Solo una categoría.
categoria_mas_frecuente([Categoria-Cantidad|Resto], CategoriaMasFrecuente, MaxCantidad) :-
    categoria_mas_frecuente(Resto, CategoriaAux, CantidadAux),
    (Cantidad > CantidadAux ->
        (CategoriaMasFrecuente = Categoria, MaxCantidad = Cantidad)
    ; 
        (CategoriaMasFrecuente = CategoriaAux, MaxCantidad = CantidadAux)
    ).

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
    ->  true  % Si se encuentran actividades, no se hace nada mas
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
    ->  true  % Si se encuentran actividades, no se hace nada mas
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
% Salida: Muestra actividades mas baratas o mas caras que el monto especificado.
consulta_por_precio :-
    writeln('Ingrese el monto:'),
    read(Monto),  % Leer el monto ingresado por el usuario
    writeln('¿Desea consultar actividades más baratas o más caras?'),
    writeln('1. Más baratas'),
    writeln('2. Más caras'),
    read(Opcion),  % Leer la opción elegida por el usuario
    (   (Opcion = 1 -> mostrar_actividades_mas_baratas(Monto)  % Si elige mas baratas
    ;   Opcion = 2 -> mostrar_actividades_mas_caras(Monto)  % Si elige mas caras
    ;   writeln('Opción no válida, regresando al menú.'), menu)  % Manejo de opción no válida
    ).

% Mostrar actividades mas baratas
% mostrar_actividades_mas_baratas/1
% Muestra las actividades cuyo costo es menor que el monto especificado.
%
% Parámetro:
%   Monto - Monto ingresado por el usuario (número).
%
% Entrada: Monto
% Salida: Muestra las actividades que son mas baratas que el monto.
mostrar_actividades_mas_baratas(Monto) :-
    findall((Actividad, Costo, Duracion, Descripcion, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, _),
             Costo < Monto),  % Filtrar actividades mas baratas
            Resultados),  % Almacena resultados en la lista
    mostrar_resultados(Resultados).  % Muestra los resultados encontrados

% Mostrar actividades mas caras
% mostrar_actividades_mas_caras/1
% Muestra las actividades cuyo costo es mayor que el monto especificado.
%
% Parámetro:
%   Monto - Monto ingresado por el usuario (número).
%
% Entrada: Monto
% Salida: Muestra las actividades que son mas caras que el monto.
mostrar_actividades_mas_caras(Monto) :-
    findall((Actividad, Costo, Duracion, Descripcion, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, _),
             Costo > Monto),  % Filtrar actividades mas caras
            Resultados),  % Almacena resultados en la lista
    mostrar_resultados(Resultados).  % Muestra los resultados encontrados