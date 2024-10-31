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
    write('    Duracion: '), write(Duracion), write(' horas'), nl,
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

/*****Nombre****************************************
 * generar_itinerario_dias
 *****Descripción***********************************
 * Carga las actividades desde un archivo, solicita al usuario
 * la duración máxima y la categoría preferida, genera un itinerario
 * basado en esos parámetros y pregunta al usuario si está satisfecho
 * con el itinerario generado.
 *****Parámetros************************************
 * No tiene parámetros.
 *****Retorno***************************************
 * No retorna un valor; llama a otras funciones que muestran resultados en
 * la salida estándar y gestiona la interacción con el usuario.
 ***************************************************/
generar_itinerario_dias :-
    consult('actividad.pl'),
    solicitar_duracion(Duracion),
    solicitar_categoria(CategoriaPreferida),
    generar_itinerario_aux(Duracion, CategoriaPreferida, Itinerario, DuracionTotal),
    write('Itinerario sugerido:'), nl,
    mostrar_itinerario(Itinerario),
    write('Duracion total del itinerario: '), write(DuracionTotal), write(' dias'), nl, nl,
    preguntar_satisfaccion(DuracionTotal, CategoriaPreferida).

/*****Nombre****************************************
 * generar_itinerario_aux
 *****Descripción***********************************
 * Filtra las actividades por la categoría preferida y genera un
 * itinerario asegurándose de que no exceda la duración máxima.
 *****Parámetros************************************
 * DuracionMax - La duración máxima permitida para el itinerario.
 * CategoriaPreferida - La categoría de actividades que el usuario prefiere.
 * Itinerario - Lista que contendrá las actividades seleccionadas.
 * DuracionTotal - Duración total de las actividades seleccionadas en el itinerario.
 *****Retorno***************************************
 * No retorna un valor; produce una lista de actividades y su duración total.
 ***************************************************/
% Predicado principal para generar itinerario con actividades de la categoría preferida
generar_itinerario_aux(DuracionMax, CategoriaPreferida, Itinerario, DuracionTotal) :-
    % Obtener todas las actividades de la categoría preferida
    findall((Actividad, Duracion), 
            (actividad(Actividad, _, Duracion, _, Categorias),
             member(CategoriaPreferida, Categorias)),
            ActividadesFiltradas),
    
    % Generar el itinerario asegurando que no exceda la duración maxima
    seleccionar_actividades(DuracionMax, ActividadesFiltradas, Itinerario, DuracionTotal).

/*****Nombre****************************************
 * seleccionar_actividades
 *****Descripción***********************************
 * Selecciona actividades de una lista dada sin exceder la duración máxima.
 *****Parámetros************************************
 * DuracionMax - La duración máxima permitida para el itinerario.
 * ActividadesFiltradas - Lista de actividades filtradas según la categoría.
 * Itinerario - Lista que contendrá las actividades seleccionadas.
 * DuracionTotal - Duración total de las actividades seleccionadas en el itinerario.
 *****Retorno***************************************
 * No retorna un valor; produce un itinerario y su duración total.
 ***************************************************/
seleccionar_actividades(_, [], [], 0).
seleccionar_actividades(DuracionMax, [(Actividad, Duracion)|Resto], [Actividad|Itinerario], DuracionTotal) :-
    Duracion =< DuracionMax,
    DuracionRestante is DuracionMax - Duracion,
    seleccionar_actividades(DuracionRestante, Resto, Itinerario, DuracionTotalRestante),
    DuracionTotal is Duracion + DuracionTotalRestante.
seleccionar_actividades(DuracionMax, [_|Resto], Itinerario, DuracionTotal) :-
    seleccionar_actividades(DuracionMax, Resto, Itinerario, DuracionTotal).

/*****Nombre****************************************
 * preguntar_satisfaccion
 *****Descripción***********************************
 * Pregunta al usuario si está satisfecho con el itinerario generado.
 * Si el usuario no está satisfecho, genera un nuevo itinerario con
 * una categoría afín.
 *****Parámetros************************************
 * DuracionTotal - Duración total del itinerario generado.
 * CategoriaPreferida - La categoría de actividades preferida por el usuario.
 *****Retorno***************************************
 * No retorna un valor; maneja la interacción con el usuario.
 ***************************************************/
preguntar_satisfaccion(DuracionTotal, CategoriaPreferida) :-
    write('Esta satisfecho con este itinerario? (s/n): '),
    read(Satisfaccion),
    (Satisfaccion == s ->
        write('Genial. Disfrute de su itinerario de actividades.'), nl
    ; 
        write('Generando un nuevo itinerario con categoria afin...'), nl,
        generar_itinerario_con_categoria_afina(DuracionTotal, CategoriaPreferida)
    ).

/*****Nombre****************************************
 * generar_itinerario_con_categoria_afina
 *****Descripción***********************************
 * Genera un nuevo itinerario basado en una categoría afín a la
 * categoría preferida del usuario si no está satisfecho con el itinerario
 * original.
 *****Parámetros************************************
 * DuracionMax - La duración máxima permitida para el nuevo itinerario.
 * CategoriaPreferida - La categoría de actividades que el usuario prefiere.
 *****Retorno***************************************
 * No retorna un valor; produce un nuevo itinerario basado en la categoría afín.
 ***************************************************/
generar_itinerario_con_categoria_afina(DuracionMax, CategoriaPreferida) :-
    % Obtener la categoría afin
    afinidad(CategoriaPreferida, CategoriaAfina),
    write('Categoria afin: '), write(CategoriaAfina), nl,
    generar_itinerario_aux(DuracionMax, CategoriaAfina, ItinerarioAfina, DuracionTotalAfina),
    write('Itinerario afin sugerido:'), nl,
    mostrar_itinerario(ItinerarioAfina),
    write('Duracion total del itinerario: '), write(DuracionTotalAfina), write(' dias'), nl, nl.

/*****Nombre****************************************
 * mostrar_itinerario
 *****Descripción***********************************
 * Muestra las actividades incluidas en el itinerario generado.
 *****Parámetros************************************
 * Itinerario - Lista de actividades que se mostrarán al usuario.
 *****Retorno***************************************
 * No retorna un valor; solo imprime las actividades en la salida estándar.
 ***************************************************/
mostrar_itinerario([]).
mostrar_itinerario([Actividad|Resto]) :-
    write('  - Actividad: '), write(Actividad), nl,
    mostrar_itinerario(Resto).

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
    consult('actividad.pl'),
    consult('destino.pl'),
    consult('destino_actividad.pl'),
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

/*
--------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------Actividades-por-tipo------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
*/

/*****Nombre****************************************
 * actividades_por_tipo
 * actividades_por_tipo/1
 *****Descripción***********************************
 * Encuentra la actividad por tipo.
 *****Parámetros************************************
 * @Tipo: Atomo, tipo de actividad.
 *****Retorno***************************************
 * Las actividades de ese tipo.
 ***************************************************/
actividades_por_tipo(Tipo) :-
    findall((Actividad, Costo, Duracion, Descripcion, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, ListaTipos),
             member(Tipo, ListaTipos),  % Verifica si el tipo está en la lista de tipos
             asociar_actividad(Destino, Actividad)),  % Encuentra el destino asociado
            Resultados),  % Almacena resultados en la lista
    mostrar_resultados_actividades(Resultados).  % Muestra los resultados encontrados

/*****Nombre****************************************
 * mostrar_resultados/1
 *****Descripción***********************************
 * Predicado auxiliar para mostrar los resultados
 * Muestra las actividades encontradas o un mensaje si no hay actividades.
 *****Parámetros************************************
 * @Resultados: Lista de actividades encontradas (lista de tuplas).
 *****Retorno***************************************
 * Mensajes que indican las actividades encontradas o que no se encontraron.
 ***************************************************/
mostrar_resultados_actividades([]) :-  % Caso base: no hay resultados
    writeln('No se encontraron actividades de este tipo.').  % Mensaje cuando no hay actividades
mostrar_resultados_actividades(Resultados) :-  % Caso cuando hay resultados
    writeln('Actividades encontradas 123:'),  % Mensaje de actividades encontradas
    forall(member((Actividad, Costo, Duracion, Descripcion, Destino), Resultados),
           format('Actividad: ~w, Costo: ~d, Duracion: ~d dias, Descripcion: ~s, Destino: ~w~n',
                  [Actividad, Costo, Duracion, Descripcion, Destino])).  % Formato de salida para cada actividad

/*
--------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------consultar-por-precio------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
*/
% Consulta por precio
% consulta_por_precio/0
% Permite al usuario consultar actividades en función de un monto.
%
% Entrada: Ninguna
% Salida: Muestra actividades mas baratas o mas caras que el monto especificado.
consulta_por_precio :-
    writeln('Ingrese el monto:'),
    read(Monto),  % Leer el monto ingresado por el usuario
    writeln('¿Desea consultar actividades mas baratas o mas caras?'),
    writeln('1. Mas baratas'),
    writeln('2. Mas caras'),
    read(Opcion),  % Leer la opción elegida por el usuario
    (   (Opcion = 1 -> mostrar_actividades_mas_baratas(Monto)  % Si elige más baratas
    ;   Opcion = 2 -> mostrar_actividades_mas_caras(Monto)  % Si elige más caras
    ;   writeln('Opcion no valida, regresando al menu.'), menu)  % Manejo de opción no válida
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
    findall((Actividad, Costo, Duracion, Descripcion, Tipos, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
             asociar_actividad(Destino, Actividad),
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
    findall((Actividad, Costo, Duracion, Descripcion, Tipos, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
             asociar_actividad(Destino, Actividad),
             Costo > Monto),  % Filtrar actividades mas caras
            Resultados),  % Almacena resultados en la lista
    mostrar_resultados(Resultados).  % Muestra los resultados encontrados

/*
--------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------Recomendar-por-frase---------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
*/

/*****Nombre****************************************
 * recomendar_por_frase
 *****Descripción***********************************
 * Permite al usuario generar un itinerario a partir de una frase.
 * Se identifican las palabras clave y se realiza un "match" contra
 * la descripción de actividades, descripción de destinos y tipos de actividades.
 *****Parámetros************************************
 * Ninguno
 *****Retorno***************************************
 * Muestra actividades que coinciden con las palabras clave de la frase ingresada.
 ***************************************************/
recomendar_por_frase :- 
    skip(10),
    writeln('Ingrese una frase para recomendar actividades:'),
    read_line_to_string(user_input, Frase),  % Leer la frase ingresada por el usuario
    
    %solicitar_frase(Frase),
    % Extraer palabras clave de la frase, ignorando artículos.
    split_string(Frase, " ", "", Palabras),
    % Filtrar palabras relevantes, omitiendo artículos.
    exclude(articulo, Palabras, PalabrasClave),
    
    % Buscar actividades que coincidan con las palabras clave.
    findall((Actividad, Costo, Duracion, Descripcion, Tipos, Destino),
            (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
             member(Palabra, PalabrasClave),
             (sub_string(Descripcion, _, _, _, Palabra) ;  % Coincidencia en descripción
              member(Palabra, Tipos)),  % Coincidencia en tipos
             asociar_actividad(Destino, Actividad)),  % Obtener el destino asociado
            ActividadesEncontradas),

    % Eliminar duplicados en actividades encontradas
    list_to_set(ActividadesEncontradas, ActividadesSinRepetir),

    % Mostrar resultados
    mostrar_resultados(ActividadesSinRepetir).

/*****Nombre****************************************
 * mostrar_resultados
 *****Descripción***********************************
 * Muestra los resultados
 *****Parámetros************************************
 * actividades y destinos
 *****Retorno***************************************
 * Destinos encontrados
 ***************************************************/
mostrar_resultados(Actividades) :-
    (   Actividades \= [] -> 
        writeln('Actividades encontradas:'),
        forall(member((Actividad, Costo, Duracion, Descripcion, Tipos, Destino), Actividades),
               format('Actividad: ~w, Costo: ~d, Duracion: ~d dias, Descripcion: ~s, Tipos: ~w, Destino: ~w~n',
                      [Actividad, Costo, Duracion, Descripcion, Tipos, Destino]));
        writeln('No se encontraron actividades.')
    ).

/*****Nombre****************************************
 * articulo
 *****Descripción***********************************
 * Verifica si una palabra es un artículo.
 *****Parámetros************************************
 * @Palabra: Palabra a verificar.
 *****Retorno***************************************
 * Retorna true si es un artículo, false en caso contrario.
 ***************************************************/
articulo(X) :- member(X, ['el', 'la', 'los', 'las', 'un', 'una', 'unos', 'unas', 'de', 'en', 'a', 'y', 'o']).


/*
--------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------Generar-itinerario-por-monto----------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
*/

/*****Nombre****************************************
 * generar_itinerario_por_monto
 *****Descripción***********************************
 * Carga las actividades desde un archivo, solicita al usuario
 * el monto máximo, la categoría preferida, la cantidad de personas
 * y la preferencia de estancia, generando un itinerario
 * basado en esos parámetros. También pregunta al usuario si está
 * satisfecho con el itinerario generado.
 *****Parámetros************************************
 * No tiene parámetros.
 *****Retorno***************************************
 * No retorna un valor; llama a otras funciones que muestran resultados en
 * la salida estándar y gestiona la interacción con el usuario.
 ***************************************************/
generar_itinerario_por_monto :- 
    consult('actividad.pl'),
    write('Ingrese el monto maximo que dispone: '),
    read(MontoMaximo),
    write('Ingrese la categoria de preferencia: '),
    read(CategoriaPreferida),
    write('Ingrese la cantidad de personas: '),
    read(CantidadPersonas), 
    write('¿Prefiere estancias largas o cortas? (largo/corto): '),
    read(OpcionEstancia),  
    generar_itinerario_aux_monto(MontoMaximo, CategoriaPreferida, CantidadPersonas, OpcionEstancia, Itinerario, CostoTotal),
    write('Itinerario sugerido:'), nl,
    mostrar_itinerario(Itinerario),
    write('Costo total del itinerario: $'), write(CostoTotal), nl, nl,
    preguntar_satisfaccion(CostoTotal).

/*****Nombre****************************************
 * generar_itinerario_aux_monto
 *****Descripción***********************************
 * Filtra las actividades por la categoría preferida y genera un
 * itinerario asegurándose de que no exceda el monto máximo.
 *****Parámetros************************************
 * MontoMaximo - El monto máximo permitido para el itinerario.
 * CategoriaPreferida - La categoría de actividades que el usuario prefiere.
 * CantidadPersonas - Cantidad de personas que realizará las actividades.
 * OpcionEstancia - Preferencia de estancia (larga o corta).
 * Itinerario - Lista que contendrá las actividades seleccionadas.
 * CostoTotal - Costo total de las actividades seleccionadas en el itinerario.
 *****Retorno***************************************
 * No retorna un valor; produce una lista de actividades y su costo total.
 ***************************************************/
generar_itinerario_aux_monto(MontoMaximo, CategoriaPreferida, CantidadPersonas, OpcionEstancia, Itinerario, CostoTotal) :-
    % Obtener todas las actividades de la categoría preferida
    findall((Actividad, Costo, Duracion, Descripcion, Tipos),
            (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
             member(CategoriaPreferida, Tipos)),
            ActividadesFiltradas),
    
    % Generar el itinerario asegurando que no exceda el monto maximo
    seleccionar_actividades_monto(MontoMaximo, ActividadesFiltradas, CantidadPersonas, OpcionEstancia, Itinerario, CostoTotal).


/*****Nombre****************************************
 * seleccionar_actividades_monto
 *****Descripción***********************************
 * Selecciona actividades de una lista dada sin exceder el monto maximo.
 *****Parametros************************************
 * MontoMaximo - El monto maximo permitido para el itinerario.
 * ActividadesFiltradas - Lista de actividades filtradas según la categoría.
 * CantidadPersonas - Cantidad de personas que realizara las actividades.
 * OpcionEstancia - Preferencia de estancia (larga o corta).
 * Itinerario - Lista que contendra las actividades seleccionadas.
 * CostoTotal - Costo total de las actividades seleccionadas en el itinerario.
 *****Retorno***************************************
 * No retorna un valor; produce un itinerario y su costo total.
 ***************************************************/
seleccionar_actividades_monto(_, [], _, _, [], 0).  % Caso base: sin actividades
seleccionar_actividades_monto(MontoMaximo, [(Actividad, Costo, Duracion, _, _)|Resto], CantidadPersonas, OpcionEstancia, 
    [Actividad|Itinerario], CostoTotal) :- 
    CostoTotalActividades is Costo * CantidadPersonas,
    CostoTotalActividades =< MontoMaximo,
    (OpcionEstancia = largo -> Duracion >= 2; Duracion < 2),
    MontoRestante is MontoMaximo - CostoTotalActividades,
    seleccionar_actividades_monto(MontoRestante, Resto, CantidadPersonas, OpcionEstancia, Itinerario, CostoTotalRestante),
    CostoTotal is CostoTotalActividades + CostoTotalRestante.
seleccionar_actividades_monto(MontoMaximo, [_|Resto], CantidadPersonas, OpcionEstancia, Itinerario, CostoTotal) :-
    seleccionar_actividades_monto(MontoMaximo, Resto, CantidadPersonas, OpcionEstancia, Itinerario, CostoTotal).

/*****Nombre****************************************
 * preguntar_satisfaccion
 *****Descripción***********************************
 * Pregunta al usuario si esta satisfecho con el itinerario generado.
 * Si el usuario no esta satisfecho, genera un nuevo itinerario con
 * una categoría afín.
 *****Parametros************************************
 * CostoTotal - Costo total del itinerario generado.
 *****Retorno***************************************
 * No retorna un valor; maneja la interacción con el usuario.
 ***************************************************/
preguntar_satisfaccion(CostoTotal) :-
    write('Esta satisfecho con este itinerario? (s/n): '),
    read(Satisfaccion),
    (Satisfaccion == s ->
        write('Genial. Disfrute de su itinerario de actividades.'), nl
    ; 
        write('Generando un nuevo itinerario con categoria afin...'), nl,
        generar_itinerario_con_categoria_afina_monto(CostoTotal)
    ).

/*****Nombre****************************************
 * generar_itinerario_con_categoria_afina
 *****Descripción***********************************
 * Genera un nuevo itinerario basado en una categoría afín a la
 * categoría preferida del usuario si no esta satisfecho con el itinerario
 * original.
 *****Parametros************************************
 * CostoMaximo - El costo maximo permitido para el nuevo itinerario.
 * CategoriaPreferida - La categoria de actividades que el usuario prefiere.
 *****Retorno***************************************
 * No retorna un valor; produce un nuevo itinerario basado en la categoria afin.
 ***************************************************/
generar_itinerario_con_categoria_afina_monto(CostoMaximo, CategoriaPreferida) :-
    % Obtener la categoría afín
    afinidad(CategoriaPreferida, CategoriaAfina),
    write('Categoría afín: '), write(CategoriaAfina), nl,
    generar_itinerario_aux_monto(CostoMaximo, CategoriaAfina, _CantidadPersonas, _OpcionEstancia, ItinerarioAfina, CostoTotalAfina),
    write('Itinerario afín sugerido:'), nl,
    mostrar_itinerario(ItinerarioAfina),
    write('Costo total del itinerario: $'), write(CostoTotalAfina), nl, nl.


