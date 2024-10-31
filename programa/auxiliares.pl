/*****Nombre****************************************
 * solicitar_nombre_destino
 *****Descripción***********************************
 * Solicita al usuario que ingrese el nombre de un destino turístico.
 * Muestra un mensaje de instrucciones, lee la entrada del usuario y verifica que el nombre
 * ingresado sea un átomo válido. Si el nombre no es válido, muestra un mensaje de error y vuelve
 * a solicitar la entrada hasta que se cumpla la condición.
 *****Parámetros************************************
 * @Nombre: Variable de salida que almacena el nombre del destino ingresado como átomo.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor directamente, pero asigna el nombre válido
 * ingresado a `Nombre` para su uso posterior.
 ***************************************************/
solicitar_nombre_destino(Nombre) :-
    write('Ingrese el nombre del destino (sin comillas, como un atomo): '), nl,
    read(Input),
    (   validar_atomo(Input) ->
        Nombre = Input
    ;   write('Error: El nombre debe ser un atomo.'), nl,
        solicitar_nombre_destino(Nombre)
    ).

/*****Nombre****************************************
 * solicitar_descripcion_destino
 *****Descripción***********************************
 * Solicita al usuario que ingrese una descripción para un destino turístico.
 * Muestra un mensaje de instrucciones, lee la entrada del usuario y verifica que la descripción
 * ingresada sea una cadena válida. Si no es válida, muestra un mensaje de error y vuelve a solicitar
 * la entrada hasta que se cumpla la condición.
 *****Parámetros************************************
 * @Descripcion: Variable de salida que almacena la descripción del destino como una cadena.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor directamente, pero asigna la descripción válida
 * ingresada a `Descripcion` para su uso posterior.
 ***************************************************/
solicitar_descripcion_destino(Descripcion) :-
    write('Ingrese la descripcion del destino (como una cadena): '), nl,
    read(Input),
    (   validar_string(Input) ->
        Descripcion = Input
    ;   write('Error: La descripcion debe ser una cadena.'), nl,
        solicitar_descripcion_destino(Descripcion)
    ).

/*****Nombre****************************************
 * solicitar_nombre_actividad
 *****Descripción***********************************
 * Solicita al usuario que ingrese el nombre de una actividad turística.
 * Muestra un mensaje de instrucciones, lee la entrada del usuario y verifica que el nombre
 * ingresado sea un átomo válido. Si el nombre no es válido, muestra un mensaje de error y vuelve
 * a solicitar la entrada hasta que se cumpla la condición.
 *****Parámetros************************************
 * @Nombre: Variable de salida que almacena el nombre de la actividad ingresada como un átomo.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor directamente, pero asigna el nombre válido
 * ingresado a `Nombre` para su uso posterior.
 ***************************************************/
solicitar_nombre_actividad(Nombre) :-
    write('Ingrese el nombre de la actividad (como un atomo): '), nl,
    read(Input),
    (   validar_atomo(Input) ->
        Nombre = Input
    ;   write('Error: El nombre debe ser un atomo.'), nl,
        solicitar_nombre_actividad(Nombre)
    ).

/*****Nombre****************************************
 * solicitar_costo
 *****Descripción***********************************
 * Solicita al usuario que ingrese el costo de una actividad turística.
 * Muestra un mensaje de instrucciones, lee la entrada del usuario y verifica que el costo
 * ingresado sea un número entero válido. Si no es válido, muestra un mensaje de error y vuelve
 * a solicitar la entrada hasta que se cumpla la condición.
 *****Parámetros************************************
 * @Costo: Variable de salida que almacena el costo de la actividad ingresado como un número entero.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor directamente, pero asigna el costo válido
 * ingresado a `Costo` para su uso posterior.
 ***************************************************/
solicitar_costo(Costo) :-
    write('Ingrese el costo de la actividad (entero): '), nl,
    read(Input),
    (   validar_entero(Input) ->
        Costo = Input
    ;   write('Error: El costo debe ser un numero entero.'), nl,
        solicitar_costo(Costo)
    ).

/*****Nombre****************************************
 * solicitar_duracion
 *****Descripción***********************************
 * Solicita al usuario que ingrese la duración de una actividad turística en días.
 * Muestra un mensaje de instrucciones, lee la entrada del usuario y verifica que la duración
 * ingresada sea un número entero válido. Si no es válido, muestra un mensaje de error y vuelve
 * a solicitar la entrada hasta que se cumpla la condición.
 *****Parámetros************************************
 * @Duracion: Variable de salida que almacena la duración de la actividad ingresada como un número entero.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor directamente, pero asigna la duración válida
 * ingresada a `Duracion` para su uso posterior.
 ***************************************************/
solicitar_duracion(Duracion) :-
    write('Ingrese la duracion en dias (entero): '), nl,
    read(Input),
    (   validar_entero(Input) ->
        Duracion = Input
    ;   write('Error: La duracion debe ser un numero entero.'), nl,
        solicitar_duracion(Duracion)
    ).

/*****Nombre****************************************
 * solicitar_descripcion_actividad
 *****Descripción***********************************
 * Solicita al usuario que ingrese una descripción para una actividad turística.
 * Muestra un mensaje de instrucciones, lee la entrada del usuario y verifica que la descripción
 * ingresada sea una cadena válida. Si no es válida, muestra un mensaje de error y vuelve a solicitar
 * la entrada hasta que se cumpla la condición.
 *****Parámetros************************************
 * @Descripcion: Variable de salida que almacena la descripción de la actividad ingresada como una cadena.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor directamente, pero asigna la descripción válida
 * ingresada a `Descripcion` para su uso posterior.
 ***************************************************/
solicitar_descripcion_actividad(Descripcion) :-
    write('Ingrese la descripcion de la actividad (cadena): '), nl,
    read(Input),
    (   validar_string(Input) ->
        Descripcion = Input
    ;   write('Error: La descripcion debe ser una cadena.'), nl,
        solicitar_descripcion_actividad(Descripcion)
    ).

/*****Nombre****************************************
 * solicitar_tipos
 *****Descripción***********************************
 * Solicita al usuario que ingrese una lista de tipos para una actividad turística.
 * Muestra un mensaje de instrucciones, lee la entrada del usuario y verifica que la lista
 * ingresada contenga solo cadenas válidas. Si no es válida, muestra un mensaje de error y vuelve
 * a solicitar la entrada hasta que se cumpla la condición.
 *****Parámetros************************************
 * @Tipos: Variable de salida que almacena la lista de tipos de actividad ingresada como una lista de cadenas.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor directamente, pero asigna la lista válida
 * ingresada a `Tipos` para su uso posterior.
 ***************************************************/
solicitar_tipos(Tipos) :-
    write('Ingrese la lista de tipos (como una lista de cadenas, por ejemplo, ["aventura", "cultural"]): '), nl,
    read(Input),
    (   validar_lista_strings(Input) ->
        Tipos = Input
    ;   write('Error: La lista debe contener solo cadenas.'), nl,
        solicitar_tipos(Tipos)
    ).

/*****Nombre****************************************
 * solicitar_categoria
 *****Descripción***********************************
 * Solicita al usuario que ingrese una categoria
 *****Parámetros************************************
 * @Descripcion: Variable de salida que almacena la descripción del destino como una cadena.
 *****Retorno***************************************
 * Este predicado no retorna ningún valor directamente, pero asigna la descripción válida
 * ingresada a `Categoria` para su uso posterior.
 ***************************************************/
solicitar_categoria(Categoria) :-
    write('Ingrese la categoria de preferencia (como un atomo): '), nl,
    read(Input),
    (   validar_atomo(Input) ->
        Categoria = Input
    ;   write('Error: La categoria debe ser un atomo.'), nl,
        solicitar_categoria(Categoria)
    ).

/*****Nombre****************************************
 * validar_atomo
 *****Descripción***********************************
 * Verifica si el valor ingresado es un átomo.
 *****Parámetros************************************
 * @Valor: El valor a validar como átomo.
 *****Retorno***************************************
 * Retorna `true` si el valor es un átomo, `false` de lo contrario.
 ***************************************************/
validar_atomo(Valor) :-
    atom(Valor).

/*****Nombre****************************************
 * validar_entero
 *****Descripción***********************************
 * Verifica si el valor ingresado es un número entero.
 *****Parámetros************************************
 * @Valor: El valor a validar como número entero.
 *****Retorno***************************************
 * Retorna `true` si el valor es un entero, `false` de lo contrario.
 ***************************************************/
validar_entero(Valor) :-
    integer(Valor).

/*****Nombre****************************************
 * validar_string
 *****Descripción***********************************
 * Verifica si el valor ingresado es una cadena.
 *****Parámetros************************************
 * @Valor: El valor a validar como cadena.
 *****Retorno***************************************
 * Retorna `true` si el valor es una cadena, `false` de lo contrario.
 ***************************************************/
validar_string(Valor) :-
    string(Valor).

/*****Nombre****************************************
 * validar_lista_strings
 *****Descripción***********************************
 * Verifica si el valor ingresado es una lista que contiene solo cadenas.
 *****Parámetros************************************
 * @Lista: La lista a validar.
 *****Retorno***************************************
 * Retorna `true` si la lista contiene solo cadenas, `false` de lo contrario.
 ***************************************************/
validar_lista_strings(Lista) :-
    is_list(Lista),
    forall(member(Elemento, Lista), string(Elemento)).