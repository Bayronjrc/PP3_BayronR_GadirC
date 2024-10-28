% Programa principal del sistema de itinerario
:- consult('bc.pl').
:- use_module(library(readutil)). 


% Menú Principal
menu_principal :-
    repeat,
    nl,
    write('=== SISTEMA DE ITINERARIO DE VIAJE ==='), nl,
    write('1. Agregar hechos'), nl,
    write('2. Consulta destino'), nl,
    write('3. Actividades por tipo'), nl,
    write('4. Consulta por precio'), nl,
    write('5. Generar itinerario por monto'), nl,
    write('6. Generar itinerario por dias'), nl,
    write('7. Recomendar por frase'), nl,
    write('8. Estadisticas'), nl,
    write('9. Salir'), nl,
    write('Seleccione una opcion: '),
    read(Opcion),
    procesar_opcion(Opcion),
    Opcion = 9, !.

% Procesamiento de opciones del men�
procesar_opcion(1) :- menu_agregar_hechos.
procesar_opcion(2) :- consulta_destino.
procesar_opcion(3) :- actividades_por_tipo.
procesar_opcion(4) :- consulta_por_precio.
procesar_opcion(5) :- itinerario_por_monto.
procesar_opcion(6) :- itinerario_por_dias.
procesar_opcion(7) :- recomendar_por_frase.
procesar_opcion(8) :- mostrar_estadisticas.
procesar_opcion(9) :- write('�Hasta luego!'), nl.





% Menú para agregar hechos
menu_agregar_hechos :-
    nl,
    write('=== AGREGAR HECHOS ==='), nl,
    write('1. Agregar destino'), nl,
    write('2. Agregar actividad'), nl,
    write('3. Agregar asociacion'), nl,
    write('4. Volver al menu principal'), nl,
    write('Seleccione una opcion: '),
    read(Opcion),
    procesar_opcion_agregar(Opcion).

procesar_opcion_agregar(1) :- !, agregar_destino.
procesar_opcion_agregar(2) :- !, agregar_actividad.
procesar_opcion_agregar(3) :- !, agregar_asociacion.
procesar_opcion_agregar(4) :- !.
procesar_opcion_agregar(_) :-
    write('Opcion invalida. Intente nuevamente.'), nl,
    menu_agregar_hechos.

% Agregar nuevo destino
agregar_destino :-
    write('Ingrese el nombre del destino (atomo): '),
    read(Nombre),
    write('Ingrese la descripcion (entre comillas simples): '),
    read(Descripcion),
    assertz(destino(Nombre, Descripcion)),
    write('Destino agregado exitosamente.'), nl.

% Agregar nueva actividad
agregar_actividad :-
    write('Ingrese el nombre de la actividad (atomo): '),
    read(Nombre),
    write('Ingrese el costo (numero): '),
    read(Costo),
    write('Ingrese la duración en dias (numero): '),
    read(Duracion),
    write('Ingrese la descripcion (entre comillas simples): '),
    read(Descripcion),
    write('Ingrese la lista de tipos (ejemplo: [''arte'', ''historia'']): '),
    read(Tipos),
    assertz(actividad(Nombre, Costo, Duracion, Descripcion, Tipos)),
    write('Actividad agregada exitosamente.'), nl.

% Agregar nueva asociación
agregar_asociacion :-
    write('Ingrese el nombre del destino (atomo): '),
    read(Destino),
    write('Ingrese el nombre de la actividad (atomo): '),
    read(Actividad),
    assertz(asociar_actividad(Destino, Actividad)),
    write('Asociación agregada exitosamente.'), nl.

% Consulta de destino
consulta_destino :-
    write('Ingrese el nombre del destino a consultar: '),
    read(Destino),
    destino(Destino, Descripcion),
    nl, write('=== INFORMACIÓN DEL DESTINO ==='), nl,
    write('Destino: '), write(Destino), nl,
    write('Descripcion: '), write(Descripcion), nl,
    write('Actividades disponibles:'), nl,
    mostrar_actividades_destino(Destino),
    calcular_totales_destino(Destino).

mostrar_actividades_destino(Destino) :-
    asociar_actividad(Destino, Act),
    actividad(Act, Costo, Duracion, Desc, Tipos),
    write('- '), write(Act), nl,
    write('  Costo: $'), write(Costo), nl,
    write('  Duracion: '), write(Duracion), write(' dias'), nl,
    write('  Descripcion: '), write(Desc), nl,
    write('  Tipos: '), write(Tipos), nl,
    fail.
mostrar_actividades_destino(_).

calcular_totales_destino(Destino) :-
    findall(Costo, (asociar_actividad(Destino, Act),
                    actividad(Act, Costo, _, _, _)), Costos),
    findall(Duracion, (asociar_actividad(Destino, Act),
                      actividad(Act, _, Duracion, _, _)), Duraciones),
    sum_list(Costos, CostoTotal),
    sum_list(Duraciones, DuracionTotal),
    nl, write('=== TOTALES ==='), nl,
    write('Costo total: $'), write(CostoTotal), nl,
    write('Duracion total: '), write(DuracionTotal), write(' dias'), nl.

% Actividades por tipo
actividades_por_tipo :-
    write('Ingrese el tipo de actividad a consultar: '),
    read(Tipo),
    nl, write('=== ACTIVIDADES DE TIPO '), write(Tipo), write(' ==='), nl,
    buscar_actividades_tipo(Tipo).

buscar_actividades_tipo(Tipo) :-
    actividad(Nombre, Costo, Duracion, Desc, Tipos),
    member(Tipo, Tipos),
    findall(Dest, asociar_actividad(Dest, Nombre), Destinos),
    write('Actividad: '), write(Nombre), nl,
    write('Costo: $'), write(Costo), nl,
    write('Duracion: '), write(Duracion), write(' dias'), nl,
    write('Descripcion: '), write(Desc), nl,
    write('Destinos: '), write(Destinos), nl, nl,
    fail.
buscar_actividades_tipo(_).

% Consulta por precio
consulta_por_precio :-
    write('Ingrese el monto de referencia: '),
    read(Monto),
    write('¿Desea ver actividades mas baratas (1) o mas caras (2)?: '),
    read(Opcion),
    nl, write('=== ACTIVIDADES ENCONTRADAS ==='), nl,
    mostrar_actividades_precio(Monto, Opcion).

mostrar_actividades_precio(Monto, 1) :-  % Más baratas
    actividad(Nombre, Costo, Duracion, Desc, Tipos),
    Costo =< Monto,
    write('Actividad: '), write(Nombre), nl,
    write('Costo: $'), write(Costo), nl,
    write('Duracion: '), write(Duracion), write(' dias'), nl,
    write('Descripcion: '), write(Desc), nl,
    write('Tipos: '), write(Tipos), nl, nl,
    fail.
mostrar_actividades_precio(Monto, 2) :-  % Más caras
    actividad(Nombre, Costo, Duracion, Desc, Tipos),
    Costo >= Monto,
    write('Actividad: '), write(Nombre), nl,
    write('Costo: $'), write(Costo), nl,
    write('Duracion: '), write(Duracion), write(' dias'), nl,
    write('Descripcion: '), write(Desc), nl,
    write('Tipos: '), write(Tipos), nl, nl,
    fail.
mostrar_actividades_precio(_, _).

itinerario_por_monto :-
    write('Ingrese el monto máximo disponible: '),
    read(MontoMax),
    write('Ingrese la categoría de preferencia: '),
    read(Categoria),
    write('Ingrese la cantidad de personas: '),
    read(NumPersonas),
    write('¿Prefiere estancias largas? (s/n): '),
    read(PrefLargas),
    nl, write('=== ITINERARIO GENERADO ==='), nl,
    generar_itinerario_monto(MontoMax, Categoria, NumPersonas, PrefLargas, Itinerario),
    mostrar_itinerario(Itinerario).

generar_itinerario_monto(MontoMax, Categoria, NumPersonas, PrefLargas, Itinerario) :-
    findall(act(Nombre, Costo, Duracion, Tipos),
            actividad(Nombre, Costo, Duracion, _, Tipos),
            TodasActividades),
    ordenar_por_preferencia(TodasActividades, PrefLargas, ActividadesOrdenadas),
    seleccionar_actividades_monto(ActividadesOrdenadas, MontoMax, NumPersonas,
                                 Categoria, [], Itinerario).

seleccionar_actividades_monto([], _, _, _, Acc, Acc).
seleccionar_actividades_monto([Act|Resto], MontoMax, NumPersonas, Categoria, Acc, Resultado) :-
    Act = act(_, Costo, _, Tipos),
    CostoTotal is Costo * NumPersonas,
    suma_costos(Acc, NumPersonas, CostoAcc),
    NuevoCosto is CostoTotal + CostoAcc,
    NuevoCosto =< MontoMax,
    prioridad_categoria(Tipos, Categoria, Prioridad),
    Prioridad > 0,
    !,
    seleccionar_actividades_monto(Resto, MontoMax, NumPersonas, Categoria, [Act|Acc], Resultado).
seleccionar_actividades_monto([_|Resto], MontoMax, NumPersonas, Categoria, Acc, Resultado) :-
    seleccionar_actividades_monto(Resto, MontoMax, NumPersonas, Categoria, Acc, Resultado).

% Generar itinerario por días
itinerario_por_dias :-
    write('Ingrese la cantidad máxima de días: '),
    read(MaxDias),
    write('Ingrese la categoría de preferencia: '),
    read(Categoria),
    generar_itinerario_dias(MaxDias, Categoria, 1, Itinerario),
    mostrar_itinerario(Itinerario),
    write('¿Desea generar otro itinerario? (s/n): '),
    read(Respuesta),
    (Respuesta = s ->
        generar_itinerario_dias(MaxDias, Categoria, 2, NuevoItinerario),
        mostrar_itinerario(NuevoItinerario)
    ; true).

generar_itinerario_dias(MaxDias, Categoria, Criterio, Itinerario) :-
    findall(act(Nombre, Costo, Duracion, Tipos),
            actividad(Nombre, Costo, Duracion, _, Tipos),
            TodasActividades),
    % Criterio 1: Prioriza duración, Criterio 2: Prioriza variedad de tipos
    (Criterio = 1 ->
        ordenar_por_duracion(TodasActividades, ActividadesOrdenadas)
    ;
        ordenar_por_variedad_tipos(TodasActividades, ActividadesOrdenadas)
    ),
    seleccionar_actividades_dias(ActividadesOrdenadas, MaxDias, Categoria, [], Itinerario).

% Opción principal para recomendar por frase
recomendar_por_frase :-
    write('Ingrese su frase de búsqueda: '),
    read_pending_input(user_input, _, []), % Consume cualquier entrada residual
    read_line_to_string(user_input, Frase),
    generar_itinerario_por_frase(Frase).

% Lista de palabras ignoradas (artículos, preposiciones, etc.)
palabra_ignorada(el).
palabra_ignorada(la).
palabra_ignorada(los).
palabra_ignorada(las).
palabra_ignorada(de).
palabra_ignorada(y).
palabra_ignorada(en).
palabra_ignorada(con).
palabra_ignorada(por).

% Generar el itinerario basándose en la frase
generar_itinerario_por_frase(Frase) :-
    atomic_list_concat(Palabras, ' ', Frase),
    exclude(palabra_ignorada, Palabras, PalabrasClave),
    buscar_actividades_unicas(PalabrasClave, ActividadesUnicas),
    mostrar_actividades(ActividadesUnicas).

% Buscar actividades eliminando duplicados
buscar_actividades_unicas(PalabrasClave, ActividadesUnicas) :-
    findall(
        Actividad,
        (
            member(Palabra, PalabrasClave),
            (
                actividad(Actividad, _, _, Descripcion, Tipos),
                (sub_atom(Descripcion, _, _, _, Palabra); member(Palabra, Tipos))
            ;
                destino(Destino, _),
                asociar_actividad(Destino, Actividad),
                sub_atom(Destino, _, _, _, Palabra)
            )
        ),
        TodasActividades
    ),
    list_to_set(TodasActividades, ActividadesUnicas).


% Mostrar actividades al usuario
mostrar_actividades([]) :-
    write('No se encontraron actividades que coincidan con la frase ingresada.'), nl.
mostrar_actividades(Actividades) :-
    write('Actividades recomendadas:'), nl,
    listar_actividades(Actividades).

% Lista las actividades encontradas
listar_actividades([]).
listar_actividades([Actividad|Resto]) :-
    actividad(Actividad, Precio, Duracion, Descripcion, Tipos),
    format('~w - Precio: ~w, Duración: ~w horas, Descripción: ~w, Tipos: ~w~n',
           [Actividad, Precio, Duracion, Descripcion, Tipos]),
    listar_actividades(Resto).


% Estadísticas
mostrar_estadisticas :-
    nl, write('=== ESTADÍSTICAS DEL SISTEMA ==='), nl,
    ciudades_mas_actividades,
    actividad_mas_cara,
    actividad_menor_duracion,
    categoria_mas_actividades.

ciudades_mas_actividades :-
    write('Top 3 ciudades con más actividades:'), nl,
    findall(Dest-Cant,
            (destino(Dest, _),
             aggregate_all(count, asociar_actividad(Dest, _), Cant)),
            Pares),
    sort(2, @>=, Pares, Ordenados),
    take(3, Ordenados, Top3),
    mostrar_top3_ciudades(Top3).

actividad_mas_cara :-
    write('Actividad más cara:'), nl,
    findall(Costo-Nombre,
            actividad(Nombre, Costo, _, _, _),
            Pares),
    max_member(MaxCosto-ActCara, Pares),
    actividad(ActCara, MaxCosto, Dur, Desc, _),
    write('Nombre: '), write(ActCara), nl,
    write('Costo: $'), write(MaxCosto), nl,
    write('Duración: '), write(Dur), write(' días'), nl,
    write('Descripción: '), write(Desc), nl.

actividad_menor_duracion :-
    write('Actividad de menor duración:'), nl,
    findall(Dur-Nombre,
            actividad(Nombre, _, Dur, _, _),
            Pares),
    min_member(MinDur-ActCorta, Pares),
    actividad(ActCorta, Costo, MinDur, Desc, _),
    write('Nombre: '), write(ActCorta), nl,
    write('Costo: $'), write(Costo), nl,
    write('Duración: '), write(MinDur), write(' días'), nl,
    write('Descripción: '), write(Desc), nl.

categoria_mas_actividades :-
    write('Categoría con más actividades:'), nl,
    findall(Tipo-1,
            (actividad(_, _, _, _, Tipos),
             member(Tipo, Tipos)),
            ParesPlanos),
    group_pairs_by_key(ParesPlanos, Agrupados),
    maplist(sumar_valores, Agrupados, Contados),
    max_member(Cant-Tipo, Contados),
    write('Categoría: '), write(Tipo), nl,
    write('Cantidad de actividades: '), write(Cant), nl.


%Ordenar actividades por duración
:- dynamic ordenar_por_duracion/2.
:- dynamic ordenar_por_variedad_tipos/2.
ordenar_por_duracion(Actividades, Ordenadas) :-
    % Ordena las actividades por su duración (tercer elemento del término act/4)
    sort(3, @=<, Actividades, Ordenadas).

% Ordenar actividades por variedad de tipos
ordenar_por_variedad_tipos(Actividades, Ordenadas) :-
    % Calcula la puntuación de variedad para cada actividad
    findall(Puntuacion-Actividad,
            (member(Actividad, Actividades),
             Actividad = act(_, _, _, Tipos),
             calcular_puntuacion_variedad(Tipos, Puntuacion)),
            PuntuacionesActividades),
    % Ordena por puntuación y extrae solo las actividades
    sort(1, @>=, PuntuacionesActividades, OrdenadosPorPuntuacion),
    findall(Act, member(_-Act, OrdenadosPorPuntuacion), Ordenadas).

% Calcula una puntuación basada en la variedad de tipos
calcular_puntuacion_variedad(Tipos, Puntuacion) :-
    % Obtiene todos los tipos únicos de todas las actividades
    findall(T,
            (actividad(_, _, _, _, TodosTipos),
             member(T, TodosTipos)),
            TodosLosTipos),
    sort(TodosLosTipos, TiposUnicos),
    % Calcula cuántos tipos diferentes tiene esta actividad
    intersection(Tipos, TiposUnicos, TiposComunes),
    length(TiposComunes, CantidadTipos),
    % Añade puntos extra por tipos que tienen relaciones
    findall(1,
            (member(T1, Tipos),
             member(T2, Tipos),
             T1 \= T2,
             categorias_relacionadas(T1, T2)),
            PuntosRelaciones),
    length(PuntosRelaciones, PuntosExtra),
    Puntuacion is CantidadTipos + PuntosExtra.

% Ejemplo de uso en seleccionar_actividades_dias
seleccionar_actividades_dias([], _, _, Acc, Acc).
seleccionar_actividades_dias([Act|Resto], MaxDias, Categoria, Acc, Resultado) :-
    Act = act(_, _, Duracion, Tipos),
    suma_duracion(Acc, DuracionAcc),
    NuevaDuracion is Duracion + DuracionAcc,
    NuevaDuracion =< MaxDias,
    prioridad_categoria(Tipos, Categoria, Prioridad),
    Prioridad > 0,
    !,
    seleccionar_actividades_dias(Resto, MaxDias, Categoria, [Act|Acc], Resultado).
seleccionar_actividades_dias([_|Resto], MaxDias, Categoria, Acc, Resultado) :-
    seleccionar_actividades_dias(Resto, MaxDias, Categoria, Acc, Resultado).

% Función auxiliar para sumar la duración de las actividades
suma_duracion([], 0).
suma_duracion([act(_, _, Duracion, _)|Resto], Total) :-
    suma_duracion(Resto, SubTotal),
    Total is SubTotal + Duracion.

% Iniciar el programa
%:- menu_principal.




















