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










