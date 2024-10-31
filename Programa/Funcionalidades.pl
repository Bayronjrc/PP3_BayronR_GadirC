% Programa principal del sistema de itinerario
:- consult('bc.pl').
:- use_module(library(readutil)). 


% menu_principal/0
% Objetivo: Muestra y gestiona el menú principal del sistema de itinerario
% Entrada: None
% Salida: 
%   - Muestra opciones del menú
%   - Procesa la selección del usuario
% Restricciones:
%   - La entrada del usuario debe ser un número entre 1 y 9
% Comportamiento:
%   - Usa repeat para mantener el menú activo hasta que se seleccione salir (9)
%   - Limpia la pantalla y muestra opciones numeradas
%   - Lee y procesa la opción seleccionada
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

% procesar_opcion/1
% Objetivo: Ejecuta la funcionalidad correspondiente a la opción seleccionada
% Entrada: 
%   - Opcion: Número entero entre 1 y 9 que representa la selección del usuario
% Salida: Ejecuta el predicado correspondiente a la opción seleccionada
% Restricciones:
%   - Requiere que existan todos los predicados referenciados:
%     * menu_agregar_hechos/0
%     * consulta_destino/0
%     * actividades_por_tipo/0
%     * consulta_por_precio/0
%     * itinerario_por_monto/0
%     * itinerario_por_dias/0
%     * recomendar_por_frase/0
%     * mostrar_estadisticas/0
procesar_opcion(1) :- menu_agregar_hechos.
procesar_opcion(2) :- consulta_destino.
procesar_opcion(3) :- actividades_por_tipo.
procesar_opcion(4) :- consulta_por_precio.
procesar_opcion(5) :- itinerario_por_monto.
procesar_opcion(6) :- itinerario_por_dias.
procesar_opcion(7) :- recomendar_por_frase.
procesar_opcion(8) :- mostrar_estadisticas.
procesar_opcion(9) :- write('�Hasta luego!'), nl.





% menu_agregar_hechos
% Objetivo: Despliega un menú para permitir al usuario seleccionar y ejecutar acciones 
%           de agregar hechos en el sistema, como destinos, actividades y asociaciones.
% Entradas: Ninguna (se basa en la entrada del usuario para seleccionar una opción).
% Salidas: Ninguna directa; el flujo continúa según la selección del usuario.
% Restricciones: La opción debe ser un número entre 1 y 4.
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

% procesar_opcion_agregar/1
% Objetivo: Procesa la opción seleccionada en el menú de agregar hechos, llamando a 
%           la función correspondiente o retornando al menú principal.
% Entradas: Opcion (número que representa la elección del usuario en el menú).
% Salidas: Ninguna directa; ejecuta la acción según la opción seleccionada.
% Restricciones: Opción debe ser un número entre 1 y 4; opciones fuera de este rango 
%                muestran un mensaje de error y vuelven al menú.
procesar_opcion_agregar(1) :- !, agregar_destino.
procesar_opcion_agregar(2) :- !, agregar_actividad.
procesar_opcion_agregar(3) :- !, agregar_asociacion.
procesar_opcion_agregar(4) :- !.
procesar_opcion_agregar(_) :-
    write('Opcion invalida. Intente nuevamente.'), nl,
    menu_agregar_hechos.

% agregar_destino
% Objetivo: Solicita al usuario ingresar el nombre y descripción de un destino y lo 
%           almacena como hecho en la base de datos.
% Entradas: Nombre (átomo que representa el nombre del destino) y Descripcion (cadena 
%           en comillas simples con la descripción del destino).
% Salidas: Un mensaje confirmando que el destino fue agregado exitosamente.
% Restricciones: Nombre debe ser un átomo y Descripcion una cadena de texto.
agregar_destino :-
    write('Ingrese el nombre del destino (atomo): '),
    read(Nombre),
    write('Ingrese la descripcion (entre comillas simples): '),
    read(Descripcion),
    assertz(destino(Nombre, Descripcion)),
    write('Destino agregado exitosamente.'), nl.

% agregar_actividad
% Objetivo: Solicita al usuario ingresar los datos de una actividad y la almacena en la 
%           base de datos con sus detalles, incluyendo nombre, costo, duración, descripción y tipos.
% Entradas:
%   - Nombre: átomo que representa el nombre de la actividad.
%   - Costo: número que representa el costo de la actividad.
%   - Duracion: número que representa la duración de la actividad en días.
%   - Descripcion: cadena en comillas simples que describe la actividad.
%   - Tipos: lista de átomos que categoriza la actividad (por ejemplo, [''arte'', ''historia'']).
% Salidas: Un mensaje que confirma que la actividad fue agregada exitosamente.
% Restricciones:
%   - Nombre debe ser un átomo.
%   - Costo y Duracion deben ser números.
%   - Descripcion debe ser una cadena en comillas simples.
%   - Tipos debe ser una lista de átomos.
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

% agregar_asociacion
% Objetivo: Solicita al usuario los nombres de un destino y una actividad, y crea una 
%           asociación entre ambos, almacenándola en la base de datos.
% Entradas:
%   - Destino: átomo que representa el nombre del destino.
%   - Actividad: átomo que representa el nombre de la actividad a asociar con el destino.
% Salidas: Un mensaje que confirma que la asociación fue agregada exitosamente.
% Restricciones:
%   - Destino y Actividad deben ser átomos.
agregar_asociacion :-
    write('Ingrese el nombre del destino (atomo): '),
    read(Destino),
    write('Ingrese el nombre de la actividad (atomo): '),
    read(Actividad),
    assertz(asociar_actividad(Destino, Actividad)),
    write('Asociación agregada exitosamente.'), nl.

% consulta_destino
% Objetivo: Consulta y muestra la información detallada de un destino específico, 
%           incluyendo su descripción, actividades asociadas y totales de costo y duración.
% Entradas: Destino (átomo que representa el nombre del destino a consultar).
% Salidas: Información en pantalla con la descripción del destino, lista de actividades 
%          asociadas y totales calculados.
% Restricciones: Destino debe ser un átomo y existir en la base de datos.
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

% mostrar_actividades_destino/1
% Objetivo: Muestra en pantalla la lista de actividades asociadas a un destino dado, 
%           detallando su costo, duración, descripción y tipos.
% Entradas: Destino (átomo que representa el nombre del destino).
% Salidas: Lista de actividades en pantalla con sus detalles.
% Restricciones: Destino debe ser un átomo que tenga actividades asociadas en la base de datos.
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

% calcular_totales_destino/1
% Objetivo: Calcula y muestra en pantalla los totales de costo y duración de todas 
%           las actividades asociadas a un destino dado.
% Entradas: Destino (átomo que representa el nombre del destino).
% Salidas: Costo total y duración total de las actividades asociadas, mostrados en pantalla.
% Restricciones: Destino debe ser un átomo y tener actividades asociadas en la base de datos.
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

% actividades_por_tipo
% Objetivo: Solicita al usuario un tipo de actividad y muestra todas las actividades 
%           de ese tipo, junto con sus detalles.
% Entradas: Tipo (átomo que representa la categoría de actividad a consultar).
% Salidas: Lista de actividades en pantalla con sus costos, duración, descripción y 
%          los destinos donde están disponibles.
% Restricciones: Tipo debe ser un átomo válido y existir en la lista de tipos de alguna actividad.
actividades_por_tipo :-
    write('Ingrese el tipo de actividad a consultar: '),
    read(Tipo),
    nl, write('=== ACTIVIDADES DE TIPO '), write(Tipo), write(' ==='), nl,
    buscar_actividades_tipo(Tipo).

% buscar_actividades_tipo/1
% Objetivo: Busca y muestra en pantalla todas las actividades que corresponden a un 
%           tipo específico, incluyendo su costo, duración, descripción y destinos asociados.
% Entradas: Tipo (átomo que representa el tipo de actividad).
% Salidas: Información de cada actividad en pantalla, con sus detalles y destinos.
% Restricciones: Tipo debe existir en la lista de tipos de alguna actividad; 
%                si no existen actividades de ese tipo, no se muestra ningún resultado.
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

% consulta_por_precio
% Objetivo: Solicita al usuario un monto de referencia y una opción para buscar actividades
%           con un costo mayor o menor que dicho monto, y muestra las actividades encontradas.
% Entradas:
%   - Monto: número que representa el monto de referencia.
%   - Opcion: número (1 para actividades más baratas, 2 para más caras).
% Salidas: Lista de actividades en pantalla que cumplen con el criterio de costo seleccionado.
% Restricciones:
%   - Monto debe ser un número.
%   - Opcion debe ser 1 o 2.
consulta_por_precio :-
    write('Ingrese el monto de referencia: '),
    read(Monto),
    write('¿Desea ver actividades mas baratas (1) o mas caras (2)?: '),
    read(Opcion),
    nl, write('=== ACTIVIDADES ENCONTRADAS ==='), nl,
    mostrar_actividades_precio(Monto, Opcion).

% mostrar_actividades_precio/2
% Objetivo: Filtra y muestra las actividades en función de su costo, comparándolo con el 
%           monto de referencia y según la opción seleccionada (más baratas o más caras).
% Entradas:
%   - Monto: número que representa el monto de referencia.
%   - Opcion: número (1 para actividades con costo <= Monto, 2 para costo >= Monto).
% Salidas: Información de cada actividad que cumple con el criterio, mostrada en pantalla 
%          con su nombre, costo, duración, descripción y tipos.
% Restricciones:
%   - Monto debe ser un número.
%   - Opcion debe ser 1 o 2.
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

% itinerario_por_monto
% Objetivo: Solicita al usuario un presupuesto máximo, categoría de preferencia, cantidad de personas 
%           y preferencia por estancias largas, y genera un itinerario de actividades basado en estos 
%           parámetros.
% Entradas:
%   - MontoMax: número que representa el monto máximo disponible para el itinerario.
%   - Categoria: átomo que indica la categoría de actividad preferida.
%   - NumPersonas: número entero que indica la cantidad de personas en el itinerario.
%   - PrefLargas: átomo ('s' o 'n') que indica si se prefieren actividades largas.
% Salidas: Muestra en pantalla el itinerario generado.
% Restricciones:
%   - MontoMax y NumPersonas deben ser números positivos.
%   - Categoria debe ser una categoría válida en la lista de tipos de actividad.
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

% generar_itinerario_monto/5
% Objetivo: Genera un itinerario de actividades dentro del presupuesto y las preferencias del usuario.
% Entradas:
%   - MontoMax: número que representa el monto máximo disponible.
%   - Categoria: categoría de actividad preferida.
%   - NumPersonas: número de personas para calcular el costo total.
%   - PrefLargas: preferencia por estancias largas ('s' o 'n').
% Salidas:
%   - Itinerario: lista de actividades que cumplen con las condiciones del usuario.
% Restricciones:
%   - Las actividades en el itinerario deben cumplir con el presupuesto y las preferencias de categoría y duración.

generar_itinerario_monto(MontoMax, Categoria, NumPersonas, PrefLargas, Itinerario) :-
    findall(act(Nombre, Costo, Duracion, Tipos),
            actividad(Nombre, Costo, Duracion, _, Tipos),
            TodasActividades),
    ordenar_por_preferencia(TodasActividades, PrefLargas, ActividadesOrdenadas),
    seleccionar_actividades_monto(ActividadesOrdenadas, MontoMax, NumPersonas,
                                 Categoria, [], Itinerario).

% seleccionar_actividades_monto/6
% Objetivo: Selecciona actividades de una lista ordenada que cumplan con el presupuesto y la categoría preferida.
% Entradas:
%   - ActividadesOrdenadas: lista de actividades ordenadas por preferencia.
%   - MontoMax: número que representa el presupuesto máximo disponible.
%   - NumPersonas: número de personas para ajustar el costo.
%   - Categoria: categoría de actividad preferida.
%   - Acc: acumulador para construir el itinerario.
% Salidas:
%   - Resultado: lista final de actividades que cumplen con el presupuesto y preferencias.
% Restricciones:
%   - Costo total de las actividades seleccionadas no debe superar MontoMax.
%   - Las actividades deben coincidir con la categoría preferida.
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

% itinerario_por_dias
% Objetivo: Genera un itinerario de actividades que se ajuste a una cantidad máxima de días y
%           una categoría de preferencia. Permite al usuario generar un segundo itinerario con
%           un criterio de selección alternativo.
% Entradas:
%   - MaxDias: número entero que representa el límite de días para el itinerario.
%   - Categoria: átomo que indica la categoría preferida de actividad.
% Salidas: Muestra en pantalla el itinerario generado, con opción de mostrar un segundo itinerario
%          basado en un criterio distinto de selección.
% Restricciones:
%   - MaxDias debe ser un número entero positivo.
%   - Categoria debe ser un tipo válido en la lista de tipos de actividad.
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

% generar_itinerario_dias/4
% Objetivo: Genera un itinerario de actividades que cumple con una cantidad máxima de días,
%           considerando un criterio de ordenación para priorizar duración o variedad de tipos.
% Entradas:
%   - MaxDias: número entero que representa el límite de días.
%   - Categoria: categoría preferida de actividad.
%   - Criterio: número (1 para priorizar duración, 2 para priorizar variedad de tipos).
% Salidas:
%   - Itinerario: lista de actividades seleccionadas que cumplen con las restricciones de días.
% Restricciones:
%   - MaxDias debe ser positivo y limitarse a la cantidad de días acumulados de las actividades seleccionadas.
%   - Categoria debe coincidir con los tipos de actividades elegidas.
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

 
% recomendar_por_frase
% Objetivo: Permite al usuario ingresar una frase de búsqueda y genera recomendaciones de actividades 
%           relacionadas con los términos de la frase, excluyendo palabras irrelevantes.
% Entradas:
%   - Frase de búsqueda ingresada por el usuario como string.
% Salidas: Muestra en pantalla una lista de actividades recomendadas, basadas en la frase.
% Restricciones:
%   - La frase debe contener palabras relevantes que coincidan con descripciones o tipos de actividades.
recomendar_por_frase :-
    write('Ingrese su frase de búsqueda: '),
    read_pending_input(user_input, _, []), % Consume cualquier entrada residual
    read_line_to_string(user_input, Frase),
    generar_itinerario_por_frase(Frase).

% palabra_ignorada/1
% Objetivo: Define palabras irrelevantes (artículos, preposiciones, etc.) que deben excluirse al 
%           procesar la frase de búsqueda.
% Entradas: Átomo representando una palabra irrelevante.
% Salidas: Verdadero si la palabra está en la lista de palabras ignoradas.
palabra_ignorada(el).
palabra_ignorada(la).
palabra_ignorada(los).
palabra_ignorada(las).
palabra_ignorada(de).
palabra_ignorada(y).
palabra_ignorada(en).
palabra_ignorada(con).
palabra_ignorada(por).

% generar_itinerario_por_frase/1
% Objetivo: Genera un itinerario de actividades basado en palabras clave extraídas de la frase
%           ingresada, excluyendo palabras irrelevantes.
% Entradas:
%   - Frase: string ingresado por el usuario.
% Salidas: Llama a mostrar_actividades con la lista de actividades relevantes encontradas.
% Restricciones:
%   - La frase debe dividirse en palabras clave, excluyendo aquellas definidas en palabra_ignorada/1.
generar_itinerario_por_frase(Frase) :-
    atomic_list_concat(Palabras, ' ', Frase),
    exclude(palabra_ignorada, Palabras, PalabrasClave),
    buscar_actividades_unicas(PalabrasClave, ActividadesUnicas),
    mostrar_actividades(ActividadesUnicas).

% buscar_actividades_unicas/2
% Objetivo: Encuentra actividades que coinciden con al menos una palabra clave en sus descripciones o tipos, 
%           eliminando duplicados para asegurar una lista única de actividades recomendadas.
% Entradas:
%   - PalabrasClave: lista de palabras relevantes derivadas de la frase de búsqueda.
% Salidas:
%   - ActividadesUnicas: lista sin duplicados de actividades que coinciden con las palabras clave.
% Restricciones:
%   - Las palabras clave deben coincidir con los atributos de actividad o destino.
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


% mostrar_actividades/1
% Objetivo: Muestra las actividades encontradas en base a las palabras clave ingresadas. 
%           Si no se encuentran actividades, muestra un mensaje informativo.
% Entradas:
%   - Actividades: lista de actividades relevantes encontradas.
% Salidas: Muestra en pantalla las actividades o un mensaje si no hay coincidencias.
mostrar_actividades([]) :-
    write('No se encontraron actividades que coincidan con la frase ingresada.'), nl.
mostrar_actividades(Actividades) :-
    write('Actividades recomendadas:'), nl,
    listar_actividades(Actividades).

% listar_actividades/1
% Objetivo: Imprime en pantalla las actividades encontradas junto a sus atributos (precio, duración, etc.).
% Entradas:
%   - Actividades: lista de actividades relevantes.
% Salidas: Muestra los detalles de cada actividad en pantalla.
listar_actividades([]).
listar_actividades([Actividad|Resto]) :-
    actividad(Actividad, Precio, Duracion, Descripcion, Tipos),
    format('~w - Precio: ~w, Duración: ~w horas, Descripción: ~w, Tipos: ~w~n',
           [Actividad, Precio, Duracion, Descripcion, Tipos]),
    listar_actividades(Resto).


% mostrar_estadisticas
% Objetivo: Presenta un resumen estadístico de las actividades en el sistema, mostrando 
%           ciudades con más actividades, actividad más cara, actividad de menor duración y
%           la categoría con más actividades.
% Entradas: Ninguna.
% Salidas: Imprime en pantalla los datos estadísticos calculados.
mostrar_estadisticas :-
    nl, write('=== ESTADÍSTICAS DEL SISTEMA ==='), nl,
    ciudades_mas_actividades,
    actividad_mas_cara,
    actividad_menor_duracion,
    categoria_mas_actividades.

% ciudades_mas_actividades
% Objetivo: Encuentra y muestra las 3 ciudades que ofrecen más actividades.
% Entradas: Ninguna.
% Salidas: Imprime en pantalla las 3 ciudades con mayor cantidad de actividades.
% Restricciones: Ordena las ciudades en orden descendente según el conteo de actividades.
ciudades_mas_actividades :-
    write('Top 3 ciudades con más actividades:'), nl,
    findall(Dest-Cant,
            (destino(Dest, _),
             aggregate_all(count, asociar_actividad(Dest, _), Cant)),
            Pares),
    sort(2, @>=, Pares, Ordenados),
    take(3, Ordenados, Top3),
    mostrar_top3_ciudades(Top3).

% actividad_mas_cara
% Objetivo: Encuentra y muestra la actividad con el costo más alto en el sistema.
% Entradas: Ninguna.
% Salidas: Imprime en pantalla el nombre, costo, duración y descripción de la actividad más cara.
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

% actividad_menor_duracion
% Objetivo: Encuentra y muestra la actividad de menor duración en el sistema.
% Entradas: Ninguna.
% Salidas: Imprime en pantalla el nombre, costo, duración y descripción de la actividad con menor duración.
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

% categoria_mas_actividades
% Objetivo: Encuentra y muestra la categoría que contiene la mayor cantidad de actividades.
% Entradas: Ninguna.
% Salidas: Imprime en pantalla la categoría y la cantidad de actividades que tiene.
% Restricciones: Agrupa las actividades por tipo y cuenta cuántas actividades pertenecen a cada tipo.
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

% Funciones auxiliares
% sumar_valores/2
% Objetivo: Calcula la cantidad de elementos en una lista de valores asociados a un tipo.
% Entradas: Tipo-Valores, donde Valores es una lista de elementos asociados al tipo.
% Salidas: Cantidad-Tipo, donde Cantidad es la cantidad de elementos en Valores.
sumar_valores(Tipo-Valores, Cantidad-Tipo) :-
    length(Valores, Cantidad).

% take/3
% Objetivo: Toma los primeros N elementos de una lista.
% Entradas: N (número de elementos), List (lista de entrada).
% Salidas: Taken (lista de los primeros N elementos de List).
take(N, List, Taken) :-
    length(Taken, N),
    append(Taken, _, List).

% mostrar_top3_ciudades/1
% Objetivo: Muestra en pantalla el top 3 de ciudades con el conteo de actividades.
% Entradas: Lista de pares Ciudad-Cant.
% Salidas: Imprime cada ciudad y el número de actividades en pantalla.
mostrar_top3_ciudades([]).
mostrar_top3_ciudades([Ciudad-Cant|Resto]) :-
    write(Ciudad), write(': '), write(Cant), write(' actividades'), nl,
    mostrar_top3_ciudades(Resto).

% prioridad_categoria/3
% Objetivo: Asigna una prioridad a la categoría deseada en relación a las categorías de una actividad.
% Entradas: Tipos (categorías de la actividad), Categoria (preferida por el usuario).
% Salidas: Devuelve la prioridad (4-0) en función de la relación entre Tipos y Categoria.
prioridad_categoria(Tipos, Categoria, 4) :- % Categoría igual
    member(Categoria, Tipos), !.
prioridad_categoria(Tipos, Categoria, 3) :- % Categoría afín
    member(Tipo, Tipos),
    categorias_relacionadas(Tipo, Categoria), !.
prioridad_categoria(Tipos, Categoria, 2) :- % Categoría adicional igual
    member(Tipo, Tipos),
    actividad(_, _, _, _, OtrosTipos),
    member(Tipo, OtrosTipos),
    member(Categoria, OtrosTipos), !.
prioridad_categoria(Tipos, Categoria, 1) :- % Categoría adicional afín
    member(Tipo, Tipos),
    actividad(_, _, _, _, OtrosTipos),
    member(Tipo, OtrosTipos),
    member(OtroTipo, OtrosTipos),
    categorias_relacionadas(OtroTipo, Categoria), !.
prioridad_categoria(_, _, 0).

% ordenar_por_preferencia/3
% Objetivo: Ordena actividades en función de la preferencia de duración larga o corta.
% Entradas: Actividades (lista de actividades), Preferencia ('s' para largas, 'n' para cortas).
% Salidas: Ordenadas, lista de actividades ordenadas por duración.
ordenar_por_preferencia(Actividades, s, Ordenadas) :-
    sort(3, @>=, Actividades, Ordenadas).
ordenar_por_preferencia(Actividades, n, Ordenadas) :-
    sort(3, @=<, Actividades, Ordenadas).

% suma_costos/3
% Objetivo: Calcula el costo total de una lista de actividades para un número dado de personas.
% Entradas: Lista de actividades, NumPersonas (cantidad de personas).
% Salidas: Total (suma del costo de todas las actividades para NumPersonas).
suma_costos([], _, 0).
suma_costos([act(_, Costo, _, _)|Resto], NumPersonas, Total) :-
    suma_costos(Resto, NumPersonas, SubTotal),
    Total is SubTotal + (Costo * NumPersonas).

% mostrar_itinerario/1
% Objetivo: Muestra el itinerario de actividades y sus totales, o un mensaje si está vacío
% Entrada: 
%   - Lista de actividades (puede estar vacía)
%   - Cada actividad tiene formato: act(Nombre, Costo, Duracion, Tipos)  
% Salida:
%   - Imprime mensaje si lista está vacía
%   - O imprime detalles de actividades y totales
% Restricciones: Ninguna
mostrar_itinerario([]) :-
    write('No se encontraron actividades que cumplan con los criterios.'), nl.
mostrar_itinerario(Itinerario) :-
    write('Actividades seleccionadas:'), nl,
    mostrar_actividades_itinerario(Itinerario),
    calcular_totales_itinerario(Itinerario).

% mostrar_actividades_itinerario/1
% Objetivo: Muestra los detalles de cada actividad en el itinerario de forma recursiva
% Entrada:
%   - Lista de actividades con formato act(Nombre, Costo, Duracion, Tipos)
% Salida: 
%   - Imprime nombre, costo, duración y tipos de cada actividad
% Restricciones:
%   - La lista debe contener actividades en el formato correcto
mostrar_actividades_itinerario([]).
mostrar_actividades_itinerario([act(Nombre, Costo, Duracion, Tipos)|Resto]) :-
    write('- '), write(Nombre), nl,
    write('  Costo: $'), write(Costo), nl,
    write('  Duración: '), write(Duracion), write(' días'), nl,
    write('  Tipos: '), write(Tipos), nl,
    mostrar_actividades_itinerario(Resto).

% calcular_totales_itinerario/1
% Objetivo: Calcula y muestra los totales de costo y duración del itinerario
% Entrada:
%   - Lista de actividades con formato act(Nombre, Costo, Duracion, Tipos)
% Salida:
%   - Imprime costo total y duración total del itinerario
% Restricciones:
%   - Requiere que las actividades tengan valores numéricos válidos para costo y duración
%   - Utiliza predicados findall/3 y sum_list/2 para los cálculos
calcular_totales_itinerario(Itinerario) :-
    findall(Costo, member(act(_, Costo, _, _), Itinerario), Costos),
    findall(Duracion, member(act(_, _, Duracion, _), Itinerario), Duraciones),
    sum_list(Costos, CostoTotal),
    sum_list(Duraciones, DuracionTotal),
    nl, write('=== TOTALES ==='), nl,
    write('Costo total: $'), write(CostoTotal), nl,
    write('Duración total: '), write(DuracionTotal), write(' días'), nl.

% Predicado auxiliar para búsqueda sin distinción de mayúsculas/minúsculas
% sub_atom_icaso/5
% Objetivo: Busca una subcadena dentro de un átomo ignorando mayúsculas/minúsculas
% Entrada:
%   - Atom: Átomo donde se realizará la búsqueda
%   - Before: Número de caracteres antes de la subcadena
%   - Length: Longitud de la subcadena a buscar
%   - After: Número de caracteres después de la subcadena
%   - SubAtom: Subcadena a buscar
% Salida: True si encuentra la subcadena, False en caso contrario
% Restricciones: 
%   - Todos los parámetros deben ser átomos válidos
%   - Before, Length y After deben ser números no negativos
sub_atom_icaso(Atom, Before, Length, After, SubAtom) :-
    atom_chars(Atom, Chars),
    atom_chars(SubAtom, SubChars),
    maplist(to_lower, Chars, LowerChars),
    maplist(to_lower, SubChars, LowerSubChars),
    append(BeforeChars, Rest, LowerChars),
    length(BeforeChars, Before),
    append(MatchChars, AfterChars, Rest),
    length(AfterChars, After),
    length(MatchChars, Length),
    MatchChars = LowerSubChars.

% to_lower/2
% Objetivo: Convierte un carácter a su equivalente en minúscula
% Entrada: 
%   - Char: Carácter a convertir
%   - LowerChar: Variable que almacenará el carácter convertido
% Salida: Carácter convertido a minúscula
% Restricciones: Char debe ser un carácter válido
to_lower(Char, LowerChar) :-
    atom_chars(CharAtom, [Char]),
    downcase_atom(CharAtom, LowerAtom),
    atom_chars(LowerAtom, [LowerChar]).


% ordenar_por_duracion/2
% Objetivo: Ordena una lista de actividades según su duración de menor a mayor
% Entrada:
%   - Actividades: Lista de actividades con formato act(Nombre, Costo, Duracion, Tipos)
%   - Ordenadas: Variable que almacenará la lista ordenada
% Salida: Lista de actividades ordenada por duración ascendente
% Restricciones: 
%   - Las actividades deben tener el formato correcto
%   - La duración debe ser un valor numérico
:- dynamic ordenar_por_duracion/2.
:- dynamic ordenar_por_variedad_tipos/2.
ordenar_por_duracion(Actividades, Ordenadas) :-
    % Ordena las actividades por su duración (tercer elemento del término act/4)
    sort(3, @=<, Actividades, Ordenadas).

% ordenar_por_variedad_tipos/2
% Objetivo: Ordena actividades según la variedad y relaciones entre sus tipos
% Entrada:
%   - Actividades: Lista de actividades con formato act(Nombre, Costo, Duracion, Tipos)
%   - Ordenadas: Variable que almacenará la lista ordenada
% Salida: Lista de actividades ordenada por puntuación de variedad descendente
% Restricciones:
%   - Requiere que exista el predicado actividad/5 con tipos válidos
%   - Requiere que exista el predicado categorias_relacionadas/2
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

% calcular_puntuacion_variedad/2
% Objetivo: Calcula una puntuación basada en la cantidad y relaciones entre tipos
% Entrada:
%   - Tipos: Lista de tipos de una actividad
%   - Puntuacion: Variable que almacenará la puntuación calculada
% Salida: Puntuación numérica que representa la variedad y relaciones de tipos
% Restricciones:
%   - Requiere predicado actividad/5 para obtener todos los tipos posibles
%   - Requiere predicado categorias_relacionadas/2 para evaluar relaciones
% Cálculo: Puntuación = Cantidad de tipos únicos + Puntos por relaciones entre tipos
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

% seleccionar_actividades_dias/5
% Objetivo: Selecciona actividades que se ajusten a un límite máximo de días y cumplan con 
%           la categoría preferida, acumulándolas en un itinerario.
% Entradas:
%   - ActividadesOrdenadas: lista de actividades ordenadas según el criterio de preferencia.
%   - MaxDias: número máximo de días disponible para el itinerario.
%   - Categoria: categoría preferida de actividad.
%   - Acc: acumulador para construir el itinerario.
% Salidas:
%   - Itinerario: lista final de actividades que cumplen con los días y la categoría preferidos.
% Restricciones:
%   - Duración acumulada de las actividades seleccionadas no debe exceder MaxDias.
%   - Las actividades deben coincidir con la categoría indicada.
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

% suma_duracion/2
% Objetivo: Calcula la suma total de la duración de todas las actividades en una lista
% Entrada:
%   - Lista de actividades con formato act(Nombre, Costo, Duracion, Tipos)
%   - Total: Variable que almacenará la suma de duraciones
% Salida: Suma total de las duraciones
% Restricciones: 
%   - Las actividades deben tener el formato correcto
%   - Duracion debe ser un valor numérico
suma_duracion([], 0).
suma_duracion([act(_, _, Duracion, _)|Resto], Total) :-
    suma_duracion(Resto, SubTotal),
    Total is SubTotal + Duracion.

% Reglas de inferencia sobre categorías (afinidad)
categoria_afin(arte, cultura).
categoria_afin(cultura, arte).
categoria_afin(historia, arquitectura).
categoria_afin(arquitectura, historia).
categoria_afin(panorama, diversion).
categoria_afin(diversion, panorama).
categoria_afin(romantico, gastronomia).
categoria_afin(gastronomia, romantico).
categoria_afin(naturaleza, educativo).
categoria_afin(educativo, naturaleza).
categoria_afin(experiencia, aventura).
categoria_afin(aventura, experiencia).

% Regla para verificar si dos categorías están relacionadas
categorias_relacionadas(Cat1, Cat2) :- 
    categoria_afin(Cat1, Cat2);
    categoria_afin(Cat2, Cat1).

% Iniciar el programa
:- menu_principal.




















