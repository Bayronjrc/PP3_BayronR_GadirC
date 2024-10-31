% Base de conocimiento para el sistema de itinerario de viaje

% Las hacemos dinamicas por que si no no se pueden modificar
:- dynamic destino/2.
:- dynamic actividad/5.
:- dynamic asociar_actividad/2.
:- dynamic categoria_afin/2.
:- dynamic categorias_relacionadas/2.

% Destinos
destino(paris, 'Ciudad de la Luz, famosa por su cultura y arquitectura').
destino(nueva_york, 'La Gran Manzana, ciudad que nunca duerme').
destino(roma, 'La Ciudad Eterna, cuna de la civilizacion romana').
destino(tokyo, 'Metropolis moderna que fusiona tradición y tecnologia').
destino(barcelona, 'Ciudad modernista con rica cultura mediterranea').

% Actividades
actividad(museo_louvre, 25, 2, 'Visitar el Museo del Louvre, hogar de la Mona Lisa', ['arte', 'historia']).
actividad(torre_eiffel, 30, 1, 'Subir a la Torre Eiffel y ver la ciudad', ['panorama', 'romantico']).
actividad(central_park, 0, 1, 'Pasear por Central Park', ['naturaleza', 'panorama']).
actividad(times_square, 0, 1, 'Explorar Times Square', ['panorama', 'diversion']).
actividad(coliseo_romano, 20, 1, 'Visitar el Coliseo Romano', ['historia', 'arquitectura']).
actividad(sagrada_familia, 35, 2, 'Visitar la Sagrada Familia', ['arquitectura', 'cultura']).
actividad(monte_fuji, 100, 3, 'Excursion al Monte Fuji', ['aventura', 'naturaleza']).
actividad(tour_gastro_paris, 80, 1, 'Tour gastronomico por Paris', ['gastronomia', 'cultura']).

% Asociaciones entre destinos y actividades
asociar_actividad(paris, museo_louvre).
asociar_actividad(paris, torre_eiffel).
asociar_actividad(paris, tour_gastro_paris).
asociar_actividad(nueva_york, central_park).
asociar_actividad(nueva_york, times_square).
asociar_actividad(roma, coliseo_romano).
asociar_actividad(barcelona, sagrada_familia).
asociar_actividad(tokyo, monte_fuji).

