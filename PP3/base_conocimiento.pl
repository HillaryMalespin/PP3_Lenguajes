% base_conocimiento.pl

% Definición de destinos
destino(paris, 'Ciudad de la Luz').
destino(nueva_york, 'La Gran Manzana').
destino(londres, 'La Capital del Reino Unido').
destino(tokio, 'La Capital de Japón').
destino(sidney, 'La Ciudad de la Opera').
destino(barcelona, 'La Ciudad Condal').
destino(roma, 'La Ciudad Eterna').
destino(madrid, 'La Capital de España').
destino(bogota, 'La Capital de Colombia').
destino(cancun, 'El Paraíso Mexicano').
destino(san_josé, 'La Capital de Costa Rica').
destino(lima, 'La Ciudad de los Reyes').
destino(buenos_aires, 'La Capital Argentina').
destino(santiago, 'La Capital Chilena').
destino(cali, 'La Sucursal del Cielo').
destino(quito, 'La Capital de Ecuador').
destino(asuncion, 'La Capital de Paraguay').
destino(montevideo, 'La Capital Uruguaya').
destino(caracas, 'La Capital de Venezuela').
destino(san_salvador, 'La Capital de El Salvador').
destino(teguigalpa, 'La Capital de Honduras').

% Definición de actividades
actividad(museo_louvre, 25, 2, 'Visitar el Museo del Louvre', ['arte', 'historia']).
actividad(paseo_en_bici, 30, 3, 'Paseo en bicicleta por la ciudad', ['aventura', 'naturaleza']).
actividad(vista_torre_eiffel, 20, 1, 'Subir a la Torre Eiffel', ['romántico', 'panorama']).
actividad(visita_central_park, 15, 1, 'Recorrer Central Park', ['naturaleza', 'relajación']).
actividad(asistir_obra_teatro, 50, 2, 'Asistir a una obra de teatro en Broadway', ['cultura', 'entretenimiento']).
actividad(visita_stonehenge, 40, 1, 'Visitar Stonehenge', ['historia', 'cultura']).
actividad(tour_gastronomico, 60, 3, 'Tour gastronómico por la ciudad', ['gastronomía', 'cultura']).
actividad(paseo_en_barco, 70, 2, 'Paseo en barco por el río Sena', ['romántico', 'naturaleza']).
actividad(clase_cocina, 55, 3, 'Clase de cocina francesa', ['gastronomía', 'experiencia']).
actividad(visita_museo_arte_moderno, 30, 2, 'Visitar el Museo de Arte Moderno', ['arte', 'cultura']).
actividad(tour_historia_paris, 45, 3, 'Tour histórico de París', ['historia', 'cultura']).
actividad(paseo_nocturno, 35, 2, 'Paseo nocturno por París', ['romántico', 'entretenimiento']).
actividad(viaje_roma_antigua, 50, 2, 'Recorrido por la Roma antigua', ['historia', 'cultura']).
actividad(visitar_coliseo, 20, 1, 'Visitar el Coliseo', ['historia', 'cultura']).
actividad(clase_bailes_latinos, 40, 3, 'Clase de bailes latinos', ['cultura', 'entretenimiento']).
actividad(explorar_tokio, 30, 2, 'Explorar los templos de Tokio', ['cultura', 'naturaleza']).
actividad(tour_sidney_opera, 45, 3, 'Tour por la Ópera de Sídney', ['cultura', 'entretenimiento']).
actividad(viaje_safari, 100, 5, 'Safari en África', ['aventura', 'naturaleza']).
actividad(cata_vinos, 60, 4, 'Cata de vinos en Mendoza', ['gastronomía', 'experiencia']).
actividad(surf_en_cancun, 80, 4, 'Clases de surf en Cancún', ['aventura', 'naturaleza']).
actividad(trekking_en_andes, 90, 6, 'Trekking en los Andes', ['aventura', 'naturaleza']).
actividad(vuelo_en_globo, 120, 3, 'Vuelo en globo sobre Capadocia', ['aventura', 'panorama']).

% Asociación de actividades con destinos
asociar_actividad(paris, museo_louvre).
asociar_actividad(paris, paseo_en_bici).
asociar_actividad(paris, vista_torre_eiffel).
asociar_actividad(nueva_york, asistencia_obra_teatro).
asociar_actividad(nueva_york, visita_central_park).
asociar_actividad(londres, visita_stonehenge).
asociar_actividad(londres, tour_gastronomico).
asociar_actividad(tokio, explorar_tokio).
asociar_actividad(tokio, clase_cocina).
asociar_actividad(sidney, tour_sidney_opera).
asociar_actividad(barcelona, visita_museo_arte_moderno).
asociar_actividad(roma, viaje_roma_antigua).
asociar_actividad(roma, visitar_coliseo).
asociar_actividad(cancun, surf_en_cancun).
asociar_actividad(cancun, paseo_en_barco).
asociar_actividad(bogota, cata_vinos).
asociar_actividad(madrid, tour_historia_paris).
asociar_actividad(buenos_aires, clase_bailes_latinos).
asociar_actividad(lima, tour_gastronomico).
asociar_actividad(santiago, paseo_nocturno).
asociar_actividad(montevideo, cata_vinos).
asociar_actividad(asuncion, exploracion_naturaleza).

% Reglas de inferencia
relacion(arte, cultura).
relacion(historia, arquitectura).
relacion(panorama, diversion).
relacion(romantico, gastronomia).
relacion(naturaleza, educativo).
relacion(experiencia, aventura).
