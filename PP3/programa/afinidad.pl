:- dynamic(afinidad/2).

% Afinidades entre categorías
% Cada una de las reglas establece una relacion entre categorías.
afinidad(arte, cultura).
afinidad(cultura, arte).
afinidad(historia, arquitectura).
afinidad(arquitectura, historia).
afinidad(panorama, diversion).
afinidad(diversion, panorama).
afinidad(romantico, gastronomia).
afinidad(gastronomia, romantico).
afinidad(naturaleza, educativo).
afinidad(educativo, naturaleza).
afinidad(experiencia, aventura).
afinidad(aventura, experiencia).