; Integrantes del grupo:
;   Autor 1: ;
;   Autor 2: ;

; -------------------------------------------------------------------------------
; Definición estructura de datos para guardar todos los datos realacionados con
; un spirograph.
; -------------------------------------------------------------------------------
(defstruct spirodata
  (spiros )
    grans
    petits
    (rgran 150)
    (rpetit 50)
    (punt 3)
    inici 
    (escala 1.8)
    interior
    x
    y
    (pas 0.2)
)

;Variable spiro global de la estructura spirodata
(defvar spiro spirodata)

; -------------------------------------------------------------------------------
; TODO: No entiendo que pide en el enunciado xd, yo asumo que es así
; -------------------------------------------------------------------------------
(defun guarda-informacio (grans petits rgran rpetit punt inici escala interior x y pas)
    (setq spiro 
          (make-spirodata
              :grans grans
              :petits petits
              :rgran rgran
              :rpetit rpetit
              :punt punt
              :inici inici
              :escala escala
              :interior interior
              :x x
              :y y
              :pas pas
          )
    )
)

; -------------------------------------------------------------------------------
; Función para cambiar el color del dibujo a rojo y el fondo a blanco.
; -------------------------------------------------------------------------------
(defun vermell ()
    (color 255 0 0 255 255 255)
)

; -------------------------------------------------------------------------------
; Función para cambiar el color del dibujo a azul y el fondo a blanco.
; -------------------------------------------------------------------------------
(defun blau ()
    (color 0 0 255 255 255 255)
)

; -------------------------------------------------------------------------------
; Función para cambiar el color del dibujo a verde y el fondo a blanco.
; -------------------------------------------------------------------------------
(defun verd ()
    (color 0 255 0 255 255 255)
)

; -------------------------------------------------------------------------------
; Función para cambiar el color del dibujo a negro y el fondo a blanco.
; -------------------------------------------------------------------------------
(defun negre ()
    (color 0 0 0 255 255 255)
)

; -------------------------------------------------------------------------------
; Función para dibujar un circulo de n segmentos de radio r y en el punto (x,y).
;
; - Parámetros: 
;   @x - Coordenada x
;   @y - Coordenada y
;   @r - Radio del circulo
;   @n - Número de segmentos en los que se divide el circulo
; -------------------------------------------------------------------------------
(defun cercle (x y r n)
    ; TODO
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del radigran del spiro.
;
; - Parámetros:
;   @r - Nuevo radiogrande del spirograph
; -------------------------------------------------------------------------------
(defun radigran (r)
    (setf (spirodata-radigran spiro) r)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del radipetit del spiro.
;
; - Parámetros:
;   @r - Nuevo radiopequeño del spirograph
; -------------------------------------------------------------------------------
(defun radipetit (r)
    (setf (spirodata-rpetit spiro) r)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del punto del spiro.
;
; - Parámetros:
;   @p - Valor del punto a establecer
; -------------------------------------------------------------------------------
(defun punt (p)
    (setf (spirodata-punt spiro) p)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del inicio del spiro.
;
; - Parámetros:
;   @a - Valor del ángulo a establecer
; -------------------------------------------------------------------------------
(defun inici (a)
    (setf (spirodata-inici spiro) a)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del escalado del spiro.
;
; - Parámetros:
;   @e - Valor del escalado a establecer
; -------------------------------------------------------------------------------
(defun escala (e)
    (setf (spirodata-escala spiro) e)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto de los puntos del spiro.
;
; - Parámetros:
;   @x - Valor del punto x a establecer
;   @y - Valor del punto y a establecer
; -------------------------------------------------------------------------------
(defun posicio (x y)
    (setf (spirodata-x spiro) x)
    (setf (spirodata-y spiro) y)
)

; -------------------------------------------------------------------------------
; -------------------------------------------------------------------------------
(defun reduir (m n)
    ; TODO
)

