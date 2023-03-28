; Integrantes del grupo:
;   Autor 1: ;
;   Autor 2: ;

;Variable spiro global
(defvar spiro)

; -------------------------------------------------------------------------------
; Función para inicializar los valores por defecto de un spiro.
; -------------------------------------------------------------------------------
(defun guarda-informacio ()
    (putprop 'spiro '((150 105) (144 96)) grans)
    (putprop 'spiro '((84 35 56)
                      (80 33 53) 
                      (75 31 50) 
                      (72 29 48) 
                      (63 25 42) 
                      (60 23 40) 
                      (56 21 37) 
                      (52 19 35) 
                      (48 17 32) 
                      (45 16 30) 
                      (42 14 28) 
                      (40 13 27) 
                      (32 9 21) 
                      (30 8 20) 
                      (24 5 16)) 
        petits)
    (putprop 'spiro 150 rgran)
    (putprop 'spiro 50 rpetit)
    (putprop 'spiro 3 punt)
    (putprop 'spiro 0 inici)
    (putprop 'spiro 1.8 escala)
    (putprop 'spiro 0 interior)
    (putprop 'spiro 0 x)
    (putprop 'spiro 0 y)
    (putprop 'spiro 0.2 pas)
)

(defun contains (e l)
    (cond ((null l) nil)
          ((atom (car l))
              (cond ((equal e (car l)) t)
                  (t (contains e (cdr l)))
             )
          )
          (t (contains e (car l)))
    )
)

; -------------------------------------------------------------------------------
; Función para cambiar el color del dibujo a rojo y el fondo a blanco.
; -------------------------------------------------------------------------------
(defun vermell ()
    (color 255 0 0 255 255 255)
    ; TODO: Dibujar circulo
)

; -------------------------------------------------------------------------------
; Función para cambiar el color del dibujo a azul y el fondo a blanco.
; -------------------------------------------------------------------------------
(defun blau ()
    (color 0 0 255 255 255 255)
    ; TODO: Dibujar circulo
)

; -------------------------------------------------------------------------------
; Función para cambiar el color del dibujo a verde y el fondo a blanco.
; -------------------------------------------------------------------------------
(defun verd ()
    (color 0 255 0 255 255 255)
    ; TODO: Dibujar circulo
)

; -------------------------------------------------------------------------------
; Función para cambiar el color del dibujo a negro y el fondo a blanco.
; -------------------------------------------------------------------------------
(defun negre ()
    (color 0 0 0 255 255 255)
    ; TODO: Dibujar circulo
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
    (cond ((= n 0) t)
           (t
            ; TODO
           )
    )
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del radigran del spiro.
;
; - Parámetros:
;   @r - Nuevo radio grande del spirograph
; -------------------------------------------------------------------------------
(defun radigran (r)
    (cond ((contains (r (getprop 'spiro grans)))
            (putprop 'spiro r 'rgran)
          )
          (t
            (error "El valor de r no es correcto")
          )
    )
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del radipetit del spiro.
;
; - Parámetros:
;   @r - Nuevo radio pequeño del spirograph
; -------------------------------------------------------------------------------
(defun radipetit (r)
    (cond ((contains (r (getprop 'spiro petits)))
            (putprop 'spiro r 'rpetit)
          )
          (t
            (error "El valor de r no es correcto")
          )
    )
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del punto del spiro.
;
; - Parámetros:
;   @p - Valor del punto a establecer
; -------------------------------------------------------------------------------
(defun punt (p)
    (cond ((contains (p (getprop 'spiro punt)))
            (putprop 'spiro p 'punt)
          )
          (t
            (error "El valor de p no es correcto")
          )
    )
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del inicio del spiro.
;
; - Parámetros:
;   @a - Valor del ángulo a establecer
; -------------------------------------------------------------------------------
(defun inici (a)
    (cond ((contains (a (getprop 'spiro inici)))
            (putprop 'spiro a 'inici)
          )
          (t
            (error "El valor de a no es correcto")
          )
    )
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del escalado del spiro.
;
; - Parámetros:
;   @e - Valor del escalado a establecer
; -------------------------------------------------------------------------------
(defun escala (e)
    (cond ((contains (e (getprop 'spiro escala)))
            (putprop 'spiro e 'escala)
          )
          (t
            (error "El valor de e no es correcto")
          )
    )
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto de los puntos del spiro.
;
; - Parámetros:
;   @x - Valor del punto x a establecer
;   @y - Valor del punto y a establecer
; -------------------------------------------------------------------------------
(defun posicio (x y)
    (cond ((contains (x (getprop 'spiro grans)))
            (putprop 'spiro x 'x)
          )
          (t
            (error "El valor de x no es correcto")
          )
    )
)

; -------------------------------------------------------------------------------
; -------------------------------------------------------------------------------
(defun reduir (m n)
    ; TODO
)
