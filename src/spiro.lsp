;Variable spiro global
(defvar spiro)

; -------------------------------------------------------------------------------
; Función para inicializar los valores por defecto de un spiro.
; -------------------------------------------------------------------------------
(defun guarda-informacio ()
    (putprop 'spiro '((150 105) (144 96)) 'grans)
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
                      (24 5 16)) 'petits)
    (putprop 'spiro 150 'rgran)  ; Radio grande
    (putprop 'spiro 32 'rpetit)  ; Radio pequeño
    (putprop 'spiro 3 'punt)     ; Numero del punto a partir del cual se dibuja
    (putprop 'spiro 0 'inici)    ; Angulo en grados del circulo gran inicial
    (putprop 'spiro 1.8 'escala) ; Valor que escalará el dibujo
    (putprop 'spiro t 'interior) ; Booleano que indica si se dibuja el interior
    (putprop 'spiro 320 'x)      ; Coordenada x (Inicialmente centro de la pantalla)
    (putprop 'spiro 187 'y)      ; Coordenada y (Inicialmente centro de la pantalla)
    (putprop 'spiro 0.2 'pas)    ; Variación del ángulo
)
(guarda-informacio)

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
(defun mou (x y)
    (move 
        (realpart (round (+ (get 'spiro 'x) (* (get 'spiro 'escala) x))))
        (realpart (round (+ (get 'spiro 'y) (* (get 'spiro 'escala) y))))
    )
)

(defun pinta (x y)
    (draw
        (realpart (round (+ (get 'spiro 'x) (* (get 'spiro 'escala) x))))
        (realpart (round (+ (get 'spiro 'y) (* (get 'spiro 'escala) y))))
    )
)

(defun cercle-aux (x y radi pas angle)
    (cond ((< angle 360)
            (pinta (+ x (* radi (cos (radians (+ angle pas)))))
                   (+ y (* radi (sin (radians (+ angle pas)))))
            )
            (cercle-aux x y radi pas (+ angle pas)))
      (t t)
    )
)

(defun radians (graus)
    (/ (* graus (* 2 pi)) 360)
)
; -------------------------------------------------------------------------------

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
    (mou (+ x r) y)
    (cercle-aux x y r(/ 360 n) 0)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del radigran del spiro.
;
; - Parámetros:
;   @r - Nuevo radio grande del spirograph
; -------------------------------------------------------------------------------
(defun radigran (r)
    (putprop 'spiro r 'rgran)
    (cercle (get 'spiro 'x) (get 'spiro 'y) r (get 'spiro 'punt))
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del radipetit del spiro.
;
; - Parámetros:
;   @r - Nuevo radio pequeño del spirograph
; -------------------------------------------------------------------------------
(defun radipetit (r)
    (putprop 'spiro r 'rpetit)
    (cercle (get 'spiro 'x) (get 'spiro 'y) r (get 'spiro 'punt))
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del punto del spiro.
;
; - Parámetros:
;   @p - Valor del punto a establecer
; -------------------------------------------------------------------------------
(defun punt (p)
    (putprop 'spiro p 'punt)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del inicio del spiro.
;
; - Parámetros:
;   @a - Valor del ángulo a establecer
; -------------------------------------------------------------------------------
(defun inici (a)
    (putprop 'spiro a 'inici)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto del escalado del spiro.
;
; - Parámetros:
;   @e - Valor del escalado a establecer
; -------------------------------------------------------------------------------
(defun escala (e)
    (putprop 'spiro e 'escala)
)

; -------------------------------------------------------------------------------
; Función para cambiar el valor por defecto de los puntos del spiro.
;
; - Parámetros:
;   @x - Valor del punto x a establecer
;   @y - Valor del punto y a establecer
; -------------------------------------------------------------------------------
(defun posicio (x y)
    (putprop 'spiro x 'x)
    (putprop 'spiro y 'y)
)

; -------------------------------------------------------------------------------
; Función para calcular la fracción reducida y devolver una lista
; -------------------------------------------------------------------------------
(defun reduir (m n)
    (setq mcd (gcd m n))
    (list (/ m mcd) (/ n mcd))
)

; -------------------------------------------------------------------------------
; Función que genera un spirographo de manera recursiva.
; -------------------------------------------------------------------------------
(defun spirograph (p gran petit te inc inici)
    (cond ((get 'spiro 'interior)
                (setq x (calc-x-hipotrocoide p te gran petit))
                (setq y (calc-y-hipotrocoide p te gran petit))
                (mou (calc-desp-x x y inici) (calc-desp-y x y inici))
                (spirograph-interior p gran petit te inc inici)
          )
          (t
                (setq x (calc-x-epitrocoide p te gran petit))
                (setq y (calc-y-epitrocoide p te gran petit))
                (mou (calc-desp-x x y inici) (calc-desp-y x y inici))
                (spirograph-exterior p gran petit te inc inici)
          )
    )
)

(defun calc-x-hipotrocoide (angle dist rgran rpetit)
    (+ 
        (* (- rgran rpetit) (cos (/ (* angle rpetit) rgran)))
        (* dist (cos (* angle (- 1 (/ rpetit rgran)))))
    )
)

(defun calc-y-hipotrocoide (angle dist rgran rpetit)
    (-
        (* (- rgran rpetit) (sin (/ (* angle rpetit) rgran)))
        (* dist (sin (* angle (- 1 (/ rpetit rgran)))))
    )
)

(defun calc-x-epitrocoide (angle dist rgran rpetit)
    (-
        (* (+ rgran rpetit) (cos (/ (* angle rpetit) rgran)))
        (* dist (cos (* angle (+ 1 (/ rpetit rgran)))))
    )
)

(defun calc-y-epitrocoide (angle dist rgran rpetit)
    (-
        (* (+ rgran rpetit) (sin (/ (* angle rpetit) rgran)))
        (* dist (sin (* angle (+ 1 (/ rpetit rgran)))))
    )
)

(defun calc-desp-x(x y angle)
    (+ (* x (cos (radians (+ angle -90)))) (* y (sin (radians (+ angle -90)))))
)

(defun calc-desp-y(x y angle)
    (+ (* (- x) (sin (radians (+ angle -90)))) (* y (cos (radians (+ angle -90)))))
)

(defun spirograph-interior (p rgran rpetit dist inc inici)
    (cond ((< p 0) t)
            (t
                (set 'x (calc-x-hipotrocoide p dist rgran rpetit))
                (set 'y (calc-y-hipotrocoide p dist rgran rpetit))
                (pinta (calc-desp-x x y inici) (calc-desp-y x y inici))
                (spirograph-interior (- p inc) rgran rpetit dist inc inici)
            )
    )
)

(defun spirograph-exterior (p rgran rpetit dist inc inici)
    (cond ((< p 0) t)
            (t
                (set 'x (calc-x-epitrocoide p dist rgran rpetit))
                (set 'y (calc-y-epitrocoide p dist rgran rpetit))
                (pinta (calc-desp-x x y inici) (calc-desp-y x y inici))
                (spirograph-exterior (- p inc) rgran rpetit dist inc inici)
            )
    )
)

; -------------------------------------------------------------------------------
; Función que genera un spirograph con el número de vueltas necesarias para 
; acabar todo el trazado.
; -------------------------------------------------------------------------------
(defun spiro (gran petit p inc inici)
    (setq new-p (/ (* 2 pi (cadr (reduir gran petit))) inc))
    (setq new-petit (get-petit petit (get 'spiro 'petits))) 
    (setq forats (cadr new-petit))
    (setq new-t (* (+ 1 (- forats p)) (/ (car new-petit) (+ 1 forats))))
    (spirograph new-p gran petit new-t inc inici)
)

(defun get-petit (rpetit list)
    (cond ((null list) nil)
            ((equal rpetit (caar list)) 
                (car list)
            )
            (t 
                (get-petit rpetit (cdr list))
            )
    )
)

; -------------------------------------------------------------------------------
; Llama a la función spiro y le pasa los valores de la variable spiro
; -------------------------------------------------------------------------------
(defun roda ()
    (spiro
        (get 'spiro 'rgran)
        (get 'spiro 'rpetit)
        (get 'spiro 'punt)
        (get 'spiro 'pas)
        (get 'spiro 'inici)
    )
)

; -------------------------------------------------------------------------------
; Llama a la función spiro y le pasa los valores de la variable spiro
; -------------------------------------------------------------------------------
(defun roda-voltes (n)
    (setq new-petit (get-petit (get 'spiro 'rpetit) (get 'spiro 'petits))) 
    (setq forats (cadr new-petit))
    (spirograph 
        (/ (* 2 pi (- n 1)) (get 'spiro 'pas))
        (get 'spiro 'rgran)
        (get 'spiro 'rpetit)
        (* (+ 1 (- forats (get 'spiro 'punt))) (/ (car new-petit) (+ 1 forats)))
        (get 'spiro 'pas)
        (get 'spiro 'inici)
    )
)

; -------------------------------------------------------------------------------
; -------------------------------------------------------------------------------
(defun spiro-voltes (voltes gran petit p inc inici)
    (setq new-petit (get-petit petit (get 'spiro 'petits)))
    (setq forats (cadr new-petit))
    (spirograph
        (/ (* 2 pi (- voltes 1)) inc)
        gran
        petit
        (* (+ 1 (- forats p)) (/ (car new-petit) (+ 1 forats)))
        inc
        inici
    )
)

; -------------------------------------------------------------------------------
; -------------------------------------------------------------------------------
(defun spiros (l)
    (cond ((null (cdr l)) (apply 'spiro (car l)))
            (t
                (apply 'spiro (car l))
                (spiros (cdr l))
            )
    )
)

; -------------------------------------------------------------------------------
; -------------------------------------------------------------------------------
(defun dibuix()
    (setq copy-escala (get 'spiro 'escala)) 
    (setq copy-x (get 'spiro 'x))
    (setq copy-y (get 'spiro 'y))

    (setq margin-x 160)
    (setq margin-y 125)
    (setq initial-x 80)
    (setq initial-y 62.5)

    (escala 0.5)
    (cls)

    (posicio (calc-new-x initial-x margin-x 0) (calc-new-y initial-y margin-y 0))
    (dibujo1)
    (posicio (calc-new-x initial-x margin-x 1) (calc-new-y initial-y margin-y 0))
    (dibujo2)
    (posicio (calc-new-x initial-x margin-x 2) (calc-new-y initial-y margin-y 0))
    (dibujo3)
    (posicio (calc-new-x initial-x margin-x 3) (calc-new-y initial-y margin-y 0))
    (dibujo4)
    (posicio (calc-new-x initial-x margin-x 0) (calc-new-y initial-y margin-y 1))
    (dibujo5)
    (posicio (calc-new-x initial-x margin-x 1) (calc-new-y initial-y margin-y 1))
    (dibujo6)
    (posicio (calc-new-x initial-x margin-x 2) (calc-new-y initial-y margin-y 1))
    (dibujo7)
    (posicio (calc-new-x initial-x margin-x 3) (calc-new-y initial-y margin-y 1))
    (dibujo8)
    (posicio (calc-new-x initial-x margin-x 0) (calc-new-y initial-y margin-y 2))
    (dibujo9)
    (posicio (calc-new-x initial-x margin-x 1) (calc-new-y initial-y margin-y 2))
    (dibujo10)
    (posicio (calc-new-x initial-x margin-x 2) (calc-new-y initial-y margin-y 2))
    (dibujo11)
    (posicio (calc-new-x initial-x margin-x 3) (calc-new-y initial-y margin-y 2))
    (dibujo12)

    (escala copy-escala)
    (posicio copy-x copy-y)
    (negre)
)

(defun calc-new-x (x margin n)
    (+ x (* margin n))
)

(defun calc-new-y (y margin n)
    (+ y (* margin n))
)

(defun dibujo1 ()
    (vermell)
    (spiros '((105 63 1 0.5 0)
                (105 63 3 0.5 0)
                (105 63 5 0.5 0)))
    (verd)
    (spiros '((105 63 7 0.5 0)
                (105 63 9 0.5 0)
                (105 63 11 0.5 0)))
    (blau)
    (spiros '((105 63 13 0.5 0)
                (105 63 15 0.5 0)
                (105 63 17 0.5 0)))
)

(defun dibujo2 ()
    (verd)
    (spiros '((94 60 11 0.5 18)
              (94 48 15 0.5 201)
              (94 24 8 0.5 260)))
    (negre)
    (spiros '((105 75 9 0.5 162)
              (94 42 2 0.5 263)
              (105 24 7 0.5 15)))
)

(defun dibujo3 ()
    (vermell)
    (roda-voltes 15)
    (blau)
    (spiros '((105 42 7 0.5 0)
              (94 24 9 0.5 0)
              (105 63 11 0.5 0)))
    (verd)
    (spiros '((105 63 1 0.5 0)
              (105 63 3 0.5 0)
              (105 63 5 0.5 0)))
)

(defun dibujo4 ()
    (blau)
    (spiro-voltes 15 105 63 1 0.2 0)
    (vermell)
    (spiro-voltes 8 94 24 3 0.3 0)
    (verd)
    (spiro-voltes 9 105 63 9 0.4 0)
)

(defun dibujo5 ()
    (negre)
    (spiro-voltes 15 105 63 1 0.5 0)
    (vermell)
    (spiros '((105 42 12 0.5 36)
              (105 45 3 0.5 67)
              (94 24 9 0.5 223)))
)

(defun dibujo6 ()
    (vermell)
    (spiros '((94 60 11 0.5 18)
              (94 24 8 0.5 260)))
    (negre)
    (spiro 94 48 5 0.5 201)
)

(defun dibujo7 ()
    (verd)
    (roda-voltes 8)
    (blau)
    (spiro 94 48 5 0.5 201)
)

(defun dibujo8 ()
    (blau)
    (spiros '((105 42 12 0.5 36)
              (105 45 3 0.5 67)
              (94 24 9 0.5 223)))
    (vermell)
    (spiros '((94 42 12 0.5 36)
              (105 45 3 0.2 67)))
)

(defun dibujo9 ()
    (vermell)
    (spiros '((105 63 1 0.5 90)
              (105 63 3 0.5 90)
              (105 63 5 0.5 90)))
    (verd)
    (spiro-voltes 10 105 63 9 0.4 45)
    (negre)
    (roda-voltes 12)
)

(defun dibujo10 ()
    (negre)
    (spiros '((105 63 13 0.5 0)
              (105 63 15 0.5 0)
              (105 63 17 0.5 0)))
    (vermell)
    (spiros '((105 42 12 0.5 36)
              (105 45 3 0.5 67)
              (94 24 9 0.5 223)))
)

(defun dibujo11 ()
    (blau)
    (spiro-voltes 10 32 63 9 0.4 45)
    (negre)
    (roda-voltes 7)
)

(defun dibujo12 ()
    (verd)
    (spiros '((105 63 1 0.5 90)
              (105 63 3 0.5 90)
              (105 63 5 0.5 90)))
    (blau)
    (spiros '((105 63 7 0.5 0)
              (105 63 9 0.5 0)
              (105 63 11 0.5 0)))
    (vermell)
    (spiros '((105 63 13 0.5 180)
              (105 63 15 0.5 180)
              (105 63 17 0.5 180)))
)
