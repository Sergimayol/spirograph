; Integrantes del grupo:
;   Autor 1: ;
;   Autor 2: ;

;Variable spiro global
(defvar spiro)

; -------------------------------------------------------------------------------
; Función para inicializar los valores por defecto de un spiro.
; -------------------------------------------------------------------------------
(defun guarda-informacio ()
    (putprop 'spiro '('(150 105) '(144 96)) 'grans)
    (putprop 'spiro '('(84 35 56)
                      '(80 33 53) 
                      '(75 31 50) 
                      '(72 29 48) 
                      '(63 25 42) 
                      '(60 23 40) 
                      '(56 21 37) 
                      '(52 19 35) 
                      '(48 17 32) 
                      '(45 16 30) 
                      '(42 14 28) 
                      '(40 13 27) 
                      '(32 9 21) 
                      '(30 8 20) 
                      '(24 5 16)) 
        'petits)
    (putprop 'spiro 105 'rgran) ; Radio grande
    (putprop 'spiro 72 'rpetit) ; Radio pequeño
    (putprop 'spiro 3 'punt) ; Numero del punto a partir del cual se dibuja
    (putprop 'spiro 0 'inici) ; Angulo en grados del circulo gran inicial
    (putprop 'spiro 1.5 'escala) ; Valor que escalará el dibujo
    (putprop 'spiro t 'interior) ; Booleano que indica si se dibuja el interior
    (putprop 'spiro 0 'x) ; Coordenada x
    (putprop 'spiro 0 'y) ; Coordenada y
    (putprop 'spiro 0.5 'pas) ; Variación del ángulo
)
(guarda-informacio)

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

(defun radians (graus)
    (/ (* graus (* 2 pi)) 360)
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
; -------------------------------------------------------------------------------
(defun pinta (x y)
    (draw (realpart (round (+ 320 (* 1.8 x))))
    (realpart (round (+ 187 (* 1.8 y)))))
)

; -------------------------------------------------------------------------------
; -------------------------------------------------------------------------------
(defun cercle2 (x y radi pas angle)
    (cond ((< angle 360)
            (pinta (+ x (* radi (cos (radians (+ angle pas)))))
                   (+ y (* radi (sin (radians (+ angle pas)))))
            )
            (cercle2 x y radi pas (+ angle pas)))
      (t t)
    )
)

(defun mou (x y)
    (move (realpart (round (+ 320 (* 1.8 x))))
    (realpart (round (+ 187 (* 1.8 y)))))
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
    (mou (+ x r) y)
    (cercle2 x y r(/ 360 n) 0)
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
    ;Epitrocoide
    (cond ((or (= gran 144) (= gran 150))
           (setq x (- (* (- gran petit) (cos (/ (* petit p) gran))) (* te (cos (* (+ 1 (/ petit gran)) p)))))
           (setq y (- (* (- gran petit) (sin (/ (* petit p) gran))) (* te (sin (* (+ 1 (/ petit gran)) p))))))
    ;Hipotrocoide
          ((or (= gran 96) (= gran 105))
           (setq x (+ (* (- gran petit) (cos (/ (* petit p) gran))) (* te (cos (* (- 1 (/ petit gran)) p )))))
           (setq y (- (* (- gran petit) (sin (/ (* petit p) gran))) (* te (sin (* (- 1 (/ petit gran)) p ))))))
          (t (error "Gran debe tener el valor de 144, 150, 96 o 105")))
    ;Rotar los puntos con el ángulo inicial
    (setq x (+ (* x (cos (radians inici))) (* y (sin (radians inici)))))
    (setq y (- (* x (sin (radians inici))) (* y (cos (radians inici)))))
    ; Movemos x e y
    (mou x y) 
    (spirograph2 p gran petit te inc inici)
)


(defun spirograph2 (p gran petit te inc inici)
    (cond ((< p 0) t)
          (t
    (cond ((or (= gran 144) (= gran 150))
           (setq x (- (* (- gran petit) (cos (/ (* petit p) gran))) (* te (cos (* (+ 1 (/ petit gran)) p)))))
           (setq y (- (* (- gran petit) (sin (/ (* petit p) gran))) (* te (sin (* (+ 1 (/ petit gran)) p))))))
          ((or (= gran 96) (= gran 105))
           (setq x (+ (* (- gran petit) (cos (/ (* petit p) gran))) (* te (cos (* (- 1 (/ petit gran)) p )))))
           (setq y (- (* (- gran petit) (sin (/ (* petit p) gran))) (* te (sin (* (- 1 (/ petit gran)) p ))))))
           )
        (setq x (+ (* x (cos (radians inici))) (* y (sin (radians inici)))))
        (setq y (- (* x (sin (radians inici))) (* y (cos (radians inici)))))
            ; Pintar x e y            
            (pinta x y)
            (spirograph2 (- p inc) gran petit te inc inici)
          )
    )
)

; -------------------------------------------------------------------------------
; Función que genera un spirographo con el número de vueltas necesarias para acabar todo el trazado.
; -------------------------------------------------------------------------------
(defun spiro (gran petit p inc inici)
    (setq distancia (sqrt (+ (* gran gran) (* petit petit) (* -2 gran petit (cos (/ (* p pi) 180))))))
    (setq vueltas (/ distancia (* petit 2)))
    (setq p (cond ((> (- gran petit) petit) 
                   (cond ((= p 1) 35)
                         ((= p 2) 33)
                         ((= p 3) 31)
                         ((= p 4) 29)
                         ((= p 5) 25)
                         ((= p 6) 23)
                         ((= p 7) 21)
                         ((= p 8) 19)
                         ((= p 9) 17)
                         ((= p 10) 16)      
                         ((= p 11) 14)
                         ((= p 12) 13)
                         ((= p 13) 9)
                         ((= p 14) 8)
                         ((= p 15) 5)
                         (t (error "El valor de p debe estar entre 1 y 15"))))
                ((= p 1) 56)
                ((= p 2) 53)
                ((= p 3) 50)
                ((= p 4) 48)
                ((= p 5) 42)
                ((= p 6) 40)
                ((= p 7) 37)
                ((= p 8) 35)
                ((= p 9) 32)
                ((= p 10) 30)      
                ((= p 11) 28)
                ((= p 12) 27)
                ((= p 13) 21)
                ((= p 14) 20)
                ((= p 15) 16)
                (t (error "El valor de p debe estar entre 1 y 15"))))
    (spirograph (* vueltas 360) gran petit p inc inici)
)

; -------------------------------------------------------------------------------
; Llama a la función sphirograph y le pasa los valores de la variable spiro
; -------------------------------------------------------------------------------
(defun roda ()
    (setq gran (get 'spiro 'rgran))
    (setq petit (get 'spiro 'rpetit))
    (setq p (get 'spiro 'pas))
    (setq inici (get 'spiro 'inici))
    (setq escala (get 'spiro 'escala))
    (setq punt (get 'spiro 'punt))
    (setq te (cond ((= punt 1) 35)
              ((= punt 2) 33)
              ((= punt 3) 31)
              ((= punt 4) 29)
              ((= punt 5) 25)
              ((= punt 6) 23)
              ((= punt 7) 21)
              ((= punt 8) 19)
              ((= punt 9) 17)
              ((= punt 10) 16)      
              ((= punt 11) 14)
              ((= punt 12) 13)
              ((= punt 13) 9)
              ((= punt 14) 8)
              ((= punt 15) 5)))
    (print te)
    (spirograph (* escala 360) gran petit (* te 2) p inici)
)

; -------------------------------------------------------------------------------
; Función que hace lo mismo que roda pero con un número de vueltas determinado
; -------------------------------------------------------------------------------
(defun roda-voltes (n)
    (setq punt (get 'spiro 'punt))
    (setq escala (get 'spiro 'escala))
    (setq te (cond ((= punt 1) 35)
              ((= punt 2) 33)
              ((= punt 3) 31)
              ((= punt 4) 29)
              ((= punt 5) 25)
              ((= punt 6) 23)
              ((= punt 7) 21)
              ((= punt 8) 19)
              ((= punt 9) 17)
              ((= punt 10) 16)      
              ((= punt 11) 14)
              ((= punt 12) 13)
              ((= punt 13) 9)
              ((= punt 14) 8)
              ((= punt 15) 5)))
    (spirograph (/ 360 (* n escala)) (get 'spiro 'rgran) (get 'spiro 'rpetit) (* te 2) (get 'spiro 'pas) (get 'spiro 'inici))
)

; -------------------------------------------------------------------------------
; Función que hace lo mismo que roda-voltes pero se le pasan los parámetro los argumentos
; -------------------------------------------------------------------------------
(defun spiro-voltes (voltes gran petit p in inici)
    (setq te (cond ((= punt 1) 35)
              ((= punt 2) 33)
              ((= punt 3) 31)
              ((= punt 4) 29)
              ((= punt 5) 25)
              ((= punt 6) 23)
              ((= punt 7) 21)
              ((= punt 8) 19)
              ((= punt 9) 17)
              ((= punt 10) 16)      
              ((= punt 11) 14)
              ((= punt 12) 13)
              ((= punt 13) 9)
              ((= punt 14) 8)
              ((= punt 15) 5)))
    (spirograph (/ 360 (* voltes escala)) gran petit (* te 2) in inici)
)


(defun spiros (l)
  (dolist (params l)
    (apply 'spiro params)) 
)


(defun dibuix()
)
    
