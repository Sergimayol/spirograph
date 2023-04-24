; -------------------------------------------------------------------------------
; Test para la parte 1 de la practica
; -------------------------------------------------------------------------------
(load "spiro.lsp")

(cls)
(vermell)
(verd)
(blau)
(negre)

(radigran 96)
(inici 45)
(vermell)
(radipetit 32)
(inici 90)
(blau)
(radipetit 32)

(punt 3)
(inici 45)
(posicio 200 200)
(escala 1.9)
(get 'spiro 'escala)
(get 'spiro 'punt)

(reduir 105 84)