;;; --- DEMO COMPATTA CL-OBELISK ---
(load "cl-obelisk.lisp")
(in-package :cl-obelisk)

;; Definiamo un grafo con un ciclo: A -> B -> A
(defparameter *appunti-complessi*
  '("Studio"
    (:importante "Teoria" 
      (:default "Esercizi" 
        (:relazionale :ref "Teoria"))) ; Ciclo!
    (:importante "Pratica"
      (:default "Progetto"
        (:relazionale :ref "Esercizi"))))) ; Convergenza!

;; Generazione rapida
(genera-da-dsl "mappa_ciclica" *appunti-complessi* :stile :umanistico
               :orientamento :orizzontale 
               :formato :svg)
