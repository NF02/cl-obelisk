;;; --- DEMO COMPATTA CL-OBELISK ---
(push (uiop:getcwd) asdf:*central-registry*)

;; 2. Caricamento della libreria e dipendenze
(ql:quickload :cl-obelisk)

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
(genera-da-dsl "mappa_ciclica" *appunti-complessi* :stile :tondo
	       :carta '(15 15)
               :orientamento :orizzontale 
               :formato :png)
