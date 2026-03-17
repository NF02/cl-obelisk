;;; --- DEMO COMPATTA CL-OBELISK ---
(push (uiop:getcwd) asdf:*central-registry*)

;; 2. Caricamento della libreria e dipendenze
(ql:quickload :cl-obelisk)

(in-package :cl-obelisk)

; vecchio dsl

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
(genera-da-dsl "mappa_ciclica" *appunti-complessi* :stile :boheme
	       :carta '(15 15)
               :orientamento :orizzontale 
               :formato :png)

					; vecchio esempio
(cl-obelisk:genera-da-dsl "mia_mappa"
			  '("Obelisk"
			    (:importante "Semplice"
					 (:default "Veloce"))
			    (:umanistico "Creativo"
					 (:boheme "Senza Regole"
						  (:relazionale :ref "Obelisk")))) ; Gestione automatica dei cicli
			  :stile :umanistico
			  :orientamento :orizzontale)



; nuovo dsl
(cl-obelisk:genera-da-dsl "mia_mappa" 
			  '("Obelisk"
			    (:normal "Creativo"
			     (:normal "Senza Regole" "Obelisk")) ;; Gestione automatica dei cicli 
			    (:importante "Semplice" "Veloce"))
			    :carta :a4
			    :margine 2
			    :formato :pdf
			    :stile :umanistico
			  )


(cl-obelisk:genera-da-dsl "test-contenitore-fixed"
			  '("Main System"
			    (:contenitore "Infrastructure"
			     "PostgreSQL"
			     "Redis")
			    (:contenitore "Frontend"
			     "React UI"
			     "Assets")
			    ;; Usiamo :ponte così "Redis" non viene collegato a "Main System"
			    (:ponte "Redis" "fine") 
			    (:ponte "Assets" "fine"))
			  :stile :umanistico
			  :carta :a4-or)
