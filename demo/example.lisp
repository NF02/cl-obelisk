(ql:quickload :cl-obelisk)

(cl-obelisk:genera-da-dsl "CL-OBELISK"
			  '("CL-OBELISK: Core Logic"
			    (:importante "DSL Input (S-Expression)"
			     (:default "Parser (parse-smart-dsl)"
			      (:tratteggiato "Cycle Detection (Hash Table)")
			      (:default "Relational List Generation")))
			    (:umanistico "Data Hydration"
			     (:default "BFS (calcola-gerarchia)"
			      (:default "Depth Assignment (Livelli)"))
			     (:default "Object Mapping"
			      (:default "nodo-mappa instances")))
			    (:tondo "Graphviz Rendering"
			     (:default "cl-dot Protocol"
			      (:default "Style Mapping (Attributes)"))
			     (:importante "Output Engine"
			      (:default "dot Executable"
			       (:relazionale :ref "CL-OBELISK: Core Logic"))))) ;; Ciclo logico
			  :stile :tondo :orientamento :orizzontale
			  :carta '(15 15)
			  :formato :svg)
