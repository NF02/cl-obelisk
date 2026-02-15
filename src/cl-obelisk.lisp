;;; cl-obelisk - Generatore di Mappe Concettuali con Smart-DSL
;;; Copyright (C) 2026 Nicola Ferru Aka NFVblog
;;; License: GPLv3

(defpackage :cl-obelisk
	    (:use :cl)
	    (:export #:nodo-mappa
		     #:mappa-grafo
		     #:esporta-mappa
		     #:genera-pdf-mappa
		     #:genera-png-mappa
		     #:genera-svg-mappa
		     #:genera-da-dsl
		     #:carica-e-genera
		     #:carica-relazioni-da-file
		     #:parse-smart-dsl))

(in-package :cl-obelisk)

;; --- 1. Data Structures ---

(defclass nodo-mappa ()
  ((id       :initarg :id       :accessor nodo-id       :documentation "Unique identifier and label for the node.")
   (livello  :initarg :livello  :initform 0   :accessor nodo-livello  :documentation "Hierarchical depth from the root.")
   (centro-p :initarg :centro-p :initform nil :accessor centro-p      :documentation "Boolean flag indicating the root node.")
   (figli    :initform nil      :accessor nodo-figli    :documentation "List of child nodes (successor nodes).")))

(defclass mappa-grafo (cl-dot:graph)
  ((stile       :initarg :stile       :accessor grafo-stile       :documentation "Visual theme: :tecnico, :humanistico, or :boheme.")
   (edge-styles :initarg :edge-styles :accessor grafo-edge-styles :documentation "Hash map storing specific styles for edges.")))

;; --- 2. Internal Logic ---

(defun calcola-gerarchia (centro-id relazioni)
  "Performs a Breadth-First Search (BFS) to determine the depth level of each node."
  (let ((distanze (make-hash-table :test 'equal))
        (coda (list (cons centro-id 0))))
    (setf (gethash centro-id distanze) 0)
    (loop while coda
          for (att-id . d) = (pop coda)
          do (dolist (rel relazioni)
               (when (string= (first rel) att-id)
                 (let ((dest (second rel)))
                   (unless (gethash dest distanze)
                     (setf (gethash dest distanze) (1+ d))
                     (setf coda (nconc coda (list (cons dest (1+ d))))))))))
    distanze))

(defun calcola-attributi-dimensione (nodo)
  "Returns font size and pen width based on node depth to establish visual hierarchy."
  (let* ((lvl (nodo-livello nodo))
         (f-size (case lvl
                       (0 26)   ; Root level
                       (1 18)   ; Main branches
                       (2 12)   ; Sub-topics
                       (t 9)))  ; Leaf nodes/details
         (p-width (case lvl
			(0 4.5)
			(1 2.5)
			(2 1.2)
			(t 0.6))))
    (values f-size p-width)))

(defun risolvi-formato-carta (formato)
  "Maps paper size keywords to Graphviz-compatible dimension strings."
  (case formato
	(:a4 "8.3,11.7!")
	(:a3 "11.7,16.5!")
	(:a2 "16.5,23.4!")
	(t (if (listp formato)
               (format nil "~A,~A!" (first formato) (second formato))
             nil))))

;; --- 3. cl-dot Protocol Implementation ---

(defmethod cl-dot:graph-object-node ((graph mappa-grafo) (obj nodo-mappa))
	   "Translates cl-obelisk nodes into cl-dot node instances with thematic attributes."
	   (multiple-value-bind (f-size p-width) (calcola-attributi-dimensione obj)
				(let ((base-attrs `(:fontsize ,f-size :penwidth ,p-width :label ,(nodo-id obj))))
				  (make-instance 'cl-dot:node
						 :attributes 
						 (case (grafo-stile graph)
						       (:tecnico 
							(append base-attrs '(:shape :box :style :filled :fillcolor "#eeeeee" :fontname "Courier")))
						       (:umanistico 
							(append base-attrs '(:shape :oval :style :filled :fillcolor "#fdf6e3" :color "#586e75" :fontname "Georgia italic")))
						       (:boheme 
							(append base-attrs '(:shape :egg :style (:filled :dashed) :fillcolor "#ff9966" :color "#993300" :fontname "Times-Bold")))
						       (:tondo 
							(append base-attrs '(:shape :circle :style :filled :fillcolor "#e1f5fe" :color "#01579b" :fontname "Helvetica")))
						       (t (append base-attrs '(:shape :ellipse))))))))

(defmethod cl-dot:graph-object-points-to ((graph mappa-grafo) (obj nodo-mappa))
	   "Generates edge instances between nodes, applying semantic styles."
	   (loop for figlio in (nodo-figli obj)
		 collect (let* ((key (cons (nodo-id obj) (nodo-id figlio)))
				(stile-arco (gethash key (grafo-edge-styles graph) :default))
				(attrs (case stile-arco
					     (:relazionale   '(:style :dashed :arrowhead :none))
					     (:tratteggiato  '(:style :dashed :arrowhead :vee))
					     (:importante    '(:penwidth 2.5 :color "#000000"))
					     (t              '(:arrowhead :vee)))))
			   (make-instance 'cl-dot:attributed
					  :object figlio
					  :attributes attrs))))

;; --- 4. DSL Parser & Data Preparation ---

(defun parse-smart-dsl (parent node-data &optional (seen (make-hash-table :test 'equal)))
  "Recursively parses the nested DSL structure. Automatically detects cycles to prevent infinite recursion."
  (let* ((stile (first node-data))
         (label-ref (second node-data))
         (is-ref (eq label-ref :ref))
         (label (if is-ref (first (cddr node-data)) label-ref))
         (children (if is-ref (rest (cddr node-data)) (cddr node-data)))
         (rel (list parent label stile)))
    (cond 
     ((gethash label seen) (list rel))
     (t (setf (gethash label seen) t)
        (cons rel (loop for child in children 
                        nconc (parse-smart-dsl label child seen)))))))

(defun prepara-mappa (relazioni centro-id)
  "Hydrates the internal node objects and edge metadata from a flat list of relations."
  (let ((distanze (calcola-gerarchia centro-id relazioni))
        (nodi-cache (make-hash-table :test 'equal))
        (edge-styles (make-hash-table :test 'equal)))
    (dolist (rel relazioni)
      (destructuring-bind (da-id a-id stile-arco) rel
			  (flet ((prendi-nodo (id)
					      (or (gethash id nodi-cache)
						  (setf (gethash id nodi-cache)
							(make-instance 'nodo-mappa 
								       :id id 
								       :centro-p (string= id centro-id)
								       :livello (gethash id distanze 3))))))
				(let ((nodo-da (prendi-nodo da-id))
				      (nodo-a (prendi-nodo a-id)))
				  (pushnew nodo-a (nodo-figli nodo-da))
				  (setf (gethash (cons da-id a-id) edge-styles) stile-arco)))))
    (values (loop for v being the hash-values of nodi-cache collect v) edge-styles)))

;; --- 5. Public API ---

(defun esporta-mappa (nome-file relazioni &key (centro-id nil) (stile :tecnico) (formato :pdf) (carta :a4) (orientamento :verticale) (dir-output "output"))
  "Primary export function. Generates a diagram file in a format-specific subdirectory."
  (let* ((root-id (or centro-id (first (first relazioni))))
         (ext (string-downcase (symbol-name formato)))
         (target-dir (format nil "~A/~A/" dir-output ext))
         (dot-file (format nil "~A~A.dot" target-dir nome-file))
         (out-file (format nil "~A~A.~A" target-dir nome-file ext))
         (dim-carta (risolvi-formato-carta carta))
         (rankdir (if (eq orientamento :orizzontale) "LR" "TB")))
    (ensure-directories-exist target-dir)
    (multiple-value-bind (nodi edge-styles) (prepara-mappa relazioni root-id)
			 (let* ((graph-attrs `(:rankdir ,rankdir :overlap "false" :splines "true"
							,@(when dim-carta (list :size dim-carta :ratio "fill"))))
				(grafo-istanza (make-instance 'mappa-grafo :stile stile :edge-styles edge-styles))
				(grafo-dot (cl-dot:generate-graph-from-roots grafo-istanza nodi graph-attrs)))
			   (with-open-file (s dot-file :direction :output :if-exists :supersede)
					   (cl-dot:print-graph grafo-dot :stream s))
			   (uiop:run-program (list "dot" (format nil "-T~A" ext) dot-file "-o" out-file))
			   (when (uiop:file-exists-p dot-file) (delete-file dot-file))
			   (format t "~%[cl-obelisk] ~A generated at: ~A~%" (string-upcase ext) out-file)))))

(defun genera-da-dsl (nome-file dsl-data &rest args)
  "Interface for generating maps directly from the nested DSL structure."
  (let* ((root (first dsl-data))
         (relazioni (loop for branch in (rest dsl-data)
                          nconc (parse-smart-dsl root branch))))
    (apply #'esporta-mappa nome-file relazioni :centro-id root args)))

(defun carica-relazioni-da-file (path)
  "Reads a Lisp expression (list) from a file."
  (with-open-file (s path) (read s)))

(defun carica-e-genera (nome-file path-dsl &rest args)
  "Loads a DSL file and triggers the map generation."
  (apply #'genera-da-dsl nome-file (carica-relazioni-da-file path-dsl) args))

;; Fast-access wrappers for common formats
(defun genera-pdf-mappa (nome-file relazioni &rest args) (apply #'esporta-mappa nome-file relazioni :formato :pdf args))
(defun genera-png-mappa (nome-file relazioni &rest args) (apply #'esporta-mappa nome-file relazioni :formato :png args))
(defun genera-svg-mappa (nome-file relazioni &rest args) (apply #'esporta-mappa nome-file relazioni :formato :svg args))
