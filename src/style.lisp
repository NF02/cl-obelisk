;;; cl-obelisk - Style
;;; Copyright (C) 2026 Nicola Ferru Aka NFVblog
;;; License: GPLv3

(defpackage :cl-obelisk.style
  (:use :cl)
  (:import-from :cl-obelisk.kernel
		#:nodo-mappa #:mappa-grafo #:nodo-id #:nodo-livello #:nodo-centro-p
		#:nodo-cluster #:nodo-figli #:grafo-stile #:grafo-edge-styles
		#:grafo-clusters #:render-lisp-math)
  (:export #:render-nodo-cluster
	   #:render-nodo-node
	   #:render-nodo-points-to)
  (:documentation "Stili visuali per i grafi cl-obelisk"))

(in-package :cl-obelisk.style)

(defun ottenere-fattore-scala (formato)
  "Restituisce un fattore di scala in virgola mobile basato sul formato della carta."
  (declare (optimize (speed 3)))
  (the single-float
       (case formato
             (:a4 1.2)
             (:a4-or 1.5)
             (:a3 1.414)
             (:a3-or 1.8)
             (:a2 2.0)
             (:a2-or 2.3)
             (t 1.0))))

(defun calcola-fattore-densita (num-nodi)
  "Calcola un fattore di densità in base al numero di nodi per adattare le dimensioni."
  (cond ((< num-nodi 10) 1.2)
        ((< num-nodi 50) 1.0)
        (t (max 0.6 (/ 50.0 num-nodi)))))

(defun calcola-scala-font (formato)
  "Calcola il fattore di scala per i font basato sul formato della carta."
  (let ((base-scala (ottenere-fattore-scala formato)))
    (sqrt base-scala)))

(defun identifica-cluster (nodo)
  "Restituisce il nome del cluster per il nodo, o NIL se è il nodo centro."
  (if (nodo-centro-p nodo)
      nil
    (format nil "cluster_~A" (nodo-id nodo))))

(defmethod cl-dot:graph-object-cluster ((graph mappa-grafo) (obj nodo-mappa))
  "Restituisce il nome del cluster per il nodo se ha un cluster assegnato (versione semplice)."
  (when (nodo-cluster obj)
    (format nil "cluster_~A" (nodo-cluster obj))))

(defmethod cl-dot:graph-object-cluster ((graph mappa-grafo) (obj nodo-mappa))
  "Restituisce o crea il cluster CL-DOT per il nodo, se ha un cluster assegnato."
  (let ((nome-cluster (nodo-cluster obj)))
    (when nome-cluster
      (or (gethash nome-cluster (grafo-clusters graph))
          (setf (gethash nome-cluster (grafo-clusters graph))
		(make-instance 'cl-dot:cluster
			     :attributes `(:label ,nome-cluster
					      :style :dashed
					      :color "#bbbbbb"
					      :style :filled
					      :fillcolor "#f5f5f5"
					      :fontsize 15)))))))

(defun calcola-attributi-adattivi (nodo formato num-totale-nodi)
  "Calcola dimensioni font e penna per il nodo basandosi su livello, formato e numero totale nodi."
  (let* ((lvl (nodo-livello nodo))
         (scala-formato (ottenere-fattore-scala formato))
         (scala-densita (calcola-fattore-densita num-totale-nodi))
         (scala-finale (* scala-formato scala-densita)))
    (values (* (case lvl (0 26) (1 18) (2 14) (t 12)) scala-finale)
            (* (case lvl (0 4.5) (1 2.5) (2 1.2) (t 0.8)) scala-finale))))

(defmethod cl-dot:graph-object-node ((graph mappa-grafo) (obj nodo-mappa))
  "Genera i nodi CL-DOT con stile adattivo in base al tipo di grafo e livello del nodo."
  (multiple-value-bind (f-size p-width)
      (calcola-attributi-adattivi obj :a4 (length (nodo-figli obj)))
      (let* ((label-clean (render-lisp-math (nodo-id obj)))
	     (base-attrs `(:fontsize ,f-size :penwidth ,p-width :label ,label-clean))
	     (group-attr (if (nodo-centro-p obj)
			    nil
			  `(:group ,(format nil "lvl_~A" (nodo-livello obj)))))
	     (style-attrs (case (grafo-stile graph)
				   (:tecnico     '(:shape :box :style :filled :fillcolor "#eeeeee" :fontname "Courier"))
				   (:scientifico '(:shape :none :fontname "Times-Italic"))
				   (:umanistico  '(:shape :oval :style :filled :fillcolor "#fdf6e3" :fontname "Georgia italic"))
				   (:boheme      '(:shape :egg :style (:filled :dashed) :fillcolor "#ff9966" :color "#993300" :fontname "Times-Bold"))
				   (:grafo       '(:shape :circle :style :filled :fillcolor "#7be9f8" :fontname "Times-Italic"))
				   (:file-system '(:shape :folder :style :filled :fillcolor "#fbf1c7" :fontname "Courier"))
				   (t            '(:shape :ellipse)))))
	(make-instance 'cl-dot:node
		       :attributes (append base-attrs style-attrs group-attr)))))

(defmethod cl-dot:graph-object-points-to ((graph mappa-grafo) (obj nodo-mappa))
  "Genera gli archi CL-DOT con stili personalizzati in base agli edge-styles del grafo."
  (loop for figlio in (nodo-figli obj)
     collect (let* ((key (cons (nodo-id obj) (nodo-id figlio)))
		  (valore-g (gethash key (grafo-edge-styles graph) :default))
		  (stile (if (listp valore-g) (first valore-g) valore-g))
		  (is-constrained (if (listp valore-g)
				      (getf (rest valore-g) :constraint t)
				    t))
		  (base-attrs (case stile
				      (:tratteggiato   '(:style :dashed :arrowhead :vee))
				      (:relazione-base '(:style :dashed :arrowhead :none))
				      (:relazione-crow '(:style :crow :arrowhead :crow))
				      (:relazione-curve '(:style :crow :arrowhead :curve))
				      (:relazione-icurve '(:style :crow :arrowhead :icurve))
				      (:importante     '(:penwidth 2.5 :color "#000000"))
				      (t               '(:arrowhead :vee))))
		  (final-attrs (if (null is-constrained)
				  (append base-attrs '(:constraint "false"))
				base-attrs)))
	     (make-instance 'cl-dot:attributed
			   :object figlio
			   :attributes final-attrs))))