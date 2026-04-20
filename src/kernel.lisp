;;; cl-obelisk - Kernel
;;; Copyright (C) 2026 Nicola Ferru Aka NFVblog
;;; License: GPLv3

(defpackage :cl-obelisk.kernel
  (:use :cl)
  (:export #:nodo-mappa
	   #:mappa-grafo
	   #:nodo-id
	   #:nodo-livello
	   #:nodo-centro-p
	   #:nodo-cluster
	   #:nodo-figli
	   #:grafo-stile
	   #:grafo-edge-styles
	   #:grafo-clusters
	   #:string-replace-all
	   #:render-lisp-math
	   #:nodo-id-sanitizzato
	   #:risolvi-formato-carta
	   #:calcola-spaziatura
	   #:calcola-gerarchia
	   #:parse-smart-dsl
	   #:prepara-mappa
	   #:graphviz-installed-p))

(in-package :cl-obelisk.kernel)

(defclass nodo-mappa ()
  ((id       :initarg :id       :accessor nodo-id                     :documentation "Etichetta originale (LaTeX ammesso).")
   (livello  :initarg :livello  :initform 0   :accessor nodo-livello  :documentation "Profondità nella gerarchia.")
   (centro-p :initarg :centro-p :initform nil :accessor nodo-centro-p :documentation "Vero se è il nodo radice.")
   (cluster  :initarg :cluster  :initform nil :accessor nodo-cluster  :documentation "Contenitore di nodi")
   (figli    :initform nil      :accessor nodo-figli                  :documentation "Lista di istanze nodo-mappa collegate.")))

(defclass mappa-grafo (cl-dot:graph)
  ((stile        :initarg :stile        :accessor grafo-stile)
   (edge-styles  :initarg :edge-styles  :accessor grafo-edge-styles)
   (cluster-objs :initform (make-hash-table :test 'equal) :accessor grafo-clusters)))

(defun string-replace-all (old new string)
  (with-output-to-string (out)
    (loop with old-len = (length old)
	  for start = 0 then (+ pos old-len)
	  for pos = (search old string :start2 start)
	  do (write-string (subseq string start (or pos (length string))) out)
	  while pos
	  do (write-string new out))))

(defun render-lisp-math (label)
  (unless (and (stringp label) (plusp (length label)) (char= (char label 0) #\$))
    (return-from render-lisp-math label))
  (let ((res (remove #\$ label))
        (mappa '(("\\frac" . "/") ("\\sqrt" . "√") ("\\alpha" . "α")
                 ("\\beta" . "β") ("\\pi" . "π") ("{" . "(") ("}" . ")"))))
    (dolist (pair mappa)
      (setf res (string-replace-all (car pair) (cdr pair) res)))
    (remove #\\ res)))

(defun nodo-id-sanitizzato (nodo)
  (concatenate 'string "n"
               (remove-if-not #'alphanumericp
                              (string-replace-all " " "" (nodo-id nodo)))))

(defun risolvi-formato-carta (formato)
  (case formato
    (:a4 "8.3,11.7!")
    (:a4-or "11.7,8.3!")
    (:a3 "11.7,16.5!")
    (:a3-or "16.5,11.7!")
    (:a2 "16.5,23.4!")
    (:a2-or "23.4,16.5!")
    (t (if (listp formato)
           (format nil "~A,~A!" (first formato) (second formato))
         nil))))

(defun calcola-spaziatura (num-nodi &optional (fattore-scala 4.0))
  (let* ((n (max 1.0 (float num-nodi)))
         (spaziatura (/ fattore-scala (sqrt n)))
         (min-s 0.2))
    (list (max min-s spaziatura)
	  (max min-s (* spaziatura 1.3)))))

(defun calcola-gerarchia (centro-id relazioni)
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

(defun ensure-gethash (key hash-table default-value)
  (multiple-value-bind (value exists)
		       (gethash key hash-table)
		       (if exists
			   value
			 (setf (gethash key hash-table) default-value))))

(defun parse-smart-dsl (parent node-data &optional (seen (make-hash-table :test 'equal)) (current-cluster nil))
  (cond
   ((stringp node-data)
    (list (list parent node-data :default :cluster current-cluster)))
   (t
    (let* ((head (first node-data))
           (is-ponte (eq head :ponte))
           (is-container (eq head :contenitore))
           (stile (if (or is-container is-ponte) :default head))
           (label (second node-data))
           (children (cddr node-data))
           (next-cluster (if is-container label current-cluster)))

      (if (and label (stringp label))
          (let ((rel (unless is-ponte
                       (list parent label stile :cluster next-cluster))))
            (append (when rel (list rel))
                    (loop for child in children
                          when child
                          nconc (parse-smart-dsl label child seen next-cluster))))
        nil)))))

(defun prepara-mappa (relazioni centro-id)
  (let ((nodi-cache (make-hash-table :test 'equal))
        (edge-styles (make-hash-table :test 'equal)))
    (setf (gethash centro-id nodi-cache)
          (make-instance 'nodo-mappa :id centro-id :centro-p t))
    (dolist (rel relazioni)
      (destructuring-bind (da-id a-id stile-arco &key cluster (constraint t) &allow-other-keys) rel
			  (let ((nodo-da (or (gethash da-id nodi-cache)
					     (setf (gethash da-id nodi-cache) (make-instance 'nodo-mappa :id da-id))))
				(nodo-a  (or (gethash a-id nodi-cache)
					     (setf (gethash a-id nodi-cache) (make-instance 'nodo-mappa :id a-id)))))
			    (unless (nodo-cluster nodo-a) (setf (nodo-cluster nodo-a) cluster))
			    (pushnew nodo-a (nodo-figli nodo-da))
			    (setf (gethash (cons da-id a-id) edge-styles) (list stile-arco :constraint constraint)))))
    (values (loop for v being the hash-values of nodi-cache collect v) edge-styles)))

(defun graphviz-installed-p ()
  (handler-case
      (let ((exit-code
             (nth-value 2 (uiop:run-program '("dot" "-V")
                                            :output nil
                                            :error-output nil
                                            :ignore-error-status t))))
        (zerop exit-code))
    (error () nil)))