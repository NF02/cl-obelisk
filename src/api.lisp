;;; cl-obelisk - API
;;; Copyright (C) 2026 Nicola Ferru Aka NFVblog
;;; License: GPLv3

(defpackage :cl-obelisk.api
  (:use :cl)
  (:import-from :cl-obelisk.kernel
		#:nodo-mappa #:mappa-grafo #:prepara-mappa #:parse-smart-dsl
		#:risolvi-formato-carta #:calcola-spaziatura #:graphviz-installed-p
		#:nodo-id #:nodo-id-sanitizzato #:nodo-figli)
  (:export #:esporta-mappa-tikz
	   #:genera-da-dsl
	   #:genera-tikz-da-dsl)
  (:documentation "API pubblica per la generazione di mappe concettuali"))

(in-package :cl-obelisk.api)

(defun esporta-mappa-tikz (nodi edge-styles stream)
  "Scrive su STREAM un ambiente TikZ che rappresenta la mappa con NODI e EDGE-STYLES."
  (let* ((num-nodi (length nodi))
         (distanza (max 1.5 (/ 15 (sqrt num-nodi)))))
    (format stream "\\begin{tikzpicture}[node distance=~,2fcm, every node/.style={draw, fill=gray!10, font=\\small}]~%" distanza)
    (dolist (n nodi)
      (format stream "  \\node (~A) {~A};~%" (nodo-id-sanitizzato n) (nodo-id n)))
    (dolist (n nodi)
      (dolist (figlio (nodo-figli n))
        (let* ((key (cons (nodo-id n) (nodo-id figlio)))
               (stile (gethash key edge-styles :default))
               (t-style (if (eq stile :tratteggiato) "dashed, ->" "->")))
          (format stream "  \\draw[~A] (~A) -- (~A);~%" t-style (nodo-id-sanitizzato n) (nodo-id-sanitizzato figlio)))))
    (format stream "\\end{tikzpicture}~%")))

(defun genera-da-dsl (nome-file dsl-data &key (stile :tecnico) (formato :png)
                                (orientamento :verticale) (carta :a4)
                                (dir-base "output")
                                (margine 0))
  "Genera un'immagine della mappa da DSL-DATA usando Graphviz e la salva in DIR-BASE."
  (unless (graphviz-installed-p)
    (error "cl-obelisk: Graphviz (dot) non trovato. Installalo o verifica il PATH."))

  (let* ((root (first dsl-data))
         (relazioni (loop for branch in (rest dsl-data) nconc (parse-smart-dsl root branch)))
         (rankdir (if (eq orientamento :orizzontale) "LR" "TB"))
         (dim-carta (risolvi-formato-carta carta))
         (est (string-downcase (symbol-name formato)))
         (base-dir (uiop:ensure-directory-pathname dir-base))
         (target-dir (uiop:merge-pathnames* (format nil "~A/" est) base-dir))
         (final-out (uiop:merge-pathnames* (format nil "~A.~A" nome-file est) target-dir)))

    (ensure-directories-exist target-dir)

    (multiple-value-bind (nodi edge-styles) (prepara-mappa relazioni root)
			 (let* ((num-totale (length nodi))
				(distanze (calcola-spaziatura num-totale))
				(graph-attrs `(:rankdir ,rankdir
							:overlap "false"
							:splines "true"
							:nodesep ,(first distanze)
							:ranksep ,(second distanze)
							,@(when dim-carta (list :size dim-carta :ratio "fill"))
							,@(when (> margine 0)
							    (list :margin (format nil "~F" (/ margine 2.54))))))
				(grafo-istanza (make-instance 'mappa-grafo :stile stile :edge-styles edge-styles))
				(grafo-dot (cl-dot:generate-graph-from-roots grafo-istanza nodi graph-attrs)))

			   (uiop:with-temporary-file (:pathname dot-path :keep nil)
						     (with-open-file (s dot-path :direction :output :if-exists :supersede)
								     (cl-dot:print-graph grafo-dot :stream s))
						     (multiple-value-bind (output error-output exit-code)
									  (uiop:run-program (list "dot" (format nil "-T~A" est)
												  (namestring dot-path) "-o" (namestring final-out))
											    :output :string
											    :error-output :string
											    :ignore-error-status t)
									  (unless (zerop exit-code)
									    (error "cl-obelisk: Graphviz (dot) ha fallito con codice ~A~%Output: ~A~%Errore: ~A"
										   exit-code output error-output))))
			   (format t "~%[cl-obelisk] Immagine generata in: ~A" (namestring final-out))))))

(defun genera-tikz-da-dsl (nome-file dsl-data &key (dir-base "output"))
  "Genera un file TikZ (TeX) da DSL-DATA e lo salva in DIR-BASE/tikz/."
  (let* ((root (first dsl-data))
         (relazioni (loop for branch in (rest dsl-data) nconc (parse-smart-dsl root branch)))
         (base-dir (uiop:ensure-directory-pathname dir-base))
         (target-dir (uiop:merge-pathnames* "tikz/" base-dir))
         (final-out (uiop:merge-pathnames* (format nil "~A.tex" nome-file) target-dir)))

    (ensure-directories-exist target-dir)

    (multiple-value-bind (nodi edge-styles) (prepara-mappa relazioni root)
			 (with-open-file (s final-out :direction :output :if-exists :supersede)
					 (esporta-mappa-tikz nodi edge-styles s))
			 (format t "~%[cl-obelisk] TikZ generato in: ~A" (namestring final-out)))))