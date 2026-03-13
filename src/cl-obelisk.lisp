;;; cl-obelisk - Generatore di Mappe Concettuali con Smart-DSL
;;; Copyright (C) 2026 Nicola Ferru Aka NFVblog
;;; License: GPLv3

(defpackage :cl-obelisk
  (:use :cl)
  (:export #:nodo-mappa
           #:mappa-grafo
           #:esporta-mappa
           #:genera-da-dsl
           #:esporta-mappa-tikz
           #:genera-tikz-da-dsl
           #:graphviz-installed-p))

(in-package :cl-obelisk)

;; --- 1. Strutture Dati (CLOS) ---

(defclass nodo-mappa ()
  ((id       :initarg :id       :accessor nodo-id                     :documentation "Etichetta originale (LaTeX ammesso).")
   (livello  :initarg :livello  :initform 0   :accessor nodo-livello  :documentation "Profondità nella gerarchia.")
   (centro-p :initarg :centro-p :initform nil :accessor nodo-centro-p :documentation "Vero se è il nodo radice.")
   (figli    :initform nil      :accessor nodo-figli                  :documentation "Lista di istanze nodo-mappa collegate.")))

(defclass mappa-grafo (cl-dot:graph)
  ((stile       :initarg :stile       :accessor grafo-stile)
   (edge-styles :initarg :edge-styles :accessor grafo-edge-styles)))

;; --- 2. Utility per Stringhe e Matematica (Pure Lisp) ---

(defun string-replace-all (old new string)
  "Sostituisce tutte le occorrenze di una sottostringa in modo iterativo e sicuro."
  (with-output-to-string (out)
    (loop with old-len = (length old)
          for start = 0 then (+ pos old-len)
          for pos = (search old string :start2 start)
          do (write-string (subseq string start (or pos (length string))) out)
          while pos
          do (write-string new out))))

(defun render-lisp-math (label)
  "Trasforma LaTeX in testo leggibile per il PNG senza sbilanciare le parentesi."
  (unless (and (stringp label) (plusp (length label)) (char= (char label 0) #\$))
    (return-from render-lisp-math label))
  (let ((res (remove #\$ label))
        (mappa '(("\\frac" . "/") ("\\sqrt" . "√") ("\\alpha" . "α")
                 ("\\beta" . "β") ("\\pi" . "π") ("{" . "(") ("}" . ")"))))
    (dolist (pair mappa)
      (setf res (string-replace-all (car pair) (cdr pair) res)))
    (remove #\\ res)))

(defun nodo-id-sanitizzato (nodo)
  "Crea un ID alfanumerico per TikZ evitando caratteri speciali."
  (concatenate 'string "n" 
               (remove-if-not #'alphanumericp 
                              (string-replace-all " " "" (nodo-id nodo)))))

(defun risolvi-formato-carta (formato)
  "Mappa i simboli dei formati carta nelle dimensioni Graphviz."
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

;; --- 3. Implementazione Protocollo cl-dot (PNG/SVG) ---

(defun ottenere-fattore-scala (formato)
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
  "Riduce il fattore di scala se ci sono troppi nodi."
  (cond ((< num-nodi 10) 1.2)
        ((< num-nodi 50) 1.0)
        (t (max 0.6 (/ 50.0 num-nodi)))))

(defun calcola-scala-font (formato)
  "Applica una radice quadrata al fattore per evitare che i font diventino giganti."
  (let ((base-scala (ottenere-fattore-scala formato)))
    (sqrt base-scala)))

(defun identifica-cluster (nodo)
  (if (nodo-centro-p nodo)
      nil
      (format nil "cluster_~A" (nodo-id nodo))))

(defun calcola-attributi-adattivi (nodo formato num-totale-nodi)
  (let* ((lvl (nodo-livello nodo))
         (scala-formato (ottenere-fattore-scala formato))
         (scala-densita (calcola-fattore-densita num-totale-nodi))
         (scala-finale (* scala-formato scala-densita)))
    (values (* (case lvl (0 26) (1 18) (2 14) (t 12)) scala-finale)
            (* (case lvl (0 4.5) (1 2.5) (2 1.2) (t 0.8)) scala-finale))))

(defmethod cl-dot:graph-object-node ((graph mappa-grafo) (obj nodo-mappa))
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
			  (t            '(:shape :ellipse)))))
      (make-instance 'cl-dot:node
                     :attributes (append base-attrs style-attrs group-attr)))))

(defmethod cl-dot:graph-object-points-to ((graph mappa-grafo) (obj nodo-mappa))
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

;; --- 4. Esportazione TikZ con distanza dinamica ---

(defun esporta-mappa-tikz (nodi edge-styles stream)
  "Scrive il codice sorgente TikZ per LaTeX usando le label originali.
   La distanza tra i nodi viene calcolata dinamicamente in base al numero di nodi."
  (let* ((num-nodi (length nodi))
         (distanza (max 1.5 (/ 15 (sqrt num-nodi))))) ; distanza in cm
    (format stream "\\begin{tikzpicture}[node distance=~,2fcm, every node/.style={draw, fill=gray!10, font=\\small}]~%" distanza)
    ;; Renderizza i nodi
    (dolist (n nodi)
      (format stream "  \\node (~A) {~A};~%" (nodo-id-sanitizzato n) (nodo-id n)))
    ;; Renderizza gli archi
    (dolist (n nodi)
      (dolist (figlio (nodo-figli n))
        (let* ((key (cons (nodo-id n) (nodo-id figlio)))
               (stile (gethash key edge-styles :default))
               (t-style (if (eq stile :tratteggiato) "dashed, ->" "->")))
          (format stream "  \\draw[~A] (~A) -- (~A);~%" t-style (nodo-id-sanitizzato n) (nodo-id-sanitizzato figlio)))))
    (format stream "\\end{tikzpicture}~%")))

;; --- 5. Logica Core e Parser DSL ---

(defun calcola-spaziatura (num-nodi &optional (fattore-scala 4.0))
  "Calcola la spaziatura basata sulla radice quadrata inversa per una distribuzione ottimale su 2D."
  (let* ((n (max 1.0 (float num-nodi)))
         (spaziatura (/ fattore-scala (sqrt n)))
         ;; Applichiamo un limite inferiore fisico (es. 0.2 unità per non sovrapporsi)
         (min-s 0.2))
    (list (max min-s spaziatura) 
	  (max min-s (* spaziatura 1.3)))))

(defun calcola-gerarchia (centro-id relazioni)
  "Calcola la distanza BFS dalla radice."
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
  "Recupera il valore per key; se non esiste, inserisce default-value e lo restituisce."
  (multiple-value-bind (value exists)
      (gethash key hash-table)
    (if exists
        value
        (setf (gethash key hash-table) default-value))))

(defun prepara-mappa (relazioni centro-id)
  (let ((nodi-cache (make-hash-table :test 'equal))
        (edge-styles (make-hash-table :test 'equal)))
    ;; Primo passaggio: assicura che il nodo centrale esista
    (setf (gethash centro-id nodi-cache) 
          (make-instance 'nodo-mappa :id centro-id :centro-p t))
    
    (dolist (rel relazioni)
      (destructuring-bind (da-id a-id stile-arco &rest opt) rel
        (let ((nodo-da (or (gethash da-id nodi-cache)
                           (setf (gethash da-id nodi-cache) (make-instance 'nodo-mappa :id da-id))))
              (nodo-a  (or (gethash a-id nodi-cache)
                           (setf (gethash a-id nodi-cache) (make-instance 'nodo-mappa :id a-id))))
              (constraint (getf opt :constraint t)))
          (pushnew nodo-a (nodo-figli nodo-da))
          (setf (gethash (cons da-id a-id) edge-styles) (list stile-arco :constraint constraint)))))
    (values (loop for v being the hash-values of nodi-cache collect v) edge-styles)))

(defun parse-smart-dsl (parent node-data &optional (seen (make-hash-table :test 'equal)))
  (cond
    ((stringp node-data)
     (list (list parent node-data :default)))
    (t
     (let* ((stile (or (first node-data) :default))
            (label (second node-data))
            (children (cddr node-data)))
       
       (if (and label (stringp label))
           (let ((rel (list parent label stile)))
             (if (gethash (cons parent label) seen)
                 (list (append rel '(:constraint "false")))
                 (progn
                   (setf (gethash (cons parent label) seen) t)
                   (cons rel (loop for child in children
                                   when child
                                   nconc (parse-smart-dsl label child seen))))))
           nil)))))

;; --- 6. Controllo Graphviz ---

(defun graphviz-installed-p ()
  "Verifica se il comando 'dot' di Graphviz è installato e accessibile."
  (handler-case
      (let ((exit-code 
              (nth-value 2 (uiop:run-program '("dot" "-V") 
                                             :output nil 
                                             :error-output nil 
                                             :ignore-error-status t))))
        (zerop exit-code))
    (error () nil)))

;; --- 7. API Pubbliche con Gestione Cartelle Output e Margini ---

(defun genera-da-dsl (nome-file dsl-data &key (stile :tecnico) (formato :png) 
                                 (orientamento :verticale) (carta :a4) 
                                 (dir-base "output")
                                 (margine 0))
  "Genera l'immagine salvandola in dir-base/formato/nome-file.estensione"
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
  "Genera il file TikZ salvandolo in dir-base/tikz/nome-file.tex"
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
