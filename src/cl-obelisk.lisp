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
           #:genera-tikz-da-dsl))

(in-package :cl-obelisk)

;; --- 1. Strutture Dati (CLOS) ---

(defclass nodo-mappa ()
  ((id       :initarg :id       :accessor nodo-id       :documentation "Etichetta originale (LaTeX ammesso).")
   (livello  :initarg :livello  :initform 0   :accessor nodo-livello  :documentation "Profondità nella gerarchia.")
   (centro-p :initarg :centro-p :initform nil :accessor centro-p      :documentation "Vero se è il nodo radice.")
   (figli    :initform nil      :accessor nodo-figli    :documentation "Lista di istanze nodo-mappa collegate.")))

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
    (:a3 "11.7,16.5!")
    (:a2 "16.5,23.4!")
    (t (if (listp formato)
           (format nil "~A,~A!" (first formato) (second formato))
           nil))))

;; --- 3. Implementazione Protocollo cl-dot (PNG/SVG) ---

(defun calcola-attributi-dimensione (nodo)
  "Definisce font e spessore in base al livello gerarchico."
  (let* ((lvl (nodo-livello nodo))
         (f-size (case lvl (0 26) (1 18) (2 12) (t 9)))
         (p-width (case lvl (0 4.5) (1 2.5) (2 1.2) (t 0.6))))
    (values f-size p-width)))

(defmethod cl-dot:graph-object-node ((graph mappa-grafo) (obj nodo-mappa))
  (multiple-value-bind (f-size p-width) (calcola-attributi-dimensione obj)
    (let* ((label-clean (render-lisp-math (nodo-id obj)))
           (base-attrs `(:fontsize ,f-size :penwidth ,p-width :label ,label-clean)))
      (make-instance 'cl-dot:node
                     :attributes 
                     (case (grafo-stile graph)
                       (:tecnico (append base-attrs '(:shape :box :style :filled :fillcolor "#eeeeee" :fontname "Courier")))
                       (:scientifico (append base-attrs '(:shape :none :fontname "Times-Italic")))
                       (:umanistico (append base-attrs '(:shape :oval :style :filled :fillcolor "#fdf6e3" :fontname "Georgia italic")))
                       (t (append base-attrs '(:shape :ellipse))))))))

(defmethod cl-dot:graph-object-points-to ((graph mappa-grafo) (obj nodo-mappa))
  (loop for figlio in (nodo-figli obj)
        collect (let* ((key (cons (nodo-id obj) (nodo-id figlio)))
                       (stile (gethash key (grafo-edge-styles graph) :default))
                       (attrs (case stile
                                (:tratteggiato '(:style :dashed :arrowhead :vee))
                                (:importante   '(:penwidth 2.5 :color "#000000"))
                                (t             '(:arrowhead :vee)))))
                  (make-instance 'cl-dot:attributed :object figlio :attributes attrs))))

;; --- 4. Esportazione TikZ ---

(defun esporta-mappa-tikz (nodi edge-styles stream)
  "Scrive il codice sorgente TikZ per LaTeX usando le label originali."
  (format stream "\\begin{tikzpicture}[node distance=3cm, every node/.style={draw, fill=gray!10, font=\\small}]~%")
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
  (format stream "\\end{tikzpicture}~%"))

;; --- 5. Logica Core e Parser DSL ---

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

(defun prepara-mappa (relazioni centro-id)
  "Istanzia gli oggetti nodo-mappa e popola gli archi."
  (let ((distanze (calcola-gerarchia centro-id relazioni))
        (nodi-cache (make-hash-table :test 'equal))
        (edge-styles (make-hash-table :test 'equal)))
    (dolist (rel relazioni)
      (destructuring-bind (da-id a-id stile-arco) rel
        (flet ((prendi-nodo (id)
                 (or (gethash id nodi-cache)
                     (setf (gethash id nodi-cache)
                           (make-instance 'nodo-mappa :id id :centro-p (string= id centro-id) :livello (gethash id distanze 3))))))
          (let ((nodo-da (prendi-nodo da-id)) (nodo-a (prendi-nodo a-id)))
            (pushnew nodo-a (nodo-figli nodo-da))
            (setf (gethash (cons da-id a-id) edge-styles) stile-arco)))))
    (values (loop for v being the hash-values of nodi-cache collect v) edge-styles)))

(defun parse-smart-dsl (parent node-data &optional (seen (make-hash-table :test 'equal)))
  "Trasforma il DSL annidato in una lista piatta di relazioni (da a stile)."
  (let* ((stile (first node-data))
         (label (second node-data))
         (children (cddr node-data))
         (rel (list parent label stile)))
    (if (gethash label seen) (list rel)
        (progn (setf (gethash label seen) t)
               (cons rel (loop for child in children nconc (parse-smart-dsl label child seen)))))))

;; --- 6. API Pubbliche con Gestione Cartelle Output ---

(defun genera-da-dsl (nome-file dsl-data &key (stile :tecnico) (formato :png) (orientamento :verticale) (carta :a4) (dir-base "output"))
  "Genera l'immagine salvandola in dir-base/formato/nome-file.estensione"
  (let* ((root (first dsl-data))
         (relazioni (loop for branch in (rest dsl-data) nconc (parse-smart-dsl root branch)))
         (rankdir (if (eq orientamento :orizzontale) "LR" "TB"))
         (dim-carta (risolvi-formato-carta carta))
         (est (string-downcase (symbol-name formato)))
         ;; Creazione del percorso dinamico: output/png/ o output/pdf/
         (target-dir (format nil "~A/~A/" dir-base est))
         (final-out (format nil "~A~A.~A" target-dir nome-file est)))
    
    (ensure-directories-exist target-dir)
    
    (multiple-value-bind (nodi edge-styles) (prepara-mappa relazioni root)
      (let* ((graph-attrs `(:rankdir ,rankdir :overlap "false" :splines "true"
                            ,@(when dim-carta (list :size dim-carta :ratio "fill"))))
             (grafo-istanza (make-instance 'mappa-grafo :stile stile :edge-styles edge-styles))
             (grafo-dot (cl-dot:generate-graph-from-roots grafo-istanza nodi graph-attrs)))
        
        (uiop:with-temporary-file (:pathname dot-path :keep nil)
          (with-open-file (s dot-path :direction :output :if-exists :supersede)
            (cl-dot:print-graph grafo-dot :stream s))
          (uiop:run-program (list "dot" (format nil "-T~A" est) 
                                 (namestring dot-path) "-o" final-out)))
        (format t "~%[cl-obelisk] Immagine generata in: ~A" final-out)))))

(defun genera-tikz-da-dsl (nome-file dsl-data &key (dir-base "output"))
  "Genera il file TikZ salvandolo in dir-base/tikz/nome-file.tex"
  (let* ((root (first dsl-data))
         (relazioni (loop for branch in (rest dsl-data) nconc (parse-smart-dsl root branch)))
         (target-dir (format nil "~A/tikz/" dir-base))
         (path (format nil "~A~A.tex" target-dir nome-file)))
    
    (ensure-directories-exist target-dir)
    
    (multiple-value-bind (nodi edge-styles) (prepara-mappa relazioni root)
      (with-open-file (s path :direction :output :if-exists :supersede)
        (esporta-mappa-tikz nodi edge-styles s))
      (format t "~%[cl-obelisk] TikZ generato in: ~A" path))))
