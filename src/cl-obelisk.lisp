;;; cl-obelisk - Generatore di Mappe Concettuali con Smart-DSL
;;; Copyright (C) 2026 Nicola Ferru Aka NFVblog
;;; License: GPLv3

(defpackage :cl-obelisk
  (:use :cl)
  (:import-from :cl-obelisk.kernel
		#:nodo-mappa #:mappa-grafo #:graphviz-installed-p
		#:prepara-mappa #:parse-smart-dsl #:risolvi-formato-carta
		#:calcola-spaziatura #:string-replace-all #:render-lisp-math
		#:nodo-id #:nodo-id-sanitizzato #:nodo-figli #:grafo-stile
		#:grafo-edge-styles)
  (:import-from :cl-obelisk.api
		#:genera-da-dsl #:genera-tikz-da-dsl #:esporta-mappa-tikz)
  (:export #:nodo-mappa
	   #:mappa-grafo
	   #:esporta-mappa
	   #:genera-da-dsl
	   #:esporta-mappa-tikz
	   #:genera-tikz-da-dsl
	   #:graphviz-installed-p
	   #:prepara-mappa #:parse-smart-dsl #:risolvi-formato-carta
	   #:calcola-spaziatura #:string-replace-all #:render-lisp-math
	   #:nodo-id #:nodo-id-sanitizzato #:nodo-figli #:grafo-stile
	   #:grafo-edge-styles))

(in-package :cl-obelisk)