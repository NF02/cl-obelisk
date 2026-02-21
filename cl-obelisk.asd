(defsystem "cl-obelisk"
  :version "0.1.0"
  :author "Nicola Ferru"
  :license "GPLv3"
  :depends-on ("cl-dot" "uiop" "cl-ppcre") ; Qui dichiariamo le dipendenze esterne
  :components ((:module "src"
                :components
                ((:file "cl-obelisk"))))
  :description "Generatore di mappe concettuali da DSL tramite Graphviz."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.org")))
