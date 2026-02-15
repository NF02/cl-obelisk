(defsystem "cl-obelisk"
  :version "0.1.0"
  :author "Nicola Ferru"
  :license "MIT"
  :depends-on ("cl-dot" "uiop") ; Qui dichiariamo le dipendenze esterne
  :components ((:module "src"
                :components
                ((:file "cl-obelisk"))))
  :description "Generatore di mappe concettuali da DSL tramite Graphviz.")
