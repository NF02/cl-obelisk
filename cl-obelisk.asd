(asdf:defsystem "cl-obelisk"
  :description "Generatore di mappe concettuali da DSL tramite Graphviz."
  :version "0.1.0"
  :author "Nicola Ferru"
  :license "GPLv3"

  :depends-on
  (:cl-dot
   :uiop
   :cl-ppcre)

  :components
  ((:module "src"
    :components
    ((:file "cl-obelisk"))))

  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname
      (or *load-pathname* *compile-file-pathname*)
      "README.org")))
