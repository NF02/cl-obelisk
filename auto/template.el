;;; cl-obelisk_template.el --- Inserisci template cl-obelisk in Emacs

;;; Usage:
;;;   M-x cl-obelisk-insert-template
;;
;;; Oppure nel tuo .emacs:
;;;   (load "/percorso/a/cl-obelisk/src/template.el")

(defun cl-obelisk-insert-template (&optional nome-file)
  "Inserisce il template cl-obelisk nel buffer corrente."
  (interactive "sNome file: ")
  (unless nome-file
    (setq nome-file (file-name-sans-extension (buffer-name))))
  (let ((nome (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" nome-file)))
    (insert (format ";;; --- %s ---\n(ql:quickload :cl-obelisk)\n\n(in-package :cl-obelisk)\n\n(defparameter *mappa-%s*\n  '(\"Nodo Centrale\"\n    (:importante \"Figlio 1\"\n     \"Nodo A\")\n    (:normal \"Figlio 2\"\n     \"Nodo B\")))\n\n(genera-da-dsl \"%s\" *mappa-%s*\n               :stile :tecnico\n               :carta :a4\n               :formato :png)\n\n;;; Genera TikZ (LaTeX):\n; (genera-tikz-da-dsl \"%s\" *mappa-%s*)\n"
 nome nome nome nome))
    (message "Template cl-obelisk inserito!")))

(provide 'cl-obelisk_template)
;;; cl-obelisk_template.el ends here