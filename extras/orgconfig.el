;;; orgconfig.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(setq org-directory "~/Documents/org/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq project-files (file-expand-wildcards (concat org-directory "project-*.org")))

(setq org-refile-targets `(("work.org" :maxlevel . 1)
                           ("inbox.org" :maxlevel . 2)
                           (,project-files :maxlevel . 1)))

(setq org-agenda-files (append project-files '("inbox.org" "work.org" "notes.org")))

(setq org-startup-indented t)
(setq org-use-fast-todo-selection 'expert)

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . jinx-mode))    ; spell checking!
  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t))


(use-package org
  :config
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (plantuml . t)
     (latex . t)
     (python . t)
     (shell . t)
     (sql . t)
     (emacs-lisp . t)))

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (or  (string= lang "ditaa")              
              (string= lang "dot")
              (string= lang "plantuml")
              (string= lang "latex")
              (string= lang "python")
              (string= lang "sql")
              (string= lang "emacs-lisp")
              (string= lang "sh")
              )))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  
  (setq org-babel-latex-pdf-svg-process "pdf2svg %F %O")
  
  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("m" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("r" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and All Todos"
             ((agenda)
              (todo)))
            ("w" "Work" agenda ""
             ((org-agenda-files '("work.org")))))))

(use-package ox-gfm
  :ensure t
  :defer t)
(use-package ox-epub
  :ensure t
  :defer t)
(use-package ox-odt
  :ensure t
  :defer t)
(use-package ox-tufte
  :ensure t
  :defer t)
(use-package ob-sql-mode
  :ensure t
  :defer t)

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))


(setq org-latex-packages-alist '(("" "listings")
                                 ("" "booktabs")
                                 ("AUTO" "polyglossia" t ("lualatex" "xelatex"))
                                 ("" "grffile")
                                 ("" "unicode-math")
                                 ("" "xcolor")
                                 ("inkscapelatex=false" "svg")
                                 ("" "fontspec")))

(setq org-latex-pdf-process '("latexmk -lualatex -shell-escape -quiet -f %f"))
(setq org-latex-compiler "lualatex")

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("memoir"
                 "\\documentclass[9pt,a4paper,extrafontsizes,article]{memoir}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")       
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("tufte"
                 "\\documentclass[8pt]{tufte-handout}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")       
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
               )
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}"
                 ("\\section{%s}"        . "\\section*{%s}")
                 ("\\subsection{%s}"     . "\\subsection*{%s}")
                 ("\\subsubsection{%s}"  . "\\subsubsection*{%s}"))))

(setq org-latex-image-default-width "")

