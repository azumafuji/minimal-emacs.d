;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package diminish
  :ensure t)

(use-package compile-angel
  :ensure t
  :demand t
  :diminish compile-angel-on-save-local-mode
  :diminish compile-angel-on-load-mode
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1


;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Emacs Base Configuration


;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.

(defun ds/find-recentf (file)
  "Use `completing-read' to open a recent FILE."
  (interactive (list (completing-read "Find recent file: "
                                      recentf-list)))
  (when file
    (find-file file)))

(use-package recentf
  :ensure t
  :defer t
  :bind
  ("C-x C-r" . #'ds/find-recentf)
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

(setq recentf-exclude `(,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory)))

(add-hook 'after-init-hook #'recentf-mode)


;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

;; Slugify
(defun ds/slugify (str)
  (interactive)
  (string-replace " " "-"
                  (string-trim
                   (string-limit
                    (string-clean-whitespace str) 32))))

(defun ds/yank-with-slugify ()
  (interactive)
  (let ((yank-transform-functions
	 '(ds/slugify)))
    (call-interactively #'yank)))

(global-set-key (kbd "C-c y") 'ds/yank-with-slugify)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config Changes

;; Hide warnings and display only errors
(setq warning-minimum-level :error)

(setq pixel-scroll-precision-use-momentum nil)
(pixel-scroll-precision-mode 1)


(show-paren-mode +1)  ; Paren match highlighting
(winner-mode 1)
(delete-selection-mode 1)  ; Replace selected text with typed text
(pixel-scroll-precision-mode 1)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)
;; Set docview DPI
(setq doc-view-resolution 300)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

(setq org-directory "~/Documents/org/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq project-files (file-expand-wildcards (concat org-directory "project-*.org")))

(setq org-refile-targets `(("work.org" :maxlevel . 1)
                           ("inbox.org" :maxlevel . 2)
                           (,project-files :maxlevel . 1)))

;(load-file (expand-file-name "extras/base.el" user-emacs-directory))
(setq org-agenda-files (append project-files '("inbox.org" "work.org" "notes.org")))

;;; Phase 3 variables
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
  '(org-level-3 ((t (:inherit outline-3 :height 1))))
  '(org-level-4 ((t (:inherit outline-4 :height 1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1)))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Extras

(load-file (expand-file-name "extras/textsize.el" user-emacs-directory))
(use-package textsize
  :init (textsize-mode))

(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts

(setq-default line-spacing 0.2)
;; For the GUI use this font and line spacing
(set-face-attribute 'default nil
                    :family "IosevkaTermSlab Nerd Font Mono" :weight 'Regular)
(set-face-attribute 'default nil
                    :family "IBM Plex Mono" :weight 'Regular)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "IBM Plex Sans" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "IBM Plex Mono" :height 1.0)

;; Setup Visual line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(add-hook 'window-setup-hook
          (lambda ()
            (set-face-attribute 'mode-line-active nil :height 1.0 :inherit 'variable-pitch)
            (set-face-attribute 'mode-line-inactive nil :height 0.8 :inherit 'variable-pitch)))
                     
