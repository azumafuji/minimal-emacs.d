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

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

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


;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config Changes

;; Hide warnings and display only errors
(setq warning-minimum-level :error)

(setq pixel-scroll-precision-use-momentum nil)
(pixel-scroll-precision-mode 1)

(show-paren-mode +1)  ; Paren match highlighting
(winner-mode 1)
(delete-selection-mode 1)  ; Replace selected text with typed text

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)
;; Set docview DPI
(setq doc-view-resolution 300)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Extras

(load-file (expand-file-name "extras/textsize.el" user-emacs-directory))
(use-package textsize
  :init (textsize-mode))

(load-file (expand-file-name "extras/themes.el" user-emacs-directory))
(load-file (expand-file-name "extras/base.el" user-emacs-directory))
(load-file (expand-file-name "extras/orgconfig.el" user-emacs-directory))
(load-file (expand-file-name "extras/dev.el" user-emacs-directory))
(load-file (expand-file-name "extras/aider.el" user-emacs-directory))
(load-file (expand-file-name "extras/setup-windows.el" user-emacs-directory))


