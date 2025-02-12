;;; base.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package auto-sudoedit
  :ensure t
  :config
  (auto-sudoedit-mode 1))

;; detecting dark modes
;; borrowed from auto-dark.el

(require 'dbus nil t)
(defun auto-dark--is-dark-mode-dbus ()
  "Use Emacs built-in D-Bus function to determine if dark theme is enabled."
  (eq 1 (caar (dbus-ignore-errors
                (dbus-call-method
                 :session
                 "org.freedesktop.portal.Desktop"
                 "/org/freedesktop/portal/desktop"
                 "org.freedesktop.portal.Settings" "Read"
                 "org.freedesktop.appearance" "color-scheme")))))

;; Set background for sudo buffers
(defun ds/sudo-bg-color ()
  (if (ds/theme-dark)
      (setq buffer-face-mode-face '(:background "#200000"))
    (setq buffer-face-mode-face '(:background "#FFF0F9")))
  (buffer-face-mode 1))

(defun ds/theme-dark ()
  (eq 'dark (frame-parameter nil 'background-mode)))

(defun ds/sudo-set-bg ()
  (cond ((string-match-p "sudo" (concat "." (file-remote-p default-directory)))
         (ds/sudo-bg-color))))

(add-hook 'find-file-hook 'ds/sudo-set-bg)

(use-package spacious-padding
  :ensure t
  :bind
  ("<f8>" . spacious-padding-mode)
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 2
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8)))

(spacious-padding-mode 1)
(define-key global-map (kbd "<f8>") #'spacious-padding-mode)

(add-to-list 'load-path (expand-file-name "lambda-themes" user-emacs-directory))
(require 'lambda-themes)
(load-theme 'lambda-light t)
(load-theme 'lambda-dark t t)

(use-package auto-dark
  :ensure t
  :config
  :custom
  (auto-dark-themes '((lambda-dark) (lambda-light)))
  :init
  (add-hook 'after-init-hook
                (lambda ()
                  (auto-dark-mode))))

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-;" . jinx-correct)
         ("C-M-$" . jinx-languages)))

