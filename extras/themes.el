;;; themes.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-


;; This file sets up themes and visual enhancements to emacs like spacious
;; padding. Configured to automatically switch between light and dark modes

;; 
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  ;; Load the theme of your choice. 
  (load-theme 'modus-operandi t t)
  (load-theme 'modus-vivendi t t)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))


;; To automatically switch between light and dark modes 
(use-package auto-dark
  :ensure t
  :config
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  :init
  (add-hook 'after-init-hook
                (lambda ()
                  (auto-dark-mode))))


;; Function to set background for sudo buffers with autosudo depending on light
;; or dark mode.
(defun ds/theme-dark ()
  (eq 'dark (frame-parameter nil 'background-mode)))

(defun ds/sudo-bg-color ()
  (if (ds/theme-dark)
      (setq buffer-face-mode-face '(:background "#200000"))
    (setq buffer-face-mode-face '(:background "#FFF0F9")))
  (buffer-face-mode 1))

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
