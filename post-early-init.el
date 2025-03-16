;;; post-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'package-archives
             '("ox-odt" . "https://kjambunathan.github.io/elpa/") t)

(add-hook 'window-setup-hook
          (lambda ()
            (set-face-attribute 'mode-line-active nil :height 1.0 :inherit 'variable-pitch)
            (set-face-attribute 'mode-line-inactive nil :height 0.8 :inherit 'variable-pitch)))
