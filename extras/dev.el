;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; mise
(use-package mise
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook #'global-mise-mode))

