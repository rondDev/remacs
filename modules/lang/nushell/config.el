;;; lang/nushell/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(use-package! nushell-mode
  :mode ("\\.nu\\'" . nushell-mode))

(when (modulep! +lsp)
  (add-hook! 'nushell-mode-local-vars-hook #'lsp! 'append))
