;;; ui/rmcs/config.el -*- lexical-binding: t; -*-

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)


(use-package! doom-themes
  ;; improve integration w/ org-mode
  :hook (doom-load-theme . rmcs-themes-org-config)
  :init (setq doom-theme 'doom-one)
  ;; more Atom-esque file icons for neotree/treemacs
  ;; (when (modulep! :ui neotree)
  ;;   (add-hook 'rmcs-load-theme-hook #'rmcs-themes-neotree-config)
  ;;   (setq rmcs-themes-neotree-enable-variable-pitch t
  ;;         rmcs-themes-neotree-file-icons 'simple
  ;;         rmcs-themes-neotree-line-spacing 2))
  )


(use-package! solaire-mode
  :hook (doom-load-theme . solaire-global-mode)
  :hook (+popup-buffer-mode . turn-on-solaire-mode))
