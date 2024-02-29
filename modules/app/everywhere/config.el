;;; app/everywhere/config.el -*- lexical-binding: t; -*-

(use-package! emacs-everywhere
  ;; Entry points into this package are autoloaded; i.e. the `emacs-everywhere'
  ;; function, meant to be called directly via emacsclient. See this module's
  ;; readme for details.
  :defer t
  :config
  (set-yas-minor-mode! 'emacs-everywhere-mode)

  ;; HACK Inhibit MAJOR-MODE-local-vars-hook in emacs-everywhere buffers,
  ;;   because Doom commonly starts servers and other extraneous services on
  ;;   this hook, which will rarely work well in emacs-everywhere's temporary
  ;;   buffers anyway.
  (setq-hook! 'emacs-everywhere-init-hooks rmcs-inhibit-local-var-hooks t)

  (after! rmcs-modeline
    (rmcs-modeline-def-segment emacs-everywhere
      (concat
       (rmcs-modeline-spc)
       (when (emacs-everywhere-markdown-p)
         (concat
          (nerd-icons-octicon "nf-oct-markdown" :face 'nerd-icons-green :v-adjust 0.02)
          (rmcs-modeline-spc)))
       (propertize (emacs-everywhere-app-class emacs-everywhere-current-app)
                   'face 'rmcs-modeline-project-dir)
       (rmcs-modeline-spc)
       (propertize (truncate-string-to-width
                    (emacs-everywhere-app-title emacs-everywhere-current-app)
                    45 nil nil "…")
                   'face 'rmcs-modeline-buffer-minor-mode)))
    (rmcs-modeline-def-modeline 'emacs-everywhere
      '(bar modals emacs-everywhere buffer-position
        word-count parrot selection-info)
      '(input-method major-mode checker
        #("  " 0 1 ; "Exit to app" icon + a little padding
          (rear-nonsticky t
           display (raise -0.25)
           face (:inherit rmcs-modeline-emphasis :family "Material Icons" :height 1.1)
           help-echo "This is an Emacs Everywhere window"))))
    (add-hook! 'emacs-everywhere-mode-hook
      (defun +everywhere-set-modeline ()
        (rmcs-modeline-set-modeline 'emacs-everywhere))))
  (add-hook! 'emacs-everywhere-init-hooks
    (defun +everywhere-clear-persp-info-h ()
      (when (bound-and-true-p persp-mode)
        (setq persp-emacsclient-init-frame-behaviour-override nil)))))
