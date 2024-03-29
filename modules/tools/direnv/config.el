;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(use-package! envrc
  :hook (rmcs-first-file . envrc-global-mode)
  :config
  (add-to-list 'rmcs-debug-variables 'envrc-debug)

  (set-popup-rule! "^\\*envrc\\*" :quit t :ttl 0)

  ;; HACK: Normally, envrc updates on `after-change-major-mode-hook' (runs after
  ;;   a major mode's body and hooks). IMHO, this is too late; a mode's hooks
  ;;   might depend on environmental state that direnv sets up (e.g. starting an
  ;;   LSP server that expects project-specific envvars), so I move it to
  ;;   `change-major-mode-after-body-hook' instead, which runs before said
  ;;   hooks, but not the body.
  (add-hook! 'envrc-global-mode-hook
    (defun +direnv-init-global-mode-earlier-h ()
      (let ((fn #'envrc-global-mode-enable-in-buffers))
        (if (not envrc-global-mode)
            (remove-hook 'change-major-mode-after-body-hook fn)
          (remove-hook 'after-change-major-mode-hook fn)
          (add-hook 'change-major-mode-after-body-hook fn 100)))))

  ;; ...However, the above hack causes envrc to trigger in its own, internal
  ;; buffers, causing extra direnv errors.
  (defadvice! +direnv--debounce-update-a (&rest _)
    "Prevent direnv from running multiple times, consecutively in a buffer."
    :before-while #'envrc--update
    (not (string-prefix-p "*envrc" (buffer-name))))

  (defadvice! +direnv--fail-gracefully-a (&rest _)
    "Don't try to use direnv if the executable isn't present."
    :before-while #'envrc-global-mode
    (or (executable-find envrc-direnv-executable)
        (ignore (rmcs-log "Failed to locate direnv executable; aborting envrc-global-mode"))))

  ;; Ensure babel's execution environment matches the host buffer's.
  (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)

  ;; Make sure any envrc changes are propagated after a `rmcs/reload'
  (add-hook 'rmcs-after-reload-hook #'envrc-reload-all))
