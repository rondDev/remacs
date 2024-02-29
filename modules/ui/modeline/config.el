;;; ui/modeline/config.el -*- lexical-binding: t; -*-

(when (modulep! +light)
  (load! "+light"))


(use-package! rmcs-modeline
  :unless (modulep! +light)
  :hook (rmcs-after-init . rmcs-modeline-mode)
  :hook (rmcs-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (rmcs-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq rmcs-modeline-bar-width 3
        rmcs-modeline-github nil
        rmcs-modeline-mu4e nil
        rmcs-modeline-persp-name nil
        rmcs-modeline-minor-modes nil
        rmcs-modeline-major-mode-icon nil
        rmcs-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        rmcs-modeline-buffer-encoding 'nondefault
        rmcs-modeline-default-eol-type
        (pcase rmcs-system ('macos 2) ('windows 1) (_ 0)))

  :config
  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook 'rmcs-load-theme-hook #'rmcs-modeline-refresh-bars)

  (add-to-list 'rmcs-modeline-mode-alist '(+rmcs-dashboard-mode . dashboard))
  (add-hook! 'magit-mode-hook
    (defun +modeline-hide-in-non-status-buffer-h ()
      "Show minimal modeline in magit-status buffer, no modeline elsewhere."
      (if (eq major-mode 'magit-status-mode)
          (rmcs-modeline-set-modeline 'magit)
        (hide-mode-line-mode))))

  ;; Some functions modify the buffer, causing the modeline to show a false
  ;; modified state, so force them to behave.
  (defadvice! +modeline--inhibit-modification-hooks-a (fn &rest args)
    :around #'ws-butler-after-save
    (with-silent-modifications (apply fn args)))


  ;;
  ;;; Extensions
  (use-package! anzu
    :after-call isearch-mode)

  (use-package! evil-anzu
    :when (modulep! :editor evil)
    :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
    :config (global-anzu-mode +1)))
