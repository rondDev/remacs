;;; lisp/lib/sandbox.el -*- lexical-binding: t; -*-

(defvar rmcs-sandbox-buffer-name "*rmcs:sandbox*"
  "Name of the Rmcs sandbox buffer.")

(defvar rmcs-sandbox-dir
  (expand-file-name "rmcs-sandbox" (temporary-file-directory))
  "TODO")

(defvar rmcs-sandbox-preamble
  ";; Welcome to the sandbox!
;;
;; This is a test bed for running Emacs Lisp in another instance of Emacs that
;; has varying amounts of Rmcs loaded:
;;
;; - vanilla Emacs (nothing loaded)         \\[rmcs--run-vanilla-emacs]
;; - vanilla Rmcs (only Rmcs core)          \\[rmcs--run-vanilla-rmcs]
;; - Rmcs + modules - your private config   \\[rmcs--run-vanilla-rmcs+]
;; - Rmcs + modules + your private config   \\[rmcs--run-full-rmcs]
;;
;; This is done without sacrificing access to installed packages. Use the sandbox
;; to reproduce bugs and determine if Rmcs is to blame.\n\n"
  "TODO")

(defun rmcs--sandbox-launch (args forms)
  (require 'package)
  (require 'restart-emacs)
  (let* ((sandbox-file (expand-file-name "init.el" rmcs-sandbox-dir))
         (args (append args (list "-l" sandbox-file))))
    (delete-directory rmcs-sandbox-dir 'recursive)
    (make-directory rmcs-sandbox-dir 'parents)
    (with-temp-file sandbox-file
      (prin1 forms (current-buffer)))
    (condition-case-unless-debug e
        (cond ((display-graphic-p)
               (if (memq system-type '(windows-nt ms-dos))
                   (restart-emacs--start-gui-on-windows args)
                 (restart-emacs--start-gui-using-sh args)))
              ((memq system-type '(windows-nt ms-dos))
               (user-error "Cannot start another Emacs from Windows shell."))
              ((suspend-emacs
                (format "%s %s -nw; fg"
                        (shell-quote-argument (restart-emacs--get-emacs-binary))
                        (mapconcat #'shell-quote-argument args " ")))))
      (error
       (delete-directory rmcs-sandbox-dir 'recursive)
       (signal (car e) (cdr e))))))


(defun rmcs--sandbox-run (&optional mode)
  "TODO"
  (rmcs--sandbox-launch
   (unless (eq mode 'rmcs) '("-Q"))
   (let ((forms
          (read (format "(progn\n%s\n)"
                        (buffer-substring-no-properties
                         (point-min)
                         (point-max))))))
     (if (eq mode 'rmcs)
         forms
       `(progn
          ;; rmcs variables
          (setq init-file-debug t
                rmcs-emacs-dir ,rmcs-emacs-dir
                rmcs-cache-dir ,(expand-file-name "cache/" rmcs-sandbox-dir)
                rmcs-data-dir  ,(expand-file-name "data/" rmcs-sandbox-dir))
          (defun rmcs--write-to-etc-dir-a (fn &rest args)
            (let ((user-emacs-directory rmcs-data-dir))
              (apply fn args)))
          (advice-add #'locate-user-emacs-file :around #'rmcs--write-to-etc-dir-a)
          ;; emacs essential variables
          (setq before-init-time (current-time)
                after-init-time nil
                init-file-debug init-file-debug
                noninteractive nil
                process-environment (get 'process-environment 'initial-value)
                exec-path (get 'exec-path 'initial-value)
                load-path ',load-path
                user-init-file load-file-name)
          ;; package.el
          (setq package--init-file-ensured t
                package-user-dir ,package-user-dir
                package-archives ',package-archives)
          ;; (add-hook 'kill-emacs-hook
          ;;           (lambda ()
          ;;             (delete-file user-init-file)
          ;;             (when (file-equal-p user-emacs-directory ,rmcs-sandbox-dir)
          ;;               (delete-directory user-emacs-directory 'recursive))))
          (with-eval-after-load 'undo-tree
            ;; HACK `undo-tree' sometimes throws errors because
            ;;      `buffer-undo-tree' isn't correctly initialized.
            (setq-default buffer-undo-tree (make-undo-tree)))
          ;; Then launch as much about Emacs as we can
          (defun --run-- () ,forms)
          ,(pcase mode
             (`rmcs
              '(--run--))
             (`vanilla-rmcs+ ; Rmcs core + modules - private config
              `(progn
                 (load-file ,(expand-file-name "rmcs.el" rmcs-core-dir))
                 (setq rmcs-modules-dirs (list rmcs-modules-dir))
                 (let ((rmcs-init-modules-p t))
                   (rmcs-initialize)
                   (rmcs-initialize-core-modules))
                 (setq rmcs-modules ',rmcs-modules)
                 (maphash (lambda (key plist)
                            (rmcs-module-put
                             (car key) (cdr key)
                             :path (rmcs-module-locate-path (car key) (cdr key))))
                          rmcs-modules)
                 (--run--)
                 (rmcs-run-hooks 'rmcs-before-modules-init-hook)
                 (maphash (rmcs-module-loader rmcs-module-init-file) rmcs-modules)
                 (rmcs-run-hooks 'rmcs-after-modules-init-hook)
                 (rmcs-run-hooks 'rmcs-before-modules-config-hook)
                 (maphash (rmcs-module-loader rmcs-module-config-file) rmcs-modules)
                 (rmcs-run-hooks 'rmcs-after-modules-config-hook)))
             (`vanilla-rmcs  ; only Rmcs core
              `(progn
                 (load-file ,(expand-file-name "rmcs.el" rmcs-core-dir))
                 (let ((rmcs-init-modules-p t))
                   (rmcs-initialize)
                   (rmcs-initialize-core-modules))
                 (--run--)))
             (`vanilla       ; nothing loaded
              `(progn
                 (if (boundp 'comp-deferred-compilation)
                     ;; REVIEW Remove me after a month
                     (setq comp-deferred-compilation nil
                           comp-deferred-compilation-deny-list ',(bound-and-true-p native-comp-async-env-modifier-form)
                           comp-async-env-modifier-form ',(bound-and-true-p native-comp-async-env-modifier-form)
                           comp-eln-load-path ',(bound-and-true-p native-comp-eln-load-path))
                   (setq native-comp-deferred-compilation nil
                         native-comp-deferred-compilation-deny-list ',(bound-and-true-p native-comp-async-env-modifier-form)
                         native-comp-async-env-modifier-form ',(bound-and-true-p native-comp-async-env-modifier-form)
                         native-comp-eln-load-path ',(bound-and-true-p native-comp-eln-load-path)))
                 (package-initialize t)
                 (--run--))))
          ;; Then rerun Emacs' startup hooks to simulate a fresh Emacs session,
          ;; because they've already fired.
          (fset 'rmcs-run-hook  #',(symbol-function #'rmcs-run-hook))
          (fset 'rmcs-run-hooks #',(symbol-function #'rmcs-run-hooks))
          (fset 'rmcs-run-all-startup-hooks-h #',(symbol-function #'rmcs-run-all-startup-hooks-h))
          (rmcs-run-all-startup-hooks-h))))))

(fset 'rmcs--run-vanilla-emacs (cmd! (rmcs--sandbox-run 'vanilla)))
(fset 'rmcs--run-vanilla-rmcs  (cmd! (rmcs--sandbox-run 'vanilla-rmcs)))
(fset 'rmcs--run-vanilla-rmcs+ (cmd! (rmcs--sandbox-run 'vanilla-rmcs+)))
(fset 'rmcs--run-full-rmcs     (cmd! (rmcs--sandbox-run 'rmcs)))

(defvar rmcs-sandbox-emacs-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'rmcs--run-vanilla-emacs)
    (define-key map (kbd "C-c C-d") #'rmcs--run-vanilla-rmcs)
    (define-key map (kbd "C-c C-p") #'rmcs--run-vanilla-rmcs+)
    (define-key map (kbd "C-c C-f") #'rmcs--run-full-rmcs)
    (define-key map (kbd "C-c C-k") #'kill-current-buffer)
    map))

(define-derived-mode rmcs-sandbox-emacs-lisp-mode emacs-lisp-mode "Sandbox Elisp"
  "TODO")

;;;###autoload
(defun rmcs/sandbox ()
  "Open the Emacs Lisp sandbox.

This is a test bed for running Emacs Lisp in another instance of Emacs with
varying amounts of Rmcs loaded, including:

  a) vanilla Emacs (nothing loaded),
  b) vanilla Rmcs (only Rmcs core),
  c) Rmcs + modules - your private config or
  c) Rmcs + modules + your private config (a complete Rmcs session)

This is done without sacrificing access to installed packages. Use the sandbox
to reproduce bugs and determine if Rmcs is to blame."
  (interactive)
  (pop-to-buffer
   (with-current-buffer (get-buffer-create rmcs-sandbox-buffer-name)
     (rmcs-sandbox-emacs-lisp-mode)
     (setq-local default-directory rmcs-emacs-dir)
     (and (buffer-live-p (get-buffer rmcs-sandbox-buffer-name))
          (= (buffer-size) 0)
          (insert (substitute-command-keys rmcs-sandbox-preamble)))
     (goto-char (point-max))
     (current-buffer))))
