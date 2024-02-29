;;; lisp/lib/debug.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Rmcs's debug mode

;;;###autoload
(defvar rmcs-debug-variables
  `(;; Rmcs variables
    (rmcs-print-minimum-level . debug)
    (rmcs-inhibit-log . nil)

    ;; Emacs variables
    async-debug
    debug-on-error
    gcmh-verbose
    init-file-debug
    jka-compr-verbose
    (message-log-max . 16384)
    (native-comp-async-report-warnings-errors . silent)
    (native-comp-warning-on-missing-source . t)
    url-debug
    use-package-verbose
    (warning-suppress-types . nil))
  "A list of variable to toggle on `rmcs-debug-mode'.

Each entry can be a variable symbol or a cons cell whose CAR is the variable
symbol and CDR is the value to set it to when `rmcs-debug-mode' is activated.")

(defvar rmcs-debug--unbound-vars nil)

(defun rmcs-debug--watch-vars-h (&rest _)
  (when-let (vars (copy-sequence rmcs-debug--unbound-vars))
    (setq rmcs-debug--unbound-vars nil)
    (mapc #'rmcs-debug--set-var vars)))

(defun rmcs-debug--set-var (spec)
  (cond ((listp spec)
         (pcase-let ((`(,var . ,val) spec))
           (if (boundp var)
               (set-default
                var (if (not rmcs-debug-mode)
                        (prog1 (get var 'initial-value)
                          (put var 'initial-value nil))
                      (rmcs-log "debug:vars: %s = %S" var (default-toplevel-value var))
                      (put var 'initial-value (default-toplevel-value var))
                      val))
             (add-to-list 'rmcs-debug--unbound-vars spec))))
        ((boundp spec)
         (rmcs-log "debug:vars: %s = %S" spec rmcs-debug-mode)
         (set-default-toplevel-value spec rmcs-debug-mode))
        ((add-to-list 'rmcs-debug--unbound-vars (cons spec t)))))

;;;###autoload
(define-minor-mode rmcs-debug-mode
  "Toggle `debug-on-error' and `init-file-debug' for verbose logging."
  :global t
  (let ((enabled rmcs-debug-mode))
    (rmcs-log "debug: enabled!")
    (mapc #'rmcs-debug--set-var rmcs-debug-variables)
    (when (called-interactively-p 'any)
      (when (fboundp 'explain-pause-mode)
        (explain-pause-mode (if enabled +1 -1))))
    ;; Watch for changes in `rmcs-debug-variables', or when packages load (and
    ;; potentially define one of `rmcs-debug-variables'), in case some of them
    ;; aren't defined when `rmcs-debug-mode' is first loaded.
    (cond (enabled
           (unless noninteractive
             (message "Debug mode enabled! (Run 'M-x view-echo-area-messages' to open the log buffer)"))
           ;; Produce more helpful (and visible) error messages from errors
           ;; emitted from hooks (particularly mode hooks), that usually go
           ;; unnoticed otherwise.
           (advice-add #'run-hooks :override #'rmcs-run-hooks)
           ;; Add time stamps to lines in *Messages*
           (advice-add #'message :before #'rmcs--timestamped-message-a)
           ;; The constant debug output from GC is mostly unhelpful. I still
           ;; want it logged to *Messages*, just out of the echo area.
           (advice-add #'gcmh-idle-garbage-collect :around #'rmcs-debug-shut-up-a)
           (add-variable-watcher 'rmcs-debug-variables #'rmcs-debug--watch-vars-h)
           (add-hook 'after-load-functions #'rmcs-debug--watch-vars-h))
          (t
           (advice-remove #'run-hooks #'rmcs-run-hooks)
           (advice-remove #'message #'rmcs--timestamped-message-a)
           (advice-remove #'gcmh-idle-garbage-collect #'rmcs-debug-shut-up-a)
           (remove-variable-watcher 'rmcs-debug-variables #'rmcs-debug--watch-vars-h)
           (remove-hook 'after-load-functions #'rmcs-debug--watch-vars-h)
           (rmcs-log "debug: disabled")
           (message "Debug mode disabled!")))))

(defun rmcs-debug-shut-up-a (fn &rest args)
  "Suppress output from FN, even in debug mode."
  (let (init-file-debug)
    (apply #'rmcs-shut-up-a fn args)))


;;
;;; Custom debugger

;; HACK: I advise `debug' instead of changing `debugger' to hide the debugger
;;   itself from the backtrace. Doing it manually would require reimplementing
;;   most of `debug', which is a lot of unnecessary work, when I only want to
;;   decorate the original one slightly.
(defadvice! rmcs-debugger-a (fn &rest args)
  :around #'debug
  ;; Without `rmcs-debug-mode', be as vanilla as possible.
  (if (not rmcs-debug-mode)
      (apply fn args)
    ;; Work around Emacs's heuristic (in eval.c) for detecting errors in the
    ;; debugger, which would run this handler again on subsequent calls. Taken
    ;; from `ert--run-test-debugger'.
    (if (and noninteractive (fboundp 'rmcs-cli-debugger))
        (apply #'rmcs-cli-debugger args)
      ;; TODO: Write backtraces to file
      ;; TODO: Write backtrace to a buffer in case recursive error interupts the
      ;;   debugger (happens more often than it should).
      (apply fn args))))

(autoload 'backtrace-get-frames "backtrace")
;;;###autoload
(defun rmcs-backtrace ()
  "Return a stack trace as a list of `backtrace-frame' objects."
  ;; (let* ((n 0)
  ;;        (frame (backtrace-frame n))
  ;;        (frame-list nil)
  ;;        (in-program-stack nil))
  ;;   (while frame
  ;;     (when in-program-stack
  ;;       (push (cdr frame) frame-list))
  ;;     (when (eq (elt frame 1) debugger)
  ;;       (setq in-program-stack t))
  ;;     ;; (when (and (eq (elt frame 1) 'rmcs-cli-execute)
  ;;     ;;            (eq (elt frame 2) :rmcs))
  ;;     ;;   (setq in-program-stack nil))
  ;;     (setq n (1+ n)
  ;;           frame (backtrace-frame n)))
  ;;   (nreverse frame-list))
  (cdr (backtrace-get-frames debugger)))

(defun rmcs-backtrace-write-to-file (backtrace file)
  "Write BACKTRACE to FILE with appropriate boilerplate."
  (make-directory (file-name-directory file) t)
  (let ((rmcs-print-indent 0))
    (with-temp-file file
      (insert ";; -*- lisp-interaction -*-\n")
      (insert ";; vim: set ft=lisp:\n")
      (insert (format ";; command=%S\n" command-line-args))
      (insert (format ";; date=%S\n\n" (format-time-string "%Y-%m-%d %H-%M-%S" before-init-time)))
      (insert ";;;; ENVIRONMENT\n" (with-output-to-string (rmcs/version)) "\n")
      (let ((standard-output (current-buffer))
            (print-quoted t)
            (print-escape-newlines t)
            (print-escape-control-characters t)
            (print-symbols-bare t)
            (print-level nil)
            (print-circle nil)
            (n -1))
        (mapc (lambda (frame)
                (princ (format ";;;; %d\n" (cl-incf n)))
                (pp (list (cons (backtrace-frame-fun frame)
                                (backtrace-frame-args frame))
                          (backtrace-frame-locals frame)))
                (terpri))
              backtrace))
      file)))


;;
;;; Time-stamped *Message* logs

(defun rmcs--timestamped-message-a (format-string &rest _args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'rmcs--timestamped-message-a)"
  (when (and (stringp format-string)
             message-log-max  ; if nil, logging is disabled
             (not (equal format-string "%s%s"))
             (not (equal format-string "\n")))
    (with-current-buffer "*Messages*"
      (let ((timestamp (format-time-string "[%F %T] " (current-time)))
            (deactivate-mark nil))
        (with-silent-modifications
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert timestamp))))
    (let ((window (get-buffer-window "*Messages*")))
      (when (and window (not (equal (selected-window) window)))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (set-window-point window (point-max)))))))


;;
;;; Hooks

;;;###autoload
(defun rmcs-run-all-startup-hooks-h ()
  "Run all startup Emacs hooks. Meant to be executed after starting Emacs with
-q or -Q, for example:

  emacs -Q -l init.el -f rmcs-run-all-startup-hooks-h"
  (setq after-init-time (current-time))
  (let ((inhibit-startup-hooks nil))
    (rmcs-run-hooks 'after-init-hook
                    'delayed-warnings-hook
                    'emacs-startup-hook
                    'tty-setup-hook
                    'window-setup-hook)))


;;
;;; Helpers

(defsubst rmcs--collect-forms-in (file form)
  (when (file-readable-p file)
    (let (forms)
      (with-temp-buffer
        (insert-file-contents file)
        (with-syntax-table emacs-lisp-mode-syntax-table
          (while (re-search-forward (format "(%s " (regexp-quote form)) nil t)
            (let ((ppss (syntax-ppss)))
              (unless (or (nth 4 ppss)
                          (nth 3 ppss))
                (save-excursion
                  (goto-char (match-beginning 0))
                  (push (sexp-at-point) forms))))))
        (nreverse forms)))))

;;;###autoload
(defun rmcs-info ()
  "Returns diagnostic information about the current Emacs session in markdown,
ready to be pasted in a bug report on github."
  (require 'vc-git)
  (require 'rmcs-packages)
  (let ((default-directory rmcs-emacs-dir))
    (letf! ((defun sh (&rest args) (cdr (apply #'rmcs-call-process args)))
            (defun cat (file &optional limit)
              (with-temp-buffer
                (insert-file-contents file nil 0 limit)
                (buffer-string)))
            (defun abbrev-path (path)
              (replace-regexp-in-string
               (regexp-opt (list (user-login-name)) 'words) "$USER"
               (abbreviate-file-name path)))
            (defun symlink-path (file)
              (format "%s%s" (abbrev-path file)
                      (if (file-symlink-p file)
                          (concat " -> " (abbrev-path (file-truename file)))
                        ""))))
      `((generated . ,(format-time-string "%b %d, %Y %H:%M:%S"))
        (system . ,(delq
                    nil (list (rmcs-system-distro-version)
                              (when (executable-find "uname")
                                (sh "uname" "-msr"))
                              (window-system))))
        (emacs . ,(delq
                   nil (list emacs-version
                             (bound-and-true-p emacs-repository-branch)
                             (and (stringp emacs-repository-version)
                                  (substring emacs-repository-version 0 9))
                             (format "EMACSDIR=%s" (symlink-path rmcs-emacs-dir))
                             (format "EMACS=%s" (expand-file-name invocation-name invocation-directory)))))
        (rmcs . ,(list rmcs-version
                       (if rmcs-profile
                           (format "PROFILE=%s@%s"
                                   (car rmcs-profile)
                                   (cdr rmcs-profile))
                         "PROFILE=_@0")
                       (if (file-exists-p! ".git" rmcs-emacs-dir)
                           (sh "git" "log" "-1" "--format=%D %h %ci")
                         "[no repo]")
                       (symlink-path rmcs-user-dir)))
        (shell  . ,(abbrev-path shell-file-name))
        (features . ,system-configuration-features)
        (traits
         . ,(mapcar
             #'symbol-name
             (delq
              nil (list (cond (noninteractive 'batch)
                              ((display-graphic-p) 'gui)
                              ('tty))
                        (if (daemonp) 'daemon)
                        (if (and (require 'server)
                                 (server-running-p))
                            'server-running)
                        (if (boundp 'chemacs-version)
                            (intern (format "chemacs-%s" chemacs-version)))
                        (if (file-exists-p rmcs-env-file)
                            'envvar-file)
                        (if (featurep 'exec-path-from-shell)
                            'exec-path-from-shell)
                        (if (file-symlink-p rmcs-emacs-dir)
                            'symlinked-emacsdir)
                        (if (file-symlink-p rmcs-user-dir)
                            'symlinked-rmcsdir)
                        (if (and (stringp custom-file) (file-exists-p custom-file))
                            'custom-file)
                        (if (rmcs-files-in rmcs-user-dir :type 'files :match "\\.elc$")
                            'compiled-user-config)
                        (if (rmcs-files-in rmcs-core-dir :type 'files :match "\\.elc$")
                            'compiled-core)
                        (if (rmcs-files-in rmcs-module-load-path :type 'files :match "\\.elc$")
                            'compiled-modules)))))
        (custom
         ,@(when (and (stringp custom-file)
                      (file-exists-p custom-file))
             (cl-loop for (type var _) in (get 'user 'theme-settings)
                      if (eq type 'theme-value)
                      collect var)))
        (modules
         ,@(or (cl-loop with lastcat = nil
                        for (cat . mod) in (seq-filter #'cdr (rmcs-module-list))
                        if (or (not lastcat)
                               (not (eq lastcat cat)))
                        do (setq lastcat cat)
                        and collect lastcat
                        collect
                        (let* ((flags (rmcs-module-get lastcat mod :flags))
                               (path  (rmcs-module-get lastcat mod :path))
                               (module
                                (append
                                 (cond ((null path)
                                        (list '&nopath))
                                       ((not (file-in-directory-p path rmcs-modules-dir))
                                        (list '&user)))
                                 (if flags
                                     `(,mod ,@flags)
                                   (list mod)))))
                          (if (= (length module) 1)
                              (car module)
                            module)))
               '("n/a")))
        (packages
         ,@(condition-case e
               (mapcar
                #'cdr (rmcs--collect-forms-in
                       (rmcs-path rmcs-user-dir rmcs-module-packages-file)
                       "package!"))
             (error (format "<%S>" e))))
        (unpin
         ,@(condition-case e
               (mapcan #'identity
                       (mapcar
                        #'cdr (rmcs--collect-forms-in
                               (rmcs-path rmcs-user-dir rmcs-module-packages-file)
                               "unpin!")))
             (error (list (format "<%S>" e)))))
        (elpa
         ,@(condition-case e
               (progn
                 (unless (bound-and-true-p package--initialized)
                   (package-initialize))
                 (cl-loop for (name . _) in package-alist
                          collect (format "%s" name)))
             (error (format "<%S>" e))))))))

;;;###autoload
(defun rmcs-info-string (&optional width nocolor)
  "Return the `rmcs-info' as a compact string.

FILL-COLUMN determines the column at which lines will be broken."
  (with-temp-buffer
    (let ((rmcs-print-backend (unless nocolor rmcs-print-backend))
          (rmcs-print-indent 0))
      (dolist (spec (cl-remove-if-not #'cdr (rmcs-info)) (buffer-string))
        ;; FIXME Refactor this horrible cludge, either here or in `format!'
        (insert! ((bold "%-10s ") (symbol-name (car spec)))
                 ("%s\n"
                  (string-trim-left
                   (indent
                    (fill
                     (if (listp (cdr spec))
                         (mapconcat (rmcs-partial #'format "%s")
                                    (cdr spec)
                                    " ")
                       (cdr spec))
                     (- (or width 80) 11))
                    11))))))))


;;
;;; Commands

;;;###autoload
(defun rmcs/version ()
  "Display the running version of Rmcs core, module sources, and Emacs."
  (interactive)
  (print! "%s\n%s\n%s"
          (format "%-13s v%-15s %s"
                  "GNU Emacs"
                  emacs-version
                  emacs-repository-version)
          (format "%-13s v%-15s %s"
                  "Rmcs core"
                  rmcs-version
                  (or (cdr (rmcs-call-process
                            "git" "-C" (expand-file-name rmcs-emacs-dir)
                            "log" "-1" "--format=%D %h %ci"))
                      "n/a"))
          ;; NOTE This is a placeholder. Our modules will be moved to its own
          ;;   repo eventually, and Rmcs core will later be capable of managing
          ;;   them like package sources.
          (format "%-13s v%-15s %s"
                  "Rmcs modules"
                  rmcs-modules-version
                  (or (cdr (rmcs-call-process
                            "git" "-C" (expand-file-name rmcs-modules-dir)
                            "log" "-1" "--format=%D %h %ci"))
                      "n/a"))))

;;;###autoload
(defun rmcs/info ()
  "Collects some debug information about your Emacs session, formats it and
copies it to your clipboard, ready to be pasted into bug reports!"
  (interactive)
  (let ((buffer (get-buffer-create "*rmcs info*")))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (with-silent-modifications
        (erase-buffer)
        (insert (rmcs-info-string 86)))
      (pop-to-buffer buffer)
      (kill-new (buffer-string))
      (when (y-or-n-p "Your rmcs-info was copied to the clipboard.\n\nOpen pastebin.com?")
        (browse-url "https://pastebin.com")))))


;;
;;; Profiling

(defvar rmcs--profiler nil)
;;;###autoload
(defun rmcs/toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not rmcs--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq rmcs--profiler (not rmcs--profiler)))

(provide 'rmcs-lib '(debug))
;;; debug.el ends here
