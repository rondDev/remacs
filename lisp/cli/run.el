;;; lisp/cli/run.el --- launching Emacs in a sandbox -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

;; None yet!


;;
;;; Commands

(defcli! run
    (;; TODO Implement sandbox functionality post-3.0
     ;; (daemon?     ("--daemon"))
     ;; (window-type ("--gui" "--tty"))
     ;; (version     ("--with-emacs" version))
     ;; (rmcsversion ("--with-rmcs" version))
     ;; (profile     ("--profile" name))
     (repl?  ("--repl") "Launch an elisp REPL")
     ;; &multiple
     ;; (calls  ("-f" "--funcall" fn))
     ;; (loads  ("-l" "--load" file))
     ;; (evals  (     "--eval" form))
     &context context
     &input input
     &rest args)
  "Launch Rmcs Emacs or an Emacs sandbox

Opens from bin/rmcs's parent directory.

Keep in mind there is some overhead opening Rmcs this way. For the best
performance, it is best to run Rmcs out of ~/.config/emacs or ~/.emacs.d."
  :benchmark nil
  ;; TODO Implement sandbox functionality post-3.0
  ;; (when version
  ;;   (unless (executable-find "nix-shell")
  ;;     (user-error "--emacs option is not supported without nix"))
  ;;   ...)
  (if repl?
      (if input
          ;; Evaluate piped-in text directly, if given.
          (eval (read input) t)
        (rmcs-run-repl context))
    (let* ((tempdir      (rmcs-path (temporary-file-directory) "rmcs.run"))
           (tempemacsdir (rmcs-path tempdir ".emacs.d")))
      (delete-directory tempdir t) ; start from scratch
      (make-directory tempemacsdir t)
      ;; HACK: So that Emacs doesn't lose track of local state, like user fonts,
      ;;   configs, or binscripts, we symlink these to the sandbox.
      ;; REVIEW: Use `--init-directory' when we drop 29 support OR when Rmcs is
      ;;   in bootloader mode.
      (dolist (dir (list (or (getenv "XDG_DATA_HOME") "~/.local/share")
                         (or (getenv "XDG_BIN_HOME") "~/.local/bin")
                         (or (getenv "XDG_CONFIG_HOME") "~/.config")
                         (or (getenv "XDG_CACHE_HOME") "~/.cache")))
        (let* ((xdg-dir (rmcs-path dir))
               (target  (rmcs-path tempdir (file-relative-name xdg-dir "~"))))
          (when (file-directory-p xdg-dir)
            (unless (file-symlink-p target)
              (make-directory (file-name-directory target) t)
              (make-symbolic-link xdg-dir target)))))
      (with-temp-file (rmcs-path tempemacsdir "early-init.el")
        (prin1 `(progn
                  (setenv "HOME" ,(getenv "HOME"))
                  (setenv "EMACSDIR" ,rmcs-emacs-dir)
                  (setenv "RMCSDIR"  ,rmcs-user-dir)
                  (setenv "RMCSPROFILE" ,(getenv "RMCSPROFILE"))
                  (setq early-init-file ,(rmcs-path rmcs-emacs-dir "early-init.el"))
                  (load early-init-file nil (not ,init-file-debug) 'nosuffix))
               (current-buffer)))
      (exit! (format "HOME=%S %s %s"
                     tempdir
                     invocation-name
                     (combine-and-quote-strings args))))))


;;
;;; Helpers

(defun rmcs-run-repl (context)
  "Launch a rudimentary Elisp REPL."
  ;; I wrote this for fun; not with any serious intention of adding a
  ;; fully-fledged REPL to the Rmcs CLI. Still, I occasionally need to check
  ;; something, and once this has nix integration and can sandbox Emacs versions
  ;; separately, it may be useful for quick tests and demos.
  (let (form)
    (while (setq form (read-from-minibuffer "(elisp) $ "))
      (when (member form '(":quit" ":q"))
        (print! "\nGoodbye!")
        (exit! 0))
      (let (debug-on-error)
        (condition-case e
            (print! "%S" (eval (read form) t))
          (error
           (let* ((n 0)
                  (frame (backtrace-frame n))
                  (frame-list nil)
                  (in-program-stack t))
             (while frame
               (when in-program-stack
                 (push (cdr frame) frame-list))
               ;; (when (eq (elt frame 1) 'rmcs-run-repl)
               ;;   (setq in-program-stack t))
               (when (eq (elt frame 1) 'rmcs-run-repl)
                 (setq in-program-stack nil))
               (setq n (1+ n)
                     frame (backtrace-frame n)))
             (let* ((depth rmcs-cli-backtrace-depth)
                    (print-escape-newlines t))
               (print! (error "There was an unexpected error"))
               (print-group!
                (print! "%s %s" (bold "Message:") (error-message-string e))
                (print! "%s %S" (bold "Details:") (cdr e))))))))
      (terpri))))

(provide 'rmcs-cli-run)
;;; run.el ends here
