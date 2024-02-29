;;; lisp/cli/sync.el --- synchronize config command -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! "packages")


;;
;;; Variables

(defvar rmcs-after-sync-hook ()
  "Hooks run after 'rmcs sync' synchronizes the user's config with Rmcs.")

(defvar rmcs-before-sync-hook ()
  "Hooks run before 'rmcs sync' synchronizes the user's config with Rmcs.")


;;
;;; Commands

(defcli-alias! (:before (sync s)) (:before build))

(defcli! ((sync s))
    ((noenvvar? ("-e") "Don't regenerate the envvar file")
     (noelc?    ("-c") "Don't recompile config")
     (update?   ("-u") "Update installed packages after syncing")
     (purge?    ("-p") "Purge orphaned package repos & regraft them")
     (jobs      ("-j" "--jobs" num) "How many CPUs to use for native compilation"))
  "Synchronize your config with Rmcs Emacs.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `rmcs!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Rmcs outside of Rmcs (e.g. with git)

It will ensure that unneeded packages are removed, all needed packages are
installed, autoloads files are up-to-date and no byte-compiled files have gone
stale.

OPTIONS:
  -j, --jobs
    Defaults to the maximum number of threads (or 1, if your CPU's threadcount
    can't be determined)."
  :benchmark t
  (when (rmcs-profiles-bootloadable-p)
    (call! '(profiles sync "--reload")))
  (run-hooks 'rmcs-before-sync-hook)
  (add-hook 'kill-emacs-hook #'rmcs-sync--abort-warning-h)
  (when jobs
    (setq native-comp-async-jobs-number (truncate jobs)))
  (print! (start "Synchronizing %S profile..." )
          (or (car rmcs-profile) "default"))
  (unwind-protect
      (print-group!
       (when (and (not noenvvar?)
                  (file-exists-p rmcs-env-file))
         (call! '(env)))
       (rmcs-packages-install)
       (rmcs-packages-build)
       (when update?
         (rmcs-packages-update))
       (rmcs-packages-purge purge? purge? purge? purge? purge?)
       (when (rmcs-profile-generate)
         (print! (item "Restart Emacs or use 'M-x rmcs/reload' for changes to take effect"))
         (run-hooks 'rmcs-after-sync-hook))
       t)
    (remove-hook 'kill-emacs-hook #'rmcs-sync--abort-warning-h)))


;;
;;; Helpers

(defun rmcs-sync--abort-warning-h ()
  (print! (warn "Script was abruptly aborted, leaving Rmcs in an incomplete state!"))
  (print! (item "Run 'rmcs sync' to repair it.")))

(provide 'rmcs-cli-sync)
;;; sync.el ends here
