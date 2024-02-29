;;; lisp/lib/config.el -*- lexical-binding: t; -*-

(defvar rmcs-bin-dir (expand-file-name "bin/" rmcs-emacs-dir))
(defvar rmcs-bin (expand-file-name "rmcs" rmcs-bin-dir))

;;;###autoload
(defvar rmcs-after-reload-hook nil
  "A list of hooks to run after `rmcs/reload' has reloaded Rmcs.")

;;;###autoload
(defvar rmcs-before-reload-hook nil
  "A list of hooks to run before `rmcs/reload' has reloaded Rmcs.")

;;;###autoload
(defun rmcs/open-private-config ()
  "Browse your `rmcs-user-dir'."
  (interactive)
  (unless (file-directory-p rmcs-user-dir)
    (make-directory rmcs-user-dir t))
  (rmcs-project-browse rmcs-user-dir))

;;;###autoload
(defun rmcs/find-file-in-private-config ()
  "Search for a file in `rmcs-user-dir'."
  (interactive)
  (rmcs-project-find-file rmcs-user-dir))

;;;###autoload
(defun rmcs/goto-private-init-file ()
  "Open your private init.el file.
And jumps to your `rmcs!' block."
  (interactive)
  (find-file (expand-file-name rmcs-module-init-file rmcs-user-dir))
  (goto-char
   (or (save-excursion
         (goto-char (point-min))
         (search-forward "(rmcs!" nil t))
       (point))))

;;;###autoload
(defun rmcs/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name rmcs-module-config-file rmcs-user-dir)))

;;;###autoload
(defun rmcs/goto-private-packages-file ()
  "Open your private packages.el file."
  (interactive)
  (find-file (expand-file-name rmcs-module-packages-file rmcs-user-dir)))


;;
;;; Managements

(defmacro rmcs--if-compile (command on-success &optional on-failure)
  (declare (indent 2))
  `(let ((default-directory rmcs-emacs-dir))
     (with-current-buffer (compile ,command t)
       (let ((w (get-buffer-window (current-buffer))))
         (select-window w)
         (add-hook
          'compilation-finish-functions
          (lambda (_buf status)
            (if (equal status "finished\n")
                (progn
                  (delete-window w)
                  ,on-success)
              ,on-failure))
          nil 'local)))))

;;;###autoload
(defun rmcs/reload ()
  "Reloads your private config.

This is experimental! It will try to do as `bin/rmcs sync' does, but from within
this Emacs session. i.e. it reload autoloads files (if necessary), reloads your
package list, and lastly, reloads your private config.el.

Runs `rmcs-after-reload-hook' afterwards."
  (interactive)
  (mapc #'require (cdr rmcs-incremental-packages))
  (rmcs--if-compile (format "%S sync -e" rmcs-bin)
      (rmcs-context-with '(reload modules)
        (rmcs-run-hooks 'rmcs-before-reload-hook)
        (rmcs-load (file-name-concat rmcs-user-dir rmcs-module-init-file) t)
        (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
          (general-auto-unbind-keys)
          (unwind-protect
              (startup--load-user-init-file nil)
            (general-auto-unbind-keys t)))
        (rmcs-run-hooks 'rmcs-after-reload-hook)
        (message "Config successfully reloaded!"))
    (user-error "Failed to reload your config")))

;;;###autoload
(defun rmcs/reload-autoloads ()
  "Reload only the autoloads of the current profile.

This is much faster and safer than `rmcs/reload', but not as comprehensive. This
reloads your package and module visibility, but does not install new packages or
remove orphaned ones. It also doesn't reload your private config.

It is useful to only pull in changes performed by 'rmcs sync' on the command
line."
  (interactive)
  (require 'rmcs-profiles)
  ;; TODO: Make this more robust
  (rmcs-context-with 'reload
    (dolist (file (mapcar #'car rmcs-profile-generators))
      (when (string-match-p "/[0-9]+-loaddefs[.-]" file)
        (load (rmcs-path rmcs-profile-dir rmcs-profile-init-dir-name file)
              'noerror)))))

;;;###autoload
(defun rmcs/reload-env ()
  "Reloads your envvar file.

DOES NOT REGENERATE IT. You must run 'rmcs env' in your shell OUTSIDE of Emacs.
Doing so from within Emacs will taint your shell environment.

An envvar file contains a snapshot of your shell environment, which can be
imported into Emacs."
  (interactive)
  (rmcs-context-with 'reload
    (let ((default-directory rmcs-emacs-dir))
      (with-temp-buffer
        (rmcs-load-envvars-file rmcs-env-file)
        (message "Reloaded %S" (abbreviate-file-name rmcs-env-file))))))

;;;###autoload
(defun rmcs/upgrade ()
  "Run 'rmcs upgrade' then prompt to restart Emacs."
  (interactive)
  (rmcs--if-compile (format "%S upgrade --force" rmcs-bin)
      (when (y-or-n-p "You must restart Emacs for the upgrade to take effect.\n\nRestart Emacs?")
        (rmcs/restart-and-restore))))
