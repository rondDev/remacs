;;; lisp/cli/install.el --- Rmcs Emacs install wizard -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! "packages")


;;
;;; Variables

;; None yet!


;;
;;; Commands

(defcli! ((install i))
    (&flags
     (config?  ("--config" :yes)  "Create `$RMCSDIR' or dummy files therein?")
     (envfile? ("--env" :yes)     "(Re)generate an envvars file? (see `$ rmcs help env`)")
     (install? ("--install" :yes) "Auto-install packages?")
     (fonts?   ("--fonts" :yes)   "Install (or prompt to install) nerd-icons fonts?")
     (hooks?   ("--hooks" :yes)   "Deploy Rmcs's git hooks to itself?")
     &context context)
  "Installs and sets up Rmcs Emacs for the first time.

This command does the following:

  1. Creates `$RMCSDIR' at ~/.config/rmcs (if it or ~/.rmcs.d doesn't exist),
  2. Copies ~/.config/emacs/templates/init.example.el to `$RMCSDIR'/init.el (if
     it doesn't exist),
  3. Creates dummy files for `$RMCSDIR'/{config,packages}.el,
  4. Prompts you to generate an envvar file (same as `$ rmcs env`),
  5. Installs any dependencies of enabled modules (specified by `$RMCSDIR'/init.el),
  6. And prompts to install nerd-icons' fonts

This command is idempotent and safe to reuse.

Change `$RMCSDIR' with the `--rmcsdir' option, e.g.

  ```
  $ rmcs --rmcsdir /other/rmcs/config install
  ```"
  (print! (green "Installing Rmcs Emacs!\n"))
  (let ((default-directory rmcs-emacs-dir)
        (yes? (rmcs-cli-context-suppress-prompts-p context)))
    ;; Create `rmcs-user-dir'
    (if (eq config? :no)
        (print! (warn "Not copying private config template, as requested"))
      ;; Create RMCSDIR in ~/.config/rmcs if ~/.config/emacs exists.
      (when (and (not (file-directory-p rmcs-user-dir))
                 (not (getenv "RMCSDIR")))
        (let ((xdg-config-dir (or (getenv "XDG_CONFIG_HOME") "~/.config")))
          (when (file-in-directory-p rmcs-emacs-dir xdg-config-dir)
            (setq rmcs-user-dir (expand-file-name "rmcs/" xdg-config-dir)))))

      (if (file-directory-p rmcs-user-dir)
          (print! (item "Skipping %s (already exists)") (relpath rmcs-user-dir))
        (make-directory rmcs-user-dir 'parents)
        (print! (success "Created %s") (relpath rmcs-user-dir)))

      ;; Create init.el, config.el & packages.el
      (print-group!
        (mapc (lambda (file)
                (cl-destructuring-bind (filename . template) file
                  (if (file-exists-p! filename rmcs-user-dir)
                      (print! (item "Skipping %s (already exists)")
                              (path filename))
                    (print! (item "Creating %s%s") (relpath rmcs-user-dir) filename)
                    (with-temp-file (rmcs-path rmcs-user-dir filename)
                      (insert-file-contents template))
                    (print! (success "Done!")))))
              (let ((template-dir (rmcs-path rmcs-emacs-dir "templates")))
                `((,rmcs-module-init-file
                   . ,(file-name-with-extension (rmcs-path template-dir rmcs-module-init-file)
                                                ".example.el"))
                  (,rmcs-module-config-file
                   . ,(file-name-with-extension (rmcs-path template-dir rmcs-module-config-file)
                                                ".example.el"))
                  (,rmcs-module-packages-file
                   . ,(file-name-with-extension (rmcs-path template-dir rmcs-module-packages-file)
                                                ".example.el")))))))

    ;; In case no init.el was present the first time it was loaded.
    (rmcs-load (rmcs-path rmcs-user-dir rmcs-module-init-file) t)

    ;; Ask if user would like an envvar file generated
    (if (eq envfile? :no)
        (print! (warn "Not generating envvars file, as requested"))
      (if (file-exists-p rmcs-env-file)
          (print! (item "Envvar file already exists, skipping"))
        (when (or yes? (y-or-n-p "Generate an envvar file? (see `rmcs help env` for details)"))
          (call! '(env)))))

    ;; Install Rmcs packages
    (if (eq install? :no)
        (print! (warn "Not installing plugins, as requested"))
      (print! "Installing plugins")
      (rmcs-packages-install))

    (print! "Regenerating autoloads files")
    (rmcs-profile-generate)

    (if (eq hooks? :no)
        (print! (warn "Not deploying commit-msg and pre-push git hooks, as requested"))
      (print! "Deploying commit-msg and pre-push git hooks")
      (print-group!
       (condition-case e
           (call! `(ci deploy-hooks ,@(if yes? '("--force"))))
         ('user-error
          (print! (warn "%s") (error-message-string e))))))

    (when (file-exists-p "~/.emacs")
      (print! (warn "A ~/.emacs file was detected. This conflicts with Rmcs and should be deleted!")))

    (print! (success "\nFinished! Rmcs is ready to go!\n"))
    (with-temp-buffer
      (insert-file-contents (rmcs-path rmcs-emacs-dir "templates/QUICKSTART_INTRO"))
      (print! "%s" (buffer-string)))))

(provide 'rmcs-cli-install)
;;; install.el ends here
