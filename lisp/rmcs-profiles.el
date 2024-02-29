;;; lisp/rmcs-profiles.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'rmcs)) ; be silent, o'byte-compiler


;;
;;; Variables

;;; File/directory variables
(defvar rmcs-profiles-generated-dir rmcs-data-dir
  "Where generated profiles are kept.

Profile directories are in the format {data-profiles-dir}/$NAME/@/$VERSION, for
example: '~/.local/share/rmcs/_/@/0/'")

(defvar rmcs-profile-load-path
  (if-let (path (getenv-internal "RMCSPROFILELOADPATH"))
      (mapcar #'expand-file-name (split-string-and-unquote path path-separator))
    (list (file-name-concat rmcs-user-dir "profiles.el")
          (file-name-concat rmcs-emacs-dir "profiles.el")
          (expand-file-name "rmcs-profiles.el" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
          (expand-file-name "~/.rmcs-profiles.el")
          (file-name-concat rmcs-user-dir "profiles")
          (file-name-concat rmcs-emacs-dir "profiles")))
  "A list of profile config files or directories that house implicit profiles.

`rmcs-profiles-initialize' loads and merges all profiles defined in the above
files/directories, then writes a profile load script to
`rmcs-profile-load-file'.

Can be changed externally by setting $RMCSPROFILELOADPATH to a colon-delimited
list of paths or profile config files (semi-colon delimited on Windows).")

(defvar rmcs-profile-load-file
  (if-let (loader (getenv-internal "RMCSPROFILELOADFILE"))
      (expand-file-name loader rmcs-emacs-dir)
    (file-name-concat rmcs-emacs-dir (format "profiles/load.el" emacs-major-version)))
  "Where Rmcs writes its interactive profile loader script.

Can be changed externally by setting $RMCSPROFILELOADFILE.")

(defvar rmcs-profile-init-file-name (format "init.%d.el" emacs-major-version)
  "TODO")

(defvar rmcs-profile-init-dir-name (format "init.%d.d" emacs-major-version)
  "The subdirectory of `rmcs-profile-dir'")

(defvar rmcs-profiles-config-file-name ".rmcsprofile"
  "TODO")

;;; Profile storage variables
(defvar rmcs-profile-generators
  '(("05-init-vars.auto.el"         . rmcs-profile--generate-init-vars)
    ("80-loaddefs.auto.el"          . rmcs-profile--generate-rmcs-autoloads)
    ("90-loaddefs-packages.auto.el" . rmcs-profile--generate-package-autoloads)
    ("95-load-modules.auto.el"      . rmcs-profile--generate-load-modules))
  "An alist mapping file names to generator functions.

The file will be generated in `rmcs-profile-dir'/`rmcs-profile-init-dir-name',
and later combined into `rmcs-profile-dir'/`rmcs-profile-init-file-name' in
lexicographical order. These partials are left behind in case the use wants to
load them directly (for whatever use), or for commands to use (e.g.
`rmcs/reload-autoloads' loads any file with a NN-loaddefs[-.] prefix to
accomplish its namesake).

Files with an .auto.el suffix will be automatically deleted whenever the profile
is regenerated. Users (or Rmcs CLIs, like `rmcs env') may add their own
generators to this list, or to `rmcs-profile-dir'/`rmcs-profile-init-dir-name',
and they will be included in the profile init file next time `rmcs sync' is
run.")

(defvar rmcs--profiles ())

(defconst rmcs-profile-default (cons "_" "0"))


;;
;;; Helpers

(defun rmcs-profiles-bootloadable-p ()
  "Return non-nil if `rmcs-emacs-dir' can be a bootloader."
  (with-memoization (get 'rmcs 'bootloader)
    (or (file-equal-p rmcs-emacs-dir "~/.config/emacs")
        (file-equal-p rmcs-emacs-dir "~/.emacs.d"))))

(defun rmcs-profiles-read (&rest paths)
  "TODO"
  (let (profiles)
    (dolist (path (delq nil (flatten-list paths)))
      (cond
       ((file-directory-p path)
        (setq path (file-truename path))
        (dolist (subdir (rmcs-files-in path :depth 0 :match "/[^.][^/]+$" :type 'dirs :map #'file-name-base))
          (if (equal subdir (car rmcs-profile-default))
              (signal 'rmcs-profile-error (list (file-name-concat path subdir) "Implicit profile has invalid name"))
            (unless (string-prefix-p "_" subdir)
              (cl-pushnew
               (cons (intern subdir)
                     (let* ((val (abbreviate-file-name (file-name-as-directory subdir)))
                            (val (if (file-name-absolute-p val)
                                     `(,val)
                                   `(,(abbreviate-file-name path) ,val))))
                       (cons `(user-emacs-directory :path ,@val)
                             (if-let (profile-file (file-exists-p! rmcs-profiles-config-file-name path))
                                 (car (rmcs-file-read profile-file :by 'read*))
                               (when (file-exists-p (rmcs-path path subdir "lisp/rmcs.el"))
                                 '((rmcs-user-dir :path ,@val)))))))
               profiles
               :test #'eq
               :key #'car)))))
       ((file-exists-p path)
        (dolist (profile (car (rmcs-file-read path :by 'read*)))
          (if (eq (symbol-name (car profile)) (car rmcs-profile-default))
              (signal 'rmcs-profile-error (list path "Profile has invalid name: _"))
            (unless (string-prefix-p "_" (symbol-name (car profile)))
              (cl-pushnew profile profiles
                          :test #'eq
                          :key #'car)))))))
    (nreverse profiles)))

(defun rmcs-profiles-autodetect (&optional _internal?)
  "Return all known profiles as a nested alist.

This reads all profile configs and directories in `rmcs-profile-load-path', then
caches them in `rmcs--profiles'. If RELOAD? is non-nil, refresh the cache."
  (rmcs-profiles-read rmcs-profile-load-path
                      ;; TODO: Add in v3
                      ;; (if internal? rmcs-profiles-generated-dir)
                      ))

(defun rmcs-profiles-outdated-p ()
  "Return non-nil if files in `rmcs-profile-load-file' are outdated."
  (cl-loop for path in rmcs-profile-load-path
           when (string-suffix-p path ".el")
           if (or (not (file-exists-p rmcs-profile-load-file))
                  (file-newer-than-file-p path rmcs-profile-load-file)
                  (not (equal (rmcs-file-read rmcs-profile-load-file :by 'read)
                              rmcs-version)))
           return t))

(defun rmcs-profile<-id (id)
  "Return a (NAME . VERSION) profile cons cell from an id string NAME@VERSION."
  (save-match-data
    (if (string-match "^\\([^@]+\\)@\\(.+\\)$" id)
        (cons (match-string 1 id)
              (match-string 2 id))
      (cons id (cdr rmcs-profile-default)))))

(defun rmcs-profile->id (profile)
  "Return a NAME@VERSION id string from profile cons cell (NAME . VERSION)."
  (cl-check-type profile cons)
  (format "%s@%s" (car profile) (cdr profile)))

;; TODO (defun rmcs-profile--read (profile)
;;   (rmcs-profile-create ))

;; TODO (defun rmcs-profile-initialize (profile-name &optional ref)
;;   )

(defun rmcs-profiles-save (profiles &optional file)
  "Generate a profile bootstrapper for Rmcs to load at startup."
  (unless file
    (setq file rmcs-profile-load-file))
  (rmcs-file-write
   file (let ((profilesym (make-symbol "profile"))
              (deferredsym (make-symbol "deferred-vars")))
          `(";; -*- lexical-binding: t; tab-width: 8; -*-\n"
            ";; Updated: " ,(format-time-string "%Y-%m-%d %H:%M:%S") "\n"
            ";; Generated by 'rmcs profiles sync' or 'rmcs sync'.\n"
            ";; DO NOT EDIT THIS BY HAND!\n"
            ,(format "%S" rmcs-version)
            (pcase (intern (getenv-internal "RMCSPROFILE"))
              ,@(cl-loop
                 for (profile-name . bindings) in profiles
                 for deferred?
                 = (seq-find (fn! (and (memq (car-safe (cdr %)) '(:prepend :prepend? :append :append?))
                                       (not (stringp (car-safe %)))))
                             bindings)
                 collect
                 `(',profile-name
                   (let ,(if deferred? '(--deferred-vars--))
                     ,@(cl-loop
                        for (var . val) in bindings
                        collect
                        (pcase (car-safe val)
                          (:path
                           `(,(if (stringp var) 'setenv 'setq)
                             ,var ,(cl-loop with form = `(expand-file-name ,(cadr val) user-emacs-directory)
                                            for dir in (cddr val)
                                            do (setq form `(expand-file-name ,dir ,form))
                                            finally return form)))
                          (:eval
                           (if (eq var '_)
                               (macroexp-progn (cdr val))
                             `(,(if (stringp var) 'setenv 'setq)
                               ,var ,(macroexp-progn (cdr val)))))
                          (:plist
                           `(,(if (stringp var) 'setenv 'setq)
                             ,var ',(if (stringp var)
                                        (prin1-to-string (cadr val))
                                      (cadr val))))
                          ((or :prepend :prepend?)
                           (if (stringp var)
                               `(setenv ,var (concat ,val (getenv ,var)))
                             (setq deferred? t)
                             `(push (cons ',var
                                          (lambda ()
                                            (dolist (item (list ,@(cdr val)))
                                              ,(if (eq (car val) :append?)
                                                   `(add-to-list ',var item)
                                                 `(push item ,var)))))
                                    --deferred-vars--)))
                          ((or :append :append?)
                           (if (stringp var)
                               `(setenv ,var (concat (getenv ,var) ,val))
                             (setq deferred? t)
                             `(push (cons ',var
                                          (lambda ()
                                            (dolist (item (list ,@(cdr val)))
                                              ,(if (eq (car val) :append?)
                                                   `(add-to-list ',var item 'append)
                                                 `(set ',var (append ,var (list item)))))))
                                    --deferred-vars--)))
                          (_ `(,(if (stringp var) 'setenv 'setq) ,var ',val))))
                     ,@(when deferred?
                         `((defun --rmcs-profile-set-deferred-vars-- (_)
                             (dolist (var --deferred-vars--)
                               (when (boundp (car var))
                                 (funcall (cdr var))
                                 (setq --deferred-vars-- (delete var --deferred-vars--))))
                             (unless --deferred-vars--
                               (remove-hook 'after-load-functions #'--rmcs-profile-set-deferred-vars--)
                               (unintern '--rmcs-profile-set-deferred-vars-- obarray)))
                           (add-hook 'after-load-functions #'--rmcs-profile-set-deferred-vars--)
                           (--rmcs-profile-set-deferred-vars-- nil)))))))
            ;; `user-emacs-directory' requires that it end in a directory
            ;; separator, but users may forget this in their profile configs.
            (setq user-emacs-directory (file-name-as-directory user-emacs-directory))))
   :mode #o600
   :printfn #'pp)
  (print-group!
    (or (let ((byte-compile-warnings (if init-file-debug byte-compile-warnings))
              (byte-compile-dest-file-function
               (lambda (_) (format "%s.%d.elc" (file-name-sans-extension file) emacs-major-version))))
          (byte-compile-file file))
        ;; Do it again? So the errors/warnings are visible?
        ;; (let ((byte-compile-warnings t))
        ;;   (byte-compile-file file))
        (signal 'rmcs-profile-error (list file "Failed to byte-compile bootstrap file")))))

(defun rmcs-profile-p (profile-name)
  "Return t if PROFILE-NAME is a valid and existing profile."
  (when (stringp profile-name)
    (setq profile-name (intern profile-name)))
  (and (assq profile-name (rmcs-profiles))
       t))

(defun rmcs-profile-get (profile-name &optional property null-value)
  "Return PROFILE-NAME's PROFILE, otherwise its PROPERTY, otherwise NULL-VALUE."
  (when (stringp profile-name)
    (setq profile-name (intern profile-name)))
  (if-let (profile (assq profile-name (rmcs-profiles)))
      (if property
          (if-let (propval (assq property (cdr profile)))
              (cdr propval)
            null-value)
        profile)
    null-value))

(defun rmcs-profile-emacs-dir (profile-name)
  "Return the `user-emacs-directory' for PROFILE-NAME.

If the profile doesn't specify one, fall back to `rmcs-emacs-dir'."
  (rmcs-profile-get profile-name 'user-emacs-directory rmcs-emacs-dir))

(defun rmcs-profile-init-file (&optional profile-id version)
  "Return the init file for PROFILE-ID at VERSION.

Defaults to the profile at `rmcs-profile-default'."
  (cl-destructuring-bind (profile . version)
      (if (and (stringp profile-id) (null version))
          (rmcs-profile<-id profile-id)
        (cl-check-type profile-id (or null string))
        (cl-check-type version (or null string))
        (cons (or profile-id ;; (car rmcs-profile-default)
                  )
              (or version    ;; (cdr rmcs-profile-default)
                  )))
    (file-name-concat rmcs-data-dir
                      profile "@" version
                      (format rmcs-profile-init-file-name emacs-major-version))))


;;
;;; Data structures

;; TODO


;;
;;; API

;; TODO (defun rmcs-profile-create (name))

;; TODO (defun rmcs-profile-hash (profile))

;; TODO (defmacro with-profile! (profile &rest body))


;;
;;; Generators

(defun rmcs-profile-generate (&optional _profile regenerate-only?)
  "Generate profile init files."
  (rmcs-initialize-packages)
  (let* ((default-directory rmcs-profile-dir)
         (init-dir  rmcs-profile-init-dir-name)
         (init-file rmcs-profile-init-file-name))
    (print! (start "(Re)building profile in %s/...") (dirname rmcs-profile-dir))
    (condition-case-unless-debug e
      (with-file-modes #o750
        (print-group!
          (make-directory init-dir t)
          (print! (start "Deleting old init files..."))
          (print-group! :level 'info
            (cl-loop for file in (cons init-file (rmcs-glob "*.elc"))
                     if (file-exists-p file)
                     do (print! (item "Deleting %s...") file)
                     and do (delete-file file)))
          (let ((auto-files (rmcs-glob init-dir "*.auto.el")))
            (print! (start "Generating %d init files...") (length rmcs-profile-generators))
            (print-group! :level 'info
              (dolist (file auto-files)
                (print! (item "Deleting %s...") file)
                (delete-file file))
              (pcase-dolist (`(,file . ,fn) rmcs-profile-generators)
                (let ((file (rmcs-path init-dir file)))
                  (rmcs-log "Building %s..." file)
                  (rmcs-file-write file (funcall fn))))))
          (with-file! init-file
            (insert ";; -*- coding: utf-8; lexical-binding: t; -*-\n"
                    ";; This file was autogenerated; do not edit it by hand!\n")
            ;; Rmcs needs to be synced/rebuilt if either Rmcs or Emacs has been
            ;; up/downgraded. This is because byte-code isn't backwards
            ;; compatible, and many packages (including Rmcs), bake in absolute
            ;; paths into their caches that need to be refreshed.
            (prin1 `(unless (equal rmcs-version ,rmcs-version)
                      (error ,(concat
                               "The installed version of Rmcs (%s) has changed (to %s) since last "
                               "'rmcs sync'. Run 'rmcs sync' to bring Rmcs up to speed")
                             ,rmcs-version rmcs-version))
                   (current-buffer))
            (dolist (file (rmcs-glob init-dir "*.el"))
              (print-group! :level 'info
                (print! (start "Reading %s...") file))
              (rmcs-file-read file :by 'insert)))
          (print! (start "Byte-compiling %s...") (relpath init-file))
          (print-group!
            (let ((byte-compile-warnings (if init-file-debug '(suspicious make-local callargs))))
              (byte-compile-file init-file)))
          (print! (success "Built %s") (byte-compile-dest-file init-file))))
      (error (delete-file init-file)
             (delete-file (byte-compile-dest-file init-file))
             (signal 'rmcs-autoload-error (list init-file e))))))

(defun rmcs-profile--generate-init-vars ()
  ;; FIX: Make sure this only runs at startup to protect us Emacs' interpreter
  ;;   re-evaluating this file when lazy-loading dynamic docstrings from the
  ;;   byte-compiled init file.
  `((when (rmcs-context-p 'init)
      ,@(cl-loop for var in rmcs-autoloads-cached-vars
                 if (boundp var)
                 collect `(set-default ',var ',(symbol-value var)))
      ,@(cl-loop with v = (version-to-list rmcs-version)
                 with ref = (rmcs-call-process "git" "-C" (rmcs-path rmcs-emacs-dir) "rev-parse" "HEAD")
                 with branch = (rmcs-call-process "git" "-C" (rmcs-path rmcs-emacs-dir) "branch" "--show-current")
                 for (var . val)
                 in `((major  . ,(nth 0 v))
                      (minor  . ,(nth 1 v))
                      (build  . ,(nth 2 v))
                      (tag    . ,(ignore-errors (cadr (split-string rmcs-version "-" t))))
                      (ref    . ,(if (zerop (car ref)) (cdr ref)))
                      (branch . ,(if (zerop (car branch)) (cdr branch))))
                 collect `(put 'rmcs-version ',var ',val)))))

(defun rmcs-profile--generate-load-modules ()
  (let* ((init-modules-list (rmcs-module-list nil t))
         (config-modules-list (rmcs-module-list))
         (pre-init-modules
          (seq-filter (fn! (<= (rmcs-module-depth (car %) (cdr %) t) -100))
                      (remove '(:user) init-modules-list)))
         (init-modules
          (seq-filter (fn! (<= 0 (rmcs-module-depth (car %) (cdr %) t) 100))
                      init-modules-list))
         (config-modules
          (seq-filter (fn! (<= 0 (rmcs-module-depth (car %) (cdr %)) 100))
                      config-modules-list))
         (post-config-modules
          (seq-filter (fn! (>= (rmcs-module-depth (car %) (cdr %)) 100))
                      config-modules-list))
         (init-file   rmcs-module-init-file)
         (config-file rmcs-module-config-file))
    (letf! ((defun module-loader (group name file &optional noerror)
              (rmcs-module-context-with (cons group name)
                `(let ((rmcs-module-context ,rmcs-module-context))
                   (rmcs-load ,(abbreviate-file-name (file-name-sans-extension file))))))
            (defun module-list-loader (modules file &optional noerror)
              (cl-loop for (cat . mod) in modules
                       if (rmcs-module-locate-path cat mod file)
                       collect (module-loader cat mod it noerror))))
      ;; FIX: Same as above (see `rmcs-profile--generate-init-vars').
      `((if (or (rmcs-context-p 'init)
                (rmcs-context-p 'reload))
            (rmcs-context-with 'modules
              (set 'rmcs-modules ',rmcs-modules)
              (set 'rmcs-disabled-packages ',rmcs-disabled-packages)
              ;; Cache module state and flags in symbol plists for quick lookup by
              ;; `modulep!' later.
              ,@(cl-loop
                 for (category . modules) in (seq-group-by #'car config-modules-list)
                 collect
                 `(setplist ',category
                   (quote ,(cl-loop for (_ . module) in modules
                                    nconc `(,module ,(get category module))))))
              (let ((old-custom-file custom-file))
                ,@(module-list-loader pre-init-modules init-file)
                (rmcs-run-hooks 'rmcs-before-modules-init-hook)
                ,@(module-list-loader init-modules init-file)
                (rmcs-run-hooks 'rmcs-after-modules-init-hook)
                (rmcs-run-hooks 'rmcs-before-modules-config-hook)
                ,@(module-list-loader config-modules config-file)
                (rmcs-run-hooks 'rmcs-after-modules-config-hook)
                ,@(module-list-loader post-config-modules config-file t)
                (when (eq custom-file old-custom-file)
                  (rmcs-load custom-file 'noerror)))))))))

(defun rmcs-profile--generate-rmcs-autoloads ()
  (rmcs-autoloads--scan
   (append (rmcs-glob rmcs-core-dir "lib/*.el")
           (cl-loop for dir
                    in (append (rmcs-module-load-path)
                               (list rmcs-user-dir))
                    if (rmcs-glob dir "autoload.el") collect (car it)
                    if (rmcs-glob dir "autoload/*.el") append it)
           (mapcan #'rmcs-glob rmcs-autoloads-files))
   nil))

(defun rmcs-profile--generate-package-autoloads ()
  (rmcs-autoloads--scan
   (mapcar #'straight--autoloads-file
           (nreverse (seq-difference (hash-table-keys straight--build-cache)
                                     rmcs-autoloads-excluded-packages)))
   rmcs-autoloads-excluded-files
   'literal))

(provide 'rmcs-profiles)
;;; rmcs-profiles.el ends here
