;;; lisp/rmcs-packages.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs package management is opinionated, and so is Rmcs. Rmcs uses `straight'
;; to create a declarative, lazy-loaded, and (nominally) reproducible package
;; management system. We use `straight' over `package' because the latter is
;; tempermental. ELPA sources suffer downtime occasionally and often fail to
;; build packages when GNU Tar is unavailable (e.g. MacOS users start with BSD
;; tar). Known gnutls errors plague the current stable release of Emacs (26.x)
;; which bork TLS handshakes with ELPA repos (mainly gnu.elpa.org). See
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434.
;;
;; What's worse, you can only get the latest version of packages through ELPA.
;; In an ecosystem that is constantly changing, this is more frustrating than
;; convenient. Straight (and Rmcs) can do rolling release, but it is opt-in.
;;
;; Interacting with this package management system is done through Rmcs's
;; bin/rmcs script. Find out more about it by running 'rmcs help' (I highly
;; recommend you add the script to your PATH). Here are some highlights:
;;
;; - `rmcs install`: a wizard that guides you through setting up Rmcs and your
;;   private config for the first time.
;; - `rmcs sync`: your go-to command for making sure Rmcs is in optimal
;;   condition. It ensures all unneeded packages are removed, all needed ones
;;   are installed, and all metadata associated with them is generated.
;; - `rmcs upgrade`: upgrades Rmcs Emacs and your packages to the latest
;;   versions. There's also 'bin/rmcs sync -u' for updating only your packages.
;;
;; How this works is: the system reads packages.el files located in each
;; activated module, your private config (`rmcs-user-dir'), and one in
;; `rmcs-core-dir'. These contain `package!' declarations that tell RMCS what
;; packages to install and where from.
;;
;; All that said, you can still use package.el's commands, but 'rmcs sync' will
;; purge ELPA packages.
;;
;;; Code:

(defvar rmcs-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `rmcs-initialize-packages'.")

(defvar rmcs-disabled-packages ()
  "A list of packages that should be ignored by `use-package!' and `after!'.")

(defvar rmcs-packages-file "packages"
  "The basename of packages file for modules.

Package files are read whenever Rmcs's package manager wants a manifest of all
desired packages. They are rarely read in interactive sessions (unless the user
uses a straight or package.el command directly).")


;;
;;; package.el

;; Ensure that, if we do need package.el, it is configured correctly. You really
;; shouldn't be using it, but it may be convenient for quickly testing packages.
(setq package-enable-at-startup nil
      package-user-dir (concat rmcs-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir))

(after! package
  (let ((s (if gnutls-verify-error "s" "")))
    (prependq! package-archives
               ;; I omit Marmalade because its packages are manually submitted
               ;; rather than pulled, and so often out of date.
               `(("melpa" . ,(format "http%s://melpa.org/packages/" s))
                 ("org"   . ,(format "http%s://orgmode.org/elpa/"   s))))))

;; Refresh package.el the first time you call `package-install', so it can still
;; be used (e.g. to temporarily test packages). Remember to run 'rmcs sync' to
;; purge them; they can conflict with packages installed via straight!
(add-transient-hook! 'package-install (package-refresh-contents))


;;
;;; Straight

(setq straight-base-dir (file-truename rmcs-local-dir)
      straight-repository-branch "develop"
      ;; Since byte-code is rarely compatible across different versions of
      ;; Emacs, it's best we build them in separate directories, per emacs
      ;; version.
      straight-build-dir (format "build-%s" emacs-version)
      straight-cache-autoloads nil ; we already do this, and better.
      ;; Rmcs doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'rmcs sync' instant (once everything set up), which is much nicer
      ;; UX than the several seconds modification checks.
      straight-check-for-modifications nil
      ;; We handle package.el ourselves (and a little more comprehensively)
      straight-enable-package-integration nil
      ;; Before switching to straight, `rmcs-local-dir' would average out at
      ;; around 100mb with half Rmcs's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This has
      ;; no affect on packages that are pinned, however (run 'rmcs purge' to
      ;; compact those after-the-fact). Some packages break when shallow cloned
      ;; (like magit and org), but we'll deal with that elsewhere.
      straight-vc-git-default-clone-depth '(1 single-branch))

(with-eval-after-load 'straight
  ;; HACK: Rmcs relies on deferred compilation, which spares the user 20-50min
  ;;   of compilation at install time, but subjects them to ~50% CPU activity
  ;;   when starting Emacs for the first time. To complete this, straight.el
  ;;   needs to be told not to do native-compilation, but it won't obey
  ;;   `straight-disable-native-compile'.
  ;;
  ;;   It *will* obey `straight--native-comp-available', though. Trouble is:
  ;;   it's a constant; it resets itself when straight is loaded, so it must be
  ;;   changed afterwards.
  (setq straight--native-comp-available nil)
  ;; `let-alist' is built into Emacs 26 and onwards
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

(defadvice! rmcs--read-pinned-packages-a (fn &rest args)
  "Read `:pin's in `rmcs-packages' on top of straight's lockfiles."
  :around #'straight--lockfile-read-all
  (append (apply fn args) ; lockfiles still take priority
          (rmcs-package-pinned-list)))

;; HACK: This fixes an issue introduced in emacs-mirror/emacs@0d383b592c2f and
;;   is present in >=29: Straight.el uses `loaddefs-generate' if it is
;;   available, which activates `emacs-lisp-mode' to read autoloads files, but
;;   does so without suppressing its hooks. Some packages (like overseer) add
;;   hooks to `emacs-lisp-mode-hook' in their autoloads, and once triggered,
;;   they will try to load their dependencies (like dash or pkg-info), causing
;;   file errors.
;; REVIEW: Report this upstream.
(defadvice! rmcs--fix-loaddefs-generate--parse-file-a (fn &rest args)
  :around #'loaddefs-generate--parse-file
  (let (emacs-lisp-mode-hook)
    (apply fn args)))


;;
;;; native-comp

(when (featurep 'native-compile)
  (after! comp
    ;; HACK Disable native-compilation for some troublesome packages
    (mapc (rmcs-partial #'add-to-list 'native-comp-deferred-compilation-deny-list)
          (list "/emacs-jupyter.*\\.el\\'"
                "/evil-collection-vterm\\.el\\'"
                "/vterm\\.el\\'"
                "/with-editor\\.el\\'"))))


;;
;;; Bootstrappers

(defun rmcs--ensure-straight (recipe pin)
  (letenv! (("GIT_CONFIG" nil)
            ("GIT_CONFIG_NOSYSTEM" "1")
            ("GIT_CONFIG_GLOBAL" (or (getenv "RMCSGITCONFIG")
                                     "/dev/null")))
    (let ((repo-dir (rmcs-path straight-base-dir "straight/repos/straight.el"))
          (repo-url (concat "http" (if gnutls-verify-error "s")
                            "://github.com/"
                            (or (plist-get recipe :repo) "radian-software/straight.el")))
          (branch (or (plist-get recipe :branch) straight-repository-branch))
          (call (if init-file-debug
                    (lambda (&rest args)
                      (print! "%s" (cdr (apply #'rmcs-call-process args))))
                  (lambda (&rest args)
                    (apply #'rmcs-call-process args)))))
      (unless (file-directory-p repo-dir)
        (save-match-data
          (unless (executable-find "git")
            (user-error "Git isn't present on your system. Cannot proceed."))
          (let* ((version (cdr (rmcs-call-process "git" "version")))
                 (version
                  (and (string-match "\\_<[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)\\_>" version)
                       (match-string 0 version))))
            (if version
                (when (version< version "2.23")
                  (user-error "Git %s detected! Rmcs requires git 2.23 or newer!"
                              version)))))
        (print! (start "Installing straight..."))
        (print-group!
         (cl-destructuring-bind (depth . options)
             (ensure-list straight-vc-git-default-clone-depth)
           (let ((branch-switch (if (memq 'single-branch options)
                                    "--single-branch"
                                  "--no-single-branch")))
             (cond
              ((eq 'full depth)
               (funcall call "git" "clone" "--origin" "origin"
                        branch-switch repo-url repo-dir))
              ((integerp depth)
               (if (null pin)
                   (progn
                     (when (file-directory-p repo-dir)
                       (delete-directory repo-dir 'recursive))
                     (funcall call "git" "clone" "--origin" "origin" repo-url
                              "--no-checkout" repo-dir
                              "--depth" (number-to-string depth)
                              branch-switch
                              "--no-tags"
                              "--branch" straight-repository-branch))
                 (make-directory repo-dir 'recursive)
                 (let ((default-directory repo-dir))
                   (funcall call "git" "init")
                   (funcall call "git" "branch" "-m" straight-repository-branch)
                   (funcall call "git" "remote" "add" "origin" repo-url
                            "--master" straight-repository-branch)
                   (funcall call "git" "fetch" "origin" pin
                            "--depth" (number-to-string depth)
                            "--no-tags")
                   (funcall call "git" "reset" "--hard" pin)))))))))
      (require 'straight (concat repo-dir "/straight.el"))
      (rmcs-log "Initializing recipes")
      (mapc #'straight-use-recipes
            '((org-elpa :local-repo nil)
              (melpa              :type git :host github
                                  :repo "melpa/melpa"
                                  :build nil)
              (nongnu-elpa        :type git
                                  :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git"
                                  :local-repo "nongnu-elpa"
                                  :build nil)
              (gnu-elpa-mirror    :type git :host github
                                  :repo "emacs-straight/gnu-elpa-mirror"
                                  :build nil)
              (el-get             :type git :host github
                                  :repo "dimitri/el-get"
                                  :build nil)
              (emacsmirror-mirror :type git :host github
                                  :repo "emacs-straight/emacsmirror-mirror"
                                  :build nil))))))

(defun rmcs--ensure-core-packages (packages)
  (rmcs-log "Installing core packages")
  (dolist (package packages)
    (let* ((name (car package))
           (repo (symbol-name name)))
      (when-let (recipe (plist-get (cdr package) :recipe))
        (straight-override-recipe (cons name recipe))
        (when-let (local-repo (plist-get recipe :local-repo))
          (setq repo local-repo)))
      (print-group!
       ;; Only clone the package, don't build them. Straight hasn't been fully
       ;; configured by this point.
       (straight-use-package name nil t))
      ;; In case the package hasn't been built yet.
      (or (member (directory-file-name (straight--build-dir (symbol-name name)))
                  load-path)
          (add-to-list 'load-path (directory-file-name (straight--repos-dir repo)))))))

(defun rmcs-initialize-core-packages (&optional force-p)
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (when (or force-p (null (bound-and-true-p straight-recipe-repositories)))
    (rmcs-log "Initializing straight")
    (let ((packages (rmcs-package-list '((:core)))))
      (cl-destructuring-bind (&key recipe pin &allow-other-keys)
          (alist-get 'straight packages)
        (rmcs--ensure-straight recipe pin))
      (rmcs--ensure-core-packages
       (seq-filter (fn! (eq (plist-get % :type) 'core))
                   packages)))))

(defun rmcs-initialize-packages (&optional force-p)
  "Process all packages, essential and otherwise, if they haven't already been.

If FORCE-P is non-nil, do it anyway.

This ensures `rmcs-packages' is populated and `straight' recipes are properly
processed."
  (rmcs-initialize-core-packages force-p)
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (rmcs-log "Initializing package.el")
    (require 'package)
    (package-initialize)
    (unless package--initialized
      (error "Failed to initialize package.el")))
  (when (or force-p (null rmcs-packages))
    (rmcs-log "Initializing straight.el")
    (setq rmcs-disabled-packages nil
          rmcs-packages (rmcs-package-list))
    (let (packages)
      (dolist (package rmcs-packages)
        (cl-destructuring-bind
            (name &key recipe disable ignore &allow-other-keys) package
          (if ignore
              (straight-override-recipe (cons name '(:type built-in)))
            (if disable
                (cl-pushnew name rmcs-disabled-packages)
              (when recipe
                (straight-override-recipe (cons name recipe)))
              (appendq! packages (cons name (straight--get-dependencies name)))))))
      (dolist (package (cl-delete-duplicates packages :test #'equal))
        (straight-register-package package)
        (let ((name (symbol-name package)))
          (add-to-list 'load-path (directory-file-name (straight--build-dir name)))
          (straight--load-package-autoloads name))))))


;;
;;; Package management API

(defun rmcs-package-get (package &optional prop nil-value)
  "Returns PACKAGE's `package!' recipe from `rmcs-packages'."
  (let ((plist (cdr (assq package rmcs-packages))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

(defun rmcs-package-set (package prop value)
  "Set PROPERTY in PACKAGE's recipe to VALUE."
  (setf (alist-get package rmcs-packages)
        (plist-put (alist-get package rmcs-packages)
                   prop value)))

(defun rmcs-package-recipe (package &optional prop nil-value)
  "Returns the `straight' recipe PACKAGE was registered with."
  (let* ((recipe (straight-recipes-retrieve package))
         (plist (rmcs-plist-merge
                 (plist-get (alist-get package rmcs-packages) :recipe)
                 (cdr (if (memq (car recipe) '(quote \`))
                          (eval recipe t)
                        recipe)))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

(defun rmcs-package-recipe-repo (package)
  "Resolve and return PACKAGE's (symbol) local-repo property."
  (if-let* ((recipe (copy-sequence (rmcs-package-recipe package)))
            (recipe (if (and (not (plist-member recipe :type))
                             (memq (plist-get recipe :host) '(github gitlab bitbucket)))
                        (plist-put recipe :type 'git)
                      recipe))
            (repo (if-let (local-repo (plist-get recipe :local-repo))
                      (directory-file-name local-repo)
                    (ignore-errors (straight-vc-local-repo-name recipe)))))
      repo
    (symbol-name package)))

(defun rmcs-package-build-recipe (package &optional prop nil-value)
  "Returns the `straight' recipe PACKAGE was installed with."
  (let ((plist (nth 2 (gethash (symbol-name package) straight--build-cache))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

(defun rmcs-package-dependencies (package &optional recursive noerror)
  "Return a list of dependencies for a package.

If RECURSIVE is `tree', return a tree of dependencies.
If RECURSIVE is nil, only return PACKAGE's immediate dependencies.
If NOERROR, return nil in case of error."
  (cl-check-type package symbol)
  (let ((deps (straight-dependencies (symbol-name package))))
    (pcase recursive
      (`tree deps)
      (`t (flatten-list deps))
      (`nil (cl-remove-if #'listp deps)))))

(defun rmcs-package-depending-on (package &optional noerror)
  "Return a list of packages that depend on PACKAGE.

If PACKAGE (a symbol) isn't installed, throw an error, unless NOERROR is
non-nil."
  (cl-check-type package symbol)
  ;; can't get dependencies for built-in packages
  (unless (or (rmcs-package-build-recipe package)
              noerror)
    (error "Couldn't find %s, is it installed?" package))
  (straight-dependents (symbol-name package)))

;;; Predicate functions
(defun rmcs-package-built-in-p (package)
  "Return non-nil if PACKAGE (a symbol) is built-in."
  (eq (rmcs-package-build-recipe package :type)
      'built-in))

(defun rmcs-package-installed-p (package)
  "Return non-nil if PACKAGE (a symbol) is installed."
  (file-directory-p (straight--build-dir (symbol-name package))))

(defun rmcs-package-is-type-p (package type)
  "TODO"
  (memq type (ensure-list (rmcs-package-get package :type))))

(defun rmcs-package-in-module-p (package category &optional module)
  "Return non-nil if PACKAGE was installed by the user's private config."
  (when-let (modules (rmcs-package-get package :modules))
    (or (and (not module) (assq :user modules))
        (member (cons category module) modules))))

(defun rmcs-package-backend (package)
  "Return 'straight, 'builtin, 'elpa or 'other, depending on how PACKAGE is
installed."
  (cond ((gethash (symbol-name package) straight--build-cache)
         'straight)
        ((or (rmcs-package-built-in-p package)
             (assq package package--builtins))
         'builtin)
        ((assq package package-alist)
         'elpa)
        ((locate-library (symbol-name package))
         'other)))


;;; Package getters
(defun rmcs-packages--read (file &optional noeval noerror)
  (condition-case-unless-debug e
      (with-temp-buffer ; prevent buffer-local state from propagating
        (if (not noeval)
            (load file noerror 'nomessage 'nosuffix)
          (when (file-exists-p file)
            (insert-file-contents file)
            (with-syntax-table emacs-lisp-mode-syntax-table
              ;; Scrape `package!' blocks from FILE for a comprehensive listing of
              ;; packages used by this module.
              (while (search-forward "(package!" nil t)
                (let ((ppss (save-excursion (syntax-ppss))))
                  ;; Don't collect packages in comments or strings
                  (unless (or (nth 3 ppss)
                              (nth 4 ppss))
                    (goto-char (match-beginning 0))
                    (cl-destructuring-bind (_ name . plist)
                        (read (current-buffer))
                      (push (cons
                             name (plist-put
                                   plist :modules
                                   (list (rmcs-module-context-key))))
                            rmcs-packages)))))))))
    (user-error
     (user-error (error-message-string e)))
    (error
     (signal 'rmcs-package-error
             (list (rmcs-module-context-key)
                   file e)))))

(defun rmcs-package-list (&optional module-list)
  "Retrieve a list of explicitly declared packages from MODULE-LIST.

If MODULE-LIST is omitted, read enabled module list in configdepth order (see
`rmcs-module-set'). Otherwise, MODULE-LIST may be any symbol (or t) to mean read
all modules in `rmcs-modules-dir', including :core and :user. MODULE-LIST may
also be a list of module keys."
  (let ((module-list (cond ((null module-list) (rmcs-module-list))
                           ((symbolp module-list) (rmcs-module-list 'all))
                           (module-list)))
        (packages-file rmcs-module-packages-file)
        rmcs-disabled-packages
        rmcs-packages)
    (letf! (defun read-packages (key)
             (rmcs-module-context-with key
               (when-let (file (rmcs-module-locate-path
                                (car key) (cdr key) rmcs-module-packages-file))
                 (rmcs-packages--read file nil 'noerror))))
      (rmcs-context-with 'packages
        (let ((user? (assq :user module-list)))
          (when user?
            ;; We load the private packages file twice to populate
            ;; `rmcs-disabled-packages' disabled packages are seen ASAP...
            (let (rmcs-packages)
              (read-packages (cons :user nil))))
          (mapc #'read-packages module-list)
          ;; ...Then again to ensure privately overriden packages are properly
          ;; overwritten.
          (if user? (read-packages (cons :user nil)))
          (nreverse rmcs-packages))))))

(defun rmcs-package-pinned-list ()
  "Return an alist mapping package names (strings) to pinned commits (strings)."
  (let (alist)
    (dolist (package rmcs-packages alist)
      (cl-destructuring-bind (name &key disable ignore pin unpin &allow-other-keys)
          package
        (when (and (not ignore)
                   (not disable)
                   (or pin unpin))
          (setf (alist-get (file-name-nondirectory (rmcs-package-recipe-repo name))
                           alist nil 'remove #'equal)
                (unless unpin pin)))))))

(defun rmcs-package-recipe-list ()
  "Return straight recipes for non-builtin packages with a local-repo."
  (let (recipes)
    (dolist (recipe (hash-table-values straight--recipe-cache))
      (cl-destructuring-bind (&key local-repo type &allow-other-keys)
          recipe
        (unless (or (null local-repo)
                    (eq type 'built-in))
          (push recipe recipes))))
    (nreverse recipes)))


;;
;;; Module package macros

(cl-defmacro package!
    (name &rest plist &key built-in recipe ignore _type _pin _disable)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install packages. It is used to
populate `rmcs-packages' with metadata about the packages Rmcs needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :type core|local|built-in|virtual
   Specifies what kind of package this is. Can be a symbol or a list thereof.
     `core' = this is a protected package and cannot be disabled!
     `local' = this package is being modified in-place. This package's repo is
       unshallowed and will be skipped when you update packages.
     `built-in' = this package is already built-in (otherwise, will be
       installed)
     `virtual' = this package is not tracked by Rmcs's package manager. It won't
       be installed or uninstalled. Use this to pin 2nd order dependencies.
 :recipe RECIPE
   Specifies a straight.el recipe to allow you to acquire packages from external
   sources. See https://github.com/radian-software/straight.el#the-recipe-format
   for details on this recipe.
 :disable BOOL
   Do not install or update this package AND disable all of its `use-package!'
   and `after!' blocks.
 :ignore FORM
   Do not install this package.
 :pin STR|nil
   Pin this package to commit hash STR. Setting this to nil will unpin this
   package if previously pinned.
 :built-in BOOL|'prefer
   Same as :ignore if the package is a built-in Emacs package. This is more to
   inform help commands like `rmcs/help-packages' that this is a built-in
   package. If set to 'prefer, the package will not be installed if it is
   already provided by Emacs.

Returns t if package is successfully registered, and nil if it was disabled
elsewhere."
  (declare (indent defun))
  (when (and recipe (keywordp (car-safe recipe)))
    (cl-callf plist-put plist :recipe `(quote ,recipe)))
  ;; :built-in t is basically an alias for :ignore (locate-library NAME)
  (when built-in
    (when (and (not ignore)
               (equal built-in '(quote prefer)))
      (setq built-in `(locate-library ,(symbol-name name) nil (get 'load-path 'initial-value))))
    (cl-callf map-delete plist :built-in)
    (cl-callf plist-put plist :ignore built-in))
  `(let* ((name ',name)
          (plist (cdr (assq name rmcs-packages)))
          (dir (dir!))
          (module (rmcs-module-from-path dir)))
     (unless (rmcs-context-p 'packages)
       (signal 'rmcs-module-error
               (list module "package! can only be used in packages.el files")))
     ;; Record what module this declaration was found in
     (let ((module-list (plist-get plist :modules)))
       (unless (member module module-list)
         (cl-callf plist-put plist :modules
                   (append module-list
                           (list module)
                           (when (file-in-directory-p dir rmcs-user-dir)
                             '((:user . modules)))
                           nil))))
     ;; Merge given plist with pre-existing one
     (cl-loop for (key value) on (list ,@plist) by 'cddr
              when value
              do (cl-callf plist-put plist key value))
     ;; Some basic key validation; throws an error on invalid properties
     (condition-case e
         (when-let (recipe (plist-get plist :recipe))
           (cl-destructuring-bind
               (&key local-repo _files _flavor _build _pre-build _post-build
                     _includes _type _repo _host _branch _protocol _remote
                     _nonrecursive _fork _depth _source _inherit)
               recipe
             ;; Expand :local-repo from current directory
             (when local-repo
               (cl-callf plist-put plist :recipe
                         (plist-put recipe :local-repo
                                    (let ((local-path (expand-file-name local-repo dir)))
                                      (if (file-directory-p local-path)
                                          local-path
                                        local-repo)))))))
       (error
        (signal 'rmcs-package-error
                (cons ,(symbol-name name)
                      (error-message-string e)))))
     ;; These are the only side-effects of this macro!
     (setf (alist-get name rmcs-packages) plist)
     (if (plist-get plist :disable)
         (add-to-list 'rmcs-disabled-packages name)
       (with-no-warnings
         (cons name plist)))))

(defmacro disable-packages! (&rest packages)
  "A convenience macro for disabling packages in bulk.
Only use this macro in a module's (or your private) packages.el file."
  (macroexp-progn
   (mapcar (lambda (p) `(package! ,p :disable t))
           packages)))

(defmacro unpin! (&rest targets)
  "Unpin packages in TARGETS.

This unpins packages, so that 'rmcs upgrade' downloads their latest version. It
can be used one of five ways:

- To disable pinning wholesale: (unpin! t)
- To unpin individual packages: (unpin! packageA packageB ...)
- To unpin all packages in a group of modules: (unpin! :lang :tools ...)
- To unpin packages in individual modules:
    (unpin! (:lang python javascript) (:tools docker))

Or any combination of the above.

This macro should only be used from the user's private packages.el. No module
should use it!"
  (if (memq t targets)
      `(mapc (rmcs-rpartial #'rmcs-package-set :unpin t)
             (mapcar #'car rmcs-packages))
    (macroexp-progn
     (mapcar
      (lambda (target)
        (when target
          `(rmcs-package-set ',target :unpin t)))
      (cl-loop for target in targets
               if (or (keywordp target) (listp target))
               append
               (cl-loop with (category . modules) = (ensure-list target)
                        for (name . plist) in rmcs-packages
                        for pkg-modules = (plist-get plist :modules)
                        if (and (assq category pkg-modules)
                                (or (null modules)
                                    (cl-loop for module in modules
                                             if (member (cons category module) pkg-modules)
                                             return t))
                                name)
                        collect it)
               else if (symbolp target)
               collect target)))))

(provide 'rmcs-packages)
;;; rmcs-packages.el ends here