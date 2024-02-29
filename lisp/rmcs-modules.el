;;; rmcs-modules.el --- module & package management system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

(defvar rmcs-modules (make-hash-table :test 'equal)
  "A hash table of enabled modules. Set by `rmcs-initialize-modules'.")

(define-obsolete-variable-alias 'rmcs-modules-dirs 'rmcs-module-load-path "3.0.0")
(defvar rmcs-module-load-path
  (list (file-name-concat rmcs-user-dir "modules")
        (file-name-concat rmcs-emacs-dir "modules"))
  "A list of paths where Rmcs should search for modules.

Order determines priority (from highest to lowest).

Each entry is a string; an absolute path to the root directory of a module tree.
In other words, they should contain a two-level nested directory structure,
where the module's group and name was deduced from the first and second level of
directories. For example: if $RMCSDIR/modules/ is an entry, a
$RMCSDIR/modules/lang/ruby/ directory represents a ':lang ruby' module.")

;;; Module file variables
(defvar rmcs-module-init-file "init.el"
  "The filename for module early initialization config files.

Init files are loaded early, just after Rmcs core, and before modules' config
files. They are always loaded, even in non-interactive sessions, and before
`rmcs-before-modules-init-hook'. Related to `rmcs-module-config-file'.")

(defvar rmcs-module-config-file "config.el"
  "The filename for module configuration files.

Config files are loaded later, and almost always in interactive sessions. These
run before `rmcs-after-modules-config-hook' and after `rmcs-module-init-file'.")

(defvar rmcs-module-packages-file "packages.el"
  "The filename for the package configuration file.

Package files are read whenever Rmcs's package manager wants a manifest of all
desired packages. They are rarely read in interactive sessions (unless the user
uses a straight or package.el command directly).")

(defvar rmcs-module-metadata-file ".rmcsmodule"
  "The filename for a module's metadata file.

NOT IMPLEMENTED YET. This file contains a module's metadata: their version,
maintainers, checks, features, submodules, debug information, etc. And are used
to locate modules in the user's file tree.")

;; DEPRECATED: Module warnings will be rewritten in v3, and this variable will no longer be needed.
(make-obsolete-variable 'rmcs-obsolete-modules nil "3.0.0")
(defconst rmcs-obsolete-modules
  '((:feature (version-control  (:emacs vc) (:ui vc-gutter))
              (spellcheck       (:checkers spell))
              (syntax-checker   (:checkers syntax))
              (evil             (:editor evil))
              (snippets         (:editor snippets))
              (file-templates   (:editor file-templates))
              (workspaces       (:ui workspaces))
              (eval             (:tools eval))
              (lookup           (:tools lookup))
              (debugger         (:tools debugger)))
    (:tools   (rotate-text      (:editor rotate-text))
              (vterm            (:term vterm))
              (password-store   (:tools pass))
              (flycheck         (:checkers syntax))
              (flyspell         (:checkers spell))
              (macos            (:os macos)))
    (:emacs   (electric-indent  (:emacs electric))
              (hideshow         (:editor fold))
              (eshell           (:term eshell))
              (term             (:term term)))
    (:ui      (rmcs-modeline    (:ui modeline))
              (fci              (:ui fill-column))
              (evil-goggles     (:ui ophints))
              (tabbar           (:ui tabs))
              (pretty-code      (:ui ligatures)))
    (:app     (email            (:email mu4e))
              (notmuch          (:email notmuch)))
    (:lang    (perl             (:lang raku))))
  "A tree alist that maps deprecated modules to their replacement(s).

Each entry is a three-level tree. For example:

  (:feature (version-control (:emacs vc) (:ui vc-gutter))
            (spellcheck (:checkers spell))
            (syntax-checker (:tools flycheck)))

This marks :feature version-control, :feature spellcheck and :feature
syntax-checker modules obsolete. e.g. If :feature version-control is found in
your `rmcs!' block, a warning is emitted before replacing it with :emacs vc and
:ui vc-gutter.")

(make-obsolete-variable 'rmcs-inhibit-module-warnings nil "3.0.0")
(defvar rmcs-inhibit-module-warnings (not noninteractive)
  "If non-nil, don't emit deprecated or missing module warnings at startup.")

;;; Custom hooks
(defcustom rmcs-before-modules-init-hook nil
  "Hooks run before module init.el files are loaded."
  :group 'rmcs
  :type 'hook)

(defcustom rmcs-after-modules-init-hook nil
  "Hooks run after module init.el files are loaded."
  :group 'rmcs
  :type 'hook)

(defcustom rmcs-before-modules-config-hook nil
  "Hooks run before module config.el files are loaded."
  :group 'rmcs
  :type 'hook)

(defcustom rmcs-after-modules-config-hook nil
  "Hooks run after module config.el files are loaded (but before the user's)."
  :group 'rmcs
  :type 'hook)


;;
;;; `rmcs-module-context'

(defvar rmcs-module--empty-context [nil nil nil nil nil nil nil])

(eval-and-compile
  (put 'rmcs-module-context 'keys '(:index 0 :initdepth 1 :configdepth 2
                                    :group 3 :name 4 :flags 5 :features 6)))
(defvar rmcs-module-context rmcs-module--empty-context
  "A vector describing the module associated it with the active context.

Contains the following: [INDEX INITDEPTH CONFIGDEPTH :GROUP MODULE FLAGS FEATURES]

Do not directly set this variable, only let-bind it.")

;; DEPRECATED: Remove this when byte-compilation is introduced to Rmcs core.
(defmacro rmcs-module--context-field (field)
  (plist-get (get 'rmcs-module-context 'keys) field))

(defun rmcs-module-context-get (field &optional context)
  "Return the FIELD of CONTEXT.

FIELD should be one of `index', `initdepth', `configdepth', `group', `name',
`flags', or `features'.  CONTEXT should be a `rmcs-module-context' vector. If
omitted, defaults to `rmcs-module-context'."
  (aref (or context rmcs-module-context)
        (plist-get (get 'rmcs-module-context 'keys)
                   field)))

(defun rmcs-module-context (group &optional name)
  "Create a `rmcs-module-context' from a module by GROUP and NAME.

If NAME is omitted, GROUP is treated as a module key cons cell: (GROUP . NAME)."
  (declare (side-effect-free t))
  (let ((key (if name (cons group name) group)))
    (or (get (or (car-safe key) key)
             (cdr-safe key))
        rmcs-module--empty-context)))

(defun rmcs-module-context-key (&optional context)
  "Return the module of the active `rmcs-module-context' as a module key."
  (declare (side-effect-free t))
  (let ((context (or context rmcs-module-context)))
    (cons (aref context (rmcs-module--context-field :group))
          (aref context (rmcs-module--context-field :name)))))

(defmacro rmcs-module-context-with (module-key &rest body)
  "Evaluate BODY with `rmcs-module-context' informed by MODULE-KEY."
  (declare (indent 1))
  `(let ((rmcs-module-context (rmcs-module-context ,module-key)))
     (rmcs-log ":context:module: =%s" rmcs-module-context)
     ,@body))


;;
;;; Module API

(defun rmcs-module-p (category module &optional flag)
  "Returns t if CATEGORY MODULE is enabled (ie. present in `rmcs-modules')."
  (declare (pure t) (side-effect-free t))
  (when-let (plist (gethash (cons category module) rmcs-modules))
    (or (null flag)
        (and (memq flag (plist-get plist :flags))
             t))))

(defun rmcs-module-depth (category module &optional initdepth?)
  "Return the depth of CATEGORY MODULE.

If INITDEPTH? is non-nil, use the CAR if a module was given two depths (see
`rmcs-module-set')."
  (if-let (depth (rmcs-module-get category module :depth))
      (or (if initdepth?
              (car-safe depth)
            (cdr-safe depth))
          depth)
    0))

(defun rmcs-module-get (category module &optional property)
  "Returns the plist for CATEGORY MODULE. Gets PROPERTY, specifically, if set."
  (declare (pure t) (side-effect-free t))
  (when-let (plist (gethash (cons category module) rmcs-modules))
    (if property
        (plist-get plist property)
      plist)))

(defun rmcs-module-put (category module &rest plist)
  "Set a PROPERTY for CATEGORY MODULE to VALUE. PLIST should be additional pairs
of PROPERTY and VALUEs.

\(fn CATEGORY MODULE PROPERTY VALUE &rest [PROPERTY VALUE [...]])"
  (puthash (cons category module)
           (if-let (old-plist (rmcs-module-get category module))
               (if (null plist)
                   old-plist
                 (when (cl-oddp (length plist))
                   (signal 'wrong-number-of-arguments (list (length plist))))
                 (while plist
                   (plist-put old-plist (pop plist) (pop plist)))
                 old-plist)
             plist)
           rmcs-modules))

(defun rmcs-module-set (category module &rest plist)
  "Enables a module by adding it to `rmcs-modules'.

CATEGORY is a keyword, module is a symbol, PLIST is a plist that accepts the
following properties:

  :path STRING
    Path to the directory where this module lives.
  :depth INT|(INITDEPTH . CONFIGDEPTH)
    Determines module load order. If a cons cell, INITDEPTH determines the load
    order of the module's init.el, while CONFIGDEPTH determines the same for all
    other config files (config.el, packages.el, doctor.el, etc).
  :flags (SYMBOL...)
    A list of activated flags for this module.
  :features (SYMBOL...)
    A list of active features, determined from module's metadata. NOT
    IMPLEMENTED YET.

If PLIST consists of a single nil, the module is purged from memory instead."
  (if (car plist)
      (let* ((depth (ensure-list (or (plist-get plist :depth) 0)))
             (idepth (or (cdr depth) (car depth)))
             (cdepth (car depth))
             (idx (hash-table-count rmcs-modules)))
        ;; PERF: Rmcs caches module index, flags, and features in symbol plists
        ;;   for fast lookups in `modulep!' and elsewhere. plists are lighter
        ;;   and faster than hash tables for datasets this size, and this
        ;;   information is looked up *very* often. The structure of this cache
        ;;   should match `rmcs-module-context's.
        (put category module
             (vector idx idepth cdepth
                     category module
                     (plist-get plist :flags)
                     (plist-get plist :features)))
        ;; The hash table will always been Rmcs's formal storage for
        ;; modules.
        (puthash (cons category module) plist rmcs-modules))
    (remhash (cons category module) rmcs-modules)
    (cl-remf (symbol-plist category) module)))

(defun rmcs-module-list (&optional paths-or-all initorder?)
  "Return a list of (:group . name) module keys in order of their :depth.

PATHS-OR-ALL can either be a non-nil value or a list of directories. If given a
list of directories, return a list of module keys for all modules present
underneath it.  If non-nil, return the same, but search `rmcs-module-load-path'
(includes :core and :user). Modules that are enabled are sorted first by their
:depth, followed by disabled modules in lexicographical order (unless a :depth
is specified in their .rmcsmodule).

If INITORDER? is non-nil, sort modules by their initdepth, rather than their
configdepth. See `rmcs-module-set' for details."
  (sort (if paths-or-all
            (delete-dups
             (append (seq-remove #'cdr (rmcs-module-list nil initorder?))
                     (rmcs-files-in (if (listp paths-or-all)
                                        paths-or-all
                                      rmcs-module-load-path)
                                    :map #'rmcs-module-from-path
                                    :type 'dirs
                                    :mindepth 1
                                    :depth 1)))
          (hash-table-keys rmcs-modules))
        (let ((idx (if initorder? 1 2)))
          (lambda! ((groupa . namea) (groupb . nameb))
            (let ((a (get groupa namea))
                  (b (get groupb nameb)))
              (or (null b)
                  (and
                   a (let ((adepth (aref a idx))
                           (bdepth (aref b idx)))
                       (if (= adepth bdepth)
                           (< (aref a 0) (aref b 0))
                         (< adepth bdepth))))))))))

(defun rmcs-module-expand-path (category module &optional file)
  "Expands a path to FILE relative to CATEGORY and MODULE.

CATEGORY is a keyword. MODULE is a symbol. FILE is an optional string path.
If the category isn't enabled this returns nil. For finding disabled modules use
`rmcs-module-locate-path'."
  (when-let (path (rmcs-module-get category module :path))
    (if file
        (file-name-concat path file)
      path)))

(defun rmcs-module-locate-path (category &optional module file)
  "Searches `rmcs-module-load-path' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g. 'python). FILE
is a string that will be appended to the resulting path. If no path exists, this
returns nil, otherwise an absolute path."
  (let (file-name-handler-alist)
    (if-let (path (rmcs-module-expand-path category module file))
        (if (or (null file)
                (file-exists-p path))
            path)
      (let* ((category (rmcs-keyword-name category))
             (module (if module (symbol-name module)))
             (path (file-name-concat category module file)))
        (if file
            ;; PERF: locate-file-internal is a little faster for finding files,
            ;;   but its interface for finding directories is clumsy.
            (locate-file-internal path rmcs-module-load-path '("" ".elc" ".el"))
          (cl-loop for default-directory in rmcs-module-load-path
                   if (file-exists-p path)
                   return (expand-file-name path)))))))

(defun rmcs-module-locate-paths (module-list file)
  "Return all existing paths to FILE under each module in MODULE-LIST.

MODULE-LIST is a list of cons cells (GROUP . NAME). See `rmcs-module-list' for
an example."
  (cl-loop for (group . name) in (or module-list (rmcs-module-list))
           if (rmcs-module-locate-path group name file)
           collect it))

(defun rmcs-module-from-path (path &optional enabled-only)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path).
If ENABLED-ONLY, return nil if the containing module isn't enabled."
  (let* ((file-name-handler-alist nil)
         (path (expand-file-name path)))
    (save-match-data
      (cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
             (when-let* ((category (rmcs-keyword-intern (match-string 1 path)))
                         (module   (intern (match-string 2 path))))
               (and (or (null enabled-only)
                        (rmcs-module-p category module))
                    (cons category module))))
            ((file-in-directory-p path rmcs-core-dir)
             (cons :core nil))
            ((file-in-directory-p path rmcs-user-dir)
             (cons :user nil))))))

(defun rmcs-module-load-path (&optional module-load-path)
  "Return a list of file paths to activated modules.

The list is in no particular order and its file paths are absolute. If
MODULE-DIRS is non-nil, include all modules (even disabled ones) available in
those directories."
  (declare (pure t) (side-effect-free t))
  (cl-loop with module-load-path = (or module-load-path rmcs-module-load-path)
           for (cat . mod) in (rmcs-module-list module-load-path)
           collect (rmcs-module-locate-path cat mod)))

(defun rmcs-module-mplist-map (fn mplist)
  "Apply FN to each module in MPLIST."
  (let ((mplist (copy-sequence mplist))
        (inhibit-message rmcs-inhibit-module-warnings)
        obsolete
        results
        category m)
    (while mplist
      (setq m (pop mplist))
      (cond ((keywordp m)
             (setq category m
                   obsolete (assq m rmcs-obsolete-modules)))
            ((null category)
             (error "No module category specified for %s" m))
            ((and (listp m) (keywordp (car m)))
             (pcase (car m)
               (:cond
                (cl-loop for (cond . mods) in (cdr m)
                         if (eval cond t)
                         return (prependq! mplist mods)))
               (:if (if (eval (cadr m) t)
                        (push (caddr m) mplist)
                      (prependq! mplist (cdddr m))))
               (test (if (xor (eval (cadr m) t)
                              (eq test :unless))
                         (prependq! mplist (cddr m))))))
            ((catch 'rmcs-modules
               (let* ((module (if (listp m) (car m) m))
                      (flags  (if (listp m) (cdr m))))
                 (when-let (new (assq module obsolete))
                   (let ((newkeys (cdr new)))
                     (if (null newkeys)
                         (print! (warn "%s module was removed"))
                       (if (cdr newkeys)
                           (print! (warn "%s module was removed and split into the %s modules")
                                   (list category module)
                                   (mapconcat #'prin1-to-string newkeys ", "))
                         (print! (warn "%s module was moved to %s")
                                 (list category module)
                                 (car newkeys)))
                       (push category mplist)
                       (dolist (key newkeys)
                         (push (if flags
                                   (nconc (cdr key) flags)
                                 (cdr key))
                               mplist)
                         (push (car key) mplist))
                       (throw 'rmcs-modules t))))
                 (push (funcall fn category module :flags (if (listp m) (cdr m)))
                       results))))))
    (when noninteractive
      (setq rmcs-inhibit-module-warnings t))
    (nreverse results)))


;;
;;; Module config macros

(put :if     'lisp-indent-function 2)
(put :when   'lisp-indent-function 'defun)
(put :unless 'lisp-indent-function 'defun)

(defmacro rmcs! (&rest modules)
  "Bootstraps RMCS Emacs and its modules.

If the first item in MODULES doesn't satisfy `keywordp', MODULES is evaluated,
otherwise, MODULES is a multiple-property list (a plist where each key can have
multiple, linear values).

The bootstrap process involves making sure the essential directories exist, core
packages are installed, `rmcs-autoloads-file' is loaded, `rmcs-packages-file'
cache exists (and is loaded) and, finally, loads your private init.el (which
should contain your `rmcs!' block).

Module load order is determined by your `rmcs!' block. See `rmcs-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  `(when noninteractive
     (rmcs-module-mplist-map
      (lambda (category module &rest plist)
        (let ((path (rmcs-module-locate-path category module)))
          (unless path
            (print! (warn "Failed to locate a '%s %s' module") category module))
          (apply #'rmcs-module-set category module
                 :path path
                 plist)))
      ,@(if (keywordp (car modules))
            (list (list 'quote modules))
          modules))
     rmcs-modules))

;; DEPRECATED Remove in 3.0
(define-obsolete-function-alias 'featurep! 'modulep! "3.0.0")

(defmacro modulep! (category &optional module flag)
  "Return t if :CATEGORY MODULE (and +FLAGS) are enabled.

If FLAG is provided, returns t if CATEGORY MODULE has FLAG enabled.

  (modulep! :config default +flag)

CATEGORY and MODULE may be omitted when this macro is used from a Rmcs module's
source (except your RMCSDIR, which is a special module). Like so:

  (modulep! +flag)

For more about modules and flags, see `rmcs!'."
  ;; PERF: This macro bypasses the module API to spare startup their runtime
  ;;   cost, as `modulep!' gets called *a lot* during startup. In the future,
  ;;   Rmcs will byte-compile its core files. At that time, we can use it again.
  (and (cond (flag (memq flag (aref (or (get category module) rmcs-module--empty-context)
                                    (rmcs-module--context-field :flags))))
             (module (get category module))
             ((aref rmcs-module-context 0)
              (memq category (aref rmcs-module-context
                                   (rmcs-module--context-field :flags))))
             ((let ((file
                     ;; This must be expanded at the call site, not in
                     ;; `modulep!'s definition, to get the file we want.
                     (macroexpand '(file!))))
                (if-let (module (rmcs-module-from-path file))
                    (memq category (aref (or (get (car module) (cdr module))
                                             rmcs-module--empty-context)
                                         (rmcs-module--context-field :flags)))
                  (error "(modulep! %s %s %s) couldn't figure out what module it was called from (in %s)"
                         category module flag file)))))
       t))


;;
;;; Defaults

;; Register Rmcs's two virtual module categories, representing Rmcs's core and
;; the user's config; which are always enabled.
(rmcs-module-set :core nil :path rmcs-core-dir :depth -110)
(rmcs-module-set :user nil :path rmcs-user-dir :depth '(-105 . 105))

;; DEPRECATED: I intend to phase out our internal usage of `use-package' and
;;   move it to a :config use-package module. The macro is far too complex and
;;   magical for our needs, but until this move is done, ':config use-package'
;;   will remain a hardcoded module for backwards compatibility.
(rmcs-module-set :config 'use-package
                 :path (rmcs-module-locate-path :config 'use-package)
                 :depth -111)

(provide 'rmcs-modules)
;;; rmcs-modules.el ends here
