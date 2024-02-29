;;; lisp/rmcs-cli.el --- API+DSL for Rmcs's CLI framework -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; The heart of Rmcs's CLI framework. This is safe to load in interactive
;; sessions (for API access and syntax highlighting), but much of the API
;; expects a noninteractive session, so take care when testing code!
;;
;;; Code:

(when noninteractive
  ;; PERF: Deferring the GC in non-interactive sessions isn't as important, but
  ;;   still yields a notable benefit. Still, avoid setting it to high here, as
  ;;   runaway memory usage is a real risk in longer sessions.
  (setq gc-cons-threshold 134217728  ; 128mb
        ;; Backported from 29 (see emacs-mirror/emacs@73a384a98698)
        gc-cons-percentage 1.0)

  ;; REVIEW: Remove these later. The endpoints should be responsibile for
  ;;   ensuring they exist. For now, they exist to quell file errors.
  (mapc (rmcs-rpartial #'make-directory 'parents)
        (list rmcs-local-dir
              rmcs-data-dir
              rmcs-cache-dir
              rmcs-state-dir))

  ;; HACK: Load `cl' and site files manually to prevent polluting logs and
  ;;   stdout with deprecation and/or file load messages.
  (let ((inhibit-message (not init-file-debug)))
    (require 'cl nil t)
    (unless site-run-file
      (let ((site-run-file "site-start")
            (tail load-path)
            (lispdir (expand-file-name "../lisp" data-directory))
            dir)
        (while tail
          (setq dir (car tail))
          (let ((default-directory dir))
            (load (expand-file-name "subdirs.el") t inhibit-message t))
          (unless (string-prefix-p lispdir dir)
            (let ((default-directory dir))
              (load (expand-file-name "leim-list.el") t inhibit-message t)))
          (setq tail (cdr tail)))
        (load site-run-file t inhibit-message))))

  (setq-default
   ;; PERF: Don't generate superfluous files when writing temp buffers.
   make-backup-files nil
   ;; COMPAT: Stop user configuration from interfering with package management.
   enable-dir-local-variables nil
   ;; PERF: Reduce ambiguity, embrace specificity, enjoy predictability.
   case-fold-search nil
   ;; UX: Don't clog the user's trash with our CLI refuse.
   delete-by-moving-to-trash nil)

  ;; Load just the... bear necessities~
  (require 'seq)
  (require 'map)

  ;; Suppress any possible coding system prompts during CLI sessions.
  (set-language-environment "UTF-8")

  ;; Load and set up our debugger first, so backtraces can be made more
  ;; presentable and logged to file.
  (rmcs-require 'rmcs-lib 'debug)
  (if init-file-debug (rmcs-debug-mode +1))

  ;; Then load the rest of Rmcs's libs eagerly, since autoloads may not be
  ;; generated/loaded yet.
  (rmcs-require 'rmcs-lib 'process)
  (rmcs-require 'rmcs-lib 'system)
  (rmcs-require 'rmcs-lib 'git)
  (rmcs-require 'rmcs-lib 'plist)
  (rmcs-require 'rmcs-lib 'files)
  (rmcs-require 'rmcs-lib 'print)
  (rmcs-require 'rmcs-lib 'autoloads)

  ;; Ensure straight and core packages are ready to go for CLI commands.
  (require 'rmcs-modules)
  (require 'rmcs-packages)
  (require 'rmcs-profiles)
  ;; Last minute initialization at the end of loading this file.
  (with-eval-after-load 'rmcs-cli
    (rmcs-run-hooks 'rmcs-before-init-hook)))


;;
;;; Variables

(defgroup rmcs-cli nil
  "Rmcs's command-line interface framework."
  :link '(url-link "https://rmcsemacs.org/cli")
  :group 'rmcs)

(defvar rmcs-cli-load-path
  (append (when-let ((rmcspath (getenv "RMCSPATH")))
            (cl-loop for dir in (split-string rmcspath path-separator)
                     collect (expand-file-name dir)))
          (list (file-name-concat (dir!) "cli")))
  "A list of paths to search for autoloaded Rmcs CLIs.

It is prefilled by the RMCSPATH envvar (a colon-separated list on Linux/macOS,
semicolon otherwise).")

;;; CLI definition variables
(defvar rmcs-cli-argument-types
  '(&args
    &cli
    &context
    &flags
    &multiple
    &optional
    &rest
    &required
    &input
    &whole)
  "A list of auxiliary keywords allowed in `defcli!'s arglist.

See `defcli!' for documentation on them.")

(defvar rmcs-cli-option-types
  '((&flag  . &flags)
    (&multi . &multiple))
  "An alist of auxiliary keywords permitted in option specs in `defcli!'.

They serve as shorter, inline aliases for `rmcs-cli-argument-types'.

See `defcli!' for documentation on them.")

(defvar rmcs-cli-option-generators
  '((&flags    . rmcs-cli--make-option-flag)
    (&multiple . rmcs-cli--make-option-multi)
    (&required . rmcs-cli--make-option-generic)
    (&optional . rmcs-cli--make-option-generic))
  "An alist of `rmcs-cli-option' factories for argument types.

Types that

See argument types in `rmcs-cli-argument-types', and `defcli!' for usage.")

(defvar rmcs-cli-option-arg-types
  `((dir    :test file-directory-p
            :read expand-file-name
            :error "Not a valid path to an existing directory"
            :zshcomp "_dirs")
    (file   :test file-exists-p
            :read expand-file-name
            :error "Not a valid path to an existing file"
            :zshcomp "_files")
    (stdout :test ,(lambda (str) (equal str "-"))
            :read identity
            :error "Not a dash to signal stdout"
            :zshcomp "(-)")
    (path   :read expand-file-name :zshcomp "_files")
    (form   :read read)
    (regexp :test ,(lambda (str) (always (string-match-p str ""))))
    (int    :test "^[0-9]+$"
            :read string-to-number
            :error "Not an integer")
    (num    :test "^[0-9]+\\(\\.[0-9]+\\)?$"
            :read string-to-number
            :error "Not a valid number or float")
    (float  :test "^[0-9]+\\(\\.[0-9]+\\)$"
            :read string-to-number
            :error "Not a float")
    (bool   :test "^y\\(?:es\\)?\\|no?\\|on\\|off\\|t\\(?:rue\\)?\\|false\\|[01]\\|$"
            :read ,(lambda (x)
                     (pcase x
                       ((or "y" "yes" "t" "true" "1" "on") :yes)
                       ((or "n" "no"  "nil" "false" "0" "off") :no)))
            :error "Not a valid boolean, should be blank or one of: yes, no, y, n, true, false, on, off"
            :zshcomp "(y n yes no true false on off 1 0)")
    (date   :test ,(lambda (str)
                     (let ((ts (parse-time-string str)))
                       (and (decoded-time-day ts)
                            (decoded-time-month ts)
                            (decoded-time-year ts))))
            :read parse-time-string
            :error "Not a valid date (try YYYY-MM-DD or a date produced by `date')")
    (time   :test ,(lambda (str)
                     (let ((ts (parse-time-string str)))
                       (and (decoded-time-hour ts)
                            (decoded-time-minute ts)
                            (decoded-time-second ts))))
            :read parse-time-string
            :error "Not a valid date (try YYYY-MM-DD or a date produced by `date')")
    (duration :test ,(lambda (str)
                       (not (cl-loop for d in (split-string-and-unquote str " ")
                                     unless (string-match-p "^[0-9]+[hmsdMY]$" d)
                                     return t)))
              :read ,(rmcs-rpartial #'split-string-and-unquote " ")
              :error "Not a valid duration (e.g. 5h 20m 40s 2Y 1M)")
    (size :test "^[0-9]+[kmgt]?b$"
          :read ,(lambda (str)
                   (save-match-data
                     (and (string-match "^\\([0-9]+\\(?:\\.[0-9]+\\)\\)\\([kmgt]?b\\)$" str)
                          (* (string-to-number (match-string 1 str))
                             (or (cdr (assoc (match-string 2 str)
                                             '(("kb" . 1000)
                                               ("mb" . 1000000)
                                               ("gb" . 1000000000)
                                               ("tb" . 1000000000000))))
                                 1)))))
          :error "Not a valid filesize (e.g. 5mb 10.4kb 2gb 1.4tb)"))
  "A list of implicit option argument datatypes and their rules.

Recognizies the following properies:

  :test FN
    Predicate function to determine if a value is valid.
  :read FN
    A transformer that converts the string argument to a desired format.
  :error STR
    The message to display if a value fails :test.")

;;; Post-script settings
(defvar rmcs-cli-exit-commands
  '(;; (:editor  . rmcs-cli--exit-editor)
    ;; (:emacs   . rmcs-cli--exit-emacs)
    (:pager   . rmcs-cli--exit-pager)
    (:pager?  . rmcs-cli--exit-pager-maybe)
    (:restart . rmcs-cli--exit-restart))
  "An alist of commands that `rmcs-cli--exit' recognizes.")

(defvar rmcs-cli-pager (getenv "RMCSPAGER")
  "The PAGER command to use.

If nil, falls back to less.")

(defvar rmcs-cli-pager-ratio 1.0
  "If output exceeds TTY height times this ratio, the pager is invoked.

Only applies if (exit! :pager) or (exit! :pager?) are called.")

;;; Logger settings
(defvar rmcs-cli-log-file-format (expand-file-name "logs/cli.%s.%s.%s" rmcs-state-dir)
  "Where to write any output/log file to.

Must have two arguments, one for session id and the other for log type.")

(defvar rmcs-cli-log-retain 12
  "Number of each log type to retain.")

(defvar rmcs-cli-log-backtrace-depth 12
  "How many frames of the backtrace to display in stdout.")

(defvar rmcs-cli-log-straight-error-lines 16
  "How many lines of straight.el errors to display in stdout.")

(defvar rmcs-cli-log-benchmark-threshold 5
  "How much execution time (in seconds) before benchmark is shown.

If set to nil, only display benchmark if a CLI explicitly requested with a
non-nil :benchmark property.
If set to `always', show the benchmark no matter what.")

;;; Internal variables
(defvar rmcs-cli--context nil)
(defvar rmcs-cli--exit-code 255)
(defvar rmcs-cli--group-plist nil)
(defvar rmcs-cli--table (make-hash-table :test 'equal))


;;
;;; Custom hooks

(defcustom rmcs-cli-create-context-functions ()
  "A hook executed once a new context has been generated.

Called by `rmcs-cli-context-parse' and `rmcs-cli-context-restore', once a
`rmcs-cli-context' is fully populated and ready to be executed (but before it
has).

Hooks are run with one argument: the newly created context."
  :type 'hook
  :group 'rmcs-cli)

(defcustom rmcs-cli-before-run-functions ()
  "Hooks run before `run!' executes the command.

Runs with a single argument: the active context (a `rmcs-cli-context' struct)."
  :type 'hook
  :group 'rmcs-cli)

(defcustom rmcs-cli-after-run-functions ()
  "Hooks run after `run!' has executed the command.

Runs with two arguments: the active context (a `rmcs-cli-context' struct) and
the return value of the executed CLI."
  :type 'hook
  :group 'rmcs-cli)


;;
;;; Errors

(define-error 'rmcs-cli-error "There was an unexpected error" 'rmcs-error)
(define-error 'rmcs-cli-definition-error "Invalid CLI definition" 'rmcs-cli-error)
(define-error 'rmcs-cli-autoload-error "Failed to autoload deferred command" 'rmcs-cli-error)
(define-error 'rmcs-cli-invalid-prefix-error "Prefix has no defined commands" 'rmcs-cli-error)
(define-error 'rmcs-cli-command-not-found-error "Could not find that command" 'rmcs-cli-error)
(define-error 'rmcs-cli-wrong-number-of-arguments-error "Wrong number of CLI arguments" 'rmcs-cli-error)
(define-error 'rmcs-cli-unrecognized-option-error "Not a recognized option" 'rmcs-cli-error)
(define-error 'rmcs-cli-invalid-option-error "Invalid option value" 'rmcs-cli-error)


;;
;;; `rmcs-cli'

(cl-defstruct rmcs-cli
  "An executable CLI command."
  (command nil :read-only t)
  type
  docs
  autoload
  alias
  options
  arguments
  plist
  fn)

(defun rmcs-cli-execute (cli bindings)
  "Execute CLI with BINDINGS (an alist).

BINDINGS is an alist of (SYMBOL . VALUE) to bind lexically during CLI's
execution. Can be generated from a `rmcs-cli-context' with
`rmcs-cli--bindings'."
  (rmcs-log "execute: %s %s" (rmcs-cli-key cli) bindings)
  (funcall (rmcs-cli-fn cli) cli bindings))

(defun rmcs-cli-key (cli)
  "Return CLI's (type . command), used as a table key or unique identifier."
  (let ((command (rmcs-cli-command cli)))
    (if-let (type (rmcs-cli-type cli))
        (cons type command)
      command)))

(defun rmcs-cli-command-normalize (command &optional plist)
  "Ensure that COMMAND is properly formatted.

This means that all non-keywords are strings, any prefixes provided by PLIST are
prepended, and the keyword is in front."
  (let* ((command (ensure-list command))
         (prefix  (plist-get plist :prefix))
         (prefix  (if prefix (rmcs-cli-command-normalize
                              prefix (append `(:prefix nil) plist))))
         (command (append prefix command))
         (type    (cl-find-if #'keywordp (remq :root command) :from-end t))
         (command (seq-subseq
                   command (or (cl-position :root command :from-end t)
                               0))))
    (when (or command prefix)
      (cl-loop with map = (fn! (if (or (stringp %) (keywordp %)) % (prin1-to-string %)))
               for c in (delq nil (cons type (seq-remove #'keywordp command)))
               if (listp c)
               collect (mapcar map c)
               else collect (funcall map c)))))

(defun rmcs-cli-command-string (command)
  "Return a joined string representation of normalized COMMAND.

COMMAND should either be a command list (e.g. '(rmcs foo bar)) or a `rmcs-cli'
struct."
  (mapconcat (rmcs-partial #'format "%s")
             (rmcs-cli--command command)
             " "))

(defun rmcs-cli-get (command &optional noresolve? noload?)
  "Return CLI at COMMAND.

Will autoload COMMAND if it was deferred with `defcli-autoload!'.

If NORESOLVE?, don't follow aliases."
  (when-let* ((command (rmcs-cli--command command))
              (cli (gethash command rmcs-cli--table))
              (cli (if noload? cli (rmcs-cli-load cli))))
    (if noresolve?
        cli
      (let (path)
        (while (setq path (ignore-errors (rmcs-cli-alias cli)))
          (setq cli (rmcs-cli-get path t noload?)))
        (unless cli
          (signal 'rmcs-cli-command-not-found-error (or path command)))
        cli))))

(defun rmcs-cli-path (cli &optional noload?)
  "Return a list of `rmcs-cli's encountered while following CLI's aliases.

If NOLOAD? is non-nil, don't autoload deferred CLIs (see `rmcs-cli-get')."
  (when cli
    (cons
     cli (let (alias paths)
           (while (setq alias (ignore-errors (rmcs-cli-alias cli)))
             (and (setq cli (rmcs-cli-get alias t noload?))
                  (push cli paths)))
           (nreverse paths)))))

(defun rmcs-cli-find (command &optional nopartials?)
  "Find all CLIs assocated with COMMAND, including partials.

COMMAND can be a command path (list of strings), a `rmcs-cli' struct, or a
`rmcs-cli-context' struct.

Returned in the order they will execute. Includes pseudo CLIs."
  (let* ((command (rmcs-cli--command command))
         (paths (nreverse (rmcs-cli--command-expand command t)))
         results clis)
    (push '(:after) results)
    (dolist (path paths)
      (push (cons :after path) results))
    (push command results)
    (dolist (path (nreverse paths))
      (push (cons :before path) results))
    (push '(:before) results)
    (dolist (result results (nreverse clis))
      (when-let ((cli (rmcs-cli-get result t))
                 ((or (not nopartials?)
                      (rmcs-cli-type cli))))
        (cl-pushnew cli clis
                    :test #'equal
                    :key #'rmcs-cli-key)))))

(defun rmcs-cli-prop (cli prop &optional null-value)
  "Returns a PROPerty of CLI's plist, or NULL-VALUE if it doesn't exist."
  (let ((plist (rmcs-cli-plist cli)))
    (if (plist-member plist prop)
        (plist-get plist prop)
      null-value)))

(cl-defun rmcs-cli-subcommands (command &optional (depth 9999) &key tree? all? predicate?)
  "Return a list of subcommands, DEPTH levels deep, below COMMAND.

  If DEPTH is non-nil, list *all* subcommands, recursively. Otherwise it expects
an integer.
  If TREE?, return commands in a tree structure.
  If ALL?, include hidden commands (like aliases)."
  (when (or (null depth) (> depth 0))
    (catch :predicate
      (let* ((command (rmcs-cli--command command t))
             (prefixlen (length command))
             results)
        (dolist (cli (hash-table-values rmcs-cli--table))
          (let ((clicmd (rmcs-cli-command cli)))
            (when (and (not (rmcs-cli-type cli))
                       (= (length clicmd) (1+ prefixlen))
                       (equal command (seq-take clicmd prefixlen))
                       (or all? (not (rmcs-cli-prop cli :hide))))
              (when predicate?
                (throw :predicate t))
              (let* ((car (if tree? (car (last clicmd)) clicmd))
                     (cdr (rmcs-cli-subcommands
                           clicmd (if depth (1- depth))
                           :tree? tree?
                           :all?  all?)))
                (if tree?
                    (push (if cdr (cons car cdr) car) results)
                  (cl-callf nconc results (cons car cdr)))))))
        (if tree?
            (nreverse results)
          results)))))

(defun rmcs-cli-aliases (cli)
  "Return all known `rmcs-cli's that are aliased to CLI.

This cannot see autoloaded CLIs. Use `rmcs-cli-load' or `rmcs-cli-load-all'
to reach them."
  (cl-loop with cli = (rmcs-cli-get cli)
           with key = (rmcs-cli-key cli)
           for rcli in (hash-table-values rmcs-cli--table)
           if (equal key (rmcs-cli-key rcli))
           collect cli))

(defun rmcs-cli-short-docs (cli)
  "Return the first line of CLI's documentation.

Return nil if CLI (a `rmcs-cli') has no explicit documentation."
  (ignore-errors (cdr (assoc "SUMMARY" (rmcs-cli-docs cli)))))

(defun rmcs-cli--bindings (cli context &optional seen)
  "Return a CLI with a value alist in a cons cell."
  (let* ((optspec (rmcs-cli-options cli))
         (argspec (rmcs-cli-arguments cli))
         alist)
    ;; Ensure all symbols are defined
    (dolist (opt optspec)
      (setf (alist-get (rmcs-cli-option-symbol opt) alist)
            (rmcs-cli-option-default opt)))
    (dolist (syms argspec)
      (dolist (sym (cdr syms))
        (setf (alist-get sym alist) nil)))
    ;; Populate options
    (let ((options (rmcs-cli-context-options context)))
      (dolist (opt optspec)
        (when-let (option (cl-loop for flag in (rmcs-cli-option-switches opt)
                                   if (cdr (assoc flag options))
                                   return (cons flag it)))
          (unless (member (car option) seen)
            (setf (alist-get (rmcs-cli-option-symbol opt) alist)
                  (cdr option))
            (push (car option) seen)))))
    ;; Populate arguments
    (let* ((arglist  (rmcs-cli-context-arguments context))
           (rest     (copy-sequence (map-elt arglist (rmcs-cli-command cli))))
           (args     (copy-sequence (alist-get t arglist)))
           (argc     (length args))
           (required (alist-get '&required argspec))
           (optional (alist-get '&optional argspec))
           (spec     (append required optional))
           (min      (length required))
           (max      (if (or (assq '&args argspec)
                             (assq '&rest argspec))
                         most-positive-fixnum
                       (length spec))))
      (when (or (< argc min)
                (> argc max))
        (signal 'rmcs-cli-wrong-number-of-arguments-error
                (list (rmcs-cli-key cli) nil args min max)))
      (dolist (sym spec)
        (setf (alist-get sym alist) (if args (pop args))))
      (dolist (type `((&args    . ,args)
                      (&cli     . ,cli)
                      (&context . ,context)
                      (&input
                       . ,(if (rmcs-cli-context-pipe-p context :in t)
                              (with-current-buffer (rmcs-cli-context-stdin context)
                                (buffer-string))))
                      (&rest    . ,rest)
                      (&whole   . ,(rmcs-cli-context-whole context))))
        (when-let (var (car (alist-get (car type) argspec)))
          (setf (alist-get var alist) (cdr type)))))
    alist))

(defun rmcs-cli--command (target &optional notype?)
  "Fetch the normalized command from TARGET.

If NOTYPE? is non-nil, omit any leading keywords from the command.

TARGET can be a `rmcs-cli', `rmcs-cli-context', or a command list."
  (cond ((rmcs-cli-p target)
         (if notype?
             (rmcs-cli-command target)
           (rmcs-cli-key target)))
        ((rmcs-cli-context-p target)
         (rmcs-cli-context-command target))
        ((and target (not (listp target)))
         (signal 'wrong-type-argument
                 (list '(rmcs-cli-p rmcs-cli-context-p listp) target)))
        ((let ((target (rmcs-cli-command-normalize target)))
           (if (and notype? (keywordp (car target)))
               (cdr target)
             target)))))

(defun rmcs-cli--command-expand (commandspec &optional recursive?)
  "Expand COMMANDSPEC into a list of commands.

If RECURSIVE, includes breadcrumbs leading up to COMMANDSPEC."
  (funcall (if recursive?
               #'identity
             (fn! (cl-loop with cmdlen = (length (car %))
                           for command in %
                           while (= (length command) cmdlen)
                           collect command)))
           (seq-reduce (lambda (init next)
                         (nconc (cl-loop with firstlen = (length (car init))
                                         for seg in (ensure-list next)
                                         nconc
                                         (cl-loop for command in init
                                                  while (= (length command) firstlen)
                                                  collect (append command (list seg))))
                                init))
                       (cdr commandspec)
                       `(,@(mapcar #'list (ensure-list (car commandspec)))))))

(defun rmcs-cli--parse-docs (docs)
  (when (and (stringp docs)
             (not (equal docs "TODO")))
    (let ((re "^\\([A-Z0-9 _-]+\\):\n") sections)
      (with-temp-buffer
        (save-excursion
          (insert "__RMCSDOCS__:\n")
          (insert docs))
        (while (re-search-forward re nil t)
          (push (cons (match-string 1)
                      (let ((buffer (current-buffer))
                            (beg (match-end 0))
                            (end (save-excursion
                                   (if (re-search-forward re nil t)
                                       (1- (match-beginning 0))
                                     (point-max)))))
                        (with-temp-buffer
                          (insert-buffer-substring buffer beg end)
                          (goto-char (point-min))
                          (indent-rigidly (point-min) (point-max) (- (skip-chars-forward " ")))
                          (string-trim-right (buffer-string)))))
                sections)))
      (let ((lines (split-string (cdr (assoc "__RMCSDOCS__" sections)) "\n"))
            (sections (assoc-delete-all "__RMCSDOCS__" sections)))
        `(("SUMMARY" . ,(car lines))
          ("MAIN"    . ,(string-trim (string-join (cdr lines) "\n")))
          ,@(nreverse sections))))))


;;
;;; `rmcs-cli-option'

(cl-defstruct rmcs-cli-option
  "A switch specification dictating the characteristics of a recognized option."
  (symbol nil :read-only t)
  docs
  multiple-p
  flag-p
  switches
  arguments
  default)

(defun rmcs-cli-option-validate (option &rest values)
  "Test if OPTION will accept VALUES, and conforms them if necessary.

OPTION is a `rmcs-cli-option' struct. VALUES can be any arbitrary values.
Returns VALUES once mapped through their respective reader (as dictated by
`rmcs-cli-option-arg-types').

Throws `rmcs-cli-invalid-option-error' for illegal values."
  (let ((args (rmcs-cli-option-arguments option))
        (values (copy-sequence values)))
    (dotimes (i (length args) values)
      (let ((value (nth i values))
            (types (ensure-list (nth i args)))
            errors)
        (catch 'done
          (dolist (type types)
            ;; REVIEW Use pcase-let + map.el when 27.x support is dropped
            (cl-destructuring-bind (&key test read error &allow-other-keys)
                (if (or (symbolp type)
                        (and (stringp type)
                             (string-match-p "^[A-Z0-9-_]+$" type)))
                    (cdr (assq (if (symbolp type) type (intern (downcase type)))
                               rmcs-cli-option-arg-types))
                  (list 'str :test #'stringp))
              (condition-case-unless-debug e
                  (or (and (or (null test)
                               (if (stringp test)
                                   (and (string-match-p test value) t)
                                 (funcall test value)))
                           (or (null read)
                               (setf (nth i values) (funcall read value)))
                           (throw 'done t))
                      (push error errors))
                ((invalid-regexp invalid-read-syntax)
                 (push (error-message-string e) errors)))))
          (signal 'rmcs-cli-invalid-option-error
                  (list types option value errors)))))))

(defun rmcs-cli--read-option-switches (optspec)
  (delq
   nil (cl-loop for spec in optspec
                if (and (stringp spec)
                        (string-match-p "^-\\(?:-[a-zA-Z0-9]\\|[^-]$\\)" spec))
                collect spec)))

(defun rmcs-cli--read-option-args (argspec)
  (delq
   nil (cl-loop for spec in argspec
                if (or (and (stringp spec)
                            (not (string-match-p "^-\\(?:-[a-zA-Z0-9]\\|[^-]$\\)" spec)))
                       (keywordp spec)
                       (symbolp spec)
                       (listp spec))
                collect spec)))

(defun rmcs-cli--make-option-generic (symbol spec &optional docs)
  (make-rmcs-cli-option
   :symbol symbol
   :docs docs
   :switches  (rmcs-cli--read-option-switches spec)
   :arguments (rmcs-cli--read-option-args spec)))

(defun rmcs-cli--make-option-flag (symbol spec &optional docs)
  (let ((switches (rmcs-cli--read-option-switches spec))
        (args     (rmcs-cli--read-option-args spec)))
    (when (and args
               (not (or (memq :yes args)
                        (memq :no args))))
      (signal 'rmcs-cli-definition-error
              (list "Argument type %s cannot accept arguments for: %s"
                    '&flag (mapconcat #'symbol-name spec ", "))))
    (make-rmcs-cli-option
     :symbol symbol
     :docs docs
     :flag-p t
     :switches switches
     :default (car args))))

(defun rmcs-cli--make-option-multi (symbol spec &optional docs)
  (make-rmcs-cli-option
   :symbol symbol
   :docs docs
   :multiple-p t
   :switches (rmcs-cli--read-option-switches spec)
   :arguments (rmcs-cli--read-option-args spec)))


;;
;;; `rmcs-cli-context'

(cl-defstruct rmcs-cli-context
  "A CLI context, containing all state pertinent to the current session."
  (init-time before-init-time) ; When this context was created
  ;; A session-specific ID of the current context (defaults to number
  (pid (if-let (pid (getenv "__RMCSPID"))
           (string-to-number pid)
         (emacs-pid)))
  ;; Number of Emacs processes this context has been processed through
  (step (if-let (step (getenv "__RMCSSTEP"))
            (string-to-number step)
          -1))
  ;; The geometry of the terminal window.
  (geometry (save-match-data
              (when-let* ((geom (getenv "__RMCSGEOM"))
                          ((string-match "^\\([0-9]+\\)x\\([0-9]+\\)$" geom)))
                (cons (string-to-number (match-string 1 geom))
                      (string-to-number (match-string 2 geom))))))
  ;; Whether the script is being piped into or out of
  (pipes (cl-loop for (env . scope) in `((,(getenv "__RMCSGPIPE") . global)
                                         (,(getenv "__RMCSPIPE")  . local))
                  if (stringp env)
                  for pipes = (string-to-list env)
                  nconc `(,@(if (memq ?0 pipes) `((:in  . ,scope)))
                          ,@(if (memq ?1 pipes) `((:out . ,scope)))))
         :skip t)
  ;; If non-nil, suppress prompts and auto-accept their consequences.
  suppress-prompts-p
  (prefix "@")  ; The basename of the script creating this context
  meta-p        ; Whether or not this is a help/meta request
  error         ;
  (command nil :skip t)    ; The full command that led to this context
  (path nil :skip t)       ; Breadcrumb list of resolved commands so far
  (whole nil :skip t)      ; Unfiltered and unprocessed list of arguments
  (options nil :skip t)    ; An alist of (flags . value)
  (arguments nil :skip t)  ; An alist of non-subcommand arguments, by command
  (stdin  (generate-new-buffer " *rmcs-cli stdin*")  :type buffer)  ; buffer containing anything piped into this session
  (stdout (generate-new-buffer " *rmcs-cli stdout*") :type buffer)  ; buffer containing user-visible output
  (stderr (generate-new-buffer " *rmcs-cli stderr*") :type buffer)  ; buffer containing all output, including debug output
  ;; An alist of persistent and arbitrary elisp state
  (state  nil :type alist))

(defun rmcs-cli-context-execute (context)
  "Execute a given CONTEXT.

Use `rmcs-cli-context-parse' or `rmcs-cli-context-restore' to produce a valid,
executable context."
  (let* ((command (rmcs-cli-context-command context))
         (cli (rmcs-cli-get command t))
         (prefix (rmcs-cli-context-prefix context)))
    (rmcs-log "context-execute: %s"
              (mapconcat #'rmcs-cli-command-string
                         (delq nil (list (car (rmcs-cli-context-path context)) command))
                         " -> "))
    (cond ((null (or command (rmcs-cli-get (list prefix) t)))
           (signal 'rmcs-cli-invalid-prefix-error (list prefix)))

          ((rmcs-cli-context-meta-p context)
           (pcase (rmcs-cli-context-meta-p context)
             ("--version"
              (rmcs-cli-call `(:version ,@(cdr command)) context)
              t)
             ((or "-?" "--help")
              (rmcs-cli-call `(:help ,@(cdr command)) context)
              t)
             (_ (error "In meta mode with no destination!"))))

          ((not (and cli (rmcs-cli-fn (rmcs-cli-get cli))))
           (signal 'rmcs-cli-command-not-found-error
                   (append command (alist-get t (rmcs-cli-context-arguments context)))))

          ((let ((seen '(t))
                 runners)
             (dolist (cli (rmcs-cli-find command (rmcs-cli-type cli)))
               (push (cons (rmcs-cli-get cli)
                           (rmcs-cli--bindings cli context seen))
                     runners))
             (pcase-dolist (`(,cli . ,bindings) (nreverse runners))
               (rmcs-cli-execute cli bindings))
             context)))))

(defun rmcs-cli-context-restore (file context)
  "Restore the last restarted context from FILE into CONTEXT."
  (when (and (stringp file)
             (file-exists-p file))
    (when-let (old-context (with-temp-buffer
                             (insert-file-contents file)
                             (read (current-buffer))))
      (unless (rmcs-cli-context-p old-context)
        (error "An invalid context was restored from file: %s" file))
      (unless (equal (rmcs-cli-context-prefix context)
                     (rmcs-cli-context-prefix old-context))
        (error "Restored context belongs to another script: %s"
               (rmcs-cli-context-prefix old-context)))
      (pcase-dolist (`(,slot ,_ . ,plist)
                     (cdr (cl-struct-slot-info 'rmcs-cli-context)))
        (unless (plist-get plist :skip)
          (let* ((idx (cl-struct-slot-offset 'rmcs-cli-context slot))
                 (old-value (aref old-context idx)))
            (aset context idx
                  (pcase (plist-get plist :type)
                    (`alist
                     (dolist (entry old-value (aref context idx))
                       (setf (alist-get (car entry) (aref context idx)) (cdr entry))))
                    (`buffer
                     (with-current-buffer (aref context idx)
                       (insert old-value)
                       (current-buffer)))
                    (_ old-value))))))
      (run-hook-with-args 'rmcs-cli-create-context-functions context)
      (delete-file file)
      (rmcs-log "context-restore: %s" (rmcs-cli-context-pid context))))
  context)

(defun rmcs-cli-context-parse (args context)
  "Parse ARGS and update CONTEXT to reflect it."
  (let* ((case-fold-search t)
         (args (delq nil (copy-sequence args)))
         (arguments)
         rest?
         arg)
    (while args
      (setq arg (pop args))
      (save-match-data
        (cond
         ((equal arg "--")
          (rmcs-log "context-parse: found arg separator" arg)
          (setq arguments (cdr args)
                args nil))

         ((and (stringp arg)
               (string-match "^\\(-\\([^-]\\{2,\\}\\)\\)" arg))
          (let ((chars (split-string (match-string 2 arg) "" t)))
            (dolist (ch (nreverse chars))
              (push (concat "-" ch) args))))

         ((and (stringp arg)
               (or (string-match "^\\(--\\w[a-z0-9-_]+\\)\\(?:=\\(.*\\)\\)?$" arg)
                   (string-match "^\\(-[^-]\\)$" arg)))
          (rmcs-log "context-parse: found switch %S" arg)
          (catch :skip
            (let* ((fullflag (match-string 1 arg))
                   (normflag (if (string-prefix-p "--no-" fullflag)
                                 (concat "--" (substring fullflag 5))
                               fullflag))
                   (option (or (rmcs-cli-context-find-option context normflag)
                               (when (member fullflag '("-?" "--help" "--version"))
                                 (rmcs-log "context-parse: found help switch %S" arg)
                                 (setf (rmcs-cli-context-meta-p context) fullflag)
                                 (throw :skip t))
                               (when rest?
                                 (push arg arguments)
                                 (throw :skip t))
                               (signal 'rmcs-cli-unrecognized-option-error
                                       (list fullflag))))
                   (explicit-arg (match-string 2 arg))
                   (arity (length (rmcs-cli-option-arguments option)))
                   (key (if (rmcs-cli-option-multiple-p option)
                            (car (rmcs-cli-option-switches option))
                          normflag)))
              (rmcs-cli-context-put
               context key
               (let ((value (seq-take args arity)))
                 (when explicit-arg
                   (push explicit-arg value))
                 (when (/= (length value) arity)
                   (signal 'rmcs-cli-wrong-number-of-arguments-error
                           (list (rmcs-cli--command context)
                                 fullflag value arity arity)))
                 (setq args  (seq-drop args arity)
                       value (apply #'rmcs-cli-option-validate option value))
                 (cond ((rmcs-cli-option-flag-p option)
                        (if (string-prefix-p "--no-" fullflag) :no :yes))
                       ((rmcs-cli-option-multiple-p option)
                        (append (rmcs-cli-context-get context key)
                                (if (rmcs-cli-option-arguments option)
                                    (cl-loop for v in value
                                             collect (cons fullflag v))
                                  (list fullflag))))
                       ((= arity 1) (car value))
                       ((> arity 1) value)
                       (fullflag)))))))

         ((when-let*
              (((null arguments))
               ((not rest?))
               (command (append (rmcs-cli-context-command context) (list arg)))
               (cli  (rmcs-cli-get command t))
               (rcli (rmcs-cli-get command))
               (key  (rmcs-cli-key rcli)))
            (rmcs-log "context-parse: found command %s" command)
            ;; Show warnings depending on CLI plists
            (when (rmcs-cli-alias cli)
              (dolist (pcli (rmcs-cli-path cli))
                (rmcs-log "context-parse: path += %s" (rmcs-cli-key pcli))
                (push (rmcs-cli-key pcli) (rmcs-cli-context-path context))))
            ;; Collect &rest for this command
            (setf (rmcs-cli-context-command context) key
                  (map-elt (rmcs-cli-context-arguments context)
                           (rmcs-cli-command rcli))
                  (copy-sequence args))
            ;; Initialize options associated with this command to a nil value;
            ;; this simplifies existence validation later.
            (dolist (cli (rmcs-cli-find key))
              (dolist (option (rmcs-cli-options cli))
                (dolist (switch (rmcs-cli-option-switches option))
                  (unless (assoc switch (rmcs-cli-context-options context))
                    (setf (map-elt (rmcs-cli-context-options context) switch)
                          nil)))))
            ;; If this command uses &rest, stop processing commands from this
            ;; point on and pass the rest (of the unprocessed arguments) to it.
            (when (and (rmcs-cli-fn rcli)
                       (alist-get '&rest (rmcs-cli-arguments rcli)))
              (setq rest? t))
            t))

         ((push arg arguments)
          (rmcs-log "context-parse: found arg %S" arg)))))

    (setf (alist-get t (rmcs-cli-context-arguments context))
          (append (alist-get t (rmcs-cli-context-arguments context))
                  (nreverse arguments)))
    (run-hook-with-args 'rmcs-cli-create-context-functions context)
    context))

(defun rmcs-cli-context-get (context key &optional null-value)
  "Fetch KEY from CONTEXT's options or state.

Context objects are essentially persistent storage, and may contain arbitrary
state tied to switches (\"--foo\" or \"-x\") or arbitrary symbols (state).

If KEY is a string, fetch KEY from context's OPTIONS (by switch).
If KEY is a symbol, fetch KEY from context's STATE.
Return NULL-VALUE if KEY does not exist."
  (if-let (value
           (if (stringp key)
               (assoc key (rmcs-cli-context-options context))
             (assq key (rmcs-cli-context-state context))))
      (cdr value)
    null-value))

(defun rmcs-cli-context-put (context key val)
  "Set KEY in CONTEXT's options or state to VAL.

Context objects contain persistent storage, and may contain arbitrary state tied
to switches (\"--foo\" or \"-x\") or arbitrary symbols (state). Use this to
register data into CONTEXT.

If KEY is a string, set the value of a switch named KEY to VAL.
If KEY is a symbol, set the value of the context's STATE to VAL."
  (setf (alist-get
         key (if (stringp key)
                 (rmcs-cli-context-options context)
               (rmcs-cli-context-state context))
         nil nil #'equal)
        val))

(defun rmcs-cli-context-find-option (context switch)
  "Return a `rmcs-cli-option' belonging to SWITCH in CONTEXT, if available.

Returns nil if SWITCH isn't a valid option in CONTEXT or none of the associated
`rmcs-cli's have a `rmcs-cli-option' associated with SWITCH."
  (when (assoc switch (rmcs-cli-context-options context))
    (cl-loop with command = (rmcs-cli-context-command context)
             for cli in (rmcs-cli-find command)
             if (seq-find (lambda (opt)
                            (let ((switches (rmcs-cli-option-switches opt)))
                              (or (member switch switches)
                                  (and (rmcs-cli-option-flag-p opt)
                                       (string-prefix-p "--no-" switch)))))
                          (rmcs-cli-options cli))
             return it)))

(defun rmcs-cli-context-width (context)
  "Return the width (in character units) of CONTEXT's original terminal."
  (or (car (rmcs-cli-context-geometry context))
      80))

(defun rmcs-cli-context-height (context)
  "Return the height (in character units) of CONTEXT's original terminal."
  (or (cdr (rmcs-cli-context-geometry context))
      40))

(defun rmcs-cli-context-pipe-p (context type &optional global?)
  "Return non-nil if TYPE is an active pipe in the local CONTEXT.

TYPE can be one of `:in' (receiving input on stdin) or `:out' (output is piped
to another process), or any of `local-in', `local-out', `global-in', or
`global-out'.

If GLOBAL? is non-nil, if TYPE is `:in' or `:out', the global context (the pipes
active in the super-session, rather than the local Emacs instance) will be
considered as well."
  (let ((pipes (rmcs-cli-context-pipes context)))
    (and (if global?
             (assq type pipes)
           (member (cons type 'local) pipes))
         t)))

(defun rmcs-cli-context-sid (context &optional nodate?)
  "Return a unique session identifier for CONTEXT."
  (if nodate?
      (rmcs-cli-context-pid context)
    (format (format-time-string
             "%y%m%d%H%M%S.%%s" (rmcs-cli-context-init-time context))
            (rmcs-cli-context-pid context))))


;;
;;; Output management

(defun rmcs-cli-debugger (type data &optional context)
  "Print a more presentable backtrace to terminal and write it to file."
  ;; HACK Works around a heuristic in eval.c for detecting errors in the
  ;;   debugger, which executes this handler again on subsequent calls. Taken
  ;;   from `ert--run-test-debugger'.
  (cl-incf num-nonmacro-input-events)
  (let* ((inhibit-read-only nil)
         (inhibit-message nil)
         (inhibit-redisplay nil)
         (inhibit-trace t)
         (executing-kbd-macro nil)
         (load-read-function #'read)
         (backtrace (rmcs-backtrace))
         (context (or context (make-rmcs-cli-context)))
         (straight-error
          (and (bound-and-true-p straight-process-buffer)
               (or (member straight-process-buffer data)
                   (string-match-p (regexp-quote straight-process-buffer)
                                   (error-message-string data)))
               (with-current-buffer (straight--process-buffer)
                 (split-string (buffer-string) "\n" t))))
         (error-file (rmcs-cli--output-file 'error context)))
    (cond
     (straight-error
      (print! (error "The package manager threw an error"))
      (print! (error "Last %d lines of straight's error log:")
              rmcs-cli-log-straight-error-lines)
      (print-group!
       (print!
        "%s" (string-join
              (seq-subseq straight-error
                          (max 0 (- (length straight-error)
                                    rmcs-cli-log-straight-error-lines))
                          (length straight-error))
              "\n")))
      (print! (warn "Wrote extended straight log to %s")
              (path (let ((coding-system-for-write 'utf-8-auto))
                      (with-file-modes #o600
                        (with-temp-file error-file
                          (insert-buffer-substring (straight--process-buffer))))
                      error-file))))
     ((eq type 'error)
      (let* ((generic? (eq (car data) 'error))
             (rmcs-cli-log-backtrace-depth rmcs-cli-log-backtrace-depth)
             (print-escape-newlines t))
        (if (rmcs-cli-context-p context)
            (print! (error "There was an unexpected runtime error"))
          (print! (bold (error "There was a fatal initialization error"))))
        (print-group!
         (print! "%s %s" (bold "Message:")
                 (if generic?
                     (error-message-string data)
                   (get (car data) 'error-message)))
         (unless generic?
           (print! "%s %s" (bold "Details:")
                   (let* ((print-level 4)
                          (print-circle t)
                          (print-escape-newlines t))
                     (prin1-to-string (cdr data)))))
         (when backtrace
           (print! (bold "Backtrace:"))
           (print-group!
            (dolist (frame (seq-take backtrace rmcs-cli-log-backtrace-depth))
              (print! "%s" (truncate (prin1-to-string
                                      (cons (backtrace-frame-fun  frame)
                                            (backtrace-frame-args frame)))
                                     (- (rmcs-cli-context-width context)
                                        rmcs-print-indent
                                        1)
                                     "..."))))
           (when-let (backtrace-file (rmcs-backtrace-write-to-file backtrace error-file))
             (print! (warn "Wrote extended backtrace to %s")
                     (path backtrace-file))))))))
    (exit! 255)))

(defmacro rmcs-cli-redirect-output (context &rest body)
  "Redirect output from BODY to the appropriate log buffers in CONTEXT."
  (declare (indent 1))
  (let ((contextsym (make-symbol "rmcsctxt")))
    `(let* ((,contextsym ,context)
            ;; Emit more user-friendly backtraces
            (debugger (rmcs-rpartial #'rmcs-cli-debugger ,contextsym))
            (debug-on-error t))
       (with-output-to! `((>= notice ,(rmcs-cli-context-stdout ,contextsym))
                          (t . ,(rmcs-cli-context-stderr ,contextsym)))
         ,@body))))

(defun rmcs-cli--output-file (type context)
  "Return a log file path for TYPE and CONTEXT.

See `rmcs-cli-log-file-format' for details."
  (format rmcs-cli-log-file-format
          (rmcs-cli-context-prefix context)
          (rmcs-cli-context-sid context)
          type))

(defun rmcs-cli--output-write-logs-h (context)
  "Write all log buffers to their appropriate files."
  (when (/= rmcs-cli--exit-code 254)
    ;; Delete the last `rmcs-cli-log-retain' logs
    (mapc #'delete-file
          (let ((prefix (rmcs-cli-context-prefix context)))
            (append (butlast (rmcs-glob (format rmcs-cli-log-file-format prefix "*" "log"))
                             rmcs-cli-log-retain)
                    (butlast (rmcs-glob (format rmcs-cli-log-file-format prefix "*" "error"))
                             rmcs-cli-log-retain))))
    ;; Then write the log file, if necessary
    (let* ((buffer (rmcs-cli-context-stderr context))
           (file (rmcs-cli--output-file "log" context)))
      (when (> (buffer-size buffer) 0)
        (with-file-modes #o700
          (make-directory (file-name-directory file) t))
        (with-file-modes #o600
          (with-temp-file file
            (insert-buffer-substring buffer)
            (ansi-color-filter-region (point-min) (point-max))))))))

(defun rmcs-cli--output-benchmark-h (context)
  "Write this session's benchmark to stdout or stderr, depending.

Will also output it to stdout if requested (CLI sets :benchmark to t) or the
command takes >5s to run. If :benchmark is explicitly set to nil (or
`rmcs-cli-log-benchmark-threshold' is nil), under no condition should a
benchmark be shown."
  (rmcs-cli-redirect-output context
    (rmcs-log "%s (GCs: %d, elapsed: %.6fs)"
              (if (= rmcs-cli--exit-code 254) "Restarted" "Finished")
              gcs-done gc-elapsed)
    (when-let* ((init-time (rmcs-cli-context-init-time context))
                (cli       (rmcs-cli-get context))
                (duration  (float-time (time-subtract (current-time) init-time)))
                (hours     (/ (truncate duration) 60 60))
                (minutes   (- (/ (truncate duration) 60) (* hours 60)))
                (seconds   (- duration (* hours 60 60) (* minutes 60))))
      (when (and (/= rmcs-cli--exit-code 254)
                 (or (eq (rmcs-cli-prop cli :benchmark) t)
                     (eq rmcs-cli-log-benchmark-threshold 'always)
                     (and (eq (rmcs-cli-prop cli :benchmark :null) :null)
                          (not (rmcs-cli-context-pipe-p context 'out t))
                          (> duration (or rmcs-cli-log-benchmark-threshold
                                          most-positive-fixnum)))))
        (print! (success "Finished in %s")
                (join (list (unless (zerop hours)   (format "%dh" hours))
                            (unless (zerop minutes) (format "%dm" minutes))
                            (format (if (> duration 60) "%ds" "%.5fs")
                                    seconds))))))))


;;
;;; Session management

(defun rmcs-cli-call (args context &optional error)
  "Process ARGS (list of string shell arguments) with CONTEXT as the basis.

If ERROR is provided, store the error in CONTEXT, in case a later CLI wants to
read/use it (e.g. like a :help CLI)."
  (let ((oldcommand (rmcs-cli-context-command context)))
    (if oldcommand
        (rmcs-log "call: %s -> %s" oldcommand args)
      (rmcs-log "call: %s" oldcommand args))
    (when error
      (setf (rmcs-cli-context-error context) error))
    (setf (rmcs-cli-context-command context) nil
          (rmcs-cli-context-arguments context) nil
          (rmcs-cli-context-meta-p context) nil)
    (rmcs-cli-context-execute
     (rmcs-cli-context-parse args (or context rmcs-cli--context)))))

(defun rmcs-cli--restart (args context)
  "Restart the current CLI session.

If CONTEXT is non-nil, this is written to file and restored in the next Rmcs
session.

This is done by writing a temporary shell script, which is executed after this
session ends (see the shebang lines of this file). It's done this way because
Emacs' batch library lacks an implementation of the exec system call."
  (cl-check-type context rmcs-cli-context)
  (when (= (rmcs-cli-context-step context) -1)
    (error "__RMCSSTEP envvar missing; extended `exit!' functionality will not work"))
  (let* ((pid  (rmcs-cli-context-pid context))
         (step (rmcs-cli-context-step context))
         (context-file (format (rmcs-path temporary-file-directory "rmcs.%s.%s.context") pid step))
         (script-file  (format (rmcs-path temporary-file-directory "rmcs.%s.%s.sh") pid step))
         (command (if (listp args) (combine-and-quote-strings (remq nil args)) args))
         (persistent-files
          (combine-and-quote-strings (delq nil (list script-file context-file))))
         (persisted-env
          (save-match-data
            (cl-loop with initial-env = (get 'process-environment 'initial-value)
                     for env in (seq-difference process-environment initial-env)
                     if (string-match "^\\([a-zA-Z0-9_][^=]+\\)=\\(.+\\)$" env)
                     collect (format "%s=%s"
                                     (match-string 1 env)
                                     (shell-quote-argument (match-string 2 env)))))))
    (cl-incf (rmcs-cli-context-step context))
    (with-file-modes #o600
      (rmcs-log "restart: writing context to %s" context-file)
      (rmcs-file-write
       context-file (let ((newcontext (copy-rmcs-cli-context context))
                          (print-level nil)
                          (print-length nil)
                          (print-circle nil)
                          (print-escape-newlines t))
                      ;; REVIEW: Use `print-unreadable-function' when 28 support
                      ;;   is dropped.
                      (letf! (defmacro convert-buffer (fn)
                               `(setf (,fn newcontext) (with-current-buffer (,fn context)
                                                         (buffer-string))))
                        (convert-buffer rmcs-cli-context-stdin)
                        (convert-buffer rmcs-cli-context-stdout)
                        (convert-buffer rmcs-cli-context-stderr))
                      newcontext))
      (rmcs-log "restart: writing post-script to %s" script-file)
      (rmcs-file-write
       script-file `("#!/usr/bin/env sh\n"
                     "trap _rmcscleanup EXIT\n"
                     "_rmcscleanup() {\n  rm -f " ,persistent-files "\n}\n"
                     "_rmcsrun() {\n  " ,command "\n}\n"
                     ,(string-join persisted-env " \\\n")
                     ,(cl-loop for (envvar . val)
                               in `(("RMCSPROFILE" . ,(ignore-errors (rmcs-profile->id rmcs-profile)))
                                    ("EMACSDIR" . ,rmcs-emacs-dir)
                                    ("RMCSDIR" . ,rmcs-user-dir)
                                    ("DEBUG" . ,(if init-file-debug "1"))
                                    ("__RMCSSTEP" . ,(number-to-string (rmcs-cli-context-step context)))
                                    ("__RMCSCONTEXT" . ,context-file))
                               if val
                               concat (format "%s=%s \\\n" envvar (shell-quote-argument val)))
                     ,(format "PATH=\"%s%s$PATH\" \\\n"
                              (rmcs-path rmcs-emacs-dir "bin")
                              path-separator)
                     "_rmcsrun \"$@\"\n")))
    (rmcs-log "_rmcsrun: %s %s" (string-join persisted-env " ") command)
    (rmcs-log "_rmcscleanup: %s" persistent-files)
    ;; Error code 254 is special: it indicates to the caller that the
    ;; post-script should be executed after this session ends. It's up to
    ;; `rmcs-cli-run's caller to enforce this (see bin/rmcs's shebang for a
    ;; comprehensive example).
    (rmcs-cli--exit 254 context)))

(defun rmcs-cli--exit (args context)
  "Accepts one of the following:

  (CONTEXT [ARGS...])
    TODO
  (STRING [ARGS...])
    TODO
  (:restart [ARGS...])
    TODO
  (:pager [FILE...])
    TODO
  (:pager? [FILE...])
    TODO
  (INT)
    TODO"
  (let ((command (or (car-safe args) args))
        (args    (if (car-safe args) (cdr-safe args))))
    (pcase command
      ;; If an integer, treat it as an exit code.
      ((pred (integerp))
       (setq rmcs-cli--exit-code command)
       (kill-emacs command))

      ;; Otherwise, run a command verbatim.
      ((pred (stringp))
       (rmcs-cli--restart (format "%s %s" command (combine-and-quote-strings args))
                          context))

      ;; Same with buffers.
      ((pred (bufferp))
       (rmcs-cli--restart (with-current-buffer command (buffer-string))
                          context))

      ;; If a context is given, restart the current session with the new context.
      ((pred (rmcs-cli-context-p))
       (rmcs-cli--exit-restart args command))

      ;; Run a custom action, defined in `rmcs-cli-exit-commands'.
      ((pred (keywordp))
       (if-let (fn (alist-get command rmcs-cli-exit-commands))
           (funcall fn args context)
         (error "Invalid exit command: %s" command)))

      ;; Any other value is invalid.
      (_ (error "Invalid exit code or command: %s" command)))))

(defun rmcs-cli--exit-restart (args context)
  "Restart the session, verbatim (persisting CONTEXT).

ARGS are addiitonal arguments to pass to the sub-process (in addition to the
ones passed to this one). It may contain :omit -- all arguments after this will
be removed from the argument list. They may specify number of arguments in the
format:

  --foo=4     omits --foo plus four following arguments
  --foo=1     omits --foo plus one following argument
  --foo=      equivalent to --foo=1
  --foo=*     omits --foo plus all following arguments

Arguments don't have to be switches either."
  (let ((pred (fn! (not (keywordp %))))
        (args (append (rmcs-cli-context-whole context)
                      (flatten-list args))))
    (let ((argv (seq-take-while pred args))
          (omit (mapcar (fn! (seq-let (arg n) (split-string % "=")
                               (cons
                                arg (cond ((not (stringp n)) 0)
                                          ((string-empty-p n) 1)
                                          ((equal n "*") -1)
                                          ((string-to-number n))))))
                        (seq-take-while pred (cdr (memq :omit args)))))
          newargs)
      (when omit
        (while argv
          (let ((arg (pop argv)))
            (if-let (n (cdr (assoc arg omit)))
                (if (= n -1)
                    (setq argv nil)
                  (dotimes (i n) (pop argv)))
              (push arg newargs)))))
      (rmcs-cli--exit (cons "$1" (or (nreverse newargs) argv))
                      context))))

(defun rmcs-cli--exit-pager (args context)
  "Invoke pager on output unconditionally.

ARGS are options passed to less. If RMCSPAGER is set, ARGS are ignored."
  (let ((pager (or rmcs-cli-pager (getenv "RMCSPAGER"))))
    (cond ((null (or pager (executable-find "less")))
           (user-error "No pager set or available")
           (rmcs-cli--exit 1 context))

          ((or (rmcs-cli-context-pipe-p context :out t)
               (equal pager ""))
           (rmcs-cli--exit 0 context))

          ((let ((tmpfile (rmcs-cli--output-file 'output context))
                 (coding-system-for-write 'utf-8-auto))
             (with-file-modes #o700
               (make-directory (file-name-directory tmpfile) t))
             (with-file-modes #o600
               (with-temp-file tmpfile
                 (insert-buffer-substring (rmcs-cli-context-stdout context))))
             (rmcs-cli--restart
              (format "%s <%s; rm -f%s %s"
                      (or pager
                          (format "less %s"
                                  (combine-and-quote-strings
                                   (append (if rmcs-print-backend '("-r")) ; process ANSI codes
                                           (or (delq nil args) '("+g"))))))
                      (shell-quote-argument tmpfile)
                      (if init-file-debug "v" "")
                      (shell-quote-argument tmpfile))
              context))))))

(defun rmcs-cli--exit-pager-maybe (args context)
  "Invoke pager if stdout is longer than TTY height * `rmcs-cli-pager-ratio'.

ARGS are options passed to less. If RMCSPAGER is set, ARGS are ignored."
  (rmcs-cli--exit
   (let ((threshold (ceiling (* (rmcs-cli-context-height context)
                                rmcs-cli-pager-ratio))))
     (if (>= (let ((stdout (rmcs-cli-context-stdout context)))
               (if (fboundp 'buffer-line-statistics)
                   (car (buffer-line-statistics stdout))
                 (with-current-buffer stdout
                   (count-lines (point-min) (point-max)))))
             threshold)
         (cons :pager args)
       0))
   context))

;; (defun rmcs-cli--exit-editor (args context))  ; TODO Launch $EDITOR

;; (defun rmcs-cli--exit-emacs (args context))   ; TODO Launch Emacs subsession



;;
;;; Migration paths

;; (defvar rmcs-cli-context-restore-functions
;;   '(rmcs-cli-context--restore-legacy-fn)
;;   "A list of functions intended to unserialize `rmcs-cli-context'.

;; They all take one argument, the raw data saved to $__RMCSCONTEXT. Each function
;; must return the version string corresponding to the version of Rmcs they have
;; transformed it for.")

;; (defun rmcs-cli-context-restore (file context)
;;   "Restore the last restarted context from FILE into CONTEXT."
;;   (when (and (stringp file)
;;              (file-exists-p file))
;;     (when-let* ((data (with-temp-buffer
;;                         (insert-file-contents file)
;;                         (read (current-buffer))))
;;                 (version (if (stringp (car data)) (car data) "0"))
;;                 (old-context (if (string (car data)) (cdr data) data))
;;                 (new-context (make-rmcs-cli-context))
;;                 (struct-info (cl-loop for (slot _initval . plist) in (cdr (cl-struct-slot-info 'rmcs-cli-context))
;;                                       collect (cons (cl-struct-slot-offset 'rmcs-cli-context slot)
;;                                                     (cons slot plist)))))

;;       ;; (let ((version (if (stringp (car data)) (car data) "0"))
;;       ;;       (data    (if (string (car data)) (cdr data) data))
;;       ;;       (newcontext (make-rmcs-cli-context)))
;;       ;;   (dolist (fn rmcs-cli-context-restore-functions)
;;       ;;     (setq newcontext (funcall fn newcontext data version))))

;;       (unless (rmcs-cli-context-p old-context)
;;         (error "An invalid context was restored from file: %s" file))
;;       (unless (equal (rmcs-cli-context-prefix context)
;;                      (rmcs-cli-context-prefix old-context))
;;         (error "Restored context belongs to another script: %s"
;;                (rmcs-cli-context-prefix old-context)))
;;       (pcase-dolist (`(,slot ,_ . ,plist)
;;                      (cdr (cl-struct-slot-info 'rmcs-cli-context)))
;;         (unless (plist-get plist :skip)
;;           (let* ((idx (cl-struct-slot-offset 'rmcs-cli-context slot))
;;                  (old-value (aref old-context idx)))
;;             (aset context idx
;;                   (pcase (plist-get plist :type)
;;                     (`alist
;;                      (dolist (entry old-value (aref context idx))
;;                        (setf (alist-get (car entry) (aref context idx)) (cdr entry))))
;;                     (`buffer
;;                      (with-current-buffer (aref context idx)
;;                        (insert old-value)
;;                        (current-buffer)))
;;                     (_ old-value))))))
;;       (run-hook-with-args 'rmcs-cli-create-context-functions context)
;;       (delete-file file)
;;       (rmcs-log "Restored context: %s" (rmcs-cli-context-pid context))
;;       context)))

;; (defun rmcs-cli-context--restore-legacy-fn (data old-version)
;;   "Update `rmcs-cli-context' from <3.0.0 to 3.0.0."
;;   (when (or (equal old-version "3.0.0-dev")
;;             (string-match-p "^2\\.0\\." old-version))

;;     "3.0.0"))

;; (defun rmcs-cli-context--restore-3.1.0-fn (data old-version))


;;
;;; Misc

(defun rmcs-cli-load (cli)
  "If CLI is autoloaded, load it, otherwise return it unchanged."
  (or (when-let* ((path (rmcs-cli-autoload cli))
                  (path (locate-file-internal path rmcs-cli-load-path load-suffixes)))
        (rmcs-log "load: autoload %s" path)
        (let ((rmcs-cli--group-plist (rmcs-cli-plist cli)))
          (rmcs-load path))
        (let* ((key (rmcs-cli-key cli))
               (cli (gethash key rmcs-cli--table)))
          (when (rmcs-cli-autoload cli)
            (signal 'rmcs-cli-autoload-error (list (rmcs-cli-command cli) path)))
          cli))
      cli))

(defun rmcs-cli-load-all ()
  "Immediately load all autoloaded CLIs."
  (dolist (key (hash-table-keys rmcs-cli--table))
    (rmcs-cli-load (gethash key rmcs-cli--table))))


;;
;;; DSL

(defmacro defcli! (commandspec arglist &rest body)
  "Defines a CLI command.

COMMANDSPEC is the specification for the command that will trigger this CLI. It
can either be a symbol or list of symbols (or nested symbols). Nested lists are
treated as a list of aliases for the command. For example:

  (defcli! rmcs () ...)              ; invoked on 'rmcs'
  (defcli! (rmcs foo) () ...)        ; invoked on 'rmcs foo'
  (defcli! (rmcs (foo bar)) () ...)  ; invoked on 'rmcs foo' or 'rmcs bar'

COMMANDSPEC may be prefixed with any of these special keywords:

  :root ...
    This command will ignore any :prefix set by a parent `defcli-group!'.
  :before ...
    This command will run before the specified command(s).
  :after ...
    This command will run after the specified command(s).
  :version
    A special handler, executed when 'X --version' is called. Define your own,
    if you don't want it spewing Rmcs's version information.
  :help COMMAND...
    A special handler, executed when help documentation is requested for a
    command. E.g. 'rmcs help foo' or 'rmcs foo --help' will call (:help foo).
    You can define your own global :help handler, or one for a specific command.
  :dump COMMAND...
    A special handler, executed when the __RMCSDUMP environment variable is set.
    You can define one for a specific COMMAND, or omit it to redefine the
    catch-all :dump handler.

    The default implementation (living in lisp/rmcs-cli.el) will either:

    a) Dump to stdout a list of `rmcs-cli' structs for the commands and pseudo
       commands that would've been executed had __RMCSDUMP not been set.
    b) Or, given only \"-\" as an argument, dump all of `rmcs-cli--table' to
       stdout. This table contains all known `rmcs-cli's (after loading
       autoloaded ones).

To interpolate values into COMMANDSPEC (e.g. to dynamically generate commands),
use the comma operator:

  (let ((somevar 'bfg))
    (defcli! (rmcs ,somevar) ...))

DOCSTRING is a string description; its first line should be a short summary
(under 60 characters) of what the command does. It will be used in the cramped
command listings served by help commands. The rest of DOCSTRING lines should be
no longer than 80 columns, and should go into greater detail. This documentation
may use `quoting' to appropriately highlight ARGUMENTS, --options, or $ENVVARS.

DOCSTRING may also contain sections denoted by a capitalized header ending with
a colon and newline, and its contents indented by 2 spaces. These will be
appended to the end of the help documentation for that command. These three
sections are special:

  ARGUMENTS:
    Use this to specify longer-form documentation for arguments. They are
    prepended to the documentation for commands. If pseudo CLIs specify their
    own ARGUMENTS sections, they are joined with that of the root command's CLI
    as well. E.g. ':before rmcs sync's ARGUMENTS will be prepended to 'rmcs
    sync's.
  OPTIONS:
    Use this to specify longer-form documentation for options. They are appended
    to the auto-generated section of the same name. Only the option needs to be
    specified for its lookup behavior to work. See bin/rmcs's `rmcs' command as
    an example.
  EXAMPLES:
    To list example uses of the containing script. These are appended to
    SYNOPSIS in generated manpages, but treated as a normal section otherwise
    (i.e. appended to 'rmcs help's output).

DOCSTRING may use any of these format specifications:

  %p  The running script's prefix. E.g. for 'rmcs ci deploy-hooks' the
      prefix is 'rmcs'.
  %c  The parent command minus the prefix. E.g. for 'rmcs ci deploy-hooks',
      the command is 'ci deploy-hooks'.

ARGLIST is a specification for options and arguments that is accepted by this
command. Arguments are represented by either a symbol or a cons cell where
(SYMBOL . DOCUMENTATION), and option specifications are lists in the following
formats:

  ([TYPE] VAR (FLAGSPEC... [ARGSPEC...]) [DESCRIPTION])

  TYPE
    Optional. One of &flag or &multi (which correspond to &flags and &multiple,
    respectively, and are used for specifying a type inline, if desired).
  VAR
    Is the symbol to bind that option's value to.
  FLAGSPEC
    A list of switches or sub-lists thereof. Each switch is a string, e.g.
    \"--foo\" \"-b\" \"--baz\".

    Nested lists will be treated as logical groups of switches in documentation.
    E.g. for

    With (\"--foo\" \"--bar\" [ARGSPEC...]) you get:

      --foo, --bar
        [Documentation]

    With ((\"--foo\") (\"--bar\") [ARGSPEC...]) you get:

      --foo
      --bar
        [Documentation]

    Use this to logically group options that have many, but semantically
    distinct switches.
  ARGSPEC
    A list of arguments or sub-lists thereof. Each argument is either a string
    or symbol.

    If a string, they are used verbatim as the argument's documentation. Use
    this to document more complex specifications, like \"[user@]host[:port]\".
    Use reference `quotes' to highlight arguments appropriately. No input
    validation is performed on these arguments.

    If a symbol, this is equivalent to (upcase (format \"`%s'\" SYMBOL)), but
    its arguments will also be implicitly validated against
    `rmcs-cli-option-arg-types'.

    A nested list indicates that an argument accepts multiple types, and are
    implicitly joined into \"`ARG1'|`ARG2'|...\". Input validation is performed
    on symbols only.

    WARNING: If this option is a &flag, the option must not accept arguments.
    Instead, use ARGSPEC to specify a single, default value (one of `:yes' or
    `:no').
  DESCRIPTION
    A one-line description of the option. Use reference `quotes' to
    appropriately highlight arguments, options, and envvars. A syntax exists for
    adding long-form option documentation from the CLI's docstring. See
    DOCSTRING above.

ARGLIST may be segmented with the following auxiliary keywords:

  &args ARG
    The rest of the literal arguments are stored in ARG.
  &cli ARG
    The called `rmcs-cli' struct is bound to ARG.
  &context ARG
    The active `rmcs-cli-context' struct is bound to ARG.
  &flags OPTION...
    An option '--foo' declared after &flags will implicitly include a
    '--no-foo', and will appear as \"--[no-]foo\" in 'rmcs help' docs.
  &multiple OPTION...
    Options specified after &multiple may be passed to the command multiple
    times. Its symbol will be bound to a list of cons cells containing (FLAG .
    VALUE).
  &optional ARG...
    Indicates that the (literal) arguments after it are optional.
  &input ARG
    ARG will be bound to the input piped in from stdin, as a string, or nil if
    unavailable. If you want access to the original buffer, use
    (rmcs-cli-context-stdin context) instead.
  &rest ARG
    All switches and arguments, unprocessed, after this command. If given, any
    unrecognized switches will not throw an error. This will also prevent
    subcommands beneath this command from being recognized. Use with care!

  Any non-option arguments before &optional, &rest, or &args are required.

BODY is a list of arbitrary elisp forms that will be executed when this command
is called. BODY may begin with a plist to set metadata about it. The recognized
properties:

  :alias (CMD...)
    Designates this command is an alias to CMD, which is a command specification
    identical to COMMANDSPEC.
  :benchmark BOOL
    If non-nil, display a benchmark after the command finishes.
  :disable BOOL
    If non-nil, the command will not be defined.
  :docs STRING
    An alternative to DOCSTRING for defining documentation for this command.
  :group (STR...)
    A breadcrumb of group names to file this command under. They will be
    organized by category in the CLI documentation (available through SCRIPT
    {--help,-?,help}).
  :hide BOOL
    If non-nil, don't display this command in the help menu or in {ba,z}sh
    completion (though it will still be callable).
  :partial BOOL
    If non-nil, this command is treated as partial, an intermediary command
    intended as a stepping stone toward a non-partial command. E.g. were you to
    define (rmcs foo bar), two \"partial\" commands are implicitly created:
    \"rmcs\" and \"rmcs foo\". When called directly, partials will list its
    subcommands and complain that a subcommand is rqeuired, rather than display
    an 'unknown command' error.
  :prefix (STR...)
    A command path to prepend to the command name. This is more useful as part
    of `defcli-group!'s inheritance.

The BODY of commands with a non-nil :alias, :disable, or :partial will be
ignored.

\(fn COMMANDSPEC ARGLIST [DOCSTRING] &rest BODY...)"
  (declare (indent 2) (doc-string 3))
  (let ((docstring (if (stringp (car body)) (pop body)))
        (plist (cl-loop for (key val) on body by #'cddr
                        while (keywordp key)
                        collect (pop body)
                        collect (pop body)))
        options arguments bindings)
    (let ((type '&required))
      (dolist (arg arglist)
        (cond ((listp arg)
               (let* ((inline-type (cdr (assq (car arg) rmcs-cli-option-types)))
                      (type (or inline-type type))
                      (args (if inline-type (cdr arg) arg)))
                 (push (apply (or (alist-get type rmcs-cli-option-generators)
                                  (signal 'rmcs-cli-definition-error
                                          (cons "Invalid option type" type)))
                              args)
                       options)
                 (push (car args) bindings)))
              ((memq arg rmcs-cli-argument-types)
               (setq type arg))
              ((string-prefix-p "&" (symbol-name arg))
               (signal 'rmcs-cli-definition-error (cons "Invalid argument specifier" arg)))
              ((push arg bindings)
               (push arg (alist-get type arguments))))))
    (dolist (arg arguments)
      (setcdr arg (nreverse (cdr arg))))
    `(let (;; Define function early to prevent overcapturing
           (fn ,(let ((clisym   (make-symbol "cli"))
                      (alistsym (make-symbol "alist")))
                  `(lambda (,clisym ,alistsym)
                     (let ,(cl-loop for arg in (nreverse bindings)
                                    unless (string-prefix-p "_" (symbol-name arg))
                                    collect `(,arg (cdr (assq ',arg ,alistsym))))
                       ,@body)))))
       ;; `cl-destructuring-bind's will validate keywords, so I don't have to
       (cl-destructuring-bind
           (&whole plist &key
                   alias autoload _benchmark docs disable hide _group partial
                   _prefix)
           (append (list ,@plist) rmcs-cli--group-plist)
         (unless disable
           (let* ((command  (rmcs-cli-command-normalize (backquote ,commandspec) plist))
                  (type     (if (keywordp (car command)) (pop command)))
                  (commands (rmcs-cli--command-expand command t))
                  (target   (pop commands)))
             (dolist (prop '(:autoload :alias :partial :hide))
               (cl-remf plist prop))
             (puthash (delq nil (cons type target))
                      (make-rmcs-cli
                       :command target
                       :type type
                       :docs (rmcs-cli--parse-docs (or ',docstring docs))
                       :arguments ',arguments
                       :options ',(nreverse options)
                       :autoload autoload
                       :alias (if alias (rmcs-cli-command-normalize alias plist))
                       :plist (append plist (list :hide (and (or hide type) t)))
                       :fn (unless (or partial autoload) fn))
                      rmcs-cli--table)
             (let ((docs (rmcs-cli--parse-docs docs)))
               (dolist (alias (cl-loop for c in commands
                                       while (= (length c) (length target))
                                       collect (pop commands)))
                 (puthash (delq nil (cons type alias))
                          (make-rmcs-cli
                           :command alias
                           :type type
                           :docs docs
                           :autoload autoload
                           :alias (unless autoload (delq nil (cons type target)))
                           :plist (append plist '(:hide t)))
                          rmcs-cli--table))
               (dolist (partial commands)
                 (let ((cli (gethash partial rmcs-cli--table)))
                   (when (or (null cli) (rmcs-cli-autoload cli))
                     (puthash (delq nil (cons type partial))
                              (make-rmcs-cli
                               :command partial
                               :type type
                               :docs docs
                               :plist (list :group (plist-get plist :group)))
                              rmcs-cli--table)))))
             target))))))

(defmacro defcli-alias! (commandspec target &rest plist)
  "Define a CLI alias for TARGET at COMMANDSPEC.

See `defcli!' for information about COMMANDSPEC.
TARGET is not a command specification, and should be a command list."
  `(defcli! ,commandspec () :alias ',target ,@plist))

(defmacro defcli-obsolete! (commandspec target when)
  "Define an obsolete CLI COMMANDSPEC that refers users to NEW-COMMAND.

See `defcli!' for information about COMMANDSPEC.
TARGET is simply a command list.
WHEN specifies what version this command was rendered obsolete."
  `(let ((ncommand (rmcs-cli-command-normalize (backquote ,target) rmcs-cli--group-plist)))
     (defcli! ,commandspec (&context context &cli cli &rest args)
       :docs (format "An obsolete alias for '%s'." (rmcs-cli-command-string ncommand))
       :hide t
       (print! (warn "'%s' was deprecated in %s")
               (rmcs-cli-command-string cli)
               ,when)
       (print! (warn "It will eventually be removed; use '%s' instead.")
               (rmcs-cli-command-string ncommand))
       (call! ',target args))))

(defmacro defcli-stub! (commandspec &optional _argspec &rest body)
  "Define a stub CLI, which will throw an error if invoked.

Use this to define commands that will eventually be implemented, but haven't
yet. They won't be included in command listings (by help documentation)."
  (declare (indent 2) (doc-string 3))
  `(defcli! ,commandspec (&rest _)
     ,(concat "THIS COMMAND IS A STUB AND HAS NOT BEEN IMPLEMENTED YET."
              (if (stringp (car body)) (concat "\n\n" (pop body))))
     :hide t
     (user-error "Command not implemented yet")))

(defmacro defcli-autoload! (commandspec &optional path &rest plist)
  "Defer loading of PATHS until PREFIX is called."
  `(let* ((rmcs-cli--group-plist (append (list ,@plist) rmcs-cli--group-plist))
          (commandspec (rmcs-cli-command-normalize ',commandspec))
          (commands (rmcs-cli--command-expand commandspec))
          (path (or ,path
                    (when-let* ((cmd  (car commands))
                                (last (car (last cmd)))
                                (last (if (listp last) (car last) last)))
                      (format "%s" last))
                    (error "Failed to deduce autoload path for: %s" spec)))
          (cli (rmcs-cli-get (car commands) nil t)))
     (when (or (null cli)
               (rmcs-cli-autoload cli))
       (defcli! ,commandspec () :autoload path))))

(defmacro defcli-group! (&rest body)
  "Declare common properties for any CLI commands defined in BODY."
  (when (stringp (car body))
    (push :group body))
  `(let ((rmcs-cli--group-plist (copy-sequence rmcs-cli--group-plist)))
     ,@(let (forms)
         (while (keywordp (car body))
           (let ((key (pop body))
                 (val (pop body)))
             (push `(cl-callf plist-put rmcs-cli--group-plist
                      ,key ,(if (eq key :prefix)
                                `(append (plist-get rmcs-cli--group-plist ,key)
                                         (ensure-list ,val))
                              val))
                   forms)))
         (nreverse forms))
     ,@body))

(defun exit! (&rest args)
  "Exits the current CLI session.

With ARGS, you may specify a shell command or action (see
`rmcs-cli-exit-commands') to execute after this Emacs process has ended. For
example:

  (exit! \"$@\") or (exit! :restart)
    This reruns the current command with the same arguments.
  (exit! \"$@ -h -c\")
    This reruns the current command with two new switches.
  (exit! :restart \"-c\" :omit \"--foo=2\" \"--bar\")
    This reruns the current command with one new switch (-c) and two switches
    removed (--foo plus two arguments and --bar).
  (exit! \"emacs -nw FILE\")
    Opens Emacs on FILE
  (exit! \"emacs\" \"-nw\" \"FILE\")
    Opens Emacs on FILE, but each argument is escaped (and nils are ignored).
  (exit! t) or (exit! nil)
    A safe way to simply abort back to the shell with exit code 0
  (exit! 42)
    Abort to shell with an explicit exit code.
  (exit! context)
    Restarts the current session, but with context (a `rmcs-cli-context' struct).
  (exit! :pager [FILES...])
    Invoke $RMCSPAGER (or less) on the output of this session. If ARGS are given, launch the pager on those
  (exit! :pager? [FILES...])
    Same as :pager, but does so only if output is longer than the terminal is
    tall.

See `rmcs-cli--restart' for implementation details."
  (rmcs-cli--exit (flatten-list args) rmcs-cli--context))

(defun call! (&rest command)
  "A convenience wrapper around `rmcs-cli-call'.

Implicitly resolves COMMAND relative to the running CLI, and uses the active
context (so you don't have to pass a context)."
  (rmcs-cli-call (rmcs-cli-command-normalize
                  (flatten-list command)
                  `(:prefix
                    ,(rmcs-cli-context-prefix rmcs-cli--context)
                    ,@(rmcs-cli-context-command rmcs-cli--context)))
                 rmcs-cli--context))

(defun run! (prefix &rest args)
  "Parse and execute ARGS.

This is the entry point for any shell script that rely on Rmcs's CLI framework.
It should be called once, at top-level, and never again (use `rmcs-cli-call' for
nested calls instead).

PREFIX is the name (string) of the top-level shell script (i.e. $0). All
commands that belong to this shell session should use PREFIX as the first
segment in their command paths.

ARGS is a list of string arguments to execute.

See bin/rmcs's shebang for an example of what state needs to be initialized for
Rmcs's CLI framework. In a nutshell, Rmcs is expecting the following environment
variables to be set:

  __RMCSGEOM   The dimensions of the current terminal (W . H)
  __RMCSPIPE   Must contain 0 if script is being piped into, 1 if piping it out
  __RMCSGPIPE  Like __RMCSPIPE, but is the pipe state of the super process
  __RMCSPID    A unique ID for this session and its exit script processes
  __RMCSSTEP   How many layers deep this session has gotten

The script should also execute ${temporary-file-directory}/rmcs.sh if Emacs
exits with code 254. This script is auto-generated as needed, to simulate exec
syscalls. See `rmcs-cli--restart' for technical details.

Once done, this function kills Emacs gracefully and writes output to log files
(stdout to `rmcs-cli--output-file', stderr to `rmcs-cli-debug-file', and any
errors to `rmcs-cli-error-file')."
  (when rmcs-cli--context
    (error "Cannot nest `run!' calls"))
  (rmcs-run-hooks 'rmcs-after-init-hook)
  (rmcs-context-with 'cli
    (let* ((args (flatten-list args))
           (context (make-rmcs-cli-context :prefix prefix :whole args))
           (rmcs-cli--context context)
           (write-logs-fn (rmcs-partial #'rmcs-cli--output-write-logs-h context))
           (show-benchmark-fn (rmcs-partial #'rmcs-cli--output-benchmark-h context)))
      ;; Clone output to stdout/stderr buffers for logging.
      (rmcs-cli-redirect-output context
        (rmcs-log "run!: %s %s" prefix (combine-and-quote-strings args))
        (add-hook 'kill-emacs-hook show-benchmark-fn 94)
        (add-hook 'kill-emacs-hook write-logs-fn 95)
        (when (rmcs-cli-context-pipe-p context :out t)
          (setq rmcs-print-backend nil))
        (when (rmcs-cli-context-pipe-p context :in)
          (with-current-buffer (rmcs-cli-context-stdin context)
            (while (if-let (in (ignore-errors (read-from-minibuffer "")))
                       (insert in "\n")
                     (ignore-errors (delete-char -1))))))
        (rmcs-cli--exit
         (condition-case e
             (let* ((args (cons (if (getenv "__RMCSDUMP") :dump prefix) args))
                    (context (rmcs-cli-context-restore (getenv "__RMCSCONTEXT") context))
                    (context (rmcs-cli-context-parse args context)))
               (run-hook-with-args 'rmcs-cli-before-run-functions context)
               (let ((result (rmcs-cli-context-execute context)))
                 (run-hook-with-args 'rmcs-cli-after-run-functions context result))
               0)
           (rmcs-cli-wrong-number-of-arguments-error
            (pcase-let ((`(,command ,flag ,args ,min ,max) (cdr e)))
              (print! (red "Error: %S expected %s argument%s, but got %d")
                      (or flag (rmcs-cli-command-string
                                (if (keywordp (car command))
                                    command
                                  (cdr command))))
                      (if (or (= min max)
                              (= max most-positive-fixnum))
                          min
                        (format "%d-%d" min max))
                      (if (or (= min 0) (> min 1)) "s" "")
                      (length args))
              (rmcs-cli-call `(:help "--synopsis" "--postamble" ,@(cdr (rmcs-cli--command context))) context e))
            5)
           (rmcs-cli-unrecognized-option-error
            (print! (red "Error: unknown option %s") (cadr e))
            (rmcs-cli-call `(:help "--synopsis" "--postamble" ,@(cdr (rmcs-cli--command context))) context e)
            5)
           (rmcs-cli-invalid-option-error
            (pcase-let ((`(,types ,option ,value ,errors) (cdr e)))
              (print! (red "Error: %s received invalid value %S")
                      (string-join (rmcs-cli-option-switches option) "/")
                      value)
              (print! (bold "\nValidation errors:"))
              (dolist (err errors) (print! (item "%s." (fill err)))))
            (rmcs-cli-call `(:help "--postamble" ,@(cdr (rmcs-cli--command context))) context e)
            5)
           (rmcs-cli-command-not-found-error
            (let* ((command (cdr e))
                   (cli (rmcs-cli-get command)))
              (cond ((null cli)
                     (print! (red "Error: unrecognized command '%s'")
                             (rmcs-cli-command-string (or (cdr command) command)))
                     (rmcs-cli-call `(:help "--similar" "--postamble" ,@(cdr command)) context e))
                    ((null (rmcs-cli-fn cli))
                     (print! (red "Error: a subcommand is required"))
                     (rmcs-cli-call `(:help "--subcommands" "--postamble" ,@(cdr command)) context e))))
            4)
           (rmcs-cli-invalid-prefix-error
            (let ((prefix (cadr e)))
              (print! (red "Error: `run!' called with invalid prefix %S") prefix)
              (if-let (suggested (cl-loop for cli being the hash-value of rmcs-cli--table
                                          unless (rmcs-cli-type cli)
                                          return (car (rmcs-cli-command cli))))
                  (print! "Did you mean %S?" suggested)
                (print! "There are no commands defined under %S." prefix)))
            4)
           (user-error
            (print! (red "Error: %s") (cadr e))
            (print! "\nAborting...")
            3))
         context)))))

(defalias 'sh! #'rmcs-call-process)

(defalias 'sh!! #'rmcs-exec-process)

;; TODO Make `git!' into a more sophisticated wrapper around git
(defalias 'git! (rmcs-partial #'straight--process-run "git"))



;;
;;; Predefined CLIs

(load! "cli/meta")  ; :help, :version, and :dump

(provide 'rmcs-cli)
;;; rmcs-cli.el ends here
