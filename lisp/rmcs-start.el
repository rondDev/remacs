;;; lisp/rmcs-start.el --- bootstraps interactive sessions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Custom hooks

(defcustom rmcs-first-input-hook ()
  "Transient hooks run before the first user input."
  :type 'hook
  :local 'permanent-local
  :group 'rmcs)

(defcustom rmcs-first-file-hook ()
  "Transient hooks run before the first interactively opened file."
  :type 'hook
  :local 'permanent-local
  :group 'rmcs)

(defcustom rmcs-first-buffer-hook ()
  "Transient hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permanent-local
  :group 'rmcs)


;;
;;; Reasonable defaults for interactive sessions

;;; Runtime optimizations
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(eval-when! (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(eval-when! (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
(add-hook 'rmcs-first-buffer-hook #'gcmh-mode)


;;; Disable UI elements early
;; PERF,UI: Rmcs strives to be keyboard-centric, so I consider these UI elements
;;   clutter. Initializing them also costs a morsel of startup time. Whats more,
;;   the menu bar exposes functionality that Rmcs doesn't endorse. Perhaps one
;;   day Rmcs will support these, but today is not that day.
;;
;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because they do extra work to manipulate frame variables
;;   that isn't necessary this early in the startup process.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
;; FIX: On MacOS, disabling the menu bar makes MacOS treat Emacs as a
;;   non-application window -- which means it doesn't automatically capture
;;   focus when it is started, among other things, so enable the menu-bar for
;;   GUI frames, but keep it disabled in terminal frames because there it
;;   activates an ugly, in-frame menu bar.
(eval-when! rmcs--system-macos-p
  (add-hook! '(window-setup-hook after-make-frame-functions)
    (defun rmcs-restore-menu-bar-in-gui-frames-h (&optional frame)
      (when-let (frame (or frame (selected-frame)))
        (when (display-graphic-p frame)
          (set-frame-parameter frame 'menu-bar-lines 1))))))


;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows could be in a wider encoding (UTF-16), so
;; leave Emacs to its own devices.
(eval-when! (not rmcs--system-windows-p)
  (setq selection-coding-system 'utf-8))


;;; Support for more file extensions
;; Add support for additional file extensions.
(dolist (entry '(("/\\.rmcs\\(?:rc\\|project\\|module\\|profile\\)\\'" . emacs-lisp-mode)
                 ("/LICENSE\\'" . text-mode)
                 ("\\.log\\'" . text-mode)
                 ("rc\\'" . conf-mode)
                 ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))
  (push entry auto-mode-alist))


;;
;;; MODE-local-vars-hook

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defvar rmcs-inhibit-local-var-hooks nil)

(defun rmcs-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (unless (or rmcs-inhibit-local-var-hooks delay-mode-hooks)
    (setq-local rmcs-inhibit-local-var-hooks t)
    (rmcs-run-hooks (intern-soft (format "%s-local-vars-hook" major-mode)))))

;; If the user has disabled `enable-local-variables', then
;; `hack-local-variables-hook' is never triggered, so we trigger it at the end
;; of `after-change-major-mode-hook':
(defun rmcs-run-local-var-hooks-maybe-h ()
  "Run `rmcs-run-local-var-hooks-h' if `enable-local-variables' is disabled."
  (unless enable-local-variables
    (rmcs-run-local-var-hooks-h)))


;;
;;; Incremental lazy-loading

(defvar rmcs-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:

  (rmcs-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the lang/org module, however.

If you want to disable incremental loading altogether, either remove
`rmcs-load-packages-incrementally-h' from `emacs-startup-hook' or set
`rmcs-incremental-first-idle-timer' to nil. Incremental loading does not occur
in daemon sessions (they are loaded immediately at startup).")

(defvar rmcs-incremental-first-idle-timer (if (daemonp) 0 2.0)
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading.
Set this to 0 to load all incrementally deferred packages immediately at
`emacs-startup-hook'.")

(defvar rmcs-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defun rmcs-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in `rmcs-incremental-idle-timer'
intervals."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (not now)
        (cl-callf append rmcs-incremental-packages packages)
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
              (rmcs-log "start:iloader: Already loaded %s (%d left)" req (length packages))
            (condition-case-unless-debug e
                (and
                 (or (null (setq idle-time (current-idle-time)))
                     (< (float-time idle-time) rmcs-incremental-first-idle-timer)
                     (not
                      (while-no-input
                        (rmcs-log "start:iloader: Loading %s (%d left)" req (length packages))
                        ;; If `default-directory' doesn't exist or is
                        ;; unreadable, Emacs throws file errors.
                        (let ((default-directory rmcs-emacs-dir)
                              (inhibit-message t)
                              (file-name-handler-alist
                               (list (rassq 'jka-compr-handler file-name-handler-alist))))
                          (require req nil t)
                          t))))
                 (push req packages))
              (error
               (message "Error: failed to incrementally load %S because: %s" req e)
               (setq packages nil)))
            (if (null packages)
                (rmcs-log "start:iloader: Finished!")
              (run-at-time (if idle-time
                               rmcs-incremental-idle-timer
                             rmcs-incremental-first-idle-timer)
                           nil #'rmcs-load-packages-incrementally
                           packages t)
              (setq packages nil))))))))

(defun rmcs-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `rmcs-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (when (numberp rmcs-incremental-first-idle-timer)
    (if (zerop rmcs-incremental-first-idle-timer)
        (mapc #'require (cdr rmcs-incremental-packages))
      (run-with-idle-timer rmcs-incremental-first-idle-timer
                           nil #'rmcs-load-packages-incrementally
                           (cdr rmcs-incremental-packages) t))))


;;
;;; Benchmark

(defun rmcs-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Rmcs loaded %d packages across %d modules in %.03fs"
           (- (length load-path) (length (get 'load-path 'initial-value)))
           (hash-table-count rmcs-modules)
           rmcs-init-time))


;;
;;; Load Rmcs's defaults and DSL

;;; Load core modules and set up their autoloads
(require 'rmcs-modules)
;; TODO (autoload 'rmcs-profiles-initialize "rmcs-profiles")
;; TODO (autoload 'rmcs-packages-initialize "rmcs-packages")

;; UX: There's a chance the user will later use package.el or straight in this
;;   interactive session. If they do, make sure they're properly initialized
;;   when they do.
(autoload 'rmcs-initialize-packages "rmcs-packages")
(with-eval-after-load 'package (require 'rmcs-packages))
(with-eval-after-load 'straight (rmcs-initialize-packages))


;;
;;; Let 'er rip!

;; A last ditch opportunity to undo dodgy optimizations or do extra
;; configuration before the session is complicated by user config and packages.
(rmcs-run-hooks 'rmcs-before-init-hook)

;;; Load envvar file
;; 'rmcs env' generates an envvar file. This is a snapshot of your shell
;; environment, which Rmcs loads here. This is helpful in scenarios where Emacs
;; is launched from an environment detached from the user's shell environment.
(when (and (or initial-window-system
               (daemonp))
           rmcs-env-file)
  (rmcs-load-envvars-file rmcs-env-file 'noerror))

;;; Last minute setup
(add-hook 'rmcs-after-init-hook #'rmcs-load-packages-incrementally-h 100)
(add-hook 'rmcs-after-init-hook #'rmcs-display-benchmark-h 110)
(rmcs-run-hook-on 'rmcs-first-buffer-hook '(find-file-hook rmcs-switch-buffer-hook))
(rmcs-run-hook-on 'rmcs-first-file-hook   '(find-file-hook dired-initial-position-hook))
(rmcs-run-hook-on 'rmcs-first-input-hook  '(pre-command-hook))
;; PERF: Activate these later, otherwise they'll fire for every buffer created
;;   between now and the end of startup.
(add-hook! 'after-init-hook
  (defun rmcs-init-local-var-hooks-h ()
    ;; These fire `MAJOR-MODE-local-vars-hook' hooks, which is a Rmcsism. See
    ;; the `MODE-local-vars-hook' section above.
    (add-hook 'after-change-major-mode-hook #'rmcs-run-local-var-hooks-h 100)
    (add-hook 'hack-local-variables-hook #'rmcs-run-local-var-hooks-h)))

;;; Load $RMCSDIR/init.el early
;; TODO: Catch errors
(load! (string-remove-suffix ".el" rmcs-module-init-file) rmcs-user-dir t)

;;; Load the rest of $RMCSDIR + modules if noninteractive
;; If the user is loading this file from a batch script, let's assume they want
;; to load their userland config as well.
(when noninteractive
  (rmcs-require 'rmcs-profiles)
  (let ((init-file (rmcs-profile-init-file)))
    (unless (file-exists-p init-file)
      (user-error "Profile init file hasn't been generated. Did you forgot to run 'rmcs sync'?"))
    (let (kill-emacs-query-functions
          kill-emacs-hook)
      ;; Loads modules, then $RMCSDIR/config.el
      (rmcs-load init-file 'noerror)
      (rmcs-initialize-packages))))

;;; Entry point
;; HACK: This advice hijacks Emacs' initfile loader to accomplish the following:
;;
;;   1. Load the profile init file directory (generated on `rmcs sync`)
;;   2. Ignore initfiles we don't care about (like $EMACSDIR/init.el, ~/.emacs,
;;      and ~/_emacs) -- and spare us the IO of searching for them, and allows
;;      savvy hackers to use $EMACSDIR as their $RMCSDIR, if they wanted.
;;   3. Cut down on unnecessary logic in Emacs' bootstrapper.
;;   4. Offer a more user-friendly error state/screen, especially for errors
;;      emitted from Rmcs's core or the user's config.
(define-advice startup--load-user-init-file (:override (&rest _) init-rmcs 100)
  (let ((debug-on-error-from-init-file nil)
        (debug-on-error-should-be-set nil)
        (debug-on-error-initial (if (eq init-file-debug t) 'startup init-file-debug))
        ;; The init file might contain byte-code with embedded NULs, which can
        ;; cause problems when read back, so disable nul byte detection. (Bug
        ;; #52554)
        (inhibit-null-byte-detection t))
    (let ((debug-on-error debug-on-error-initial))
      (condition-case-unless-debug error
          (when init-file-user
            (let ((init-file-name
                   ;; This dynamically generated init file stores a lot of
                   ;; precomputed information, such as module and package
                   ;; autoloads, and values for expensive variables like
                   ;; `rmcs-modules', `rmcs-disabled-packages', `load-path',
                   ;; `auto-mode-alist', and `Info-directory-list'. etc.
                   ;; Compiling them in one place is a big reduction in startup
                   ;; time, and by keeping a history of them, you get a snapshot
                   ;; of your config in time.
                   (file-name-concat
                    rmcs-profile-dir (format "init.%d.elc" emacs-major-version))))
              ;; If `user-init-file' is t, then `load' will store the name of
              ;; the next file it loads into `user-init-file'.
              (setq user-init-file t)
              (when init-file-name
                (load init-file-name 'noerror 'nomessage 'nosuffix))
              ;; If it's still `t', then it failed to load the profile initfile.
              ;; This likely means the user has forgotten to run `rmcs sync'!
              (when (eq user-init-file t)
                (signal 'rmcs-nosync-error (list init-file-name)))
              ;; If we loaded a compiled file, set `user-init-file' to the
              ;; source version if that exists.
              (setq user-init-file
                    (concat (string-remove-suffix ".elc" user-init-file)
                            ".el"))))
        ;; TODO: Add safe-mode profile.
        ;; (error
        ;;  ;; HACK: This is not really this variable's intended purpose, but it
        ;;  ;;   doesn't mind what value its set to, only that its non-nil, so I'm
        ;;  ;;   exploiting its dynamic scope to pass the error to the profile.
        ;;  (setq init-file-had-error error)
        ;;  (load (file-name-concat rmcs-emacs-dir "profiles" "safe-mode" "init.el")
        ;;        nil 'nomessage 'nosuffix))
        (error
         (display-warning
          'initialization
          (format-message "\
An error occurred while loading `%s':\n\n%s%s%s\n\n\
To ensure normal operation, you should investigate and remove the
cause of the error in your initialization file.  Start Emacs with
the `--debug-init' option to view a complete error backtrace."
                          user-init-file
                          (get (car error) 'error-message)
                          (if (cdr error) ": " "")
                          (mapconcat (lambda (s) (prin1-to-string s t))
                                     (cdr error) ", "))
          :warning)
         (setq init-file-had-error t)))
      ;; If we can tell that the init file altered debug-on-error, arrange to
      ;; preserve the value that it set up.
      (or (eq debug-on-error debug-on-error-initial)
          (setq debug-on-error-should-be-set t
                debug-on-error-from-init-file debug-on-error)))
    (when debug-on-error-should-be-set
      (setq debug-on-error debug-on-error-from-init-file))))

(provide 'rmcs-start)
;;; rmcs-start.el ends here
