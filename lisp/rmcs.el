;;; rmcs.el --- the heart of the beast -*- lexical-binding: t; -*-
;;
;; Author:  Henrik Lissner <contact@henrik.io>
;; URL:     https://github.com/rmcsemacs/rmcsemacs
;;
;;   =================     ===============     ===============   ========  ========
;;   \\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //
;;   ||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||
;;   || . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||
;;   ||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||
;;   || . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||
;;   ||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||
;;   || . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||
;;   ||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||
;;   ||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||
;;   ||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||
;;   ||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||
;;   ||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||
;;   ||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||
;;   ||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||
;;   ||.=='    _-'                                                     `' |  /==.||
;;   =='    _-'                                                            \/   `==
;;   \   _-'                                                                `-_   /
;;    `''                                                                      ``'
;;
;; These demons are not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This is Rmcs's heart, where I define all its major constants and variables,
;; set its saner global defaults, then prepare Emacs to bootstrap Rmcs.
;;
;; The overall load order of Rmcs is as follows:
;;
;;   > $EMACSDIR/early-init.el
;;     > $EMACSDIR/lisp/rmcs.el
;;       - $EMACSDIR/lisp/rmcs-lib.el
;;     > $EMACSDIR/lisp/rmcs-start.el
;;       - hook: `rmcs-before-init-hook'
;;       - $RMCSDIR/init.el
;;   - hook: `before-init-hook'
;;   > $XDG_DATA_HOME/rmcs/$PROFILE/@/$VERSION/init.el   (replaces $EMACSDIR/init.el)
;;     - $EMACSDIR/rmcs-{keybinds,ui,projects,editor}.el
;;     - hook: `rmcs-before-modules-init-hook'
;;     - {$RMCSDIR,$EMACSDIR}/modules/*/*/init.el
;;     - hook: `rmcs-after-modules-init-hook'
;;     - hook: `rmcs-before-modules-config-hook'
;;     - {$RMCSDIR,$EMACSDIR}/modules/*/*/config.el
;;     - hook: `rmcs-after-modules-config-hook'
;;     - $RMCSDIR/config.el
;;     - `custom-file' or $RMCSDIR/custom.el
;;   - hook: `after-init-hook'
;;   - hook: `emacs-startup-hook'
;;   - hook: `window-setup-hook'
;;   - hook: `rmcs-init-ui-hook'
;;   - hook: `rmcs-after-init-hook'
;;   > After startup is complete:
;;     - On first input:              `rmcs-first-input-hook'
;;     - On first switched-to buffer: `rmcs-first-buffer-hook'
;;     - On first opened file:        `rmcs-first-file-hook'
;;
;; This is Rmcs's heart, where I define all its major constants and variables,
;; set only its sanest global defaults, employ its hackiest (and least
;; offensive) optimizations, and load the minimum for all Rmcs sessions.
;;
;;; Code:

;; For `when-let' and `if-let' on versions of Emacs before they were autoloaded.
(eval-when-compile (require 'subr-x))

(eval-and-compile  ; Check version at both compile and runtime.
  ;; Rmcs's minimum supported version of Emacs is 27.1. Its my goal to support
  ;; one major version below the stable release, for about a year or until
  ;; stable is ubiquitous (or at least easily accessible) across Linux distros.
  (when (< emacs-major-version 27)
    (user-error
     (concat
      "Detected Emacs " emacs-version ", but Rmcs requires 27.1 or newer (28.1 is\n\n"
      "recommended). The current Emacs executable in use is:\n\n  " (car command-line-args)
      "\n\nA guide for installing a newer version of Emacs can be found at:\n\n  "
      (format "https://docs.rmcsemacs.org/-/install/%s"
              (cond ((eq system-type 'darwin) "on-macos")
                    ((memq system-type '(cygwin windows-nt ms-dos)) "on-windows")
                    ("on-linux")))
      "\n\n"
      (if noninteractive
          (concat "Alternatively, either update your $PATH environment variable to include the\n"
                  "path of the desired Emacs executable OR alter the $EMACS environment variable\n"
                  "to specify the exact path or command needed to invoke Emacs."
                  (when-let ((script (cadr (member "--load" command-line-args)))
                             (command (file-name-nondirectory script)))
                    (concat " For example:\n\n"
                            "  $ EMACS=/path/to/valid/emacs " command " ...\n"
                            "  $ EMACS=\"/Applications/Emacs.app/Contents/MacOS/Emacs\" " command " ...\n"
                            "  $ EMACS=\"snap run emacs\" " command " ..."))
                  "\n\nAborting...")
        (concat "If you believe this error is a mistake, run 'rmcs doctor' on the command line\n"
                "to diagnose common issues with your config and system."))))))

;; Rmcs needs to be synced/rebuilt if either Rmcs or Emacs has been
;; up/downgraded. This is because byte-code isn't backwards compatible, and many
;; packages (including Rmcs), bake in absolute paths into their caches that need
;; to be refreshed.
(let ((old-version (eval-when-compile emacs-version)))
  (unless (equal emacs-version old-version)
    (user-error (concat "Rmcs was compiled with Emacs %s, but was loaded with %s. Run 'rmcs sync' to"
                        "recompile it.")
                emacs-version old-version)))

;;; Custom features & global constants
;; Rmcs has its own features that its modules, CLI, and user extensions can
;; announce, and don't belong in `features', so they are stored here, which can
;; include information about the external system environment.
(defconst rmcs-features
  (pcase system-type
    ('darwin                           '(macos bsd))
    ((or 'cygwin 'windows-nt 'ms-dos)  '(windows))
    ((or 'gnu 'gnu/linux)              '(linux))
    ((or 'gnu/kfreebsd 'berkeley-unix) '(linux bsd)))
  "A list of symbols denoting available features in the active Rmcs profile.")

;; Convenience aliases for internal use only (may be removed later).
(defconst rmcs-system            (car rmcs-features))
(defconst rmcs--system-windows-p (eq 'windows rmcs-system))
(defconst rmcs--system-macos-p   (eq 'macos rmcs-system))
(defconst rmcs--system-linux-p   (eq 'linux rmcs-system))

;; `system-type' is esoteric, so I create a pseudo feature as a stable and
;; consistent alternative, and all while using the same `featurep' interface
;; we're already familiar with.
(push :system features)
(put :system 'subfeatures rmcs-features)

;; Emacs needs a more consistent way to detect build features, and the docs
;; claim `system-configuration-features' is not da way. Some features (that
;; don't represent packages) can be found in `features' (which `featurep'
;; consults), but aren't consistent, so I'll impose some consistency:
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features) ; no alternative
    (push 'harfbuzz features))

;; The `native-compile' feature exists whether or not it is functional (e.g.
;; libgcc is available or not). This seems silly, so pretend it doesn't exist if
;; it isn't functional.
(if (featurep 'native-compile)
    (if (not (native-comp-available-p))
        (delq 'native-compile features)))

;; DEPRECATED remove in v3
(with-no-warnings
  (defconst IS-MAC      rmcs--system-macos-p)
  (defconst IS-LINUX    rmcs--system-linux-p)
  (defconst IS-WINDOWS  rmcs--system-windows-p)
  (defconst IS-BSD      (memq 'bsd rmcs-features))
  (defconst EMACS28+    (> emacs-major-version 27))
  (defconst EMACS29+    (> emacs-major-version 28))
  (defconst MODULES     (featurep 'dynamic-modules))
  (defconst NATIVECOMP  (featurep 'native-compile))

  (make-obsolete-variable 'IS-MAC     "Use (featurep :system 'macos) instead" "3.0.0")
  (make-obsolete-variable 'IS-LINUX   "Use (featurep :system 'linux) instead" "3.0.0")
  (make-obsolete-variable 'IS-WINDOWS "Use (featurep :system 'windows) instead" "3.0.0")
  (make-obsolete-variable 'IS-BSD     "Use (featurep :system 'bsd) instead" "3.0.0")
  (make-obsolete-variable 'EMACS28+   "Use (>= emacs-major-version 28) instead" "3.0.0")
  (make-obsolete-variable 'EMACS29+   "Use (>= emacs-major-version 29) instead" "3.0.0")
  (make-obsolete-variable 'MODULES    "Use (featurep 'dynamic-modules) instead" "3.0.0")
  (make-obsolete-variable 'NATIVECOMP "Use (featurep 'native-compile) instead" "3.0.0"))


;;; Fix $HOME on Windows
;; $HOME isn't normally defined on Windows, but many unix tools expect it.
(when rmcs--system-windows-p
  (when-let (realhome
             (and (null (getenv-internal "HOME"))
                  (getenv "USERPROFILE")))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))

;;; Load Rmcs's stdlib
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'rmcs-lib)


;;
;;; Core globals

(defgroup rmcs nil
  "An Emacs framework for the stubborn martian hacker."
  :link '(url-link "https://rmcsemacs.org")
  :group 'emacs)

(defconst rmcs-version "3.0.0-pre"
  "Current version of Rmcs Emacs core.")

;; DEPRECATED: Remove these when the modules are moved out of core.
(defconst rmcs-modules-version "24.02.0-pre"
  "Current version of Rmcs Emacs.")

(defvar rmcs-init-time nil
  "The time it took, in seconds, for Rmcs Emacs to initialize.")

(defconst rmcs-profile
  (if-let (profile (getenv-internal "RMCSPROFILE"))
      (save-match-data
        (if (string-match "^\\([^@]+\\)@\\(.+\\)$" profile)
            (cons (match-string 1 profile)
                  (match-string 2 profile))
          (cons profile "0")))
    ;; TODO Restore this in 3.0
    ;; (cons "_" "0")
    )
  "The active profile as a cons cell (NAME . VERSION).")

;;; Data directory variables
(defvar rmcs-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst rmcs-core-dir (file-name-directory load-file-name)
  "The root directory of Rmcs's core files. Must end with a slash.")

(defvar rmcs-modules-dir (expand-file-name "modules/" rmcs-emacs-dir)
  "The root directory for Rmcs's modules. Must end with a slash.")

(define-obsolete-variable-alias 'rmcs-private-dir 'rmcs-user-dir "3.0.0")
(defvar rmcs-user-dir
  (expand-file-name
   (if-let (rmcsdir (getenv-internal "RMCSDIR"))
       (file-name-as-directory rmcsdir)
     (or (let ((xdgdir
                (file-name-concat
                 (or (getenv-internal "XDG_CONFIG_HOME")
                     "~/.config")
                 "rmcs/")))
           (if (file-directory-p xdgdir) xdgdir))
         "~/.rmcs.d/")))
  "Where your private configuration is placed.

Defaults to ~/.config/rmcs, ~/.rmcs.d or the value of the RMCSDIR envvar;
whichever is found first. Must end in a slash.")

;; DEPRECATED: .local will be removed entirely in 3.0
(defvar rmcs-local-dir
  (if-let (localdir (getenv-internal "RMCSLOCALDIR"))
      (expand-file-name (file-name-as-directory localdir))
    (expand-file-name ".local/" rmcs-emacs-dir))
  "Root directory for local storage.

Use this as a storage location for this system's installation of Rmcs Emacs.

These files should not be shared across systems. By default, it is used by
`rmcs-data-dir' and `rmcs-cache-dir'. Must end with a slash.")

(define-obsolete-variable-alias 'rmcs-etc-dir 'rmcs-data-dir "3.0.0")
(defvar rmcs-data-dir
  (if rmcs-profile
      (if rmcs--system-windows-p
          (expand-file-name "rmcsemacs/data/" (getenv-internal "APPDATA"))
        (expand-file-name "rmcs/" (or (getenv-internal "XDG_DATA_HOME") "~/.local/share")))
    ;; DEPRECATED: .local will be removed entirely in 3.0
    (file-name-concat rmcs-local-dir "etc/"))
  "Where Rmcs stores its global data files.

Data files contain shared and long-lived data that Rmcs, Emacs, and their
packages require to function correctly or at all. Deleting them by hand will
cause breakage, and require user intervention (e.g. a `rmcs sync` or `rmcs env`)
to restore.

Use this for: server binaries, package source, pulled module libraries,
generated files for profiles, profiles themselves, autoloads/loaddefs, etc.

For profile-local data files, use `rmcs-profile-data-dir' instead.")

(defvar rmcs-cache-dir
  (if rmcs-profile
      (if rmcs--system-windows-p
          (expand-file-name "rmcsemacs/cache/" (getenv-internal "APPDATA"))
        (expand-file-name "rmcs/" (or (getenv-internal "XDG_CACHE_HOME") "~/.cache")))
    ;; DEPRECATED: .local will be removed entirely in 3.0
    (file-name-concat rmcs-local-dir "cache/"))
  "Where Rmcs stores its global cache files.

Cache files represent unessential data that shouldn't be problematic when
deleted (besides, perhaps, a one-time performance hit), lack portability (and so
shouldn't be copied to other systems/configs), and are regenerated when needed,
without user input (e.g. a `rmcs sync`).

Some examples: images/data caches, elisp bytecode, natively compiled elisp,
session files, ELPA archives, authinfo files, org-persist, etc.

For profile-local cache files, use `rmcs-profile-cache-dir' instead.")

(defvar rmcs-state-dir
  (if rmcs-profile
      (if rmcs--system-windows-p
          (expand-file-name "rmcsemacs/state/" (getenv-internal "APPDATA"))
        (expand-file-name "rmcs/" (or (getenv-internal "XDG_STATE_HOME") "~/.local/state")))
    ;; DEPRECATED: .local will be removed entirely in 3.0
    (file-name-concat rmcs-local-dir "state/"))
  "Where Rmcs stores its global state files.

State files contain unessential, unportable, but persistent data which, if lost
won't cause breakage, but may be inconvenient as they cannot be automatically
regenerated or restored. For example, a recently-opened file list is not
essential, but losing it means losing this record, and restoring it requires
revisiting all those files.

Use this for: history, logs, user-saved data, autosaves/backup files, known
projects, recent files, bookmarks.

For profile-local state files, use `rmcs-profile-state-dir' instead.")

;;; Profile file/directory variables
(defvar rmcs-profile-cache-dir
  (file-name-concat rmcs-cache-dir (car rmcs-profile))
  "For profile-local cache files under `rmcs-cache-dir'.")

(defvar rmcs-profile-data-dir
  (file-name-concat rmcs-data-dir (car rmcs-profile))
  "For profile-local data files under `rmcs-data-dir'.")

(defvar rmcs-profile-state-dir
  (file-name-concat rmcs-state-dir (car rmcs-profile))
  "For profile-local state files under `rmcs-state-dir'.")

(defconst rmcs-profile-dir
  (file-name-concat rmcs-profile-data-dir "@" (cdr rmcs-profile))
  "Where generated files for the active profile are kept.")

;; DEPRECATED: Will be moved to cli/env
(defconst rmcs-env-file
  (file-name-concat (if rmcs-profile
                        rmcs-profile-dir
                      rmcs-local-dir)
                    "env")
  "The location of your envvar file, generated by `rmcs env`.

This file contains environment variables scraped from your shell environment,
which is loaded at startup (if it exists). This is helpful if Emacs can't
\(easily) be launched from the correct shell session (particularly for MacOS
users).")


;;
;;; Startup optimizations

;; Here are Rmcs's hackiest (and least offensive) startup optimizations. They
;; exploit implementation details and unintended side-effects, and will change
;; often between major Emacs releases. However, I disable them if this is a
;; daemon session (where startup time matters less).
;;
;; Most of these have been tested on Linux and on fairly fast machines (with
;; SSDs), so your mileage may vary depending on your hardware.
(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; HACK: If the bundled elisp for this Emacs install isn't byte-compiled
     ;;   (but is compressed), then leave the gzip file handler there so Emacs
     ;;   won't forget how to read read them.
     ;;
     ;;   calc-loaddefs.el is our heuristic for this because it is built-in to
     ;;   all supported versions of Emacs, and calc.el explicitly loads it
     ;;   uncompiled. This ensures that the only other, possible fallback would
     ;;   be calc-loaddefs.el.gz.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Make sure the new value survives any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)
    ;; COMPAT: ...but restore `file-name-handler-alist' later, because it is
    ;;   needed for handling encrypted or compressed files, among other things.
    (add-hook! 'emacs-startup-hook :depth 101
      (defun rmcs--reset-file-handler-alist-h ()
        (set-default-toplevel-value
         'file-name-handler-alist
         ;; Merge instead of overwrite because there may have been changes to
         ;; `file-name-handler-alist' since startup we want to preserve.
         (delete-dups (append file-name-handler-alist old-value))))))

  (unless noninteractive
    ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
    ;;   larger than the system font) appears to impact startup time
    ;;   dramatically. The larger the delta in font size, the greater the delay.
    ;;   Even trivial deltas can yield a ~1000ms loss, though it varies wildly
    ;;   depending on font size.
    (setq frame-inhibit-implied-resize t)

    ;; PERF,UX: Reduce *Message* noise at startup. An empty scratch buffer (or
    ;;   the dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    ;; PERF,UX: Remove "For information about GNU Emacs..." message at startup.
    ;;   It's redundant with our dashboard and incurs a premature redraw.
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
    ;;   with `inhibit-startup-screen', but it would still initialize anyway.
    ;;   This involves some file IO and/or bitmap work (depending on the frame
    ;;   type) that we can no-op for a free 50-100ms boost in startup time.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; PERF: Shave seconds off startup time by starting the scratch buffer in
    ;;   `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
    ;;   pull in a ton of packages. `rmcs/open-scratch-buffer' provides a better
    ;;   scratch buffer anyway.
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless initial-window-system
      ;; PERF: Inexplicably, `tty-run-terminal-initialization' can sometimes
      ;;   take 2-3s when starting up Emacs in the terminal. Whatever slows it
      ;;   down at startup doesn't appear to affect it if it's called a little
      ;;   later in the startup process, so that's what I do.
      ;; REVIEW: This optimization is not well understood. Investigate it!
      (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
        (advice-remove #'tty-run-terminal-initialization #'tty-run-terminal-initialization@defer)
        (add-hook 'window-setup-hook
                  (rmcs-partial #'tty-run-terminal-initialization
                                (selected-frame) nil t))))

    (unless init-file-debug
      ;; PERF,UX: Site files tend to use `load-file', which emits "Loading X..."
      ;;   messages in the echo area. Writing to the echo-area triggers a
      ;;   redisplay, which can be expensive during startup. This may also cause
      ;;   an flash of white when creating the first frame.
      (define-advice load-file (:override (file) silence)
        (load file nil 'nomessage))
      ;; COMPAT: But undo our `load-file' advice later, as to limit the scope of
      ;;   any edge cases it could induce.
      (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
        (advice-remove #'load-file #'load-file@silence))

      ;; PERF: `load-suffixes' and `load-file-rep-suffixes' are consulted on
      ;;   each `require' and `load'. Rmcs won't load any modules this early, so
      ;;   omit .so for a tiny startup boost. Is later restored in rmcs-start.
      (put 'load-suffixes 'initial-value (default-toplevel-value 'load-suffixes))
      (put 'load-file-rep-suffixes 'initial-value (default-toplevel-value 'load-file-rep-suffixes))
      (set-default-toplevel-value 'load-suffixes '(".elc" ".el"))
      (set-default-toplevel-value 'load-file-rep-suffixes '(""))
      ;; COMPAT: Undo any problematic startup optimizations; from this point, I
      ;;   make no assumptions about what might be loaded in userland.
      (add-hook! 'rmcs-before-init-hook
        (defun rmcs--reset-load-suffixes-h ()
          (setq load-suffixes (get 'load-suffixes 'initial-value)
                load-file-rep-suffixes (get 'load-file-rep-suffixes 'initial-value))))

      ;; PERF: Rmcs uses `defcustom' to indicate variables that users are
      ;;   expected to reconfigure. Trouble is it fires off initializers meant
      ;;   to accommodate any user attempts to configure them before they were
      ;;   defined. This is unnecessary before $RMCSDIR/init.el is loaded, so I
      ;;   disable them until it is.
      (setq custom-dont-initialize t)
      (add-hook! 'rmcs-before-init-hook
        (defun rmcs--reset-custom-dont-initialize-h ()
          (setq custom-dont-initialize nil)))

      ;; PERF: The mode-line procs a couple dozen times during startup. This is
      ;;   normally quite fast, but disabling the default mode-line and reducing
      ;;   the update delay timer seems to stave off ~30-50ms.
      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf (setq mode-line-format nil)))
      ;; PERF,UX: Premature redisplays can substantially affect startup times and
      ;;   produce ugly flashes of unstyled Emacs.
      (setq-default inhibit-redisplay t
                    inhibit-message t)
      ;; COMPAT: Then reset with advice, because `startup--load-user-init-file'
      ;;   will never be interrupted by errors.  And if these settings are left
      ;;   set, Emacs could appear frozen or garbled.
      (defun rmcs--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))
      (add-hook 'after-init-hook #'rmcs--reset-inhibited-vars-h)
      (define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
        (when init-file-had-error
          (rmcs--reset-inhibited-vars-h))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value))))

      ;; PERF: Rmcs disables the UI elements by default, so that there's less
      ;;   for the frame to initialize. However, the toolbar is still populated
      ;;   regardless, so I lazy load it until tool-bar-mode is actually used.
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file (:before (&rest _) defer-tool-bar-setup)
        (advice-remove #'tool-bar-setup #'ignore)
        (add-transient-hook! 'tool-bar-mode (tool-bar-setup)))

      ;; PERF: Unset a non-trivial list of command line options that aren't
      ;;   relevant to this session, but `command-line-1' still processes.
      (unless rmcs--system-macos-p
        (setq command-line-ns-option-alist nil))
      (unless (memq initial-window-system '(x pgtk))
        (setq command-line-x-option-alist nil)))))


;;
;;; `rmcs-context'

(defvar rmcs-context '(t)
  "A list of symbols identifying all active Rmcs execution contexts.

This should never be directly changed, only let-bound, and should never be
empty. Each context describes what phase Rmcs is in, and may respond to.

All valid contexts:
  cli        -- while executing a Rmcs CLI
  compile    -- while byte-compilation is in progress
  eval       -- during inline evaluation of elisp
  init       -- while rmcs is formally starting up for the first time, after its
                core libraries are loaded, but before user config is.
  modules    -- while loading modules and their files
  sandbox    -- This session was launched from Rmcs's sandbox.
  packages   -- when packagedefs are being read
  reload     -- while reloading rmcs")
(put 'rmcs-context 'valid-values '(cli compile eval init modules packages reload doctor sandbox))
(put 'rmcs-context 'risky-local-variable t)

(defun rmcs-context--assert (context)
  (let ((valid (get 'rmcs-context 'valid-values)))
    (unless (memq context valid)
      (signal 'rmcs-context-error
              (list context "Unrecognized context" valid)))))

(defun rmcs-context-p (context)
  "Return t if CONTEXT is active (i.e. in `rmcs-context')."
  (if (memq context rmcs-context) t))

(defun rmcs-context-push (context)
  "Add CONTEXT to `rmcs-context', if it isn't already.

Return non-nil if successful. Throws an error if CONTEXT is invalid."
  (unless (memq context rmcs-context)
    (rmcs-context--assert context)
    (rmcs-log ":context: +%s %s" context rmcs-context)
    (push context rmcs-context)))

(defun rmcs-context-pop (context &optional strict?)
  "Remove CONTEXT from `rmcs-context'.

Return non-nil if successful. If STRICT? is non-nil, throw an error if CONTEXT
wasn't active when this was called."
  (if (not (rmcs-context-p context))
      (when strict?
        (signal 'rmcs-context-error
                (list rmcs-context "Attempt to pop missing context" context)))
    (rmcs-log ":context: -%s %s" context rmcs-context)
    (setq rmcs-context (delq context rmcs-context))))

(defmacro rmcs-context-with (contexts &rest body)
  "Evaluate BODY with CONTEXTS added to `rmcs-context'."
  (declare (indent 1))
  `(let ((rmcs-context rmcs-context))
     (dolist (context (ensure-list ,contexts))
       (rmcs-context-push context))
     ,@body))


;;
;;; Reasonable, global defaults

;;; Don't litter `rmcs-emacs-dir'/$HOME
;; HACK: I change `user-emacs-directory' because many packages (even built-in
;;   ones) abuse it to build paths for storage/cache files (instead of correctly
;;   using `locate-user-emacs-file'). This change ensures that said data files
;;   are never saved to the root of your emacs directory *and* saves us the
;;   trouble of setting a million directory/file variables. But it may throw off
;;   anyone (or any package) that uses it to search for your Emacs initfiles.
(setq user-emacs-directory rmcs-profile-cache-dir)

;; ...However, this may surprise packages (and users) that read
;; `user-emacs-directory' expecting to find the location of your Emacs config,
;; such as server.el!
(setq server-auth-dir (file-name-concat rmcs-emacs-dir "server/"))

;; Packages with file/dir settings that don't use `user-emacs-directory' or
;; `locate-user-emacs-file' to initialize will need to set explicitly, to stop
;; them from littering in ~/.emacs.d/.
(setq desktop-dirname  (file-name-concat rmcs-profile-state-dir "desktop")
      pcache-directory (file-name-concat rmcs-profile-cache-dir "pcache/"))

;; Allow the user to store custom.el-saved settings and themes in their Rmcs
;; config (e.g. ~/.rmcs.d/).
(setq custom-file (file-name-concat rmcs-user-dir "custom.el"))

;; By default, Emacs stores `authinfo' in $HOME and in plain-text. Let's not do
;; that, mkay? This file stores usernames, passwords, and other treasures for
;; the aspiring malicious third party. You'll need a GPG setup though.
(setq auth-sources (list (file-name-concat rmcs-profile-state-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

(define-advice en/disable-command (:around (fn &rest args) write-to-data-dir)
  "Save safe-local-variables to `custom-file' instead of `user-init-file'.

Otherwise, `en/disable-command' (in novice.el.gz) is hardcoded to write them to
`user-init-file')."
  (let ((user-init-file custom-file))
    (apply fn args)))

;;; Native compilation support (see http://akrl.sdf.org/gccemacs.html)
(when (boundp 'native-comp-eln-load-path)
  ;; Don't store eln files in ~/.emacs.d/eln-cache (where they can easily be
  ;; deleted by 'rmcs upgrade').
  ;; REVIEW Use `startup-redirect-eln-cache' when 28 support is dropped
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" rmcs-profile-cache-dir))

  ;; UX: Suppress compiler warnings and don't inundate users with their popups.
  ;;   They are rarely more than warnings, so are safe to ignore.
  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug)

  ;; HACK: native-comp-deferred-compilation-deny-list is replaced in later
  ;;   versions of Emacs 29, and with no deprecation warning. I alias them to
  ;;   ensure backwards compatibility for packages downstream that may have not
  ;;   caught up yet. I avoid marking it obsolete because obsolete warnings are
  ;;   unimportant to end-users. It's the package devs that should be informed.
  (unless (boundp 'native-comp-deferred-compilation-deny-list)
    (defvaralias 'native-comp-deferred-compilation-deny-list 'native-comp-jit-compilation-deny-list))

  ;; UX: By default, native-comp uses 100% of half your cores. If you're
  ;;   expecting this this should be no issue, but the sudden (and silent) spike
  ;;   of CPU and memory utilization can alarm folks, overheat laptops, or
  ;;   overwhelm less performant systems.
  (define-advice comp-effective-async-max-jobs (:before (&rest _) set-default-cpus)
    "Default to 1/4 of cores in interactive sessions and all of them otherwise."
    (and (null comp-num-cpus)
         (zerop native-comp-async-jobs-number)
         (setq comp-num-cpus
               (max 1 (/ (num-processors) (if noninteractive 1 4))))))

  (define-advice comp-run-async-workers (:around (fn &rest args) dont-litter-tmpdir)
    "Normally, native-comp writes a ton to /tmp. This advice forces it to write
to `rmcs-cache-dir'/comp/ instead, so that Rmcs can safely clean it up as part
of 'rmcs sync' or 'rmcs gc'."
    (let ((temporary-file-directory (expand-file-name "comp/" rmcs-profile-cache-dir)))
      (make-directory temporary-file-directory t)
      (apply fn args))))

;;; Suppress package.el
;; Since Emacs 27, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Rmcs handles package initialization, so
;; we must prevent Emacs from doing it again.
(setq package-enable-at-startup nil)

;;; Reduce unnecessary/unactionable warnings/logs
;; Disable warnings from the legacy advice API. They aren't actionable or
;; useful, and often come from third party packages.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased". Otherwise the user
;; gets very intrusive popup warnings about our (intentional) uses of
;; defvaralias, which are done because ensuring aliases are created before
;; packages are loaded is an unneeded and unhelpful maintenance burden. Emacs
;; still aliases them fine regardless.
(setq warning-suppress-types '((defvaralias)))

;; Reduce debug output unless we've asked for it.
(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

;;; Stricter security defaults
;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be a
;; *little* more discerning.
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not rmcs--system-windows-p)
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with gnutls.el by default, so `tls-program' won't
      ;; typically be used, but in the odd case that it does, we ensure a more
      ;; secure default for it (falling back to `openssl' if absolutely
      ;; necessary). See https://redd.it/8sykl1 for details.
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))


;;
;;; Custom hooks

(defcustom rmcs-before-init-hook ()
  "A hook run after Rmcs's core has initialized; before user configuration.

This is triggered right before $RMCSDIR/init.el is loaded, in the context of
early-init.el. Use this for configuration at the latest opportunity before the
session becomes unpredictably complicated by user config, packages, etc. This
runs in both interactive and non-interactive contexts, so guard hooks
appropriately against `noninteractive' or the `cli' context (see
`rmcs-context').

In contrast, `before-init-hook' is run just after $RMCSDIR/init.el is loaded,
but long before your modules and $RMCSDIR/config.el are loaded."
  :group 'rmcs
  :type 'hook)

(defcustom rmcs-after-init-hook ()
  "A hook run once Rmcs's core and modules, and the user's config are loaded.

This triggers at the absolute latest point in the eager startup process, and
runs in both interactive and non-interactive sessions, so guard hooks
appropriately against `noninteractive' or the `cli' context."
  :group 'rmcs
  :type 'hook)


;;
;;; Last minute initialization

(when (daemonp)
  (message "Starting Rmcs Emacs in daemon mode!")
  (unless rmcs-inhibit-log
    (add-hook! 'rmcs-after-init-hook :depth 106
      (unless rmcs-inhibit-log
        (setq rmcs-inhibit-log (not (or noninteractive init-file-debug))))
      (message "Disabling verbose mode. Have fun!"))
    (add-hook! 'kill-emacs-hook :depth 110
      (message "Killing Emacs. Sayonara!"))))

(add-hook! 'rmcs-before-init-hook :depth -105
  (defun rmcs--begin-init-h ()
    "Begin the startup process."
    (when (rmcs-context-push 'init)
      ;; HACK: Ensure OS checks are as fast as possible (given their ubiquity).
      (setq features (cons :system (delq :system features)))
      ;; Remember these variables' initial values, so we can safely reset them at
      ;; a later time, or consult them without fear of contamination.
      (dolist (var '(exec-path load-path process-environment))
        (put var 'initial-value (default-toplevel-value var))))))

(add-hook! 'rmcs-after-init-hook :depth 105
  (defun rmcs--end-init-h ()
    "Set `rmcs-init-time'."
    (when (rmcs-context-pop 'init)
      (setq rmcs-init-time (float-time (time-subtract (current-time) before-init-time))))))

(unless noninteractive
  ;; This is the absolute latest a hook can run in Emacs' startup process.
  (define-advice command-line-1 (:after (&rest _) run-after-init-hook)
    (rmcs-run-hooks 'rmcs-after-init-hook)))

(provide 'rmcs)
;;; rmcs.el ends here
