;;; rmcs-ui.el --- defaults for Rmcs's aesthetics -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code;

;;
;;; Variables

(defcustom rmcs-theme nil
  "A symbol representing the Emacs theme to load at startup.

Set to `nil' to load no theme at all. This variable is changed by
`load-theme'.")

(defcustom rmcs-font nil
  "The default font to use.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string.

This affects the `default' and `fixed-pitch' faces.

Examples:
  (setq rmcs-font (font-spec :family \"Fira Mono\" :size 12))
  (setq rmcs-font \"Terminus (TTF):pixelsize=12:antialias=off\")
  (setq rmcs-font \"Fira Code-14\")")

(defcustom rmcs-variable-pitch-font nil
  "The default font to use for variable-pitch text.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`rmcs-font' for examples.

An omitted font size means to inherit `rmcs-font''s size.")

(defcustom rmcs-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`rmcs-font' for examples.

An omitted font size means to inherit `rmcs-font''s size.")

(defcustom rmcs-symbol-font nil
  "Fallback font for symbols.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`rmcs-font' for examples. Emacs defaults to Symbola.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

(define-obsolete-variable-alias 'rmcs-unicode-font 'rmcs-symbol-font "3.0.0")

(defcustom rmcs-emoji-font nil
  "Fallback font for emoji.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`rmcs-font' for examples.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

(defconst rmcs-emoji-fallback-font-families
  '("Apple Color Emoji"
    "Segoe UI Emoji"
    "Noto Color Emoji"
    "Noto Emoji")
  "A list of fallback font families to use for emojis.
These are platform-specific fallbacks for internal use. If you
want to change your emoji font, use `rmcs-emoji-font'.")

(defconst rmcs-symbol-fallback-font-families
  '("Segoe UI Symbol"
    "Apple Symbols")
  "A list of fallback font families for general symbol glyphs.
These are platform-specific fallbacks for internal use. If you
want to change your symbol font, use `rmcs-symbol-font'.")


;;
;;; Custom hooks

(defcustom rmcs-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defcustom rmcs-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme' or reloaded with
`rmcs/reload-theme'.")

(defcustom rmcs-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defcustom rmcs-switch-window-hook nil
  "A list of hooks run after changing the focused windows.")

(defcustom rmcs-switch-frame-hook nil
  "A list of hooks run after changing the focused frame.")

(defun rmcs-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'rmcs-switch-buffer-hook)))

(defvar rmcs--last-frame nil)
(defun rmcs-run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'rmcs-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'rmcs-switch-window-hook))))

(defun rmcs-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (rmcs-fallback-buffer))))

(defun rmcs-highlight-non-default-indentation-h ()
  "Highlight whitespace at odds with `indent-tabs-mode'.
That is, highlight tabs if `indent-tabs-mode' is `nil', and highlight spaces at
the beginnings of lines if `indent-tabs-mode' is `t'. The purpose is to make
incorrect indentation in the current buffer obvious to you.

Does nothing if `whitespace-mode' or `global-whitespace-mode' is already active
or if the current buffer is read-only or not file-visiting."
  (unless (or (eq major-mode 'fundamental-mode)
              (bound-and-true-p global-whitespace-mode)
              (null buffer-file-name))
    (require 'whitespace)
    (set (make-local-variable 'whitespace-style)
         (cl-union (if indent-tabs-mode
                       '(indentation)
                     '(tabs tab-mark))
                   (when whitespace-mode
                     (remq 'face whitespace-active-style))))
    (cl-pushnew 'face whitespace-style) ; must be first
    (whitespace-mode +1)))


;;
;;; General UX

;; A simple confirmation prompt when killing Emacs. But only prompt when there
;; are real buffers open.
(setq confirm-kill-emacs #'rmcs-quit-p)
;; Prompt for confirmation when deleting a non-empty frame; a last line of
;; defense against accidental loss of work.
(global-set-key [remap delete-frame] #'rmcs/delete-frame-with-prompt)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Larger column width for function name in profiler reports
(after! profiler
  (setf (caar profiler-report-cpu-line-format) 80
        (caar profiler-report-memory-line-format) 80))


;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered. The default (0) triggers this too
      ;; aggressively, so I've set it to 10 to recenter if scrolling too far
      ;; off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;
;;; Buffers

(defadvice! rmcs--switch-to-fallback-buffer-maybe-a (&rest _)
  "Switch to `rmcs-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `rmcs-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
  :before-until #'kill-current-buffer
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window)
           t)
          ((eq buf (rmcs-fallback-buffer))
           (message "Can't kill the fallback buffer.")
           t)
          ((rmcs-real-buffer-p buf)
           (let ((visible-p (delq (selected-window) (get-buffer-window-list buf nil t))))
             (unless visible-p
               (when (and (buffer-modified-p buf)
                          (not (y-or-n-p
                                (format "Buffer %s is modified; kill anyway?"
                                        buf))))
                 (user-error "Aborted")))
             (let ((inhibit-redisplay t)
                   buffer-list-update-hook)
               (when (or ;; if there aren't more real buffers than visible buffers,
                      ;; then there are no real, non-visible buffers left.
                      (not (cl-set-difference (rmcs-real-buffer-list)
                                              (rmcs-visible-buffers)))
                      ;; if we end up back where we start (or previous-buffer
                      ;; returns nil), we have nowhere left to go
                      (memq (switch-to-prev-buffer nil t) (list buf 'nil)))
                 (switch-to-buffer (rmcs-fallback-buffer)))
               (unless visible-p
                 (with-current-buffer buf
                   (restore-buffer-modified-p nil))
                 (kill-buffer buf)))
             (run-hooks 'buffer-list-update-hook)
             t)))))


;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Rmcs Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: GUIs are inconsistent across systems, desktop environments, and themes,
;;   and don't match the look of Emacs. They also impose inconsistent shortcut
;;   key paradigms. I'd rather Emacs be responsible for prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'rmcs-init-ui-hook #'window-divider-mode)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)


;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: Remove when we drop 27.x support
  (advice-add #'yes-or-no-p :override #'y-or-n-p))

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;
;;; Built-in packages

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)


(after! comint
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048)) ; double the default


(after! compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  (add-hook 'compilation-filter-hook
            (if (< emacs-major-version 28)
                #'rmcs-apply-ansi-color-to-compilation-buffer-h
              #'ansi-color-compilation-filter))
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))


(after! ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  (defvar rmcs--ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff
  (add-hook! 'ediff-before-setup-hook
    (defun rmcs-ediff-save-wconf-h ()
      (setq rmcs--ediff-saved-wconf (current-window-configuration))))
  (add-hook! '(ediff-quit-hook ediff-suspend-hook) :append
    (defun rmcs-ediff-restore-wconf-h ()
      (when (window-configuration-p rmcs--ediff-saved-wconf)
        (set-window-configuration rmcs--ediff-saved-wconf)))))


(use-package! hl-line
  ;; Highlights the current line
  :hook (rmcs-first-buffer . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode text-mode conf-mode special-mode
      org-agenda-mode dired-mode)
    "What modes to enable `hl-line-mode' in.")
  :config
  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar rmcs--hl-line-mode nil)

  (add-hook! 'hl-line-mode-hook
    (defun rmcs-truly-disable-hl-line-h ()
      (unless hl-line-mode
        (setq-local rmcs--hl-line-mode nil))))

  (add-hook! '(evil-visual-state-entry-hook activate-mark-hook)
    (defun rmcs-disable-hl-line-h ()
      (when hl-line-mode
        (hl-line-mode -1)
        (setq-local rmcs--hl-line-mode t))))

  (add-hook! '(evil-visual-state-exit-hook deactivate-mark-hook)
    (defun rmcs-enable-hl-line-maybe-h ()
      (when rmcs--hl-line-mode
        (hl-line-mode +1)))))


(use-package! winner
  ;; undo/redo changes to Emacs' window layout
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook (rmcs-first-buffer . winner-mode)
  :config
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))


(use-package! paren
  ;; highlight matching delimiters
  :hook (rmcs-first-buffer . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


;;;###package whitespace
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))


;;
;;; Third party packages

(use-package! nerd-icons
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline))

;; Hide the mode line in completion popups and MAN pages because they serve
;; little purpose there, and is better hidden.
;;;###package hide-mode-line-mode
(add-hook! '(completion-list-mode-hook Man-mode-hook)
           #'hide-mode-line-mode)

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;###package image
(setq image-animate-loop t)

;;;###package rainbow-delimiters
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp. I reduce it from it's default of 9 to reduce the
;; complexity of the font-lock keyword and hopefully buy us a few ms of
;; performance.
(setq rainbow-delimiters-max-face-count 4)


;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'display-line-numbers-mode)

;; Fix #2742: cursor is off by 4 characters in `artist-mode'
;; REVIEW Reported upstream https://debbugs.gnu.org/cgi/bugreport.cgi?bug=43811
;; DEPRECATED Fixed in Emacs 28; remove when we drop 27 support
(unless (> emacs-major-version 27)
  (add-hook 'artist-mode-hook #'rmcs-disable-line-numbers-h))


;;
;;; Theme & font

;; User themes should live in $RMCSDIR/themes, not ~/.emacs.d
(setq custom-theme-directory (concat rmcs-user-dir "themes/"))

;; Third party themes add themselves to `custom-theme-load-path', but the themes
;; living in $RMCSDIR/themes should always have priority.
(setq custom-theme-load-path
      (cons 'custom-theme-directory
            (delq 'custom-theme-directory custom-theme-load-path)))

(defun rmcs--make-font-specs (face font)
  (let* ((base-specs (cadr (assq 'user (get face 'theme-face))))
         (base-specs (or base-specs '((t nil))))
         (attrs '(:family :foundry :slant :weight :height :width))
         (new-specs nil))
    (dolist (spec base-specs)
      ;; Each SPEC has the form (DISPLAY ATTRIBUTE-PLIST)
      (let ((display (car spec))
            (plist   (copy-tree (nth 1 spec))))
        ;; Alter only DISPLAY conditions matching this frame.
        (when (or (memq display '(t default))
                  (face-spec-set-match-display display this-frame))
          (dolist (attr attrs)
            (setq plist (plist-put plist attr (face-attribute face attr)))))
        (push (list display plist) new-specs)))
    (nreverse new-specs)))

(defun rmcs-init-fonts-h (&optional reload)
  "Loads `rmcs-font'."
  (dolist (map `((default . ,rmcs-font)
                 (fixed-pitch . ,rmcs-font)
                 (fixed-pitch-serif . ,rmcs-serif-font)
                 (variable-pitch . ,rmcs-variable-pitch-font)))
    (condition-case e
        (when-let* ((face (car map))
                    (font (cdr map)))
          (dolist (frame (frame-list))
            (when (display-multi-font-p frame)
              (set-face-attribute face frame
                                  :width 'normal :weight 'normal
                                  :slant 'normal :font font)))
          (let ((new-specs (rmcs--make-font-specs face font)))
            ;; Don't save to `customized-face' so it's omitted from `custom-file'
            ;;(put face 'customized-face new-specs)
            (custom-push-theme 'theme-face face 'user 'set new-specs)
            (put face 'face-modified nil)))
      ('error
       (ignore-errors (rmcs--reset-inhibited-vars-h))
       (if (string-prefix-p "Font not available" (error-message-string e))
           (signal 'rmcs-font-error (list (font-get (cdr map) :family)))
         (signal (car e) (cdr e))))))
  (when (fboundp 'set-fontset-font)
    (let* ((fn (rmcs-rpartial #'member (font-family-list)))
           (symbol-font (or rmcs-symbol-font
                            (cl-find-if fn rmcs-symbol-fallback-font-families)))
           (emoji-font (or rmcs-emoji-font
                           (cl-find-if fn rmcs-emoji-fallback-font-families))))
      (when symbol-font
        (dolist (script '(symbol mathematical))
          (set-fontset-font t script symbol-font)))
      (when emoji-font
        ;; DEPRECATED: make unconditional when we drop 27 support
        (when (version<= "28.1" emacs-version)
          (set-fontset-font t 'emoji emoji-font))
        ;; some characters in the Emacs symbol script are often covered by emoji
        ;; fonts
        (set-fontset-font t 'symbol emoji-font nil 'append)))
    ;; Nerd Fonts use these Private Use Areas
    (dolist (range '((#xe000 . #xf8ff) (#xf0000 . #xfffff)))
      (set-fontset-font t range "Symbols Nerd Font Mono")))
  ;; Users should inject their own font logic in `after-setting-font-hook'
  (run-hooks 'after-setting-font-hook))

(defun rmcs-init-theme-h (&rest _)
  "Load the theme specified by `rmcs-theme' in FRAME."
  (when (and rmcs-theme (not (custom-theme-enabled-p rmcs-theme)))
    (load-theme rmcs-theme t)))

(defadvice! rmcs--load-theme-a (fn theme &optional no-confirm no-enable)
  "Record `rmcs-theme', disable old themes, and trigger `rmcs-load-theme-hook'."
  :around #'load-theme
  ;; Run `load-theme' from an estranged buffer, where we can ensure that
  ;; buffer-local face remaps (by `mixed-pitch-mode', for instance) won't
  ;; interfere with recalculating faces in new themes.
  (with-temp-buffer
    (let ((last-themes (copy-sequence custom-enabled-themes)))
      ;; Disable previous themes so there are no conflicts. If you truly want
      ;; multiple themes enabled, then use `enable-theme' instead.
      (mapc #'disable-theme custom-enabled-themes)
      (prog1 (funcall fn theme no-confirm no-enable)
        (when (and (not no-enable) (custom-theme-enabled-p theme))
          (setq rmcs-theme theme)
          (put 'rmcs-theme 'previous-themes (or last-themes 'none))
          ;; DEPRECATED Hook into `enable-theme-functions' when we target 29
          (rmcs-run-hooks 'rmcs-load-theme-hook))))))


;;
;;; Bootstrap

(defun rmcs-init-ui-h (&optional _)
  "Initialize Rmcs's user interface by applying all its advice and hooks.

These should be done as late as possible, as to avoid/minimize prematurely
triggering hooks during startup."
  (rmcs-run-hooks 'rmcs-init-ui-hook)

  (add-hook 'kill-buffer-query-functions #'rmcs-protect-fallback-buffer-h)
  (add-hook 'after-change-major-mode-hook #'rmcs-highlight-non-default-indentation-h 'append)

  ;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
  (push '(buffer-predicate . rmcs-buffer-frame-predicate) default-frame-alist)

  ;; Initialize `rmcs-switch-window-hook' and `rmcs-switch-frame-hook'
  (add-hook 'window-selection-change-functions #'rmcs-run-switch-window-or-frame-hooks-h)
  ;; Initialize `rmcs-switch-buffer-hook'
  (add-hook 'window-buffer-change-functions #'rmcs-run-switch-buffer-hooks-h)
  ;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
  (add-hook 'server-visit-hook #'rmcs-run-switch-buffer-hooks-h))

;; Apply fonts and theme
(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'rmcs-init-fonts-h -100)
  (add-hook hook #'rmcs-init-theme-h -90))

;; PERF: Init UI late, but not too late. Its impact on startup time seems to
;;   vary wildly depending on exact placement. `window-setup-hook' appears to be
;;   the sweet spot.
(add-hook 'window-setup-hook #'rmcs-init-ui-h -100)


;;
;;; Fixes/hacks

;; Rmcs doesn't support `customize' and it never will. It's a clumsy interface
;; that sets variables at a time where it can be easily and unpredictably
;; overwritten. Configure things from your $RMCSDIR instead.
(dolist (sym '(customize-option customize-browse customize-group customize-face
               customize-rogue customize-saved customize-apropos
               customize-changed customize-unsaved customize-variable
               customize-set-value customize-customized customize-set-variable
               customize-apropos-faces customize-save-variable
               customize-apropos-groups customize-apropos-options
               customize-changed-options customize-save-customized))
  (put sym 'disabled "Rmcs doesn't support `customize', configure Emacs from $RMCSDIR/config.el instead"))
(put 'customize-themes 'disabled "Set `rmcs-theme' or use `load-theme' in $RMCSDIR/config.el instead")

;; These two functions don't exist in terminal Emacs, but some Emacs packages
;; (internal and external) use it anyway, leading to void-function errors. I
;; define a no-op substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

(after! whitespace
  (defun rmcs-is-childframes-p ()
    "`whitespace-mode' inundates child frames with whitespace markers, so
disable it to fix all that visual noise."
    (null (frame-parameter nil 'parent-frame)))
  (add-function :before-while whitespace-enable-predicate #'rmcs-is-childframes-p))

(provide 'rmcs-ui)
;;; rmcs-ui.el ends here
