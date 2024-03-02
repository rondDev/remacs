;;; rmcs-keybinds.el --- defaults for Rmcs's keybinds -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' are ignored (i.e. omitted
;; entirely for performance reasons).
;;
;;; Code:

(defvar rmcs-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar rmcs-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

(defvar rmcs-localleader-key ","
  "The localleader prefix key, for major-mode specific commands.")

(defvar rmcs-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users.")

(defvar rmcs-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")


;;
;;; Global keybind settings

(cond
 (rmcs--system-macos-p
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))
 (rmcs--system-windows-p
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;; HACK: Emacs cannot distinguish between C-i from TAB. This is largely a
;;   byproduct of its history in the terminal, which can't distinguish them
;;   either, however, when GUIs came about Emacs greated separate input events
;;   for more contentious keys like TAB and RET. Therefore [return] != RET,
;;   [tab] != TAB, and [backspace] != DEL.
;;
;;   In the same vein, this keybind adds a [C-i] event, so users can bind to it.
;;   Otherwise, it falls back to regular C-i keybinds.
(define-key key-translation-map [?\C-i]
            (cmd! (if (let ((keys (this-single-command-raw-keys)))
                        (and keys
                             (not (cl-position 'tab    keys))
                             (not (cl-position 'kp-tab keys))
                             (display-graphic-p)
                             ;; Fall back if no <C-i> keybind can be found, otherwise
                             ;; we've broken all pre-existing C-i keybinds.
                             (let ((key
                                    (rmcs-lookup-key
                                     (vconcat (cl-subseq keys 0 -1) [C-i]))))
                               (not (or (numberp key) (null key))))))
                      [C-i] [?\C-i])))


;;
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar rmcs-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `rmcs/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun rmcs/escape (&optional interactive)
  "Run `rmcs-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'rmcs-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'rmcs/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'rmcs/escape))


;;
;;; General + leader/localleader keys

(require 'general)
;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)
;; Prevent "X starts with non-prefix key Y" errors except at startup.
(add-hook 'rmcs-after-modules-init-hook #'general-auto-unbind-keys)

;; HACK: `map!' uses this instead of `define-leader-key!' because it consumes
;;   20-30% more startup time, so we reimplement it ourselves.
(defmacro rmcs--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (rmcs-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key rmcs-leader-map (general--kbd ,key)
                      ,bdef)
                    forms))
            (when-let (desc (cadr (memq :which-key udef)))
              (prependq!
               wkforms `((which-key-add-key-based-replacements
                           (general--concat t rmcs-leader-alt-key ,key)
                           ,desc)
                         (which-key-add-key-based-replacements
                           (general--concat t rmcs-leader-key ,key)
                           ,desc))))))))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.

Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.

See `rmcs-leader-key' and `rmcs-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'rmcs-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `rmcs-localleader-key' and `rmcs-localleader-alt-key' to change the
localleader prefix."
  (if (modulep! :editor evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix rmcs-localleader-key
        :non-normal-prefix rmcs-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix rmcs-localleader-alt-key
      ,@args)))

;; PERF: We use a prefix commands instead of general's
;;   :prefix/:non-normal-prefix properties because general is incredibly slow
;;   binding keys en mass with them in conjunction with :states -- an effective
;;   doubling of Rmcs's startup time!
(define-prefix-command 'rmcs/leader 'rmcs-leader-map)
(define-key rmcs-leader-map [override-state] 'all)

;; Bind `rmcs-leader-key' and `rmcs-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'rmcs-after-init-hook
  (defun rmcs-init-leader-keys-h ()
    "Bind `rmcs-leader-key' and `rmcs-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (featurep 'evil))
          (progn
            (cond ((equal rmcs-leader-alt-key "C-c")
                   (set-keymap-parent rmcs-leader-map mode-specific-map))
                  ((equal rmcs-leader-alt-key "C-x")
                   (set-keymap-parent rmcs-leader-map ctl-x-map)))
            (define-key map (kbd rmcs-leader-alt-key) 'rmcs/leader))
        (evil-define-key* '(normal visual motion) map (kbd rmcs-leader-key) 'rmcs/leader)
        (evil-define-key* '(emacs insert) map (kbd rmcs-leader-alt-key) 'rmcs/leader))
      (general-override-mode +1))))


;;
;;; Packages

(use-package! which-key
  :hook (rmcs-first-input . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  (add-hook! 'rmcs-before-reload-hook
    (defun rmcs-reset-which-key-replacements-h ()
      (setq which-key-replacement-alist (get 'which-key-replacement-alist 'initial-value))))
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-add-key-based-replacements rmcs-leader-key "<leader>")
  (which-key-add-key-based-replacements rmcs-localleader-key "<localleader>"))


;;
;;; `map!' macro

(defvar rmcs-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun rmcs--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`rmcs-evil-state-alist' to customize this."
  (cl-loop for l across (rmcs-keyword-name keyword)
           if (assq l rmcs-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))


;; specials
(defvar rmcs--map-forms nil)
(defvar rmcs--map-fn nil)
(defvar rmcs--map-batch-forms nil)
(defvar rmcs--map-state '(:dummy t))
(defvar rmcs--map-parent-state nil)
(defvar rmcs--map-evil-p nil)
(after! evil (setq rmcs--map-evil-p t))

(defun rmcs--map-process (rest)
  (let ((rmcs--map-fn rmcs--map-fn)
        rmcs--map-state
        rmcs--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (rmcs--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (rmcs--map-commit)
                  (setq rmcs--map-fn 'rmcs--define-leader-key))
                 (:localleader
                  (rmcs--map-commit)
                  (setq rmcs--map-fn 'define-localleader-key!))
                 (:after
                  (rmcs--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (rmcs--map-set :keymaps `(quote ,(ensure-list (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (ensure-list (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (rmcs--map-nested (list (intern (rmcs-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (let ((keymap (intern (format "rmcs-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            rmcs--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (rmcs--map-set (if rmcs--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          rmcs--map-forms)))
                 (_
                  (condition-case _
                      (rmcs--map-def (pop rest) (pop rest)
                                     (rmcs--map-keyword-to-states key)
                                     desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((rmcs--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (rmcs--map-commit)
    (macroexp-progn (nreverse (delq nil rmcs--map-forms)))))

(defun rmcs--map-append-keys (prop)
  (let ((a (plist-get rmcs--map-parent-state prop))
        (b (plist-get rmcs--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun rmcs--map-nested (wrapper rest)
  (rmcs--map-commit)
  (let ((rmcs--map-parent-state (rmcs--map-state)))
    (push (if wrapper
              (append wrapper (list (rmcs--map-process rest)))
            (rmcs--map-process rest))
          rmcs--map-forms)))

(defun rmcs--map-set (prop &optional value)
  (unless (equal (plist-get rmcs--map-state prop) value)
    (rmcs--map-commit))
  (setq rmcs--map-state (plist-put rmcs--map-state prop value)))

(defun rmcs--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (rmcs-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state rmcs--map-batch-forms)))
  t)

(defun rmcs--map-commit ()
  (when rmcs--map-batch-forms
    (cl-loop with attrs = (rmcs--map-state)
             for (state . defs) in rmcs--map-batch-forms
             if (or rmcs--map-evil-p (not state))
             collect `(,(or rmcs--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) rmcs--map-forms))
    (setq rmcs--map-batch-forms nil)))

(defun rmcs--map-state ()
  (let ((plist
         (append (list :prefix (rmcs--map-append-keys :prefix)
                       :infix  (rmcs--map-append-keys :infix)
                       :keymaps
                       (append (plist-get rmcs--map-parent-state :keymaps)
                               (plist-get rmcs--map-state :keymaps)))
                 rmcs--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

Properties
  :leader [...]                   an alias for (:prefix rmcs-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

  These must be placed right before the key string.

  Do
    (map! :leader :desc \"Description\" :n \"C-c\" #'dosomething)
  Don't
    (map! :n :leader :desc \"Description\" \"C-c\" #'dosomething)
    (map! :leader :n :desc \"Description\" \"C-c\" #'dosomething)"
  (when (or (bound-and-true-p byte-compile-current-file)
            (not noninteractive))
    (rmcs--map-process rest)))

(provide 'rmcs-keybinds)
;;; rmcs-keybinds.el ends here
