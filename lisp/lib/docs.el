;;; core/lib/docs.el -- a reader mode for Rmcs's Org docs -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; This file defines `rmcs-docs-org-mode', a major mode derived from org-mode,
;; intended to make Rmcs's documentation more readable, insert virtual
;; navigation in the header, prettify org buffers (hiding syntax and special
;; tags), and defines special link types.
;;
;; This mode isn't only for Rmcs's docs, but also for docs found in other Rmcs
;; projects, like rmcsemacs/themes, rmcsemacs/snippets, and more. Most of its
;; variables can be customized in its root .dir-locals.el.
;;
;;; Code:

;;;###autoload
(defvar rmcs-docs-dir (file-name-concat rmcs-emacs-dir "docs/")
  "Where Rmcs's documentation files are stored. Must end with a slash.")

(defvar rmcs-docs-header-specs
  '(("/docs/index\\.org$"
     (:label "FAQ"
      :icon "nf-md-message_question_outline"
      :link "rmcs-faq:"
      :help-echo "Open the FAQ document"))
    (("/docs/[^/]+\\.org$" "/modules/README\\.org$")
     (:label "Back to index"
      :icon "nf-md-arrow_left"
      :link "rmcs-index"
      :help-echo "Navigate to the root index"))
    ("/modules/[^/]+/README\\.org$"
     (:label "Back to module index"
      :icon "nf-md-arrow_left"
      :link "rmcs-module-index:"))
    ("/modules/[^/]+/[^/]+/README\\.org$"
     (:label "Back to module index"
      :icon "nf-md-arrow_left"
      :link "rmcs-module-index:")
     (:label "History"
      :icon "nf-md-history"
      :icon-face font-lock-variable-name-face
      :link (lambda ()
              (cl-destructuring-bind (category . module) (rmcs-module-from-path (buffer-file-name))
                (format "rmcs-module-history:%s/%s" (rmcs-keyword-name category) module)))
      :help-echo "View the module history"
      :align right)
     (:label "Issues"
      :icon "nf-md-flag"
      :icon-face error
      :link (lambda ()
              (cl-destructuring-bind (category . module) (rmcs-module-from-path (buffer-file-name))
                (format "rmcs-module-issues::%s %s" category module)))
      :align right))
     (t
      (:label "Suggest edits"
       :icon "nf-md-account_edit"
       :icon-face warning
       :link "rmcs-suggest-edit"
       :align right)
      (:label "Help"
       :icon "nf-md-timeline_help_outline"
       :icon-face font-lock-function-name-face
       :link (lambda ()
               (let ((title (cadar (org-collect-keywords '("TITLE")))))
                 (cond ((equal title "Changelog") "rmcs-help-changelog:")
                       ((string-prefix-p ":" title) "rmcs-help-modules:")
                       (t "rmcs-help:"))))
       :align right))))

(defun rmcs-docs--make-header ()
  "Create a header string for the current buffer."
  (let* ((applicable-specs
          (cl-loop for (condition . specs) in rmcs-docs-header-specs
                   when (if (symbolp condition)
                            (symbol-value condition)
                          (seq-some (rmcs-rpartial #'string-match-p (buffer-file-name))
                                    (ensure-list condition)))
                   append specs))
         (left-specs
          (cl-remove-if-not
           (lambda (s) (memq (plist-get s :align) '(nil left)))
           applicable-specs))
         (right-specs
          (cl-remove-if-not
           (lambda (s) (eq (plist-get s :align) 'right))
           applicable-specs))
         (left-string
          (mapconcat #'rmcs-docs--make-header-link left-specs " "))
         (right-string
          (mapconcat #'rmcs-docs--make-header-link right-specs " ")))
    (if (string-empty-p right-string)
        (concat " " left-string)
      (concat " " left-string
              (make-string (max (- (window-width)
                                   (length left-string)
                                   (length right-string)
                                   4)
                                1)
                           ?\s)
              right-string))))

(defun rmcs-docs--make-header-link (spec)
  "Create a header link according to SPEC."
  (let ((icon (and (plist-get spec :icon)
                   (with-demoted-errors "DOCS ERROR: %s"
                     (funcall (or (plist-get spec :icon-function)
                                  #'nerd-icons-mdicon)
                              (plist-get spec :icon)))))
        (label (pcase (plist-get spec :label)
                 ((and (pred functionp) lab)
                  (funcall lab))
                 ((and (pred stringp) lab)
                  lab)))
        (link (pcase (plist-get spec :link)
                ((and (pred functionp) link)
                 (funcall link))
                ((and (pred stringp) link)
                 link))))
    (propertize
     (concat
      (and icon
           (propertize icon 'face
                       (cadr (or (plist-member spec :icon-face)
                                 (plist-member spec :face)))))
      (and icon label " ")
      (and label
           (propertize label 'face (cadr (or (plist-get spec :face)
                                             '(nil link))))))
     'rmcs-docs-link link
     'keymap rmcs-docs--header-link-keymap
     'help-echo (or (plist-get spec :help-echo)
                    (format "LINK: %s" link))
     'mouse-face 'highlight)))

(setq rmcs-docs--header-link-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [header-line mouse-2] 'rmcs-docs--open-header-link)
    (define-key km [mouse-2] 'rmcs-docs--open-header-link)
    (define-key km [follow-link] 'mouse-face)
    km))

(defun rmcs-docs--open-header-link (ev)
  "Open the header link which is the target of the event EV."
  (interactive "e")
  (let* ((string-and-pos (posn-string (event-start ev)))
         (docs-buf (window-buffer (posn-window (event-start ev))))
         (link (get-pos-property (cdr string-and-pos)
                                 'rmcs-docs-link
                                 (car string-and-pos)))
         (parent-link-abbrevs
          (buffer-local-value 'org-link-abbrev-alist-local docs-buf)))
    (with-temp-buffer
      (setq buffer-file-name (buffer-file-name docs-buf))
      (let ((org-inhibit-startup t))
        (org-mode))
      (setq-local org-link-abbrev-alist-local parent-link-abbrevs)
      (insert "[[" link "]]")
      (set-buffer-modified-p nil)
      (org-link-open (org-element-context)))))

;; DEPRECATED Will be renamed once docs "framework" is generalized
(defvar rmcs-docs-link-alist
  '(("rmcs-tag"                . "https://github.com/hlissner/rmcs-emacs/releases/tag/%s")
    ("rmcs-contrib-core"       . "id:9ac0c15c-29e7-43f8-8926-5f0edb1098f0")
    ("rmcs-contrib-docs"       . "id:31f5a61d-d505-4ee8-9adb-97678250f4e2")
    ("rmcs-contrib-maintainer" . "id:e71e9595-a297-4c49-bd11-f238329372db")
    ("rmcs-contrib-module"     . "id:b461a050-8702-4e63-9995-c2ef3a78f35d")
    ("rmcs-faq"                . "id:5fa8967a-532f-4e0c-8ae8-25cd802bf9a9")
    ("rmcs-help"               . "id:9bb17259-0b07-45a8-ae7a-fc5e0b16244e")
    ("rmcs-help-changelog"     . "id:7c56cc08-b54b-4f4b-b106-a76e2650addd")
    ("rmcs-help-modules"       . "id:1ee0b650-f09b-4454-8690-cc145aadef6e")
    ("rmcs-index"              . "id:3051d3b6-83e2-4afa-b8fe-1956c62ec096")
    ("rmcs-module-index"       . "id:12d2de30-c569-4b8e-bbc7-85dd5ccc4afa")
    ("rmcs-module-issues"      . "https://github.com/rmcsemacs/rmcsemacs/labels/%s")
    ("rmcs-module-history"     . "https://github.com/rmcsemacs/rmcsemacs/commits/master/modules/%s")
    ("rmcs-report"             . "https://github.com/rmcsemacs/rmcsemacs/issues/new/choose")
    ("rmcs-suggest-edit"       . "id:31f5a61d-d505-4ee8-9adb-97678250f4e2")
    ("rmcs-suggest-faq"        . "id:aa28b732-0512-49ed-a47b-f20586c0f051")
    ("github"                  . "https://github.com/%s")

    ;; TODO Implement later, once docs are generalized
    ;; ("github-release"          . (lambda (link)
    ;;                                (format "%s/releases/tag/%s"
    ;;                                        rmcs-docs-this-repo
    ;;                                        link)))
    ;; ("github-label"            . (lambda (link)
    ;;                                (format "%s/labels/%s"
    ;;                                        rmcs-docs-this-repo
    ;;                                        link)))
    ;; ("github-commits"          . (lambda (link)
    ;;                                (format "%s/commits/%s/modules/%s"
    ;;                                        rmcs-docs-this-repo
    ;;                                        "master"
    ;;                                        link))"github-repo:/commits/%b/modules/%%s")
    ;; ("github-report"           . "github-repo:/issues/new/choose")
    ))


;;
;;; `rmcs-docs-mode'

(defun rmcs-docs--display-menu-h ()
  "Toggle virtual menu line at top of buffer."
  (setq header-line-format
        (and buffer-read-only
             (rmcs-docs--make-header)))
  (add-hook 'window-state-change-hook #'rmcs-docs--display-menu-h nil t))

(defun rmcs-docs--hide-meta-h ()
  "Hide all meta or comment lines."
  (org-with-wide-buffer
   (goto-char (point-min))
   (save-match-data
     (let ((case-fold-search t))
       (while (re-search-forward "^[ \t]*\\#" nil t)
         (unless (org-in-src-block-p t)
           (catch 'abort
             (org-fold-core-region
              (line-beginning-position)
              (cond ((looking-at "+\\(?:title\\|subtitle\\): +")
                     (match-end 0))
                    ((looking-at "+\\(?:created\\|since\\|author\\|email\\|date\\): +")
                     (throw 'abort nil))
                    ((or (eq (char-after) ?\s)
                         (looking-at "+\\(begin\\|end\\)_comment"))
                     (line-beginning-position 2))
                    ((looking-at "+\\(?:begin\\|end\\)_\\([^ \n]+\\)")
                     (line-end-position))
                    ((line-beginning-position 2)))
              rmcs-docs-mode 'rmcs-doc-hidden))))))))

(defun rmcs-docs--hide-drawers-h ()
  "Hide all property drawers."
  (let (pt)
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (looking-at-p org-drawer-regexp)
       (setq pt (org-element-property :end (org-element-at-point))))
     (while (re-search-forward org-drawer-regexp nil t)
       (when-let ((el (org-element-at-point))
                  (beg (max (point-min) (1- (org-element-property :begin el))))
                  (end (org-element-property :end el))
                  ((memq (org-element-type el) '(drawer property-drawer))))
         (when (org-element-property-inherited :level el)
           (cl-decf end))
         (org-fold-core-region beg end rmcs-docs-mode 'rmcs-doc-hidden))))
    ;; FIX: If the cursor remains within a newly folded region, that folk will
    ;;   come undone, so we move it.
    (if pt (goto-char pt))))

(defun rmcs-docs--hide-tags-h ()
  "Hide tags in org headings."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward org-heading-regexp nil t)
     (when-let (tags (org-get-tags nil t))
       (when (or (member "noorg" tags)
                 (member "unfold" tags))
         ;; prevent `org-ellipsis' around hidden regions
         (org-show-entry))
       (if (member "noorg" tags)
           (org-fold-core-region (line-end-position 0)
                                 (save-excursion
                                   (org-end-of-subtree t)
                                   (forward-line 1)
                                   (if (and (bolp) (eolp))
                                       (line-beginning-position)
                                     (line-end-position 0)))
                                 rmcs-docs-mode 'rmcs-doc-hidden)
         (org-fold-core-region (save-excursion
                                 (goto-char (line-beginning-position))
                                 (re-search-forward " +:[^ ]" (line-end-position))
                                 (match-beginning 0))
                               (line-end-position)
                               rmcs-docs-mode 'rmcs-doc-hidden))))))

(defun rmcs-docs--hide-stars-h ()
  "Update invisible property to VISIBILITY for markers in the current buffer."
  (org-with-wide-buffer
   (goto-char (point-min))
   (with-silent-modifications
     (while (re-search-forward "^\\(\\*[ \t]\\|\\*\\*+\\)" nil t)
       (org-fold-core-region (match-beginning 0)
                             (match-end 0)
                             rmcs-docs-mode
                             'rmcs-doc-hidden)))))

(defvar rmcs-docs--babel-cache nil)
(defun rmcs-docs--hide-src-blocks-h ()
  "Hide babel blocks (and/or their results) depending on their :exports arg."
  (org-with-wide-buffer
   (let ((inhibit-read-only t))
     (goto-char (point-min))
     (make-local-variable 'rmcs-docs--babel-cache)
     (while (re-search-forward org-babel-src-block-regexp nil t)
       (let* ((beg (match-beginning 0))
              (end (save-excursion (goto-char (match-end 0))
                                   (skip-chars-forward "\n")
                                   (point)))
              (exports
               (save-excursion
                 (goto-char beg)
                 (and (re-search-forward " :exports \\([^ \n]+\\)" (line-end-position) t)
                      (match-string-no-properties 1))))
              (results (org-babel-where-is-src-block-result)))
         (save-excursion
           (when (and (if (stringp exports)
                          (member exports '("results" "both"))
                        org-export-use-babel)
                      (not results)
                      rmcs-docs-mode)
             (cl-pushnew beg rmcs-docs--babel-cache)
             (quiet! (org-babel-execute-src-block))
             (setq results (org-babel-where-is-src-block-result))
             (org-element-cache-refresh beg)
             (restore-buffer-modified-p nil)))
         (save-excursion
           (when results
             (when (member exports '("code" "both" "t"))
               (setq beg results))
             (when (member exports '("none" "code"))
               (setq end (progn (goto-char results)
                                (goto-char (org-babel-result-end))
                                (skip-chars-forward "\n")
                                (point))))))
         (unless (member exports '(nil "both" "code" "t"))
           (org-fold-core-region beg end rmcs-docs-mode 'rmcs-doc-hidden))))
     (unless rmcs-docs-mode
       (save-excursion
         (dolist (pos rmcs-docs--babel-cache)
           (goto-char pos)
           (org-babel-remove-result)
           (org-element-cache-refresh pos))
         (kill-local-variable 'rmcs-docs--babel-cache)
         (restore-buffer-modified-p nil))))))

(defvar rmcs-docs--macro-cache nil)
(defun rmcs-docs--expand-macros-h ()
  "Expand {{{macros}}} with their value."
  (org-with-wide-buffer
    (goto-char (point-min))
    (make-local-variable 'rmcs-docs--macro-cache)
    (while (re-search-forward "{{{[^}]+}}}" nil t)
      (with-silent-modifications
        (if rmcs-docs-mode
            (when-let* ((element (org-element-context))
                        (key (org-element-property :key element))
                        (cachekey (org-element-property :value element))
                        (template (cdr (assoc-string key org-macro-templates t))))
              (let ((value (or (cdr (assoc-string cachekey rmcs-docs--macro-cache))
                               (setf (alist-get cachekey rmcs-docs--macro-cache nil nil 'equal)
                                     (org-macro-expand element org-macro-templates)))))
                (add-text-properties (match-beginning 0)
                                     (match-end 0)
                                     `(display ,value))))
          (remove-text-properties (match-beginning 0)
                                  (match-end 0)
                                  '(display))))
      (org-element-cache-refresh (point)))))

(defvar rmcs-docs-mode-alist
  '((flyspell-mode . -1)
    (spell-fu-mode . -1)
    (visual-line-mode . -1)
    (mixed-pitch-mode . -1)
    (variable-pitch-mode . -1))
  "An alist of minor modes to activate or deactivate in `rmcs-docs-mode'.

The CAR is the minor mode symbol, and CDR should be either +1 or -1,
depending.")

(defvar rmcs-docs--initial-values nil)
(defvar rmcs-docs--cookies nil)
;;;###autoload
(define-minor-mode rmcs-docs-mode
  "Hides metadata, tags, & drawers and activates all org-mode prettifications.
This primes `org-mode' for reading."
  :lighter " Rmcs Docs"
  :after-hook (org-restart-font-lock)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org mode buffer"))
  (org-fold-add-folding-spec
   'rmcs-doc-hidden '(:visible nil
                      :ellipsis nil
                      :isearch-ignore t))
  (mapc (lambda (sym)
          (if rmcs-docs-mode
              (set (make-local-variable sym) t)
            (kill-local-variable sym)))
        '(org-pretty-entities
          org-hide-emphasis-markers
          org-hide-macro-markers))
  (when rmcs-docs-mode
    (make-local-variable 'rmcs-docs--initial-values))
  (mapc (lambda! ((face . plist))
          (if rmcs-docs-mode
              (push (apply #'face-remap-add-relative face plist) rmcs-docs--cookies)
            (mapc #'face-remap-remove-relative rmcs-docs--cookies)))
        '((org-document-title :weight bold :height 1.4)
          (org-document-info  :weight normal :height 1.15)))
  (mapc (lambda! ((mode . state))
          (if rmcs-docs-mode
              (if (and (boundp mode) (symbol-value mode))
                  (unless (> state 0)
                    (setf (alist-get mode rmcs-docs--initial-values) t)
                    (funcall mode -1))
                (unless (< state 0)
                  (setf (alist-get mode rmcs-docs--initial-values) nil)
                  (funcall mode +1)))
            (when-let (old-val (assq mode rmcs-docs--initial-values))
              (funcall mode (if old-val +1 -1)))))
        rmcs-docs-mode-alist)
  (unless rmcs-docs-mode
    (kill-local-variable 'rmcs-docs--initial-values)))

(add-hook! 'rmcs-docs-mode-hook
           #'rmcs-docs--display-menu-h
           #'rmcs-docs--hide-meta-h
           #'rmcs-docs--hide-tags-h
           #'rmcs-docs--hide-drawers-h
           ;; #'rmcs-docs--hide-stars-h
           #'rmcs-docs--expand-macros-h
           #'rmcs-docs--hide-src-blocks-h)

(defun rmcs-docs--toggle-read-only-h ()
  (rmcs-docs-mode (if buffer-read-only +1 -1)))


(defvar rmcs-docs--id-locations nil)
(defvar rmcs-docs--id-files nil)
(defvar rmcs-docs--id-location-file (file-name-concat rmcs-cache-dir "rmcs-docs-org-ids"))
;;;###autoload
(defun rmcs/reload-docs (&optional force)
  "Reload the ID locations in Rmcs's documentation and open docs buffers."
  (interactive (list 'interactive))
  (with-temp-buffer
    (let ((org-id-locations-file rmcs-docs--id-location-file)
          (org-id-track-globally t)
          org-agenda-files
          org-id-extra-files
          org-id-files
          org-id-locations
          org-id-extra-files
          (org-inhibit-startup t)
          org-mode-hook)
      (if force
          (org-id-update-id-locations
           (rmcs-files-in (list rmcs-docs-dir rmcs-modules-dir)
                          :match "/[^.].+\\.org$"))
        (org-id-locations-load))
      (setq rmcs-docs--id-files org-id-files
            rmcs-docs--id-locations org-id-locations)))
  (dolist (buf (rmcs-buffers-in-mode 'rmcs-docs-org-mode))
    (with-current-buffer buf
      (setq-local org-id-files rmcs-docs--id-files
                  org-id-locations rmcs-docs--id-locations))))


;;
;;; `rmcs-docs-org-mode'

;;;###autoload
(defun rmcs-docs-generate-id (&optional force?)
  "Generate an ID for a `rmcs-docs-org-mode' buffer."
  (let ((org-id-link-to-org-use-id t)
        (org-id-method 'uuid)
        (org-id-track-globally t)
        (org-id-locations-file rmcs-docs--id-location-file)
        (org-id-locations rmcs-docs--id-locations)
        (org-id-files rmcs-docs--id-files))
    (rmcs/reload-docs)
    (when-let (fname (buffer-file-name (buffer-base-buffer)))
      (let ((id (org-id-new)))
        (org-id-add-location id fname)
        id))))

(defconst rmcs-docs-org-font-lock-keywords
  '(("^\\( *\\)#\\+begin_quote\n\\1 \\([󰝗󱌣󰐃󰔓󰟶󰥔]\\) "
     2 (pcase (match-string 2)
         ("󰝗" 'font-lock-comment-face)
         ("󱌣" 'font-lock-comment-face)
         ("󰐃" 'error)
         ("󰔓" 'success)
         ("󰟶" 'font-lock-keyword-face)
         ("󰥔" 'font-lock-constant-face)
         ("" 'warning))))
  "Extra font-lock keywords for Rmcs documentation.")

;;;###autoload
(define-derived-mode rmcs-docs-org-mode org-mode "Rmcs Docs"
  "A derivative of `org-mode' for Rmcs's documentation files.

Keeps track of its own IDs in `rmcs-docs-dir' and toggles `rmcs-docs-mode' when
`read-only-mode' is activated."
  :after-hook (visual-line-mode -1)
  (font-lock-add-keywords nil rmcs-docs-org-font-lock-keywords)
  (let ((gc-cons-threshold most-positive-fixnum)
        (gc-cons-percentage 1.0))
    (require 'org-id)
    (require 'ob)
    (setq-local org-id-link-to-org-use-id t
                org-id-method 'uuid
                org-id-track-globally t
                org-id-locations-file rmcs-docs--id-location-file
                org-id-locations rmcs-docs--id-locations
                org-id-files rmcs-docs--id-files
                org-num-max-level 3
                org-footnote-define-inline nil
                org-footnote-auto-label t
                org-footnote-auto-adjust t
                org-footnote-section nil
                wgrep-change-readonly-file t
                org-link-abbrev-alist-local (append org-link-abbrev-alist-local rmcs-docs-link-alist)
                org-babel-default-header-args
                (append '((:eval . "no") (:tangle . "no"))
                        org-babel-default-header-args)
                save-place-ignore-files-regexp ".")
    (unless org-inhibit-startup
      (rmcs/reload-docs)
      (unless (local-variable-p 'org-startup-with-inline-images)
        (setq org-display-remote-inline-images 'cache)
        (org-display-inline-images))
      (unless (local-variable-p 'org-startup-indented)
        (org-indent-mode +1))
      (unless (local-variable-p 'org-startup-numerated)
        (when (bound-and-true-p org-num-mode)
          (org-num-mode -1))
        (org-num-mode +1))
      (unless (local-variable-p 'org-startup-folded)
        (let ((org-startup-folded 'content)
              org-cycle-hide-drawer-startup)
          (org-set-startup-visibility))))
    (add-hook 'read-only-mode-hook #'rmcs-docs--toggle-read-only-h nil 'local)))

(defun rmcs-docs-init-glossary-h ()
  "Activates `org-glossary-mode', if it's available."
  (when (require 'org-glossary nil t)
    (setq-local org-glossary-global-terms (rmcs-glob rmcs-docs-dir "appendix.org"))
    (org-glossary-mode +1)))
(add-hook 'rmcs-docs-org-mode-hook #'rmcs-docs-init-glossary-h)

;;;###autoload
(defun rmcs-docs-read-only-h ()
  "Activate `read-only-mode' if the current file exists and is non-empty."
  ;; The rationale: if it's empty or non-existant, you want to write an org
  ;; file, not read it.
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (when (and file-name
               (> (buffer-size) 0)
               (not (string-prefix-p "." (file-name-base file-name)))
               (file-exists-p file-name))
      (read-only-mode +1))))

(add-hook 'rmcs-docs-org-mode-hook #'rmcs-docs-read-only-h)

;;; docs.el ends here
