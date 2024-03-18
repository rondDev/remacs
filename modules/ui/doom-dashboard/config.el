;;; ui/rmcs-dashboard/config.el -*- lexical-binding: t; -*-

(defvar +rmcs-dashboard-name "*rmcs*"
  "The name to use for the dashboard buffer.")

(defvar +rmcs-dashboard-functions
  '(rmcs-dashboard-widget-banner
    rmcs-dashboard-widget-shortmenu
    rmcs-dashboard-widget-loaded
    rmcs-dashboard-widget-footer)
  "List of widget functions to run in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard buffer is current
while they run.")

(defvar +rmcs-dashboard-banner-file "default.png"
  "The path to the image file to be used in on the dashboard. The path is
relative to `+rmcs-dashboard-banner-dir'. If nil, always use the ASCII banner.")

(defvar +rmcs-dashboard-banner-dir (concat (dir!) "/banners/")
  "Where to look for `+rmcs-dashboard-banner-file'.")

(defvar +rmcs-dashboard-ascii-banner-fn #'rmcs-dashboard-draw-ascii-banner-fn
  "The function used to generate the ASCII banner on Rmcs's dashboard.")

(defvar +rmcs-dashboard-banner-padding '(0 . 4)
  "Number of newlines to pad the banner with, above and below, respectively.")

(defvar +rmcs-dashboard-inhibit-refresh nil
  "If non-nil, the rmcs buffer won't be refreshed.")

(defvar +rmcs-dashboard-inhibit-functions ()
  "A list of functions which take no arguments. If any of them return non-nil,
dashboard reloading is inhibited.")

(defvar +rmcs-dashboard-pwd-policy 'last-project
  "The policy to use when setting the `default-directory' in the dashboard.

Possible values:

  'last-project  The `rmcs-project-root' of the last open buffer. Falls back
                 to `default-directory' if not in a project.
  'last          The `default-directory' of the last open buffer
  a FUNCTION     A function run with the `default-directory' of the last
                 open buffer, that returns a directory path
  a STRING       A fixed path
  nil            `default-directory' will never change")

(defvar +rmcs-dashboard-menu-sections
  '(("Recently opened files"
     :icon (nerd-icons-faicon "nf-fa-file_text" :face 'rmcs-dashboard-menu-title)
     :action recentf-open-files)
    ("Reload last session"
     :icon (nerd-icons-octicon "nf-oct-history" :face 'rmcs-dashboard-menu-title)
     :when (cond ((modulep! :ui workspaces)
                  (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                 ((require 'desktop nil t)
                  (file-exists-p (desktop-full-file-name))))
     :action rmcs/quickload-session)
    ("Open org-agenda"
     :icon (nerd-icons-octicon "nf-oct-calendar" :face 'rmcs-dashboard-menu-title)
     :when (fboundp 'org-agenda)
     :action org-agenda)
    ("Open project"
     :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'rmcs-dashboard-menu-title)
     :action projectile-switch-project)
    ("Jump to bookmark"
     :icon (nerd-icons-octicon "nf-oct-bookmark" :face 'rmcs-dashboard-menu-title)
     :action bookmark-jump)
    ("Open private configuration"
     :icon (nerd-icons-octicon "nf-oct-tools" :face 'rmcs-dashboard-menu-title)
     :when (file-directory-p rmcs-user-dir)
     :action rmcs/open-private-config)
    ("Open documentation"
     :icon (nerd-icons-octicon "nf-oct-book" :face 'rmcs-dashboard-menu-title)
     :action rmcs/help))
  "An alist of menu buttons used by `rmcs-dashboard-widget-shortmenu'. Each
element is a cons cell (LABEL . PLIST). LABEL is a string to display after the
icon and before the key string.

PLIST can have the following properties:

  :icon FORM
    Uses the return value of FORM as an icon (can be literal string).
  :key STRING
    The keybind displayed next to the button.
  :when FORM
    If FORM returns nil, don't display this button.
  :face FACE
    Displays the icon and text with FACE (a face symbol).
  :action FORM
    Run FORM when the button is pushed.")

;;
(defvar +rmcs-dashboard--last-cwd nil)
(defvar +rmcs-dashboard--width 80)
(defvar +rmcs-dashboard--old-fringe-indicator fringe-indicator-alist)
(defvar +rmcs-dashboard--pwd-alist ())
(defvar +rmcs-dashboard--reload-timer nil)


;;
;;; Bootstrap

(defun +rmcs-dashboard-init-h ()
  "Initializes Rmcs's dashboard."
  (unless noninteractive
    ;; Ensure the dashboard becomes Emacs' go-to buffer when there's nothing
    ;; else to show.
    (setq rmcs-fallback-buffer-name +rmcs-dashboard-name
          initial-buffer-choice #'rmcs-fallback-buffer)
    (unless fancy-splash-image
      (setq fancy-splash-image
            (expand-file-name +rmcs-dashboard-banner-file
                              +rmcs-dashboard-banner-dir)))
    (when (equal (buffer-name) "*scratch*")
      (set-window-buffer nil (rmcs-fallback-buffer))
      (+rmcs-dashboard-reload))
    (add-hook 'rmcs-load-theme-hook #'+rmcs-dashboard-reload-on-theme-change-h)
    ;; Ensure the dashboard is up-to-date whenever it is switched to or resized.
    (add-hook 'window-configuration-change-hook #'+rmcs-dashboard-resize-h)
    (add-hook 'window-size-change-functions #'+rmcs-dashboard-resize-h)
    (add-hook 'rmcs-switch-buffer-hook #'+rmcs-dashboard-reload-maybe-h)
    (add-hook 'delete-frame-functions #'+rmcs-dashboard-reload-frame-h)
    ;; `persp-mode' integration: update `default-directory' when switching perspectives
    (add-hook 'persp-created-functions #'+rmcs-dashboard--persp-record-project-h)
    (add-hook 'persp-activated-functions #'+rmcs-dashboard--persp-detect-project-h)
    ;; HACK Fix #2219 where, in GUI daemon frames, the dashboard loses center
    ;;      alignment after switching (or killing) workspaces.
    (when (daemonp)
      (add-hook 'persp-activated-functions #'+rmcs-dashboard-reload-maybe-h))
    (add-hook 'persp-before-switch-functions #'+rmcs-dashboard--persp-record-project-h)))

(add-hook 'rmcs-init-ui-hook #'+rmcs-dashboard-init-h 'append)

;;
;;; Faces
(defgroup rmcs-dashboard nil
  "Manage how rmcs-dashboard is coloured and themed."
  :prefix "rmcs-dashboard"
  :group 'rmcs-themes)

(defface rmcs-dashboard-banner '((t (:inherit font-lock-comment-face)))
  "Face used for the RMCS banner on the dashboard"
  :group 'rmcs-dashboard)

(defface rmcs-dashboard-footer '((t (:inherit font-lock-keyword-face)))
  "Face used for the footer on the dashboard"
  :group 'rmcs-dashboard)

(defface rmcs-dashboard-footer-icon '((t (:inherit nerd-icons-green)))
  "Face used for the icon of the footer on the dashboard"
  :group 'rmcs-dashboard)

(defface rmcs-dashboard-loaded '((t (:inherit font-lock-comment-face)))
  "Face used for the loaded packages benchmark"
  :group 'rmcs-dashboard)

(defface rmcs-dashboard-menu-desc '((t (:inherit font-lock-constant-face)))
  "Face used for the key description of menu widgets on the dashboard"
  :group 'rmcs-dashboard)

(defface rmcs-dashboard-menu-title '((t (:inherit font-lock-keyword-face)))
  "Face used for the title of menu widgets on the dashboard"
  :group 'rmcs-dashboard)


;;
;;; Major mode

(define-derived-mode +rmcs-dashboard-mode special-mode
  (format "RMCS v%s" rmcs-version)
  "Major mode for the RMCS dashboard buffer."
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  ;; Don't scroll to follow cursor
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  ;; Line numbers are ugly with large margins
  (setq-local display-line-numbers-type nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq-local fringe-indicator-alist alist))
  ;; Ensure point is always on a button
  (add-hook 'post-command-hook #'+rmcs-dashboard-reposition-point-h nil 'local)
  ;; hl-line produces an ugly cut-off line highlight in the dashboard, so don't
  ;; activate it there (by pretending it's already active).
  (setq-local hl-line-mode t))

(define-key! +rmcs-dashboard-mode-map
  [left-margin mouse-1]   #'ignore
  [remap forward-button]  #'+rmcs-dashboard/forward-button
  [remap backward-button] #'+rmcs-dashboard/backward-button
  "n"       #'forward-button
  "p"       #'backward-button
  "C-n"     #'forward-button
  "C-p"     #'backward-button
  [down]    #'forward-button
  [up]      #'backward-button
  [tab]     #'forward-button
  [backtab] #'backward-button

  ;; Evil remaps
  [remap evil-next-line]     #'forward-button
  [remap evil-previous-line] #'backward-button
  [remap evil-next-visual-line]     #'forward-button
  [remap evil-previous-visual-line] #'backward-button
  [remap evil-paste-pop-next] #'forward-button
  [remap evil-paste-pop]      #'backward-button
  [remap evil-delete]         #'ignore
  [remap evil-delete-line]    #'ignore
  [remap evil-insert]         #'ignore
  [remap evil-append]         #'ignore
  [remap evil-replace]        #'ignore
  [remap evil-enter-replace-state] #'ignore
  [remap evil-change]         #'ignore
  [remap evil-change-line]    #'ignore
  [remap evil-visual-char]    #'ignore
  [remap evil-visual-line]    #'ignore)


;;
;;; Hooks

(defun +rmcs-dashboard-reposition-point-h ()
  "Trap the point in the buttons."
  (when (region-active-p)
    (setq deactivate-mark t)
    (when (bound-and-true-p evil-local-mode)
      (evil-change-to-previous-state)))
  (or (ignore-errors
        (if (button-at (point))
            (forward-button 0)
          (backward-button 1)))
      (ignore-errors
        (goto-char (point-min))
        (forward-button 1))))

(defun +rmcs-dashboard-reload-maybe-h (&rest _)
  "Reload the dashboard or its state.

If this isn't a dashboard buffer, move along, but record its `default-directory'
if the buffer is real. See `rmcs-real-buffer-p' for an explanation for what
'real' means.

If this is the dashboard buffer, reload it completely."
  (cond ((+rmcs-dashboard-p (current-buffer))
         (let (+rmcs-dashboard-inhibit-refresh)
           (ignore-errors (+rmcs-dashboard-reload))))
        ((and (not (file-remote-p default-directory))
              (rmcs-real-buffer-p (current-buffer)))
         (setq +rmcs-dashboard--last-cwd default-directory)
         (+rmcs-dashboard-update-pwd-h))))

(defun +rmcs-dashboard-reload-frame-h (_frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (when (timerp +rmcs-dashboard--reload-timer)
    (cancel-timer +rmcs-dashboard--reload-timer)) ; in case this function is run rapidly
  (setq +rmcs-dashboard--reload-timer
        (run-with-timer 0.1 nil #'+rmcs-dashboard-reload t)))

(defun +rmcs-dashboard-resize-h (&rest _)
  "Recenter the dashboard, and reset its margins and fringes."
  (let (buffer-list-update-hook
        window-configuration-change-hook
        window-size-change-functions)
    (when-let (windows (get-buffer-window-list (rmcs-fallback-buffer) nil t))
      (dolist (win windows)
        (set-window-start win 0)
        (set-window-fringes win 0 0)
        (set-window-margins
         win (max 0 (/ (- (window-total-width win) +rmcs-dashboard--width) 2))))
      (with-current-buffer (rmcs-fallback-buffer)
        (save-excursion
          (with-silent-modifications
            (goto-char (point-min))
            (delete-region (line-beginning-position)
                           (save-excursion (skip-chars-forward "\n")
                                           (point)))
            (insert (make-string
                     (+ (max 0 (- (/ (window-height (get-buffer-window)) 2)
                                  (round (/ (count-lines (point-min) (point-max))
                                            2))))
                        (car +rmcs-dashboard-banner-padding))
                     ?\n))))))))

(defun +rmcs-dashboard--persp-detect-project-h (&rest _)
  "Set dashboard's PWD to current persp's `last-project-root', if it exists.

This and `+rmcs-dashboard--persp-record-project-h' provides `persp-mode'
integration with the Rmcs dashboard. It ensures that the dashboard is always in
the correct project (which may be different across perspective)."
  (when (bound-and-true-p persp-mode)
    (when-let (pwd (persp-parameter 'last-project-root))
      (+rmcs-dashboard-update-pwd-h pwd))))

(defun +rmcs-dashboard--persp-record-project-h (&optional persp &rest _)
  "Record the last `rmcs-project-root' for the current persp.
See `+rmcs-dashboard--persp-detect-project-h' for more information."
  (when (bound-and-true-p persp-mode)
    (set-persp-parameter
     'last-project-root (rmcs-project-root)
     (if (persp-p persp)
         persp
       (get-current-persp)))))


;;
;;; Library

(defun +rmcs-dashboard-p (buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq buffer (get-buffer +rmcs-dashboard-name)))

(defun +rmcs-dashboard-update-pwd-h (&optional pwd)
  "Update `default-directory' in the Rmcs dashboard buffer.
What it is set to is controlled by `+rmcs-dashboard-pwd-policy'."
  (if pwd
      (with-current-buffer (rmcs-fallback-buffer)
        (rmcs-log "Changed dashboard's PWD to %s" pwd)
        (setq-local default-directory pwd))
    (let ((new-pwd (+rmcs-dashboard--get-pwd)))
      (when (and new-pwd (file-accessible-directory-p new-pwd))
        (+rmcs-dashboard-update-pwd-h
         (concat (directory-file-name new-pwd)
                 "/"))))))

(defun +rmcs-dashboard-reload-on-theme-change-h ()
  "Forcibly reload the Rmcs dashboard when theme changes post-startup."
  (when after-init-time
    (+rmcs-dashboard-reload 'force)))

(defun +rmcs-dashboard-reload (&optional force)
  "Update the RMCS scratch buffer (or create it, if it doesn't exist)."
  (when (or (and (not +rmcs-dashboard-inhibit-refresh)
                 (get-buffer-window (rmcs-fallback-buffer))
                 (not (window-minibuffer-p (frame-selected-window)))
                 (not (run-hook-with-args-until-success '+rmcs-dashboard-inhibit-functions)))
            force)
    (with-current-buffer (rmcs-fallback-buffer)
      (rmcs-log "Reloading dashboard at %s" (format-time-string "%T"))
      (with-silent-modifications
        (let ((pt (point)))
          (unless (eq major-mode '+rmcs-dashboard-mode)
            (+rmcs-dashboard-mode))
          (erase-buffer)
          (run-hooks '+rmcs-dashboard-functions)
          (goto-char pt)
          (+rmcs-dashboard-reposition-point-h))
        (+rmcs-dashboard-resize-h)
        (+rmcs-dashboard--persp-detect-project-h)
        (+rmcs-dashboard-update-pwd-h)
        (current-buffer)))))

;; helpers
(defun +rmcs-dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun +rmcs-dashboard--get-pwd ()
  (let ((lastcwd +rmcs-dashboard--last-cwd)
        (policy +rmcs-dashboard-pwd-policy))
    (cond ((null policy)
           default-directory)
          ((stringp policy)
           (expand-file-name policy lastcwd))
          ((functionp policy)
           (funcall policy lastcwd))
          ((null lastcwd)
           default-directory)
          ((eq policy 'last-project)
           (or (rmcs-project-root lastcwd)
               lastcwd))
          ((eq policy 'last)
           lastcwd)
          ((warn "`+rmcs-dashboard-pwd-policy' has an invalid value of '%s'"
                 policy)))))


;;
;;; Widgets

(defun rmcs-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("=================     ===============     ===============   ========  ========"
            "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //"
            "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||"
            "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||"
            "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||"
            "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||"
            "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||"
            "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||"
            "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||"
            "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||"
            "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||"
            "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||"
            "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||"
            "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||"
            "||   .=='    _-'          '-__\\._-'         '-_./__-'         `' |. /|  |   ||"
            "||.=='    _-'                                                     `' |  /==.||"
            "=='    _-'                         E M A C S                          \\/   `=="
            "\\   _-'                                                                `-_   /"
            " `''                                                                      ``'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+rmcs-dashboard--center
                +rmcs-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'rmcs-dashboard-banner)))

(defun rmcs-dashboard-widget-banner ()
  (let ((point (point)))
    (when (functionp +rmcs-dashboard-ascii-banner-fn)
      (funcall +rmcs-dashboard-ascii-banner-fn))
    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +rmcs-dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert (make-string (or (cdr +rmcs-dashboard-banner-padding) 0)
                           ?\n)))))

(defun rmcs-dashboard-widget-loaded ()
  (when rmcs-init-time
    (insert
     "\n\n"
     (propertize
      (+rmcs-dashboard--center
       +rmcs-dashboard--width
       (rmcs-display-benchmark-h 'return))
      'face 'rmcs-dashboard-loaded)
     "\n")))

(defun rmcs-dashboard-widget-shortmenu ()
  (insert "\n")
  (dolist (section +rmcs-dashboard-menu-sections)
    (cl-destructuring-bind (label &key icon action when face key) section
      (when (and (fboundp action)
                 (or (null when)
                     (eval when t)))
        (insert
         (+rmcs-dashboard--center
          (- +rmcs-dashboard--width 1)
          (let ((icon (if (stringp icon) icon (eval icon t))))
            (format (format "%s%%s%%-10s" (if icon "%3s\t" "%3s"))
                    (or icon "")
                    (with-temp-buffer
                      (insert-text-button
                       label
                       'action
                       `(lambda (_)
                          (call-interactively (or (command-remapping #',action)
                                                  #',action)))
                       'face (or face 'rmcs-dashboard-menu-title)
                       'follow-link t
                       'help-echo
                       (format "%s (%s)" label
                               (propertize (symbol-name action) 'face 'rmcs-dashboard-menu-desc)))
                      (format "%-37s" (buffer-string)))
                    ;; Lookup command keys dynamically
                    (propertize
                     (or key
                         (when-let*
                             ((keymaps
                               (delq
                                nil (list (when (bound-and-true-p evil-local-mode)
                                            (evil-get-auxiliary-keymap +rmcs-dashboard-mode-map 'normal))
                                          +rmcs-dashboard-mode-map)))
                              (key
                               (or (when keymaps
                                     (where-is-internal action keymaps t))
                                   (where-is-internal action nil t))))
                           (with-temp-buffer
                             (save-excursion (insert (key-description key)))
                             (while (re-search-forward "<\\([^>]+\\)>" nil t)
                               (let ((str (match-string 1)))
                                 (replace-match
                                  (upcase (if (< (length str) 3)
                                              str
                                            (substring str 0 3))))))
                             (buffer-string)))
                         "")
                     'face 'rmcs-dashboard-menu-desc))))
         (if (display-graphic-p)
             "\n\n"
           "\n"))))))

(defun rmcs-dashboard-widget-footer ()
  (insert
   "\n"
   (+rmcs-dashboard--center
    (- +rmcs-dashboard--width 2)
    (with-temp-buffer
      (insert-text-button (or (nerd-icons-codicon "nf-cod-octoface" :face 'rmcs-dashboard-footer-icon :height 1.3 :v-adjust -0.15)
                              (propertize "github" 'face 'rmcs-dashboard-footer))
                          'action (lambda (_) (browse-url "https://github.com/rondDev/remacs"))
                          'follow-link t
                          'help-echo "Open Remacs github page")
      (buffer-string)))
   "\n"))
