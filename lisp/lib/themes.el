;;; lisp/lib/themes.el -*- lexical-binding: t; -*-

;;;###autoload
(defconst rmcs-customize-theme-hook nil)

(add-hook! 'rmcs-load-theme-hook
  (defun rmcs-apply-customized-faces-h ()
    "Run `rmcs-customize-theme-hook'."
    (run-hooks 'rmcs-customize-theme-hook)))

(defun rmcs--custom-theme-set-face (spec)
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect
                  (car (rmcs--custom-theme-set-face (cons face (cdr spec))))))
        ((keywordp (cadr spec))
         `((,(car spec) ((t ,(cdr spec))))))
        (`((,(car spec) ,(cdr spec))))))

;;;###autoload
(defmacro custom-theme-set-faces! (theme &rest specs)
  "Apply a list of face SPECS as user customizations for THEME.

THEME can be a single symbol or list thereof. If nil, apply these settings to
all themes. It will apply to all themes once they are loaded."
  (declare (indent defun))
  (let ((fn (gensym "rmcs--customize-themes-h-")))
    `(progn
       (defun ,fn ()
         (let (custom--inhibit-theme-enable)
           (dolist (theme (ensure-list (or ,theme 'user)))
             (when (or (eq theme 'user)
                       (custom-theme-enabled-p theme))
               (apply #'custom-theme-set-faces theme
                      (mapcan #'rmcs--custom-theme-set-face
                              (list ,@specs)))))))
       ;; Apply the changes immediately if the user is using the default theme
       ;; or the theme has already loaded. This allows you to evaluate these
       ;; macros on the fly and customize your faces iteratively.
       (when (or (get 'rmcs-theme 'previous-themes)
                 (null rmcs-theme))
         (funcall #',fn))
       ;; FIXME Prevent clobbering this on-the-fly
       (add-hook 'rmcs-customize-theme-hook #',fn 100))))

;;;###autoload
(defmacro custom-set-faces! (&rest specs)
  "Apply a list of face SPECS as user customizations.

This is a convenience macro alternative to `custom-set-face' which allows for a
simplified face format, and takes care of load order issues, so you can use
rmcs-themes' API without worry."
  (declare (indent defun))
  `(custom-theme-set-faces! 'user ,@specs))

;;;###autoload
(defun rmcs/reload-theme ()
  "Reload the current Emacs theme."
  (interactive)
  (unless rmcs-theme
    (user-error "No theme is active"))
  (let ((themes (copy-sequence custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (let (rmcs-load-theme-hook)
      (mapc #'enable-theme (reverse themes)))
    (rmcs-run-hooks 'rmcs-load-theme-hook)
    (rmcs/reload-font)
    (message "%s %s"
             (propertize
              (format "Reloaded %d theme%s:"
                      (length themes)
                      (if (cdr themes) "s" ""))
              'face 'bold)
             (mapconcat #'prin1-to-string themes ", "))))


;;
;;; Helpers

;;;###autoload
(defun rmcs-theme-face-attribute (theme face attribute &optional recursive)
  "Read a FACE's ATTRIBUTE for a loaded THEME.

This is different from `face-attribute', which reads the attribute of an active
face for the current theme, but an active theme can change (or fail to load) in
non-interactive or frame-less sessions."
  (let* ((spec
          (cl-loop for (type f _ spec) in (get theme 'theme-settings)
                   if (and (eq type 'theme-face) (eq face f))
                   return spec))
         (spec
          (letf! ((defun window-system (_frame) 'x)
                  (defun display-color-cells (_frame) 257)
                  (defun frame-parameter (frame parameter)
                    (pcase parameter
                      (`display-type 'color)
                      (`background-mode 'dark)
                      (_ (funcall frame-parameter frame parameter))))
                  (#'display-supports-face-attributes-p #'always))
            (face-spec-choose spec)))
         (inherit (if recursive (plist-get spec :inherit)))
         (value (if (plist-member spec attribute)
                    (plist-get spec attribute)
                  'unspecified)))
    (when (and inherit (not (eq inherit 'unspecified)))
      (letf! (defun face-attribute (face attribute &optional _frame inherit)
               (rmcs-theme-face-attribute theme face attribute inherit))
        (setq value (face-attribute-merged-with attribute value inherit))))
    value))
