;;; lisp/lib/fonts.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar rmcs-font-increment 2
  "How many steps to increase the font size each time `rmcs/increase-font-size'
or `rmcs/decrease-font-size' are invoked.")

;;;###autoload
(defvar rmcs-big-font nil
  "The font to use for `rmcs-big-font-mode'.
If nil, `rmcs-font' will be used, scaled up by `rmcs-big-font-increment'. See
`rmcs-font' for details on acceptable values for this variable.")

;;;###autoload
(defvar rmcs-big-font-increment 4
  "How many steps to increase the font size (with `rmcs-font' as the base) when
`rmcs-big-font-mode' is enabled and `rmcs-big-font' is nil.")


;;
;;; Library

;;;###autoload
(defun rmcs-normalize-font (font)
  "Return FONT as a normalized font spec.

The font will be normalized (i.e. :weight, :slant, and :width will set to
'normal if not specified) before it is converted.

FONT can be a `font-spec', a font object, an XFT font string, or an XLFD font
string."
  (cl-check-type font (or font string vector))
  (when (and (stringp font)
             (string-prefix-p "-" font))
    (setq font (x-decompose-font-name font)))
  (let* ((font
          (cond ((stringp font)
                 (dolist (prop '("weight" "slant" "width") (aref (font-info font) 0))
                   (unless (string-match-p (format ":%s=" prop) font)
                     (setq font (concat font ":" prop "=normal")))))
                ((fontp font)
                 (dolist (prop '(:weight :slant :width) (font-xlfd-name font))
                   (unless (font-get font prop)
                     (font-put font prop 'normal))))
                ((vectorp font)
                 (dolist (i '(1 2 3) (x-compose-font-name font))
                   (unless (aref font i)
                     (aset font i "normal"))))))
         (font (x-resolve-font-name font))
         (font (font-spec :name font)))
    (unless (font-get font :size)
      (font-put font :size
                (font-get (font-spec :name (face-font 'default))
                          :size)))
    font))

;;;###autoload
(defun rmcs-adjust-font-size (increment &optional fixed-size-p font-alist)
  "Increase size of font in FRAME by INCREMENT.

If FIXED-SIZE-P is non-nil, treat INCREMENT as a font size, rather than a
scaling factor.

FONT-ALIST is an alist give temporary values to certain Rmcs font variables,
like `rmcs-font' or `rmcs-variable-pitch-font'. e.g.

  `((rmcs-font . ,(font-spec :family \"Sans Serif\" :size 12)))

Doesn't work in terminal Emacs."
  (unless (display-multi-font-p)
    (user-error "Cannot resize fonts in terminal Emacs"))
  (condition-case-unless-debug e
      (let (changed)
        (dolist (sym '((rmcs-font . default)
                       (rmcs-serif-font . fixed-pitch-serif)
                       (rmcs-variable-pitch-font . variable-pitch))
                     (when changed
                       (rmcs-init-fonts-h 'reload)
                       t))
          (cl-destructuring-bind (var . face) sym
            (if (null increment)
                (when (get var 'initial-value)
                  (set var (get var 'initial-value))
                  (put var 'initial-value nil)
                  (setq changed t))
              (let* ((original-font (or (symbol-value var)
                                        (face-font face t)
                                        (with-temp-buffer (face-font face))))
                     (font (rmcs-normalize-font original-font))
                     (dfont
                      (or (if-let* ((remap-font (alist-get var font-alist))
                                    (remap-xlfd (rmcs-normalize-font remap-font)))
                              remap-xlfd
                            (purecopy font))
                          (error "Could not decompose %s font" var))))
                (let* ((step      (if fixed-size-p 0 (* increment rmcs-font-increment)))
                       (orig-size (font-get font :size))
                       (new-size  (if fixed-size-p increment (+ orig-size step))))
                  (cond ((<= new-size 0)
                         (error "`%s' font is too small to be resized (%d)" var new-size))
                        ((= orig-size new-size)
                         (user-error "Could not resize `%s' for some reason" var))
                        ((setq changed t)
                         (unless (get var 'initial-value)
                           (put var 'initial-value original-font))
                         (font-put dfont :size new-size)
                         (set var dfont)))))))))
    (error
     (ignore-errors (rmcs-adjust-font-size nil))
     (signal (car e) (cdr e)))))

;;;###autoload
(defun rmcs-font-exists-p (font)
  "Return non-nil if FONT exists on this system."
  (declare (pure t) (side-effect-free t))
  (ignore-errors (find-font (rmcs-normalize-font font))))


;;
;;; Commands

;;;###autoload
(defun rmcs/reload-font ()
  "Reload your fonts, if they're set.
See `rmcs-init-fonts-h'."
  (interactive)
  (rmcs-init-fonts-h 'reload))

;;;###autoload
(defun rmcs/increase-font-size (count &optional increment)
  "Enlargens the font size across the current and child frames."
  (interactive "p")
  (rmcs-adjust-font-size (* count (or increment rmcs-font-increment))))

;;;###autoload
(defun rmcs/decrease-font-size (count &optional increment)
  "Shrinks the font size across the current and child frames."
  (interactive "p")
  (rmcs-adjust-font-size (* (- count) (or increment rmcs-font-increment))))

;;;###autoload
(defun rmcs/reset-font-size ()
  "Reset font size and `text-scale'.

Assuming it has been adjusted via `rmcs/increase-font-size' and
`rmcs/decrease-font-size', or `text-scale-*' commands."
  (interactive)
  (let (success)
    (when (and (boundp 'text-scale-mode-amount)
               (/= text-scale-mode-amount 0))
      (text-scale-set 0)
      (setq success t))
    (cond (rmcs-big-font-mode
           (message "Disabling `rmcs-big-font-mode'")
           (rmcs-big-font-mode -1)
           (setq success t))
          ((rmcs-adjust-font-size nil)
           (setq success t)))
    (unless success
      (user-error "The font hasn't been resized"))))

;;;###autoload
(define-minor-mode rmcs-big-font-mode
  "Globally resizes your fonts for streams, screen-sharing or presentations.

Uses `rmcs-big-font' if its set, otherwise uses `rmcs-font' (falling back to
your system font).

Also resizees `rmcs-variable-pitch-font' and `rmcs-serif-font'."
  :init-value nil
  :lighter " BIG"
  :global t
  (if rmcs-big-font
      ;; Use `rmcs-big-font' in lieu of `rmcs-font'
      (rmcs-adjust-font-size
       (when rmcs-big-font-mode
         (font-get (rmcs-normalize-font rmcs-big-font) :size))
       t `((rmcs-font . ,rmcs-big-font)))
    ;; Resize the current font
    (rmcs-adjust-font-size (if rmcs-big-font-mode rmcs-big-font-increment))))
