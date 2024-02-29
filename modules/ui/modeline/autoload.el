;;; ui/modeline/autoload/modeline.el -*- lexical-binding: t; -*-

(defvar +modeline--old-bar-height nil)
;;;###autoload
(defun +modeline-resize-for-font-h ()
  "Adjust the modeline's height when the font size is changed by
`rmcs/increase-font-size' or `rmcs/decrease-font-size'.

Meant for `rmcs-change-font-size-hook'."
  (unless +modeline--old-bar-height
    (setq +modeline--old-bar-height rmcs-modeline-height))
  (let ((default-height +modeline--old-bar-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (setq rmcs-modeline-height
          (if (> scale 0)
              (+ default-height (* scale rmcs-font-increment))
            default-height))))

;;;###autoload
(defun +modeline-update-env-in-all-windows-h (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (when (fboundp 'rmcs-modeline-update-env)
        (rmcs-modeline-update-env))
      (force-mode-line-update))))

;;;###autoload
(defun +modeline-clear-env-in-all-windows-h (&rest _)
  "Blank out version strings in all buffers."
  (unless (modulep! +light)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq rmcs-modeline-env--version
              (bound-and-true-p rmcs-modeline-load-string)))))
  (force-mode-line-update t))
