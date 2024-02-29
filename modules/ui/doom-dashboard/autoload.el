;;; ui/rmcs-dashboard/autoload.el -*- lexical-binding: t; -*-

(defun +rmcs-dashboard--help-echo ()
  (when-let* ((btn (button-at (point)))
              (msg (button-get btn 'help-echo)))
    (message "%s" msg)))

;;;###autoload
(defun +rmcs-dashboard/open (frame)
  "Switch to the dashboard in the current window, of the current FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (switch-to-buffer (rmcs-fallback-buffer))
    (+rmcs-dashboard-reload t)))

;;;###autoload
(defun +rmcs-dashboard/forward-button (n)
  "Like `forward-button', but don't wrap."
  (interactive "p")
  (forward-button n nil)
  (+rmcs-dashboard--help-echo))

;;;###autoload
(defun +rmcs-dashboard/backward-button (n)
  "Like `backward-button', but don't wrap."
  (interactive "p")
  (backward-button n nil)
  (+rmcs-dashboard--help-echo))
