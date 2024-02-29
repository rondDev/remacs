;;; lisp/lib/scratch.el -*- lexical-binding: t; -*-

(defvar rmcs-scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `rmcs-scratch-dir'.")

(defvar rmcs-scratch-dir (concat rmcs-data-dir "scratch")
  "Where to save persistent scratch buffers.")

(defvar rmcs-scratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar rmcs-scratch-buffers nil
  "A list of active scratch buffers.")

(defvar rmcs-scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put 'rmcs-scratch-current-project 'permanent-local t)

(defvar rmcs-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")


(defun rmcs--load-persistent-scratch-buffer (project-name)
  (setq-local rmcs-scratch-current-project
              (or project-name
                  rmcs-scratch-default-file))
  (let ((smart-scratch-file
         (expand-file-name (concat rmcs-scratch-current-project ".el")
                           rmcs-scratch-dir)))
    (make-directory rmcs-scratch-dir t)
    (when (file-readable-p smart-scratch-file)
      (message "Reading %s" smart-scratch-file)
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents smart-scratch-file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

;;;###autoload
(defun rmcs-scratch-buffer (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name
                          (format "*rmcs:scratch (%s)*" project-name)
                        "*rmcs:scratch*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer
        (or buffer (get-buffer-create buffer-name))
      (setq default-directory directory)
      (setq-local so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless buffer
          (rmcs--load-persistent-scratch-buffer project-name)
          (when (and (eq major-mode 'fundamental-mode)
                     (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) rmcs-scratch-buffers)
      (add-transient-hook! 'rmcs-switch-buffer-hook (rmcs-persist-scratch-buffers-h))
      (add-transient-hook! 'rmcs-switch-window-hook (rmcs-persist-scratch-buffers-h))
      (add-hook 'kill-buffer-hook #'rmcs-persist-scratch-buffer-h nil 'local)
      (run-hooks 'rmcs-scratch-buffer-created-hook)
      (current-buffer))))


;;
;;; Persistent scratch buffer

;;;###autoload
(defun rmcs-persist-scratch-buffer-h ()
  "Save the current buffer to `rmcs-scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (point (point))
        (mode major-mode))
    (with-temp-file
        (expand-file-name (concat (or rmcs-scratch-current-project
                                      rmcs-scratch-default-file)
                                  ".el")
                          rmcs-scratch-dir)
      (prin1 (list content
                   point
                   mode)
             (current-buffer)))))

;;;###autoload
(defun rmcs-persist-scratch-buffers-h ()
  "Save all scratch buffers to `rmcs-scratch-dir'."
  (setq rmcs-scratch-buffers
        (cl-delete-if-not #'buffer-live-p rmcs-scratch-buffers))
  (dolist (buffer rmcs-scratch-buffers)
    (with-current-buffer buffer
      (rmcs-persist-scratch-buffer-h))))

;;;###autoload
(defun rmcs-persist-scratch-buffers-after-switch-h ()
  "Kill scratch buffers when they are no longer visible, saving them to disk."
  (unless (cl-some #'get-buffer-window rmcs-scratch-buffers)
    (mapc #'kill-buffer rmcs-scratch-buffers)
    (remove-hook 'rmcs-switch-buffer-hook #'rmcs-persist-scratch-buffers-after-switch-h)))

;;;###autoload
(unless noninteractive
  (add-hook 'kill-emacs-hook #'rmcs-persist-scratch-buffers-h))


;;
;;; Commands

(defvar projectile-enable-caching)
;;;###autoload
(defun rmcs/open-scratch-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     (if same-window-p
         #'switch-to-buffer
       #'pop-to-buffer)
     (rmcs-scratch-buffer
      arg
      (cond ((eq rmcs-scratch-initial-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null rmcs-scratch-initial-major-mode)
             nil)
            ((symbolp rmcs-scratch-initial-major-mode)
             rmcs-scratch-initial-major-mode))
      default-directory
      (when project-p
        (rmcs-project-name))))))

;;;###autoload
(defun rmcs/switch-to-scratch-buffer (&optional arg project-p)
  "Like `rmcs/open-scratch-buffer', but switches to it in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (rmcs/open-scratch-buffer arg project-p 'same-window))

;;;###autoload
(defun rmcs/open-project-scratch-buffer (&optional arg same-window-p)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (rmcs/open-scratch-buffer arg 'project same-window-p))

;;;###autoload
(defun rmcs/switch-to-project-scratch-buffer (&optional arg)
  "Like `rmcs/open-project-scratch-buffer', but switches to it in the current
window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (rmcs/open-project-scratch-buffer arg 'same-window))

;;;###autoload
(defun rmcs/revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*rmcs:scratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (rmcs--load-persistent-scratch-buffer rmcs-scratch-current-project)
    (message "Reloaded scratch buffer")))

;;;###autoload
(defun rmcs/delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `rmcs-scratch-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory rmcs-scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name rmcs-scratch-dir)))
    (make-directory rmcs-scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " rmcs-scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))
