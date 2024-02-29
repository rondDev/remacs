;;; lisp/lib/cache.el -*- lexical-binding: t; -*-

;; This little library abstracts the process of writing arbitrary elisp values
;; to a 2-tiered file store (in `rmcs-store-dir'/`rmcs-store-location').

(defvar rmcs-store-dir (concat rmcs-data-dir "store/")
  "Directory to look for and store data accessed through this API.")

(defvar rmcs-store-persist-alist ()
  "An alist of alists, containing lists of variables for the rmcs cache library
to persist across Emacs sessions.")

(defvar rmcs-store-location "default"
  "The default location for cache files. This symbol is translated into a file
name under `pcache-directory' (by default a subdirectory under
`rmcs-store-dir'). One file may contain multiple cache entries.")

(defvar rmcs--store-table (make-hash-table :test 'equal))

(defun rmcs-save-persistent-store-h ()
  "Hook to persist `rmcs-store's storage when Emacs is killed."
  (let (locations)
    ;; Persist `rmcs-store-persist-alist'
    (dolist (alist (butlast rmcs-store-persist-alist 1))
      (cl-loop with location = (car alist)
               for var in (cdr alist)
               do (rmcs-store-put var (symbol-value var) nil location 'noflush)
               and do (cl-pushnew location locations :test #'equal)))
    ;; Clean up expired entries,
    (dolist (location (rmcs-files-in rmcs-store-dir :relative-to rmcs-store-dir))
      (maphash (lambda (key val)
                 (when (rmcs--store-expired-p key val)
                   (cl-pushnew location locations :test #'equal)
                   (rmcs--store-rem key location 'noflush)))
               (rmcs--store-init location)))
    (mapc #'rmcs--store-flush locations)))
(add-hook 'kill-emacs-hook #'rmcs-save-persistent-store-h)


;;
;;; Library

;;;###autoload
(defun rmcs-store-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).
This populates these variables with cached values, if one exists, and saves them
to file when Emacs quits. This cannot persist buffer-local variables."
  (cl-check-type location string)
  (dolist (var variables)
    (when (rmcs-store-member-p var location)
      (set var (rmcs-store-get var location))))
  (setf (alist-get location rmcs-store-persist-alist)
        (append variables (alist-get location rmcs-store-persist-alist))))

;;;###autoload
(defun rmcs-store-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol).
Variables to persist are recorded in `rmcs-store-persist-alist'. Does not affect
the actual variables themselves or their values."
  (cl-check-type location string)
  (if variables
      (setf (alist-get location rmcs-store-persist-alist)
            (cl-set-difference (cdr (assq location rmcs-store-persist-alist))
                               variables))
    (delq! location rmcs-store-persist-alist 'assoc)))

(defun rmcs--store-init (&optional location)
  (cl-check-type location (or null string))
  (let ((location (or location rmcs-store-location)))
    (or (gethash location rmcs--store-table)
        (let* ((file-name-handler-alist nil)
               (location-path (expand-file-name location rmcs-store-dir)))
          (if (file-exists-p location-path)
              (puthash location
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (setq buffer-file-coding-system 'binary)
                         (insert-file-contents-literally location-path)
                         (read (current-buffer)))
                       rmcs--store-table)
            (puthash location (make-hash-table :test 'equal)
                     rmcs--store-table))))))

(defun rmcs--store-expired-p (key data)
  (let ((ttl (car data)))
    (cond ((functionp ttl)
           (not (funcall ttl key data)))
          ((consp ttl)
           (time-less-p ttl (current-time))))))

(defun rmcs--store-flush (location)
  "Write `rmcs--store-table' to `rmcs-store-dir'."
  (let ((file-name-handler-alist nil)
        (coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (let* ((location (or location rmcs-store-location))
           (data (rmcs--store-init location)))
      (make-directory rmcs-store-dir 'parents)
      (with-temp-file (expand-file-name location rmcs-store-dir)
        (prin1 data (current-buffer)))
      data)))


;;;###autoload
(defun rmcs-store-get (key &optional location default-value noflush)
  "Retrieve KEY from LOCATION (defaults to `rmcs-store-location').
If it doesn't exist or has expired, DEFAULT_VALUE is returned."
  (let ((data (gethash key (rmcs--store-init location) default-value)))
    (if (not (or (eq data default-value)
                 (rmcs--store-expired-p key data)))
        (cdr data)
      (rmcs-store-rem key location noflush)
      default-value)))

;;;###autoload
(defun rmcs-store-put (key value &optional ttl location noflush)
  "Set KEY to VALUE in the store at LOCATION.
KEY can be any lisp object that is comparable with `equal'. TTL is the duration
(in seconds) after which this cache entry expires; if nil, no cache expiration.
LOCATION is the super-key to store this cache item under. It defaults to
`rmcs-store-location'."
  (cl-check-type ttl (or null integer function))
  (puthash key (cons (if (integerp ttl)
                         (time-add (current-time) ttl)
                       ttl)
                     value)
           (rmcs--store-init location))
  (unless noflush
    (rmcs--store-flush location)))

;;;###autoload
(defun rmcs-store-rem (key &optional location noflush)
  "Clear a cache LOCATION (defaults to `rmcs-store-location')."
  (remhash key (rmcs--store-init location))
  (unless noflush
    (rmcs--store-flush location)))

;;;###autoload
(defun rmcs-store-member-p (key &optional location)
  "Return t if KEY in LOCATION exists.
LOCATION defaults to `rmcs-store-location'."
  (let ((nil-value (format "--nilvalue%s--" (current-time))))
    (not (equal (rmcs-store-get key location nil-value)
                nil-value))))

;;;###autoload
(defun rmcs-store-clear (&optional location)
  "Clear the store at LOCATION (defaults to `rmcs-store-location')."
  (let* ((location (or location rmcs-store-location))
         (path (expand-file-name location rmcs-store-dir)))
    (remhash location rmcs--store-table)
    (when (file-exists-p path)
      (delete-file path)
      t)))
