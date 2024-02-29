;;; lisp/cli/profiles.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

(defvar rmcs-cli-known-profiles-file
  (file-name-concat rmcs-cache-dir (format "profiles.%s.el" (or (car rmcs-profile) "@")))
  ;; REVIEW: Use `rmcs-profile-data-dir' in v3
  ;; (file-name-concat rmcs-profile-data-dir "known-profiles.el")
  "TODO")


;;
;;; rmcs profile ...

(defcli-stub! ((profile pf)) ())

(defcli-stub! (profile hash) ())

(defcli-stub! (profile new) ())

(defcli-stub! (profile archive) ())

(defcli-stub! (profile (remove rm)) ())

(defcli-stub! (profile (rename mv)) ())

(defcli-stub! (profile nuke) ())


;;
;;; rmcs profiles ...

(defcli! (profiles sync) ((reload? ("--reload")))
  "Synchronize your profiles with Rmcs."
  :benchmark t
  (let* ((old-profiles (rmcs-profiles-read rmcs-cli-known-profiles-file))
         (new-profiles (rmcs-profiles-autodetect))
         (load-file rmcs-profile-load-file)
         (version (rmcs-file-read load-file :by 'read :noerror t))
         (recreate? (or (not reload?) (rmcs-profiles-outdated-p))))
    (unless (file-exists-p load-file)
      (print! (warn "No profile loader found. Generating one..."))
      (print-group! (print! (start "Regenerating it...")))
      (setq recreate? t))
    (unless (equal (or version rmcs-version) rmcs-version)
      (print! (warn "Detected version mismatch in profile loader (%s != %s)"
                    version rmcs-version))
      (print! (start "Generating profile manifest..."))
      (setq recreate? t))
    (print-group!
      (if (not recreate?)
          (rmcs-log "Profiles are up-to-date!")
        (let* ((pred    (lambda (a b) (eq (car a) (car b))))
               (added   (seq-difference new-profiles old-profiles pred))
               (removed (seq-difference old-profiles new-profiles pred))
               (changed (cl-loop for profile in (seq-intersection new-profiles old-profiles pred)
                                 unless (equal (cdr profile)
                                               (alist-get (car profile) old-profiles))
                                 collect profile)))
          (when (or added removed changed recreate?)
            (print! (start "Synchronizing %d known profile%s...")
                    (length new-profiles)
                    (if (/= (length new-profiles) 1) "s" ""))
            (print-group!
              (dolist (p added)   (print! (item "Added %S") (car p)))
              (dolist (p removed) (print! (item "Removed %S") (car p)))
              (dolist (p changed) (print! (item "Changed %S") (car p)))
              (rmcs-file-write rmcs-cli-known-profiles-file (list new-profiles) :mode #o600)
              (rmcs-profiles-save new-profiles load-file)
              (print! (success "Regenerated profile loader: %s")
                      (path load-file)))))))))


;;
;;; Helpers



(provide 'rmcs-cli-profiles)
;;; profiles.el ends here
