;; -*- no-byte-compile: t; -*-
;;; ui/rmcs-dashboard/test/test-rmcs-dashboard.el

(require 'core-projects)
(require 'projectile)
(require! :ui rmcs-dashboard)

(describe "ui/rmcs-dashboard"
  :var (default-directory projectile-enable-caching)
  (before-all
    (setq projectile-enable-caching nil
          rmcs-fallback-buffer-name +rmcs-dashboard-name))

  (before-each (projectile-mode +1))
  (after-each  (projectile-mode -1))

  (describe "get-pwd"
    :var (+rmcs-dashboard--last-cwd)
    (before-each
      (setq +rmcs-dashboard--last-cwd rmcs-core-dir
            default-directory rmcs-core-dir))
    (it "returns the current directory when policy is nil"
      (let (+rmcs-dashboard-pwd-policy)
        (expect (+rmcs-dashboard--get-pwd) :to-equal default-directory)))
    (it "returns a path if policy is a path"
      (let ((+rmcs-dashboard-pwd-policy "~"))
        (expect (+rmcs-dashboard--get-pwd) :to-equal (expand-file-name "~"))))
    (it "returns return value of policy as a function"
      (let ((+rmcs-dashboard-pwd-policy (lambda (x) "x")))
        (expect (+rmcs-dashboard--get-pwd) :to-equal "x")))
    (it "returns last cwd if policy is 'last"
      (let ((+rmcs-dashboard-pwd-policy 'last))
        (expect (+rmcs-dashboard--get-pwd) :to-equal rmcs-core-dir)))
    (it "returns last project if policy is 'last-project"
      (let ((+rmcs-dashboard-pwd-policy 'last-project))
        (expect (+rmcs-dashboard--get-pwd) :to-equal rmcs-emacs-dir))))

  (describe "dashboard-p"
    (it "changes the fallback buffer to the dashboard buffer"
      (expect (+rmcs-dashboard-p (rmcs-fallback-buffer))))))
