;;; lisp/init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; :core is now treated like a normal module, and this is its (temporary) init
;; file, which will be removed once we've resolved our `use-package' dependency
;; (which will soon be moved to its own module), then these will be returned to
;; the profile init file.
;;
;;; Code:

(rmcs-require 'rmcs-keybinds)
(rmcs-require 'rmcs-ui)
(rmcs-require 'rmcs-projects)
(rmcs-require 'rmcs-editor)

;;; init.el ends here
