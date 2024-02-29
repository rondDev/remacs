;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/ivy/doctor.el

(dolist (module '(helm ido vertico))
  (when (rmcs-module-p :completion module)
    (error! "This module is incompatible with :completion %s; disable one or the other"
            module)))
