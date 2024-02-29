;;; lisp/cli/info.el --- information about your Rmcs install -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

;; None yet!


;;
;;; Commands

(defcli! info
    ((format ("--lisp" "--json") "What format to dump info into")
     &context context)
  "Print detailed information about your config for bug reports."
  (with-temp-buffer
    (pcase format
      ("--json"
       (require 'json)
       (insert (json-encode (rmcs-info)))
       (json-pretty-print-buffer))
      ("--lisp"
       (pp (rmcs-info)))
      (_
       (insert (rmcs-info-string
                (if (rmcs-cli-context-pipe-p context :out t)
                    72
                  (rmcs-cli-context-width context))))))
    (print! "%s" (string-trim-right (buffer-string)))))

(provide 'rmcs-cli-info)
;;; info.el ends here
