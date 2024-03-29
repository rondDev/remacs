;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(use-package! plantuml-mode
  :commands plantuml-download-jar
  :init
  (setq plantuml-jar-path (concat rmcs-data-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  :config
  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0)

  (setq plantuml-default-exec-mode
        (cond ((file-exists-p plantuml-jar-path) 'jar)
              ((executable-find "plantuml") 'executable)
              (plantuml-default-exec-mode))))


(use-package! flycheck-plantuml
  :when (modulep! :checkers syntax)
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup)
  (when (eq plantuml-default-exec-mode 'executable)
    ;; Surprisingly, this works, even though flycheck-plantuml specifies -Djava.awt...
    (setq-default flycheck-plantuml-executable plantuml-executable-path)))


(after! ob-plantuml
  ;; HACK Force ob-plantuml to use `plantuml-mode''s building mechanism, which
  ;;      is more sophisticated.
  (advice-add #'org-babel-execute:plantuml
              :override #'+plantuml-org-babel-execute:plantuml-a)
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))
