;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "0e8f25a8d8011328f2bf082232c720b24c2a12c2")
  (when (modulep! +forge)
    (package! forge :pin "3fc6c362b0162082317c128c9c3226529f6965ae")
    (package! code-review
      :recipe (:host github
               :repo "doomelpa/code-review"
               :files ("graphql" "code-review*.el"))
      :pin "2670a4beb6636e6ee596c5b7cb5e784cf33d5a98"))
  (package! magit-todos :pin "debb77b3589f2d83c8b43706edc1f8f90bf1ad91"))
