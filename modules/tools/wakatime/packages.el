;; -*- no-byte-compile: t; -*-
;;; tools/wakatime/packages.el


(when (modulep! :tools wakatime)
  (package! wakatime-mode :pin "25fb775178d16decb818b75f32fd23301c0f5da0"))
