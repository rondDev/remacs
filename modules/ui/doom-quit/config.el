;;; ui/rmcs-quit/config.el -*- lexical-binding: t; -*-

(defvar +rmcs-quit-messages
  `(;; from Rmcs 1
    "Please don't leave, there's more demons to toast!"
    "Let's beat it -- This is turning into a bloodbath!"
    ,(format "I wouldn't leave if I were you. %s is much worse."
             (if (member system-type '(ms-dos-windows-nt cygwin))
                 "DOS"
               "UNIX"))
    "Don't leave yet -- There's a demon around that corner!"
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; from Portal
    "Thank you for participating in this Aperture Science computer-aided enrichment activity."
    "You can't fire me, I quit!"
    "I don't know what you think you are doing, but I don't like it. I want you to stop."
    "This isn't brave. It's murder. What did I ever do to you?"
    "I'm the man who's going to burn your house down! With the lemons!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    ;; Custom
    "(setq nothing t everything 'permitted)"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "It's not like I'll miss you or anything, b-baka!"
    "Wake up, Mr. Stallman. Wake up and smell the ashes."
    "You are *not* prepared!"
    "Please don't go. The drones need you. They look up to you.")
  "A list of quit messages, picked randomly by `+rmcs-quit'. Taken from
http://rmcs.wikia.com/wiki/Quit_messages and elsewhere.")

(defun +rmcs-quit-fn (&rest _)
  (rmcs-quit-p
   (format "%s  %s"
           (propertize (nth (random (length +rmcs-quit-messages))
                            +rmcs-quit-messages)
                       'face '(italic default))
           "Really quit Emacs?")))

;;
(setq confirm-kill-emacs #'+rmcs-quit-fn)
