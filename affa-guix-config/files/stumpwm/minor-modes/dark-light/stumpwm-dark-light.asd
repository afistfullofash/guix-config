;;; -*- Mode: Lisp -*-
;; (load #P"/home/natalie/src/guix-config/affa-guix-config/files/stumpwm/mode-line/stumpwm-mode-line.asd")
;; (asdf:load-system :stumpwm-mode-line)

(asdf:defsystem :stumpwm-dark-light
  :name "StumpWM Dark-Light Minor Mode"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "A minor mode which sets the stumpwm theme based on the System Dark-Light mode setting using Darkman"
  :depends-on (#:stumpwm
	       #:trivia
	       #:stumpwm-utils
	       #:stumpwm-themeing)
  :serial t
  :components ((:file "package")
	       (:file "dark-light")))
