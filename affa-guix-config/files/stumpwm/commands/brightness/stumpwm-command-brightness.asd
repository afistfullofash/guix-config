;;; -*- Mode: Lisp -*-
;; (load #P"/home/natalie/src/guix-config/affa-guix-config/files/stumpwm/mode-line-pills/email/email.asd")
;; (asdf:load-system :stumpwm-pill-email)

(asdf:defsystem :stumpwm-command-brightness
  :name "StumpWM Brightness Commands"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "Commands for controlling the brightness with Brillo"
  ;; :depends-on (#:stumpwm
  ;; #:stumpwm-utils
  ;; 	       #:stumpwm-themeing)
  :serial t
  :components ((:file "package")
	       (:file "brightness")))
