;;; -*- Mode: Lisp -*-
;; (load #P"/home/natalie/src/guix-config/affa-guix-config/files/stumpwm/mode-line-pills/email/email.asd")
;; (asdf:load-system :stumpwm-pill-email)

(asdf:defsystem :stumpwm-pill-temperature
  :name "StumpWM Temperature Pill"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "A pill which displays system temperature"
  :depends-on (#:stumpwm
	       #:stumpwm-mode-line)
  :serial t
  :components ((:file "package")
	       (:file "pill")))
