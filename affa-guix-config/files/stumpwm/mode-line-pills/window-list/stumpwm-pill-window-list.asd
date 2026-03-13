;;; -*- Mode: Lisp -*-
;; (load #P"/home/natalie/src/guix-config/affa-guix-config/files/stumpwm/mode-line-pills/email/email.asd")
;; (asdf:load-system :stumpwm-pill-email)

(asdf:defsystem :stumpwm-pill-window-list
  :name "StumpWM Email Pill"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "A pill which displays the window list with formatting applied between windows"
  ;; :depends-on (#:stumpwm
  ;; 	       #:stumpwm-themeing)
  :serial t
  :components ((:file "package")
	       (:file "pill")))
