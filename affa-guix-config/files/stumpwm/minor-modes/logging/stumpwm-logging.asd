;;; -*- Mode: Lisp -*-
;;
;; (load #P"/home/natalie/src/guix-config/affa-guix-config/files/stumpwm/themeing/stumpwm-themeing.asd")
;; (asdf:load-system :stumpwm-themeing)
;; 

(asdf:defsystem :stumpwm-logging
  :name "StumpWM Themeing"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "A logging subsystem"
  :serial t
  :components ((:file "package")
               (:file "logging")))
