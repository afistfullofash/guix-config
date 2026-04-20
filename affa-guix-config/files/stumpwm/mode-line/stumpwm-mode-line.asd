;;; -*- Mode: Lisp -*-
;; (load #P"/home/natalie/src/guix-config/affa-guix-config/files/stumpwm/mode-line/stumpwm-mode-line.asd")
;; (asdf:load-system :stumpwm-mode-line)

(asdf:defsystem :stumpwm-mode-line
  :name "StumpWM Mode Line Extensions"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "A set of mode line extensions for StumpWM"
  :depends-on (#:stumpwm
	       #:stumpwm-themeing)
  :serial t
  :components ((:file "package")
	       (:file "internal")
               (:file "background")
	       (:file "foreground")

	       (:file "clear")
	       (:file "color")
	       (:file "pill")))
