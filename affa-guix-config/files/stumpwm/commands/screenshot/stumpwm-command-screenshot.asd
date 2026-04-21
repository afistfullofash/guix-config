;;; -*- Mode: Lisp -*-

(asdf:defsystem :stumpwm-command-screenshot
  :name "StumpWM Screenshot Commands"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "Commands for capturing screenshots"
  :depends-on (#:stumpwm
	       #:local-time)
  :serial t
  :components ((:file "package")
	       (:file "screenshot")))
