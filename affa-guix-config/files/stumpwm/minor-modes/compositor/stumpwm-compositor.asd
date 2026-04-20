;;; -*- Mode: Lisp -*-
(asdf:defsystem :stumpwm-compositor
		:name "StumpWM Compositor"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "A Minor Mode for interacting with the compositor"
  :depends-on (#:stumpwm
	       #:clx)
    
  :serial t
  :components ((:file "package")
               (:file "compositor")))
