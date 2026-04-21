;;; -*- Mode: Lisp -*-
;;
;; (load #P"/home/natalie/src/guix-config/affa-guix-config/files/stumpwm/themeing/stumpwm-themeing.asd")
;; (asdf:load-system :stumpwm-themeing)
;; 

(asdf:defsystem :stumpwm-themeing
  :name "StumpWM Themeing"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "A simple themeing system for StumpWM"
  :depends-on (#:stumpwm
	       #:stumpwm-utils
	       #:trivia)
  :serial t
  :components ((:file "package")
               (:file "theme")
	       (:module "themes"
		:components 
			    ((:file "default")
			     (:file "gruvbox")
			     (:file "dracula")
			     (:file "spacemacs")
			     (:file "catppuccin")))))
