(asdf:defsystem :stumpwm-utils
  :name "StumpWM Utils"
  :author "Natalie Atkinson <natalie.atkinson95@pm.me>"
  :version "0.0.1"
  :maintainer "Natalie Atkinson <natalie.atkinson95@pm.me>"
  ;; :license "GNU General Public License"
  :description "Utility functions for the rest of this config"
  ;; :depends-on (#:stumpwm
  ;; 	       #:stumpwm-themeing)
  :serial t
  :components ((:file "package")
	       (:file "utils")))
