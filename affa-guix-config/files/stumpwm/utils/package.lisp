(defpackage :stumpwm-utils
	    (:use :cl)
	    (:export
	     ;; Filesystem
	     #:mkdir-p
	     #:xdg-state-home
	     ;; ui
	     #:make-percent-bar	  
	     #:toggle-modeline-all-screens
	     #:reload-modeline
	     ;; Utils
	     #:make-program-binding
	     #:trimmed-shell-command

	     #:define-minor-mode-safe))
