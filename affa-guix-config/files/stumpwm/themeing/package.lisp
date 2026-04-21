(defpackage :stumpwm-themeing
	    (:use :cl)
	    (:export #:*themes*

		     #:add-theme
		     #:set-theme
		     #:get-theme

		     #:set-key-seq-color

		     #:with-current-theme
		     #:get-color

		     #:foreground-color
		     #:background-color))
