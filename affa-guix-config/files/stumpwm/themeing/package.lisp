(defpackage :stumpwm-themeing
	    (:use :cl)
	    (:export #:*themes*

		     #:add-theme
		     #:apply-theme
		     #:get-theme

		     #:set-key-seq-color

		     #:with-current-theme
		     #:get-color))
