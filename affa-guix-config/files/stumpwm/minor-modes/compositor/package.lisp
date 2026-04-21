(defpackage :stumpwm-compositor
	    (:use :cl)
	    (:import-from :stumpwm-logging
			  :log-message)
	    (:export
	     #:toggle-window-dimming
	     #:undim-all-windows-on-stumpwm-message-removal
	     #:dim-window-on-stumpwm-message
	     
	     ;; #:enable-compositor
	     ;; #:disable-compositor
	     ;; #:toggle-compositor
	     ))
