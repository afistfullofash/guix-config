(defpackage :stumpwm-logging
	    (:use :cl)
	    (:export #:*log-formatter*

		     #:file-logger
		     #:message-logger

		     #:*logging-levels*

		     #:log-message
		     
		     #:logging-minor-mode))
