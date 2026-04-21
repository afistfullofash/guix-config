(defpackage :stumpwm-mode-line
	    (:use :cl)
	    (:export
	     ;; Pills
	     #:light-pill
	     #:medium-pill
	     #:dark-pill

	     #:focus-pill
		     
	     #:low-priority-pill
	     #:medium-priority-pill
	     #:high-priority-pill

	     #:text-by-range-pill
	     #:priority-by-range-pill

	     #:space-pill

	     #:error-message-pill))
