(in-package :stumpwm-pill-email)

(defvar *ml-email-accounts*
  (list
   (list :name "old-account-fowarding"
	 :notmuch-search "tag:forwarded_tna and tag:unread"
	 :display-filter nil)
   (list :name "natalie-atkinson"
	 :notmuch-search "tag:natalieatkinson95 and tag:unread and not tag:promotions"
	 :display-filter nil)))

(defun ml-email-get-data ()
  (flet
      ((notmuch-count (tags)
	 (let ((raw-count (stumpwm-utils:trimmed-shell-command (format nil "notmuch count ~A" tags))))
	   (if (string= raw-count "")
	       nil
	       (parse-integer raw-count)))))
    (mapcar (lambda (account)
	      (append account
		      (list :email-count (notmuch-count (getf account :notmuch-search)))))
	    *ml-email-accounts*)))


(defun email-space-pill (email-count)
  (if email-count
      (stumpwm-mode-line:space-pill
       (stumpwm-mode-line:text-by-range-pill email-count
  				  (write-to-string email-count)
  				  5 10 15))
      (stumpwm-mode-line:space-pill (stumpwm-mode-line:error-message-pill "Error"))))

(defun in-work-hours (work-times)
  ;; Extract relevent data
  (let* ((workdays (getf work-times :days))
     	 (workhours (getf work-times :hours))
     	 ;; Make this clearer
     	 (start-time (car workhours))
     	 (end-time (cadr workhours))
    	 ;; Make the current day and hour nicer  		  
     	 (current-time (local-time:now))
     	 (day-of-week (local-time:timestamp-day-of-week current-time))
     	 (hour-of-day (local-time:timestamp-hour current-time)))
    (if (and (member day-of-week workdays)
     	     (and (>= hour-of-day start-time)
     		  (< hour-of-day end-time)))
     	nil
	t)))

(stumpwm:add-screen-mode-line-formatter #\E 'email-pill)
(defun email-pill (ml)
  (declare (ignorable ml))
  (labels ((is-account-displayable (account)
	     (let ((display-filter (getf account :display-filter)))
	       (if (and display-filter (functionp display-filter))
		   (funcall display-filter)
		   nil)))
	   (add-count-to-string (mode-line-string account)
	     (concatenate 'string
			  mode-line-string
			  (stumpwm-mode-line:space-pill (getf account :email-count)))))
    (let* ((email-account-information (ml-email-get-data))
	   (display-accounts (remove-if
			      #'is-account-displayable
			      email-account-information))
	   (account-display-string (reduce
				    #'add-count-to-string
				    display-accounts
				    :initial-value "")))
      (if (and
	   (> (reduce (lambda (count account)
			(+ count (getf account :email-count)))
		      display-accounts
		      :initial-value 0)
	      0)
	   (not (string= "" account-display-string)))
	  (stumpwm-mode-line:medium-pill
	   (concatenate
	    'string
	    account-display-string
	    " Emails "))
	  ""))))
