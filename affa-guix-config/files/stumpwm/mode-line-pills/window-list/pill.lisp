(in-package :stumpwm-pill-window-list)

(stumpwm:add-screen-mode-line-formatter #\M 'window-list-pill)
(defun window-list-pill (ml)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^~}"
       	  (let ((base-fmt
		 (mapcar (lambda (w)
			   (stumpwm:format-with-on-click-id 
			    (let ((str (stumpwm:format-expand stumpwm:*window-formatters*
						      stumpwm:*window-format*
						      w)))
			      (if (eq w (stumpwm:current-window))
				  (stumpwm::fmt-highlight (format nil " ~a " str))
				(format nil "~a|" str)))
			    :ml-on-click-focus-window
			    (stumpwm::window-id w)))
			 (stumpwm::sort1 (stumpwm::head-windows
					  (stumpwm::mode-line-current-group ml)
					  (stumpwm::mode-line-head ml))
					 #'< :key #'stumpwm:window-number))))
       	    ;; Reparse strings and remove the | from the element before the active window
       	    (if (> (length base-fmt) 0)
       		(loop for win-fmt-el from 1 to (- (length base-fmt) 1)
       		      do (if (search "^r^(:on-click-end)" (nth win-fmt-el base-fmt))
       			     (let* ((pos (- win-fmt-el 1))
       				    (pos-str (nth pos base-fmt)))
       			       (setf (nth pos base-fmt)
       				     (cl-ppcre:regex-replace-all "\\|\\^\\(:on-click-end\\)"
       								 pos-str
       								 "\^\(\:on-click-end\)"))))))
       	    base-fmt)))
