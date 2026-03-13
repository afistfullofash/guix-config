(in-package :stumpwm-command-screenshot)

;; ** Screenshots
(defun timestamp-string ()
  (local-time:format-timestring
   nil (local-time:now)
   :format '(:YEAR "-" (:MONTH 2) "-" :DAY "-" :SHORT-WEEKDAY "-" :HOUR12 "_" :MIN "_" :SEC "_" :AMPM)))

(defun screenshot-path ()
  (format nil "~a/Pictures/Screenshots/~a.png"
  	  (getenv "HOME")
  	  (timestamp-string)))

;; Setup bindings for less common aplications which would be opened then closed
(defcommand screenshot () ()
	    "Take a screenshot and save it to screenshot directory"
	    (let ((save-path (screenshot-path)))
	      (run-shell-command (format nil "maim ~a" save-path))
	      (message (format #f "Saved Screenshot to: ~a" save-path))))

(defcommand screenshot-select () ()
	    "Select a area for a screenshot and save it to screenshot directory"
	    (let ((save-path (screenshot-path)))
	      (run-shell-command (format nil "maim --select ~a" save-path))
	      (message (format #f "Saved Screenshot to: ~a" save-path))))
