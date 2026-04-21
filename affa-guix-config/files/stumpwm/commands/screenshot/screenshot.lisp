(in-package :stumpwm-command-screenshot)

;; ** Screenshots
(defun timestamp-string ()
  (local-time:format-timestring
   nil (local-time:now)
   :format '(:YEAR "-" (:MONTH 2) "-" :DAY "-" :SHORT-WEEKDAY "-" :HOUR12 "_" :MIN "_" :SEC "_" :AMPM)))

(defun screenshot-path ()
  (format nil "~a/Pictures/Screenshots/~a.png"
  	  (uiop:getenv "HOME")
  	  (timestamp-string)))

;; Setup bindings for less common aplications which would be opened then closed
(stumpwm:defcommand screenshot () ()
	    "Take a screenshot and save it to screenshot directory"
	    (let ((save-path (screenshot-path)))
	      (stumpwm:run-shell-command (format nil "maim ~a" save-path))))

(stumpwm:defcommand screenshot-select () ()
	    "Select a area for a screenshot and save it to screenshot directory"
	    (let ((save-path (screenshot-path)))
	      (stumpwm:run-shell-command (format nil "maim --select ~a" save-path))))
