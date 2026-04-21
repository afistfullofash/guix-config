(in-package :stumpwm-utils)

;; * User functions
(defun make-percent-bar (percent &optional title)
  "Return a string that represents a percent bar"
  (format nil "~a~%^B~3d%^b [^[^7*~a^]]"
          title
    	  percent
    	  (stumpwm::bar (min 100 percent) 50 #\# #\:)))

(defun toggle-modeline-all-screens ()
  "We almost always want to interact with the mode-line on all screens"
  (mapcar (lambda (head)
       	    (stumpwm:toggle-mode-line (stumpwm:current-screen) head))
       	  (stumpwm:screen-heads (stumpwm:current-screen))))

(defun reload-modeline ()
  "This runs toggle-modeline-all-screens twice so that we get the settings refreshed without dissapearing the mode-line"
  (toggle-modeline-all-screens)
  (toggle-modeline-all-screens))
