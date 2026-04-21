(in-package :stumpwm-command-brightness)

;; * Commands
;; ** Brightness
(defun show-screen-brightness ()
  (stumpwm:message (stumpwm-utils:make-percent-bar
  		    (parse-integer (stumpwm:run-shell-command "sudo brillo -G" t) :junk-allowed t)	    
  		    "Screen Brightness")))

(stumpwm:defcommand screen-brightness-up () ()
	    "Increase the brightness of the screen"
	    (stumpwm:run-shell-command "sudo brillo -A 10")
	    (show-screen-brightness))

(stumpwm:defcommand screen-brightness-down () ()
	    "Decrease the brightness of the screen"
	    (stumpwm:run-shell-command "sudo brillo -U 10")
	    (show-screen-brightness))  

(defun show-keyboard-brightness ()
  (stumpwm:message (stumpwm-utils:make-percent-bar
  		    (parse-integer (stumpwm:run-shell-command "sudo brillo -Gk" t) :junk-allowed t)
  		    "Keyboard Brightness")))

(stumpwm:defcommand keyboard-brightness-up () ()
	    "Increase the brightness of the keyboard"
	    (stumpwm:run-shell-command "sudo brillo -kA 10")
	    (show-keyboard-brightness))

(stumpwm:defcommand keyboard-brightness-down () ()
	    "Decrease the brightness of the keyboard"
	    (stumpwm:run-shell-command "sudo brillo -kU 10")
	    (show-keyboard-brightness))

