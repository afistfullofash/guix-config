(in-package :stumpwm-command-volume)

;; ** Volume
(setf pamixer:*allow-boost* t)  

(defun run-volume-command (command)
  "Run a command to modify the volume and show a message of the current volume setting"
  (let ((muted-message (stumpwm-utils:make-percent-bar 0 "Volume: Muted"))
	(volume-message (stumpwm-utils:make-percent-bar (pamixer:get-volume) "Volume")))
    (cond
      ;; Check if we are toggling muting
      ((equal command "pamixer-toggle-mute")
       (stumpwm:run-commands command))
      ;; If we are not mute
      ((not (pamixer:get-mute))
       (stumpwm:run-commands command))
      (t nil))
    (stumpwm:message (if (not (pamixer:get-mute))
			 volume-message
			 muted-message))))

(stumpwm:defcommand volume-up () ()
		    (run-volume-command "pamixer-volume-up"))

(stumpwm:defcommand volume-down () ()
		    (run-volume-command "pamixer-volume-down"))

(stumpwm:defcommand volume-mute () ()
		    (run-volume-command "pamixer-toggle-mute"))

(stumpwm:defcommand volume-control () ()
	    "Start volume control"
	    (stumpwm:run-or-raise "pavucontrol" '(:class "Pavucontrol")))
