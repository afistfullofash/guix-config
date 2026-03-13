(in-package :stumpwm-command-volume)

;; ** Volume
(setf pamixer:*allow-boost* t)  

(defun run-volume-command (command)
  "Run a command to modify the volume and show a message of the current volume setting"
  (let ((muted-message (make-percent-bar 0 "Volume: Muted"))
	(volume-message (make-percent-bar (pamixer:get-volume) "Volume")))
    (cond
      ;; Check if we are toggling muting
      ((equal command "pamixer-toggle-mute")
       (run-commands command))
      ;; If we are not mute
      ((not (pamixer:get-mute))
       (run-commands command))
      (t nil))
    (stumpwm:message (if (not (pamixer:get-mute))
			 volume-message
			 muted-message))))

(defcommand notify-volume-up () ()
  (run-volume-command "pamixer-volume-up"))

(defcommand notify-volume-down () ()
  (run-volume-command "pamixer-volume-down"))

(defcommand notify-volume-mute () ()
  (run-volume-command "pamixer-toggle-mute"))

(defcommand volume-control () ()
	    "Start volume control"
	    (run-or-raise "pavucontrol" '(:class "Pavucontrol")))
