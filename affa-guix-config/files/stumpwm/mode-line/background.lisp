(in-package :stumpwm-mode-line)

;;
;; Background color
;; 
(defun ml-background-color-opener (color)
  "Given color get a string which sets the background color to the one selected"
  (stumpwm-themeing:background-color
   (stumpwm-themeing:get-color
    (stumpwm-themeing:with-current-theme)
    color)))

(defun ml-background-color (color msg)
  "Set the background color to color for the duration of msg"
  (ml-fmt-safe-change
   (concatenate 'string (ml-background-color-opener color) msg)))


;;
;; Preset Backgrounds
;; 
;; These are used for splitting the bar sections
;; They should follow becoming shades of the normal backgrounds
;; They do not correct the foreground
(defun ml-fmt-light-bg (msg)
  "Gets the current brightest background shade"
  (ml-background-color :red msg))

(defun ml-fmt-medium-bg (msg)
  "Gets the current medium background shade"
  (ml-background-color :custom-one msg))

(defun ml-fmt-dark-bg (msg)
  "Gets the current darkest background shade"
  (ml-background-color :custom-two msg))

;; These are for colored prioity needing colors
(defun ml-fmt-low-priority-bg (msg)
  "Gets a Ok Background Shade (normally green)"
  (ml-background-color :low msg))

(defun ml-fmt-medium-priority-bg (msg)
  "Gets a Warning background shade"
  (ml-background-color :medium msg))

(defun ml-fmt-high-priority-bg (msg)
  "Gets a Alert background shade"
  (ml-background-color :high msg))


(defun ml-fmt-background-color-by-range (message amount &optional (med 20) (hi 50) (crit 90) reverse)
  (flet ((past (n) (funcall (if reverse #'<= #'>=) amount n)))
	(let ((background-color (cond ((past crit) :red)
				      ((past hi) :yellow)
				      ((past med) :green)
				      (t 'identity))))
	  (ml-fmt-colors :black
			 background-color
			 message))))
