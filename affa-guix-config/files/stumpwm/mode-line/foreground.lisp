(in-package :stumpwm-mode-line)
;; *** Mode Line
;;
;; Foreground Colors
;; 
(defun ml-foreground-color-opener (color)
  "Given color get a string which sets the foreground color to the one selected"
  (stumpwm-themeing:foreground-color (stumpwm-themeing:get-color (stumpwm-themeing:with-current-theme) color)))

(defun ml-foreground-color (color msg)
  "Set the foreground color to color for the duration of msg"
  (ml-fmt-safe-change
   (concatenate 'string (ml-foreground-color-opener color) msg)))

;;
;; Preset foregrounds
;; 
(defun ml-fmt-low-priority-foreground (msg)
  "Set the foreground to a low priority message"
  (ml-foreground-color :low msg))

(defun ml-fmt-medium-priority-foreground (msg)
  "Set the foreground to a medium priority message"
  (ml-foreground-color :medium msg))

(defun ml-fmt-high-priority-foreground (msg)
  "Sets the foreground to a high priority message"
  (ml-foreground-color :high msg))

