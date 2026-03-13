(in-package :stumpwm-mode-line)

(export '(light-pill
	  medium-pill
	  dark-pill
	  
	  low-priority-pill
	  medium-priority-pill
	  high-priority-pill

	  text-by-range-pill
	  priority-by-range-pill

	  space-pill

	  error-message-pill))
;;
;; Complete Color Sets
;; 
(defun light-pill (msg)
  "Set the foreground and background to a low bar"
  (ml-fmt-colors :low :bg msg))


(defun medium-pill (msg)
  "Set the foreground and background to a medium bar"
  (ml-fmt-colors :fg :custom-one msg))

(defun dark-pill (msg)
  "Set the foreground and background to a medium bar"
  (ml-fmt-colors :fg :custom-two msg))

(defun low-priority-pill (msg)
  "Set the foreground and background to a low bar"
  (ml-fmt-colors :light-fg :low msg))

(defun medium-priority-pill (msg)
  "Set the foreground and background to a low bar"
  (ml-fmt-colors :light-fg :medium msg))

(defun high-priority-pill (msg)
  "Set the foreground and background to a low bar"
  (ml-fmt-colors :light-fg :high msg))

(defun text-by-range-pill (amount message &optional (med 20) (hi 50) (crit 90) reverse)
  (ml-fmt-safe-change
   (concatenate 'string
    		(stumpwm:bar-zone-color amount med hi crit reverse)
    		message)))

(defun priority-by-range-pill (message amount &optional (med 20) (hi 50) (crit 90) reverse)
  (flet ((past (n) (funcall (if reverse #'<= #'>=) amount n)))
    (cond ((past crit) (high-priority-pill message))
	  ((past hi) (medium-priority-pill message))
	  ((past med) (low-priority-pill message))
	      (t (high-priority-pill "Error")))))

(defun space-pill (el)
  (format nil " ~a" el))

(defun error-message-pill (msg)
  (ml-foreground-color :red msg))
