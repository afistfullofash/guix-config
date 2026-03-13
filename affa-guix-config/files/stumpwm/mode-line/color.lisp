(in-package :stumpwm-mode-line)

;;
;; Set Both
;; 
(defun ml-fmt-colors (foreground background message)
  (ml-fmt-safe-change (concatenate 'string
    				   (ml-foreground-color-opener foreground)
    				   (ml-background-color-opener background)
    				   message)))
;; 
;; Set internal stumpwm vars
;; 
(defun set-bar-med-color ()
  (setf stumpwm::*bar-med-color*
        (ml-foreground-color-opener :low)))

(defun set-bar-hi-color ()
  (setf stumpwm::*bar-hi-color*
        (ml-foreground-color-opener :medium)))

(defun set-bar-crit-color ()
  (setf stumpwm::*bar-crit-color*
        (ml-foreground-color-opener :high)))
