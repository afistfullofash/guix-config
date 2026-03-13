(in-package :stumpwm-mode-line)

;;
;; Clear
;; 
(defun mode-line-clear-foreground ()
  (ml-foreground-color-opener :mode-line-fg))

(defun mode-line-clear-background ()
  (ml-background-color-opener :mode-line-bg))

(defun mode-line-clear-changes ()
  (concatenate 'string
  	       (mode-line-clear-foreground)
  	       (mode-line-clear-background)))
