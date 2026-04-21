;; TODO: Add functionality to listen for dark-light mode settings instead of triggering only
(in-package :stumpwm-dark-light)

(defun get-mode ()
  "Gets the dark-light mode from darkman"
  (stumpwm-utils:trimmed-shell-command "darkman get"))

(defun set-mode (mode)
  (if (not (trivia:match mode
	     ("dark" t)
	     ("light" t)
	     (_ nil)))

      (stumpwm:message
       (format
	nil
	"Dark Light: ~a is not either \"light\" or \"dark\"" mode))
      
      (progn
  	(stumpwm:run-shell-command (format nil "darkman set ~a" mode))

  	(stumpwm-themeing:set-theme (trivia:match mode
						  ("dark" :dracula)
						  ("light" :catppuccin-latte)))

  	(stumpwm-utils:toggle-modeline-all-screens)
  	(stumpwm-utils:toggle-modeline-all-screens))))

(defun toggle-mode ()
  (set-mode (trivia:match (get-mode)
			 ("dark" "light")
			 ("light" "dark"))))

(stumpwm:defcommand dark-light-toggle-mode () ()
  (toggle-mode))

(stumpwm-utils:define-minor-mode-safe
 (stumpwm:define-minor-mode dark-light-minor-mode () ()
			    (:scope :screen)
			    (:interactive nil)
			    (:make-hooks t)))

(defmethod update-instance-for-different-class :after
	   (prev (obj dark-light-minor-mode) &rest rest)
  (declare (ignore prev rest))  
  (set-mode (get-mode)))


(defvar *dark-light-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "t") "dark-light-toggle-mode")
    m))
