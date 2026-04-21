(in-package :stumpwm-utils)

(defun trimmed-shell-command (command)
  (string-trim '(#\Space #\Newline #\Tab #\Linefeed #\Return)
       	       (stumpwm:run-shell-command command t)))



;; ** Keybinding Macros
(defmacro make-program-binding (program-name window-class &optional program-alias)
  "Create run-or-raise and run-or-pull commands for program-name
  window-class is the windows-class
  Also add keybinding to the commands. 
  C-keybinding r calls run-or-raise
  C-keybinding p calls run-or-pull
  C-keybinding n creates a new instance of the program"
  (let* ((alias (or program-alias program-name))
	 
	 (run-name (format nil "~a" alias))
	 (run-cmd (intern run-name))

	 (raise-name (format nil "run-or-raise-~a" alias))
	 (raise-cmd (intern raise-name))

	 (pull-name (format nil "run-or-pull-~a" alias))
	 (pull-cmd (intern pull-name)))
    `(let ((keymap (stumpwm:make-sparse-keymap)))

       (stumpwm:defcommand ,run-cmd () ()
	 (stumpwm:run-shell-command ,program-name))
       
       (stumpwm:defcommand ,raise-cmd () ()
	 (stumpwm:run-or-raise ,program-name '(:class ,window-class)))
       
       (stumpwm:defcommand ,pull-cmd () ()
	 (stumpwm:run-or-pull ,program-name '(:class ,window-class)))

       (stumpwm:define-key keymap (stumpwm:kbd "p") ,pull-name)
       (stumpwm:define-key keymap (stumpwm:kbd "r") ,raise-name)
       (stumpwm:define-key keymap (stumpwm:kbd "n") ,run-name)

       keymap)))


(defmacro define-minor-mode-safe (form)
  "When compiling within a guix environment we don't have a live system.
This causes the call to stumpwm:define-minor-mode to fail due to a call to stumpwm:sync-keys
This macro suppresses the compiling error"
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((old (and (fboundp 'stumpwm::sync-keys)
                       (symbol-function 'stumpwm::sync-keys))))
         (unwind-protect
             (progn
               (when old
                 (setf (symbol-function 'stumpwm::sync-keys)
                       (lambda ()
                         (when (and stumpwm::*display*
                                    stumpwm::*screen-list*)
                           (funcall old)))))
               ,form)
           (when old
             (setf (symbol-function 'stumpwm::sync-keys) old)))))))
