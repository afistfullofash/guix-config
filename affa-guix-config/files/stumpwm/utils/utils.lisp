(in-package :stumpwm-utils)

(defun trimmed-shell-command (command)
  (string-trim '(#\Space #\Newline #\Tab #\Linefeed #\Return)
       	       (stumpwm:run-shell-command command t)))


;; ** Keybinding Macros
(defmacro make-program-binding (program-name window-class &optional alias)
  "Create run-or-raise and run-or-pull commands for program-name
  window-class is the windows-class
  Also add keybinding to the commands. 
  C-keybinding r calls run-or-raise
  C-keybinding p calls run-or-pull
  C-keybinding n creates a new instance of the program"
  (if (not alias)
      (setf alias program-name))
  `(progn
     (defvar ,(intern (format nil "*~a-map*" alias)) nil)

     (defcommand ,(intern (format nil "~a" alias)) () () (run-shell-command ,program-name))
     
     (defcommand ,(intern (format nil "run-or-raise-~a" alias)) () ()
		 (run-or-raise ,program-name '(:class ,window-class)))
     
     (defcommand ,(intern (format nil "run-or-pull-~a" alias)) () ()
		 (run-or-pull ,program-name '(:class ,window-class)))
     
     (stumpwm::fill-keymap ,(intern (format nil "*~a-map*" alias))
  			   (kbd "p") ,(format nil "run-or-pull-~a" alias)
  			   (kbd "r") ,(format nil "run-or-raise-~a" alias)
  			   (kbd "n") ,(format nil "~a" alias))))


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
