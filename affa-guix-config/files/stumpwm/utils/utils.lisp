(in-package :stumpwm-utils)

(export '(trimmed-shell-command
	  make-percent-bar
	  make-program-binding

	  mkdir-p))

(defun trimmed-shell-command (command)
  (string-trim '(#\Space #\Newline #\Tab #\Linefeed #\Return)
       	       (stumpwm:run-shell-command command t)))

;; * User functions
(defun make-percent-bar (percent &optional title)
  "Return a string that represents a percent bar"
  (format nil "~a~%^B~3d%^b [^[^7*~a^]]"
          title
    	  percent
    	  (stumpwm::bar (min 100 percent) 50 #\# #\:)))

(defun mkdir-p (p)
  "Creates a directory and then returns its path"
  (ensure-directories-exist p)
  p)


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

