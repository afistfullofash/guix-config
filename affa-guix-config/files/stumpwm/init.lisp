;; #+TITLE: StumpWM Config
;; #+PROPERTY: header-args:lisp :tangle init.lisp :exports both :eval never
;; In general we are not allowing any variable setting or function calls outside of a defun. The only exected exception to this is the inital load underneath this and the finishing actions section which runs all the init-* defuns.
;; * Initial Config
;; Start slynk for debugging purposes first and then load in our required stumpwm contrib modules
(in-package :stumpwm-user)

(asdf:load-system :stumpwm-utils)

(defvar *logging-levels*
  (list "fatal"
	"error"
	"warn"
	"info"
	"debug"
	"trace"))

(defvar *original-logging-streams*
  (list :error stumpwm::*error-output*
	:standard stumpwm::*standard-output*
	:trace stumpwm::*trace-output*
	:debug stumpwm::*debug-stream*))

(defvar *log-formatter* 'file-logger)

(defparameter *stumpwm-debug-dir*
	      (let ((dir (merge-pathnames (stumpwm-utils:xdg-state-home) "stumpwm/")))
		(if (stumpwm-utils:mkdir-p dir)
		    dir
		  (progn
		    (stumpwm:message (format nil "Failed to use logging directory~%~a" dir)
				     dir)))))

(defparameter *stumpwm-log-file*
  (merge-pathnames *stumpwm-debug-dir* "stumpwm.log"))

(defparameter *stumpwm-oplog-file*
  (merge-pathnames *stumpwm-debug-dir* "stumpwm-events.log"))

(defun logging-to-debug-mapping (level)
  (cond ((string= level "fatal") 1)
	((string= level "error") 2)
	((string= level "warn") 3)
	((string= level "info") 4)
	((string= level "debug") 5)
	((string= level "trace") 6)))

(defun logging-enabled-for-level (level)
  (cond ((and (string= level "fatal")
	      (eq stumpwm:*debug-level* 1))
	 t)
	((and (string= level "error")
	      (eq stumpwm:*debug-level* 2))
	 t)
	((and (string= level "warn")
	      (eq stumpwm:*debug-level* 3))
	 t)
	((and (string= level "info")
	      (eq stumpwm:*debug-level* 4))
	 t)
	((and (string= level "debug")
	      (eq stumpwm:*debug-level* 5))
	 t)
	((and (string= level "trace")
	      (eq stumpwm:*debug-level* 6))
	 t)
	(t nil)))

(defun can-log (level)
  (and (stumpwm:minor-mode-enabled-p 'logging-minor-mode)
       (logging-enabled-for-level level)))

(defun file-logger (level log-line stream &rest extra-data)
  "A default formatter for the logger

This will output in the form:
12-32-05: info: stuff other stuff"
  (format stream
	  "~&~A: ~a: ~a "
	  (local-time:format-timestring nil (local-time:now))
	  level
	  log-line)
  (apply #'format stream "~a" extra-data))

(defun message-logger (level log-line stream &rest extra-data)
  (declare (ignorable stream))
  (let* ((fmt (format nil
		      "~a:  ~a"
		      level
		      log-line))
	 (args (apply #'format nil fmt extra-data))
	 (message-string (reduce (lambda (arg acc)
				   (concatenate 'string acc arg))
				 args)))
    (stumpwm:message message-string)))

(defun log-message (level fmt &rest args)
  (when (can-log level)
    (with-open-file (s *stumpwm-oplog-file*
		       :direction :output :if-does-not-exist :create :if-exists :append)
      (funcall *log-formatter* level fmt s args)
      (finish-output s))))

(defun enable-logging (&optional (level "info"))
  (if (not (member level *logging-levels* :test 'string=))
      (stumpwm:message "Logging Level not set to a valid option"))

  (setf *original-logging-streams*
	(list :error stumpwm::*error-output*
	      :standard stumpwm::*standard-output*
	      :trace stumpwm::*trace-output*
	      :debug stumpwm::*debug-stream*))
  
  (setf stumpwm:*debug-level* (logging-to-debug-mapping level))
  ;; Redirect *everything* to a file (stdout/stderr included).
  (stumpwm:redirect-all-output *stumpwm-log-file*))

(defun disable-logging ()
  (setf stumpwm::*error-output* (getf *original-logging-streams* :error)
	stumpwm::*standard-output* (getf *original-logging-streams* :standard)
	stumpwm::*trace-output* (getf *original-logging-streams* :trace)
	stumpwm::*debug-stream* (getf *original-logging-streams* :debug)))

(enable-logging)

(if *initializing*
    (progn
      (setf *startup-message* (format nil "Welcome Natalie!"))
      (asdf:load-system :slynk)
      (slynk:create-server :port 1337
    			   :dont-close t)))

(asdf:load-system :stumpwm-themeing)
(asdf:load-system :stumpwm-dark-light)
(asdf:load-system :stumpwm-logging)
(asdf:load-system :stumpwm-compositor)
      
(asdf:load-system :stumpwm-pill-cpu)
(asdf:load-system :stumpwm-pill-temperature)
(asdf:load-system :stumpwm-pill-email)
(asdf:load-system :stumpwm-pill-window-list)

(run-shell-command "darkman set dark")

;; ** Configuration
(defvar *show-mode-line-time* t)

(defun init-window-number-fixes ()
       ;;; When windows are desroyed window numbers are not synced
       ;;; 2kays <https://github.com/2kays> posted a solution on
       ;;; the TipsAndTricks section of the wiki
       ;;; This will repack window numbers every time a window is killed
  (stumpwm:add-hook stumpwm:*destroy-window-hook*
  		    #'(lambda (win)
			(declare (ignorable win))
			(stumpwm:repack-window-numbers))))

(setf *window-format* " %n %10c ")  
(setf *screen-mode-line-format*      
      (list
       "%M^>%C%H%E"
       '(:eval (stumpwm-mode-line:focus-pill" %d ") "")
       '(:eval (if (equal (getenv "GUIX_HOME_SYSTEM_FORMAT") "laptop")
       		   ;; This looks weird if we don't add a double space after the battery
       		   " %B "))

       ;; '(:eval (multiple-value-bind
       ;; 		     (output error exit-code)
       ;; 		   (uiop:run-program "ping -c 1 8.8.8.8" :ignore-error-status t)
       ;; 		 (if (not (equal exit-code 0))
       ;; 		     (error-message-bar "Network Error"))))
       ))

(init-window-number-fixes)

(stumpwm-utils:toggle-modeline-all-screens)

;; *** Run init
(defvar *fonts* (list "-adobe-courier-normal-r-normal-*-14-*-*-*-m-*-*-*"
  		      ;; also bold
   		      "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-*-*"
   		      "-*-new century schoolbook-r-normal-*-*-*-*-*-*-*-*-*-*"
   		      "-*-times-*-*-*-*-*-*-*-*-*-*-*-*"))

;; (stumpwm-themeing:apply-theme (stumpwm-themeing:get-theme 'dracula))
(set-font (nth 0 *fonts*))
(which-key-mode)

(setf stumpwm:*input-window-gravity* :center
      stumpwm:*message-window-gravity* :center
      
      stumpwm:*message-window-padding* 10
      stumpwm:*message-window-y-padding* 10)

;; ** System
 ;;; Shutdown and Reboot
(defcommand shutdown (confirm) ((:y-or-n "Confirm Shutdown "))
	    "Ask for the user to confirm before shutting down."
	    (if confirm
		(run-shell-command "sudo shutdown")))

(defcommand reboot (confirm) ((:y-or-n "Confirm Reboot "))
	    "Ask for the user to confirm before rebooting."
	    (if confirm
		(run-shell-command "sudo reboot")))

(defcommand reload-init (confirm) ((:y-or-n "Confirm Reloading init file "))
	    "Ask for the user to confirm before reloading init file."
	    (if confirm
		(reload-init t)))

(defcommand reload-mode-line-cmd () ()
	    "Runs reload-mode-line. This allows the themeing etc to be changed"
	    (reload-mode-line))
;; ** Misc
(defcommand user-switch-to-screen (screen-num) ((:number "Screen Number: "))
	    "Only works when there is a currently open window on the screen"
	    (select-window-by-number
	     (window-number
	      (car (stumpwm::head-windows
		    (current-group)
		    (nth screen-num (stumpwm::group-heads (current-group)))))))
	    (group-wake-up (current-group)))

;; * Keybindings
(set-prefix-key (kbd "C-t"))
;; ** Program Bindings
(stumpwm-utils:make-program-binding "firefox" "Firefox")

(stumpwm-utils:make-program-binding "alacritty" "Alacritty")

(stumpwm-utils:make-program-binding "emacs" "Emacs" "emacs")

(stumpwm-utils:make-program-binding "keepassxc" "keepassxc")

(stumpwm-utils:make-program-binding "steam" "steam")

(stumpwm-utils:make-program-binding "firefox -P work --class firefox-work" "firefox-work" "firefox-work")

(stumpwm-utils:make-program-binding "firefox -P media --class firefox-media" "firefox-media" "firefox-media")

;; ** Keymaps
;; *** System Map
 ;;; System Command Keymap
(defparameter *screenshot-map*
	      (let ((m (make-sparse-keymap)))
		(define-key m (kbd "f") "screenshot")
		(define-key m (kbd "s") "screenshot-select")
		m))

(defparameter *power-map*
	      (let ((m (make-sparse-keymap)))
		(define-key m (kbd "p") "shutdown")
		(define-key m (kbd "r") "reboot")
		m)) 

(defparameter *system-map*
	      (let ((m (make-sparse-keymap)))
		(define-key m (kbd "s") '*screenshot-map*)
		(define-key m (kbd "t") '*theme-map*)
		(define-key m (kbd "p") '*power-map*)
		(define-key m (kbd "r") "reload-rc-clean")
		(define-key m (kbd "v") "volume-control")
		m))
;; *** Program Map
(defparameter *program-map*
	      (let ((m (make-sparse-keymap)))
		(define-key m (kbd "f") '|*firefox-map*|)
		(define-key m (kbd "m") '|*firefox-media-map*|)
		(define-key m (kbd "e") '|*emacs-map*|)
		(define-key m (kbd "c") '|*alacritty-map*|)
		(define-key m (kbd "p") '|*keepassxc-map*|)
		(define-key m (kbd "s") '|*steam-map*|)
		m))
;; *** Root Map
(define-key *root-map* (kbd "0") "remove")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")

(define-key *root-map* (kbd "F1") "user-switch-to-screen 2")
(define-key *root-map* (kbd "F2") "user-switch-to-screen 1")
(define-key *root-map* (kbd "F3") "user-switch-to-screen 0")


(define-key *root-map* (kbd "p") '*program-map*)
(define-key *root-map* (kbd "s") '*system-map*)

;; *** Top Map
;; Handle Fn keys for systems like laptops etc
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "notify-volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "notify-volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "notify-volume-mute")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "screen-brightness-up")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "screen-brightness-down")

(define-key *top-map* (kbd "XF86KbdBrightnessUp") "keyboard-brightness-up")
(define-key *top-map* (kbd "XF86KbdBrightnessDown") "keyboard-brightness-down")

(define-key *top-map* (kbd "Print") "screenshot")

;; Finally set some global things
(run-shell-command "brillo -S 100")
(run-shell-command "darkman set dark")
(run-shell-command "dex -a -s $XDG_CONFIG_HOME/autostart/")
