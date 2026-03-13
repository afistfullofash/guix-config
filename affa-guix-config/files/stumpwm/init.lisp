;; #+TITLE: StumpWM Config
;; #+PROPERTY: header-args:lisp :tangle init.lisp :exports both :eval never
;; In general we are not allowing any variable setting or function calls outside of a defun. The only exected exception to this is the inital load underneath this and the finishing actions section which runs all the init-* defuns.
;; * Initial Config
;; Start slynk for debugging purposes first and then load in our required stumpwm contrib modules
(in-package :stumpwm-user)

(asdf:load-system :stumpwm-logging)
(stumpwm-logging:enable-logging)

(if *initializing*
    (progn
      (asdf:load-system :slynk)
      (slynk:create-server :port 1337
    			   :dont-close t)))

(asdf:load-system :stumpwm-themeing)
(asdf:load-system :stumpwm-dark-light)

(asdf:load-system :stumpwm-compositor)
      
(asdf:load-system :stumpwm-pill-cpu)
(asdf:load-system :stumpwm-pill-temperature)
(asdf:load-system :stumpwm-pill-email)
(asdf:load-system :stumpwm-pill-window-list)

(asdf:load-system :stumpwm-command-volume)
(asdf:load-system :stumpwm-command-brightness)
(asdf:load-system :stumpwm-command-screenshot)

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
       "%M^>%C%H"
       '(:eval (stumpwm-mode-line:focus-pill " %d ") "")
       '(:eval (if (equal (getenv "GUIX_HOME_SYSTEM_FORMAT") "laptop")
       		   ;; This looks weird if we don't add a double space after the battery
       		   " %B "))

       ;; '(:eval (multiple-value-bind
       ;; 		     (output error exit-code)
       ;; 		   (uiop:run-program "ping -c 1 8.8.8.8" :ignore-error-status t)
       ;; 		 (if (not (equal exit-code 0))
       ;; 		     (error-message-bar "Network Error"))))
       ))

(if nil
    (setf *screen-mode-line-format*      
	  (list
	   "%W^>%C%H"
	   '(:eval (stumpwm-mode-line:focus-pill " %d ") "")
	   '(:eval (if (equal (getenv "GUIX_HOME_SYSTEM_FORMAT") "laptop")
		       ;; This looks weird if we don't add a double space after the battery
		       " %B "))

	   ;; '(:eval (multiple-value-bind
	   ;; 		     (output error exit-code)
	   ;; 		   (uiop:run-program "ping -c 1 8.8.8.8" :ignore-error-status t)
	   ;; 		 (if (not (equal exit-code 0))
	   ;; 		     (error-message-bar "Network Error"))))
	   )))

(init-window-number-fixes)

(stumpwm-utils:toggle-modeline-all-screens)

;; *** Run init
(defvar *fonts* (list "-adobe-courier-normal-r-normal-*-14-*-*-*-m-*-*-*"
  		      ;; also bold
   		      "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-*-*"
   		      "-*-new century schoolbook-r-normal-*-*-*-*-*-*-*-*-*-*"
   		      "-*-times-*-*-*-*-*-*-*-*-*-*-*-*"))

(stumpwm-themeing:set-theme :dracula)
(set-font (nth 0 *fonts*))
(which-key-mode)

(setf stumpwm:*input-window-gravity* :center
      stumpwm:*message-window-gravity* :center
      
      stumpwm:*message-window-padding* 10
      stumpwm:*message-window-y-padding* 10)

(stumpwm-compositor:toggle-window-dimming)


;; * User functions
(defun make-percent-bar (percent &optional title)
  "Return a string that represents a percent bar"
  (format nil "~a~%^B~3d%^b [^[^7*~a^]]"
          title
    	  percent
    	  (stumpwm::bar (min 100 percent) 50 #\# #\:)))

(defun reload-rc-clean ()
  "Restart Slynk and reload source.
     This is needed if Sly updates while StumpWM is running"
  (slynk:stop-server 1337)
  (loadrc)
  (toggle-modeline-all-screens)
  (toggle-modeline-all-screens))

;; * Commands
;; ** Brightness
(defun show-screen-brightness ()
  (stumpwm:message (make-percent-bar
  		    (parse-integer (run-shell-command "sudo brillo -G" t) :junk-allowed t)	    
  		    "Screen Brightness")))

(defcommand screen-brightness-up () ()
	    "Increase the brightness of the screen"
	    (run-shell-command "sudo brillo -A 10")
	    (show-screen-brightness))

(defcommand screen-brightness-down () ()
	    "Decrease the brightness of the screen"
	    (run-shell-command "sudo brillo -U 10")
	    (show-screen-brightness))  

(defun show-keyboard-brightness ()
  (stumpwm:message (make-percent-bar
  		    (parse-integer (run-shell-command "sudo brillo -Gk" t) :junk-allowed t)
  		    "Keyboard Brightness")))

(defcommand keyboard-brightness-up () ()
	    "Increase the brightness of the keyboard"
	    (run-shell-command "sudo brillo -kA 10")
	    (show-keyboard-brightness))

(defcommand keyboard-brightness-down () ()
	    "Decrease the brightness of the keyboard"
	    (run-shell-command "sudo brillo -kU 10")
	    (show-keyboard-brightness))

;; ** Screenshots
(defun timestamp-string ()
  (local-time:format-timestring
   nil (local-time:now)
   :format '(:YEAR "-" (:MONTH 2) "-" :DAY "-" :SHORT-WEEKDAY "-" :HOUR12 "_" :MIN "_" :SEC "_" :AMPM)))

(defun screenshot-path ()
  (format nil "~a/Pictures/Screenshots/~a.png"
  	  (getenv "HOME")
  	  (timestamp-string)))

;; Setup bindings for less common aplications which would be opened then closed
(defcommand screenshot () ()
	    "Take a screenshot and save it to screenshot directory"
	    (let ((save-path (screenshot-path)))
	      (run-shell-command (format nil "maim ~a" save-path))
	      (message (format #f "Saved Screenshot to: ~a" save-path))))

(defcommand screenshot-select () ()
	    "Select a area for a screenshot and save it to screenshot directory"
	    (let ((save-path (screenshot-path)))
	      (run-shell-command (format nil "maim --select ~a" save-path))
	      (message (format #f "Saved Screenshot to: ~a" save-path))))
;; ** Volume
(setf pamixer:*allow-boost* t)  

(defun run-volume-command (command)
  "Run a command to modify the volume and show a message of the current volume setting"
  (let ((muted-message (make-percent-bar 0 "Volume: Muted"))
	(volume-message (make-percent-bar (pamixer:get-volume) "Volume")))
    (cond
      ;; Check if we are toggling muting
      ((equal command "pamixer-toggle-mute")
       (run-commands command))
      ;; If we are not mute
      ((not (pamixer:get-mute))
       (run-commands command))
      (t nil))
    (stumpwm:message (if (not (pamixer:get-mute))
			 volume-message
			 muted-message))))

(defcommand notify-volume-up () ()
  (run-volume-command "pamixer-volume-up"))

(defcommand notify-volume-down () ()
  (run-volume-command "pamixer-volume-down"))

(defcommand notify-volume-mute () ()
  (run-volume-command "pamixer-toggle-mute"))

(defcommand volume-control () ()
	    "Start volume control"
	    (run-or-raise "pavucontrol" '(:class "Pavucontrol")))

;; ** Theme
(defcommand toggle-theme () ()
	    "Toggle the system theme"
	    (toggle-system-themeing))
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
	    (stumpwm-utils:reload-modeline))
;; ** Misc
(defcommand user-switch-to-screen (screen-num) ((:number "Screen Number: "))
	    "Only works when there is a currently open window on the screen"
	    (select-window-by-number
	     (window-number
	      (car (stumpwm::head-windows
		    (current-group)
		    (nth screen-num (stumpwm::group-heads (current-group)))))))
	    (group-wake-up (current-group)))


;; 
;; * Keybindings
;; 
(set-prefix-key (kbd "C-t"))
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
		(define-key m (kbd "t") 'stumpwm-dark-light:*dark-light-map*)
		(define-key m (kbd "p") '*power-map*)
		(define-key m (kbd "r") "reload-rc-clean")
		(define-key m (kbd "v") "volume-control")
		m))
;; *** Program Map
(defparameter *program-map*
	      (let ((m (make-sparse-keymap)))
		(define-key m (kbd "f")
		  (stumpwm-utils:make-program-binding "firefox" "Firefox"))
		
		(define-key m (kbd "e")
		  (stumpwm-utils:make-program-binding "emacs" "Emacs" "emacs"))
		
		(define-key m (kbd "c")
		  (stumpwm-utils:make-program-binding "alacritty" "Alacritty"))
		
		(define-key m (kbd "p")
		  (stumpwm-utils:make-program-binding "keepassxc" "keepassxc"))
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
<<<<<<< HEAD
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "volume-mute")
=======
(defun init-top-map ()
  (define-key *top-map* (kbd "XF86AudioRaiseVolume") "notify-volume-up")
  (define-key *top-map* (kbd "XF86AudioLowerVolume") "notify-volume-down")
  (define-key *top-map* (kbd "XF86AudioMute") "notify-volume-mute")
>>>>>>> 08cbd86 (Clean up some stumpwm keybindings)

(define-key *top-map* (kbd "XF86MonBrightnessUp") "screen-brightness-up")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "screen-brightness-down")

<<<<<<< HEAD
(define-key *top-map* (kbd "XF86KbdBrightnessUp") "keyboard-brightness-up")
(define-key *top-map* (kbd "XF86KbdBrightnessDown") "keyboard-brightness-down")
=======
  (define-key *top-map* (kbd "XF86KbdBrightnessUp") "keyboard-brightness-up")
  (define-key *top-map* (kbd "XF86KbdBrightnessDown") "keyboard-brightness-down")

  (define-key *top-map* (kbd "Print") "screenshot"))
>>>>>>> 08cbd86 (Clean up some stumpwm keybindings)

(define-key *top-map* (kbd "Print") "screenshot")

;; Finally set some global things
(run-shell-command "brillo -S 100")
(run-shell-command "dex -a -s $XDG_CONFIG_HOME/autostart/")

(setf *startup-message* (format nil "Welcome Natalie!"))
