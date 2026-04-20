;; #+TITLE: StumpWM Config
;; #+PROPERTY: header-args:lisp :tangle init.lisp :exports both :eval never
;; In general we are not allowing any variable setting or function calls outside of a defun. The only exected exception to this is the inital load underneath this and the finishing actions section which runs all the init-* defuns.
;; * Initial Config
;; Start slynk for debugging purposes first and then load in our required stumpwm contrib modules
(in-package :stumpwm-user)

(if *initializing*
    (progn
      (setf *startup-message* (format nil "Welcome Natalie!"))
      (asdf:load-system :slynk)
      (slynk:create-server :port 1337
    			   :dont-close t)))

(run-shell-command "darkman set dark")


;; ** Configuration
(defvar *show-mode-line-time* t)

(defun init-window-number-fixes ()
       ;;; When windows are desroyed window numbers are not synced
       ;;; 2kays <https://github.com/2kays> posted a solution on
       ;;; the TipsAndTricks section of the wiki
       ;;; This will repack window numbers every time a window is killed
  (stumpwm:add-hook stumpwm:*destroy-window-hook*
  		    #'(lambda (win) (declare (ignorable win))(stumpwm:repack-window-numbers))))

(defun init-mode-line ()
  (setf *window-format* " %n %10c ")  
  (setf *screen-mode-line-format*      
       	(list
       	 "%M^>%C%H%E"
       	 '(:eval (if *show-mode-line-time* (ml-fmt-colors 'bg 'focus " %d ") ""))
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

  (toggle-modeline-all-screens))

;; *** Run init
(defvar *fonts* (list "-adobe-courier-normal-r-normal-*-14-*-*-*-m-*-*-*"
  		      ;; also bold
   		      "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-*-*"
   		      "-*-new century schoolbook-r-normal-*-*-*-*-*-*-*-*-*-*"
   		      "-*-times-*-*-*-*-*-*-*-*-*-*-*-*"))

(defun init-system-themeing ()
  (set-system-themeing "dark")
  (set-font (nth 0 *fonts*))
  (which-key-mode)

  (setf stumpwm:*input-window-gravity* :center
  	stumpwm:*message-window-gravity* :center
        
  	stumpwm:*message-window-padding* 10
  	stumpwm:*message-window-y-padding* 10))

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
	    (select-window-by-number (window-number
				      (car (stumpwm::head-windows (current-group)
								  (nth screen-num (stumpwm::group-heads (current-group)))))))
	    (group-wake-up (current-group)))

;; * Keybindings
(defun init-keybindings ()
  (set-prefix-key (kbd "C-t")))
;; ** Program Bindings
(defun init-program-binding ()
  (stumpwm-utils:make-program-binding "firefox" "Firefox")

  (stumpwm-utils:make-program-binding "alacritty" "Alacritty")

  (stumpwm-utils:make-program-binding "emacs" "Emacs" "emacs")

  (stumpwm-utils:make-program-binding "keepassxc" "keepassxc")

  (stumpwm-utils:make-program-binding "steam" "steam")

  (stumpwm-utils:make-program-binding "firefox -P work --class firefox-work" "firefox-work" "firefox-work")

  (stumpwm-utils:make-program-binding "firefox -P media --class firefox-media" "firefox-media" "firefox-media"))

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
(defun init-root-map ()
  (define-key *root-map* (kbd "0") "remove")
  (define-key *root-map* (kbd "1") "only")
  (define-key *root-map* (kbd "2") "vsplit")
  (define-key *root-map* (kbd "3") "hsplit")

  (define-key *root-map* (kbd "F1") "user-switch-to-screen 2")
  (define-key *root-map* (kbd "F2") "user-switch-to-screen 1")
  (define-key *root-map* (kbd "F3") "user-switch-to-screen 0")


  (define-key *root-map* (kbd "p") '*program-map*)
  (define-key *root-map* (kbd "s") '*system-map*))
;; *** Top Map
;; Handle Fn keys for systems like laptops etc
(defun init-top-map ()
  (define-key *top-map* (kbd "XF86AudioRaiseVolume") "notify-volume-up")
  (define-key *top-map* (kbd "XF86AudioLowerVolume") "notify-volume-down")
  (define-key *top-map* (kbd "XF86AudioMute") "notify-volume-mute")

  (define-key *top-map* (kbd "XF86MonBrightnessUp") "screen-brightness-up")
  (define-key *top-map* (kbd "XF86MonBrightnessDown") "screen-brightness-down")

  (define-key *top-map* (kbd "XF86KbdBrightnessUp") "keyboard-brightness-up")
  (define-key *top-map* (kbd "XF86KbdBrightnessDown") "keyboard-brightness-down")

  (define-key *top-map* (kbd "Print") "screenshot"))

;; * Final Actions
(defun run-all-inits ()
  ;; Themes
  (init-system-themeing)
  (init-mode-line)

  ;; Keymaps
  (init-keybindings)
  (init-program-binding)
  (init-root-map)
  (init-top-map)

  (run-shell-command "brillo -S 100")
  (run-shell-command "darkman set dark")
  (run-shell-command "dex -a -s $XDG_CONFIG_HOME/autostart/"))


(run-all-inits)
