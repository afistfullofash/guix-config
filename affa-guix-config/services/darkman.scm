(define-module (affa-guix-config services darkman)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)

  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages wm)

  #:export (home-darkman-service-type
	    home-darkman-configuration
	    home-darkman-configuration?))

;; ──────────────────────────────────────────────────────────────
;; Configuration record
;; ──────────────────────────────────────────────────────────────

(define-record-type* <home-darkman-configuration>
  home-darkman-configuration make-home-darkman-configuration
  home-darkman-configuration?

  ;; The darkman package
  (package home-darkman-package
	   (default darkman))

  ;; Latitude/longitude for automatic sunrise/sunset switching
  (latitude  home-darkman-latitude  (default #f))
  (longitude home-darkman-longitude (default #f))

  ;; Portal support (for xdg-desktop-portal integration)
  (use-portal? home-darkman-use-portal? (default #f))

  ;; Extra transition scripts beyond the built-in ones.
  ;; Each entry is (name . script-file-like-object) and will be placed
  ;; into ~/.local/share/dark-mode.d/ or ~/.local/share/light-mode.d/
  (extra-dark-scripts  home-darkman-extra-dark-scripts  (default '()))
  (extra-light-scripts home-darkman-extra-light-scripts (default '())))

;; ──────────────────────────────────────────────────────────────
;; Transition scripts
;;
;; Each script is a program-file that calls the relevant application
;; APIs to switch themes at runtime — no config file rewriting needed
;; for apps that support live switching.
;; ──────────────────────────────────────────────────────────────

(define (dark-mode-gtk-script)
  "Switch GTK to dark mode via gsettings."
  (program-file "gtk-dark.sh"
    #~(begin
	(system* #$(file-append glib "/bin/gsettings")
		 "set" "org.gnome.desktop.interface"
		 "color-scheme" "prefer-dark")
	(system* #$(file-append glib "/bin/gsettings")
		 "set" "org.gnome.desktop.interface"
		 "gtk-theme" "Dracula"))))

(define (light-mode-gtk-script)
  "Switch GTK to light mode via gsettings."
  (program-file "gtk-light.sh"
    #~(begin
	(system* #$(file-append glib "/bin/gsettings")
		 "set" "org.gnome.desktop.interface"
		 "color-scheme" "prefer-light")
	(system* #$(file-append glib "/bin/gsettings")
		 "set" "org.gnome.desktop.interface"
		 "gtk-theme" "Adwaita"))))

(define (dark-mode-emacs-script)
  "Switch Emacs to Dracula via emacsclient."
  (program-file "emacs-dark.sh"
    (let ((emacsclient (file-append emacs "/bin/emacsclient")))
      #~(begin
	  (system* #$emacsclient "--eval"
		   "(progn (load-theme 'doom-dracula t))")))))

(define (light-mode-emacs-script)
  "Switch Emacs to Catppuccin Latte via emacsclient."
  (program-file "emacs-light.sh"
    (let ((emacsclient (file-append emacs "/bin/emacsclient")))
      #~(begin
	  (system* #$emacsclient "--eval"
		   (string-append
		    "(progn"
		    " (setq catppuccin-flavor 'latte)"
		    " (load-theme 'catppuccin t)"
		    " (catppuccin-reload))"))))))

(define (dark-mode-stumpwm-script)
  "Switch StumpWM to Dracula theme via stumpish."
  (program-file "stumpwm-dark.sh"
    (let ((stumpish (file-append stumpwm "/bin/stumpish")))
      #~(begin
	  (setenv "AFFOA_SYSTEM_THEME_TYPE" "dark")
	  (system* #$stumpish "eval"
		   "(progn (setf (uiop:getenv \"AFFOA_SYSTEM_THEME_TYPE\") \"dark\") (set-system-themeing))")))))

(define (light-mode-stumpwm-script)
  "Switch StumpWM to Catppuccin Latte theme via stumpish."
  (program-file "stumpwm-light.sh"
    (let ((stumpish (file-append stumpwm "/bin/stumpish")))
      #~(begin
	  (setenv "AFFOA_SYSTEM_THEME_TYPE" "light")
	  (system* #$stumpish "eval"
		   "(progn (setf (uiop:getenv \"AFFOA_SYSTEM_THEME_TYPE\") \"light\") (set-system-themeing))")))))

;; ──────────────────────────────────────────────────────────────
;; Combine into a single transition script per mode
;; ──────────────────────────────────────────────────────────────

(define (dark-mode-script extra-scripts)
  "Master script that runs all dark-mode transitions."
  (program-file "darkman-dark-mode"
    #~(begin
	(for-each (lambda (script) (system* script))
		  (list #$(dark-mode-gtk-script)
			#$(dark-mode-emacs-script)
			#$(dark-mode-stumpwm-script)
			#$@extra-scripts)))))

(define (light-mode-script extra-scripts)
  "Master script that runs all light-mode transitions."
  (program-file "darkman-light-mode"
    #~(begin
	(for-each (lambda (script) (system* script))
		  (list #$(light-mode-gtk-script)
			#$(light-mode-emacs-script)
			#$(light-mode-stumpwm-script)
			#$@extra-scripts)))))

;; ──────────────────────────────────────────────────────────────
;; Config file generation
;; ──────────────────────────────────────────────────────────────

(define (darkman-config-file config)
  "Generate the darkman YAML config at ~/.config/darkman/config.yaml."
  (match-record config <home-darkman-configuration>
    (latitude longitude use-portal?)
    (list
     `("darkman/config.yaml"
       ,(plain-file "darkman-config.yaml"
		    (string-append
		     (if latitude
			 (string-append "lat: " (number->string latitude) "\n")
			 "")
		     (if longitude
			 (string-append "lng: " (number->string longitude) "\n")
			 "")
		     (if use-portal?
			 "portal: true\n"
			 "")))))))

;; ──────────────────────────────────────────────────────────────
;; Transition script file installation
;; ──────────────────────────────────────────────────────────────

(define (darkman-data-files config)
  "Install the transition scripts into ~/.local/share/dark-mode.d/ and light-mode.d/."
  (match-record config <home-darkman-configuration>
    (extra-dark-scripts extra-light-scripts)
    (list
     `("dark-mode.d/theme-switch"
       ,(dark-mode-script extra-dark-scripts))
     `("light-mode.d/theme-switch"
       ,(light-mode-script extra-light-scripts)))))

;; ──────────────────────────────────────────────────────────────
;; Shepherd service
;; ──────────────────────────────────────────────────────────────

(define (darkman-shepherd-services config)
  (match-record config <home-darkman-configuration>
    (package)
    (list
     (shepherd-service
      (documentation "Run darkman for automatic dark/light theme switching")
      (requirement '(dbus))
      (auto-start? #t)
      (provision '(darkman))
      (start #~(make-forkexec-constructor
		(list #$(file-append package "/bin/darkman") "run")
		#:log-file
		(string-append (or (getenv "XDG_STATE_HOME")
				   (string-append (getenv "HOME") "/.local/state"))
			       "/darkman.log")))
      (stop #~(make-kill-destructor))))))

;; ──────────────────────────────────────────────────────────────
;; Service type
;; ──────────────────────────────────────────────────────────────

(define home-darkman-service-type
  (service-type
   (name 'home-darkman)
   (description
    "Manage automatic dark/light theme switching using darkman.
Runs darkman as a Shepherd service and installs transition scripts
that switch GTK, Emacs, and StumpWM themes at runtime.")
   (extensions
    (list
     (service-extension home-xdg-configuration-files-service-type
			darkman-config-file)
     (service-extension home-xdg-data-files-service-type
			darkman-data-files)
     (service-extension home-shepherd-service-type
			darkman-shepherd-services)))
   (default-value (home-darkman-configuration))))
