(define-module (affa-guix-config home laptop)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  
  #:use-module (gnu services)

  #:use-module (gnu home services guix)
  #:use-module (guix channels)
  
  
  #:use-module (affa-guix-config home base)
  #:use-module (affa-guix-config services backup)
  #:use-module (affa-guix-config services mail)

  #:export (laptop-home-environment))

(define laptop-environment-variables-service
  (simple-service 'laptop-environment-variables-service
		  home-environment-variables-service-type
		  `(("GUIX_HOME_SYSTEM_FORMAT" .  "laptop"))))

(define (daily-at h m)
  (format #f "~a ~a * * *" m h))

(define laptop-restic-backup-timer
  (home-restic-backup-timer
   "natalie"
   (list "~/.restic"
	 "~/.pass"
         "~/.config/rclone"
         "~/Pictures"
	 "~/Passwords"
	 "~/Documents"
	 "~/src/guix-config"
	 "~/src/afistfullofash"
	 "~/org")
   #:schedule (daily-at 8 30)
   #:extra-args '("-vv")))

(define laptop-home-timers
  (simple-service 'laptop-home-timers
                  home-shepherd-service-type
                  (list laptop-restic-backup-timer
			home-mail-sync-timer)))

(define laptop-home-channels-service
  (simple-service 'laptop-home-channels-service
		  home-channels-service-type
		  (list
		   (channel
		    (name 'affa-guix-config)
		    (url "file://~/src/guix-config")))))

(define laptop-home-services
  (list laptop-environment-variables-service
	laptop-home-channels-service
	home-hydroxide-service
        laptop-home-timers))

(define laptop-home-environment
  (home-environment
   (inherit base-home-environment)
   (services (append laptop-home-services
		     base-home-services))))
