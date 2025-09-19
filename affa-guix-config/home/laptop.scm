(define-module (affa-guix-config home laptop)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  
  #:use-module (gnu services)
  
  #:use-module (affa-guix-config home base)
  #:use-module (affa-guix-config services backup)

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
	 "~/src/guix-config"
	 "~/src/afistfullofash"
	 "~/org")
   `(("AWS_ACCESS_KEY_ID" . ,(getenv "B2_ACCESS_KEY_ID"))
     ("AWS_SECRET_ACCESS_KEY" . ,(getenv "B2_SECRET_ACCESS_KEY"))
     ("AWS_DEFAULT_REGION"    . ,(getenv "B2_REGION"))
     ("RESTIC_REPOSITORY"     . ,(getenv "RESTIC_REPOSITORY"))
     ("RESTIC_PASSWORD_FILE"  . ,(getenv "RESTIC_PASSWORD_FILE")))
   #:schedule (daily-at 8 30)
   #:extra-args '("--one-file-system" "--verbose")))

(define laptop-home-timers
  (simple-service 'laptop-home-timers
                  home-shepherd-service-type
                  (list laptop-restic-backup-timer)))

(define laptop-home-services
  (list laptop-environment-variables-service
	laptop-home-timers))

(define laptop-home-environment
  (home-environment
   (inherit base-home-environment)
   (services (append laptop-home-services
		     base-home-services))))

laptop-home-environment
