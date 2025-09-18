(define-module (home desktop)
  #:use-module (home base)
  
  #:use-module (gnu home)
  #:use-module (gnu home services)

  #:use-module (gnu services)
  
  #:export (desktop-home-environment))

(define desktop-environment-variables-service
  (simple-service 'desktop-environment-variables-service
		  home-environment-variables-service-type
		  `(("GUIX_HOME_SYSTEM_FORMAT" .  "desktop")
		    ("GUIX_SANDBOX_EXTRA_SHARES" . "/steam"))))


(define desktop-home-services
  (list desktop-environment-variables-service))

(define desktop-home-environment
  (home-environment
   (inherit base-home-environment)
   (services (append desktop-home-services
		     base-home-services))))
