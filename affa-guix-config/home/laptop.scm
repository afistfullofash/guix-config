(define-module (affa-guix-config home laptop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  
  #:use-module (gnu services)

  #:use-module (gnu home services desktop)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services pm)

  #:use-module (guix channels)  
  
  #:use-module (affa-guix-config home base)
  #:use-module (afistfullofash home services mail)
  #:use-module (afistfullofash home services compositor)
  #:use-module (afistfullofash home timers backup)
  #:use-module (afistfullofash home timers mail)

  #:export (laptop-home-environment))

(define laptop-environment-variables-service
  (simple-service 'laptop-environment-variables-service
		  home-environment-variables-service-type
		  `(("GUIX_HOME_SYSTEM_FORMAT" .  "laptop"))))

(define laptop-home-services
  (list laptop-environment-variables-service
	(service home-picom-service-type)
	(service home-batsignal-service-type
		 (home-batsignal-configuration
		  (warning-level 20)
		  (critical-level 10)
		  (danger-level 2)
		  ;; optional: do something before it faceplants
		  (danger-command "sudo shutdown")
		  (poll-delay 60)))))

(define laptop-home-environment
  (home-environment
   (inherit base-home-environment)
   (services (append laptop-home-services
		     base-home-services))))
