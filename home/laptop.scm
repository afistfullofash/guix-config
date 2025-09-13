(define-module (home laptop)
  #:use-module (home base)
  
  #:use-module (gnu home)
  #:use-module (gnu home services)

  #:use-module (gnu services)
  
  #:export (laptop-home-environment))

(define laptop-environment-variables-service
  (simple-service 'laptop-environment-variables-service
		  home-environment-variables-service-type
		  `(("GUIX_HOME_SYSTEM_FORMAT" .  "laptop"))))


(define laptop-home-services
  (list laptop-environment-variables-service))

(define laptop-home-environment
  (home-environment
   (inherit base-home-environment)
   (services (append laptop-home-services
		     base-home-services))))

laptop-home-environment
