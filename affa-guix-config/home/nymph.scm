(define-module (affa-guix-config home nymph)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  
  #:use-module (gnu services)

  #:use-module (gnu home services guix)
  #:use-module (guix channels)
  
  
  #:use-module (affa-guix-config home base)
  #:use-module (affa-guix-config services backup)
  #:use-module (affa-guix-config services mail)

  #:export (nymph-home-environment))

(define nymph-home-environment
  (home-environment
   (inherit base-home-environment)))
