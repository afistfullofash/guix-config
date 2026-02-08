(define-module (affa-guix-config home nymph)
  #:use-module (gnu home)

  #:use-module (affa-guix-config home laptop)

  #:export (nymph-home-environment))

(define nymph-home-environment
  (home-environment
   (inherit laptop-home-environment)))
