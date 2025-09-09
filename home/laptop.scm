(define-module (home laptop)
  #:use-module (home base)
  #:use-module (gnu home)

  #:export (laptop-home-environment))


(define laptop-home-environment
  (home-environment
    (inherit base-home-environment)))

laptop-home-environment
