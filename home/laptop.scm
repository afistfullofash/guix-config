(define-module (home laptop))

(use-modules (home base)
	     (gnu home))

(define laptop-home-environment
  (home-environment
    (inherit base-home-environment)))

laptop-home-environment
