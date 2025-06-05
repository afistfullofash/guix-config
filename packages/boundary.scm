(define-module (packages boundary)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)

  #:export (boundary))

(define-public boundary
  (package
   (name "boundary")
   (version "0.12.0")
   (source (origin
            (method url-fetch/zipbomb)
            (uri (string-append "https://releases.hashicorp.com/boundary/" version "/boundary_" version "_linux_amd64.zip"))
            (sha256
             (base32
             "1qzfd593i538np6520sdijihxg0zgnvjll1kc2pjrc46msvvnjzp"))))
   (build-system copy-build-system)
   (arguments
    '(#:install-plan '(("boundary" "bin/boundary"))))
   (synopsis "Boundary is an identity-aware proxy that provides a simple, secure way to access hosts and critical systems on your network.")
   (description
    "HashiCorp Boundary is an identity-aware proxy aimed at simplifying and securing least-privileged access to cloud infrastructure")
   (home-page "https://developer.hashicorp.com/boundary")
   (license license:isc)))
