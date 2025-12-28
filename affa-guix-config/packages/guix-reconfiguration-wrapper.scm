(define-module (affa-guix-config packages guix-reconfiguration-wrapper)
  #:use-module (guix gexp)
  ;; #:use-module (guix packages)
  #:use-module (guix utils)
  ;; #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system pyproject)

  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  
  
  #:export (guix-reconfiguration-wrapper))


(define guix-reconfiguration-wrapper
  (package
    (name "guix-reconfiguration-wrapper")
    (version "0.0.2")
    (source (local-file (string-append (current-source-directory) "/guix-reconfiguration-wrapper")
			#:recursive? #t))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-rich
			     python-dotenv
			     python-gitpython))
    (native-inputs (list python-toolchain))
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis
     "A wrapper for guix home and system reconfiguration")
    (description
     "A wrapper for guix home and system reconfiguration")
    (license license:gpl3+)))
