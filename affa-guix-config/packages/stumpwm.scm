(define-module (affa-guix-config packages guix-reconfiguration-wrapper)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system asdf)
  #:use-module (guix gexp)
  #:use-module (guix hash)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages lisp-xyz)
  
  #:export (stumpwm-themeing))

(define stumpwm-themeing
  (package
    (name "stumpwm-themeing")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/themeing" "stumpwm-themeing"
                        #:recursive? #t))
    (inputs (list stumpwm
		  sbcl-trivia))	
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Themeing Module")
    (description
     "This package provides a minimalistic themeing module for StumpWM.")
    (license license:gpl3)))

(list stumpwm-themeing)
