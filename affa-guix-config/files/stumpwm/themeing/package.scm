(use-modules (guix packages)
	     ((guix licenses) #:prefix license:)
	     (guix download)
	     (guix build-system asdf)
	     (guix gexp)
	     (guix hash)
	     (gnu packages wm)
	     (gnu packages lisp-xyz))

(define stumpwm-themeing
  (package
    (name "stumpwm-themeing")
    (version "0.0.1")
    (source (local-file "./" "stumpwm-themeing"
                        #:recursive? #t))
    (inputs (list stumpwm
		  sbcl-trivia))	
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/Junker/stumpwm-pamixer")
    (synopsis "StumpWM Themeing Module")
    (description
     "This package provides a minimalistic themeing module for StumpWM.")
    (license license:gpl3)))

stumpwm-themeing
