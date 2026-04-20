(define-module (affa-guix-config packages stumpwm)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system asdf)
  #:use-module (guix gexp)
  #:use-module (guix hash)

  #:use-module (gnu packages wm)
  #:use-module (gnu packages lisp-xyz)

  #:use-module (afistfullofash packages wm)
  
  #:export (stumpwm-themeing
	    stumpwm-utils
	    stumpwm-mode-line
	    stumpwm-pill-cpu
	    stumpwm-pill-email
	    stumpwm-pill-temperature
	    stumpwm-pill-window-list
	    stumpwm-pills
	    stumpwm-dark-light
	    stumpwm-logging
	    stumpwm-compositor
	    affoa-stumpwm))

(define stumpwm stumpwm-with-message-hide-hook)

(define stumpwm-utils
  (package
    (name "stumpwm-utils")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/utils" "stumpwm-utils"
                        #:recursive? #t))
    (inputs (list stumpwm))	
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Utility Functions")
    (description
     "Utility functions for evening out interacting with stumpwm.")
    (license license:gpl3)))

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

(define stumpwm-mode-line
  (package
    (name "stumpwm-mode-line")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/mode-line" "stumpwm-mode-line"
                        #:recursive? #t))
    (inputs (list stumpwm
		  stumpwm-themeing))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Mode Line Module")
    (description
     "This package extends mode-line functionality so that it is easier to handle creation and themeing of individual sections (called pills internally).")
    (license license:gpl3)))

(define stumpwm-pill-cpu
  (package
    (name "stumpwm-pill-cpu")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/mode-line-pills/cpu/" "stumpwm-pill-cpu"
                        #:recursive? #t))
    (inputs (list stumpwm
		  stumpwm-mode-line
		  sbcl-stumpwm-cpu))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Cpu Mode Line Pill")
    (description
     "This package extends mode-line functionality so that it is easier to handle creation and themeing of individual sections (called pills internally).")
    (license license:gpl3)))

(define stumpwm-pill-email
  (package
    (name "stumpwm-pill-email")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/mode-line-pills/email/" "stumpwm-pill-email"
                        #:recursive? #t))
    (inputs (list stumpwm
		  stumpwm-mode-line
		  stumpwm-utils
		  sbcl-local-time))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Cpu Mode Line Pill")
    (description
     "This package adds a pill for displaying email counts based on notmuch tags")
    (license license:gpl3)))

(define stumpwm-pill-temperature
  (package
    (name "stumpwm-pill-temperature")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/mode-line-pills/temperature/" "stumpwm-pill-temperature"
                        #:recursive? #t))
    (inputs (list stumpwm
		  stumpwm-mode-line))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Temperature Pill")
    (description
     "This package add a pill which tracks the temperatures of system hardware")
    (license license:gpl3)))

(define stumpwm-pill-window-list
  (package
    (name "stumpwm-pill-window-list")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/mode-line-pills/window-list/" "stumpwm-pill-window-list"
                        #:recursive? #t))
    (inputs (list stumpwm
		  sbcl-cl-ppcre))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Window List Pill")
    (description
     "This add a pill displaying the window list like the built in StumpWM varient but with expanded themeing options")
    (license license:gpl3)))

(define stumpwm-pills
  (list stumpwm-pill-cpu
	stumpwm-pill-email
	stumpwm-pill-temperature
	stumpwm-pill-window-list))

(define stumpwm-dark-light
  (package
    (name "stumpwm-dark-light")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/minor-modes/dark-light" "stumpwm-dark-light"
                        #:recursive? #t))
    (inputs (list stumpwm
		  stumpwm-utils
		  stumpwm-themeing
		  sbcl-trivia))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Dark Light Minor Mode")
    (description
     "This package adds a minor mode to apply themeing based on system dark/light mode settings.")
    (license license:gpl3)))

(define stumpwm-logging
  (package
    (name "stumpwm-logging")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/minor-modes/logging" "stumpwm-logging"
                        #:recursive? #t))
    (inputs (list stumpwm
		  stumpwm-utils
		  sbcl-local-time))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Logging Minor Mode")
    (description
     "StumpWM Minor Mode for configuring logging")
    (license license:gpl3)))

(define stumpwm-compositor
  (package
    (name "stumpwm-compositor")
    (version "0.0.1")
    (source (local-file "../files/stumpwm/minor-modes/compositor" "stumpwm-compositor"
                        #:recursive? #t))
    (inputs (list stumpwm
		  sbcl-clx))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/afistfullofash/guix-config/")
    (synopsis "StumpWM Compositor Minor Mode")
    (description
     "StumpWM Minor Mode for configuring compositior options such as opactity")
    (license license:gpl3)))

(define affoa-stumpwm
  (stumpwm-extension-builder stumpwm "affoa-stumpwm"
			     `((,stumpwm-themeing . "stumpwm-themeing")
			       (,stumpwm-pill-cpu . "stumpwm-pill-cpu")
			       (,stumpwm-pill-temperature . "stumpwm-pill-temperature")
			       (,stumpwm-pill-email . "stumpwm-pill-email")
			       (,stumpwm-pill-window-list . "stumpwm-pill-window-list")
			       (,stumpwm-dark-light . "stumpwm-dark-light")
			       (,stumpwm-logging . "stumpwm-logging")
			       (,stumpwm-compositor . "stumpwm-compositor"))))
