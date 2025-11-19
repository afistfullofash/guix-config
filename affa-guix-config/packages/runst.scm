(define-module (affa-guix-config packages runst)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)

  #:use-module (affa-guix-config packages cargo-crates)

  #:export (runst))

(define runst
  (package
    (name "runst")
    (version "0.1.7")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "runst" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"0brvi6an9mx2hd8gn4ynrcmy9ni64n25dvyxr67xyr6swachg7ki"))))
    (build-system cargo-build-system)
    (arguments `(#:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs (append
	     (list dbus
		   `(,zstd "lib")
		   glib
		   cairo
		   pango)
	     (cargo-inputs 'runst #:module '(affa-guix-config packages cargo-crates))))
    (synopsis "A dead simple notification daemon")
    (description
     "Desktop notifications are small, passive popup dialogs that notify the user of particular events in an asynchronous manner. These passive popups can automatically disappear after a short period of time.")
    (home-page "https://github.com/orhun/runst")
    (license (list license:expat
		   license:asl2.0))))
