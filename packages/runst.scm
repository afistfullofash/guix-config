(define-module (packages runst)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-shell)
  #:use-module (gnu packages crates-io)

  #:use-module (packages crates-io)
  
  #:export (runst))

(define-public runst
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
    (arguments `(#:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.19)
				 ("dbus" ,rust-dbus-0.9)
				 ("dbus-crossroads" ,rust-dbus-crossroads-0.5)
				 ("dbus-codegen" ,rust-dbus-codegen-0.12)
				 ("xllrb" ,rust-x11rb-0.13)
				 ("pangocairo" ,rust-pangocairo-0.19)
				 ("pango" ,rust-pango-0.20)
				 ("thiserror" ,rust-thiserror-2)
				 ("serde" ,rust-serde-1)
				 ("toml" ,rust-toml-0.8)
				 ("sscanf" ,rust-sscanf-0.4.3)
				 ("colorsys" ,rust-colorsys-0.6.7)
				 ("dirs" ,rust-dirs-5)
				 ("rust-embed" ,rust-rust-embed-8)
				 ("tera" ,rust-tera-1)
				 ("estimated_read_time" ,rust-estimated-read-time-1)
				 ("regex" ,rust-regex-1)
				 ("serde_regex" ,rust-serde-regex-1)
				 ("serde_json" ,rust-serde-json-1)
				 ("tracing" ,rust-tracing-0.1.40)
				 ("tracing-subscriber" ,rust-tracing-subscriber-0.3.18)
				 ("humantime" ,rust-humantime-2))))
    (native-inputs (list pkg-config))
    (inputs (list dbus
		  glib
		  cairo
		  pango))
    (synopsis "A dead simple notification daemon")
    (description
     "Desktop notifications are small, passive popup dialogs that notify the user of particular events in an asynchronous manner. These passive popups can automatically disappear after a short period of time.")
    (home-page "https://github.com/orhun/runst")
    (license license:isc)))
