(define-module (packages crates-io)
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
  
  #:export (rust-const-format-0.2.31
	    rust-const-format-proc-macros-0.2.31
	    rust-sscanf-0.4.3
	    rust-sscanf-macro-0.4.3
	    rust-colorsys-0.6.7
	    rust-estimated-read-time-1
	    rust-dbus-codegen-0.12
	    rust-unicode-width-0.1.12
	    rust-tracing-0.1.40
	    rust-tracing-subscriber-0.3.18)

  )


(define-public rust-const-format-0.2.31
  (package
    (name "rust-const-format")
    (version "0.2.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "const_format" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j7zs1aar3daic7yy18sg34a518f5zzimn3q8fd1yww5lb3yz469"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--features=__test")
       #:cargo-inputs
       (("rust-const-format-proc-macros" ,rust-const-format-proc-macros-0.2.31)
        ("rust-konst" ,rust-konst-0.2))
       #:cargo-development-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                                   ("rust-fastrand" ,rust-fastrand-1))))
    (home-page "https://github.com/rodrimati1992/const_format_crates/")
    (synopsis "Compile-time string formatting")
    (description "This package provides compile-time string formatting.")
    (license license:zlib)))

(define-public rust-const-format-proc-macros-0.2.31
  (package
    (name "rust-const-format-proc-macros")
    (version "0.2.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "const_format_proc_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xibiffpmwvlina6amybiz66g5zgs5r5gk9jrywlr1sa377bc9p0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2))
       #:cargo-development-inputs (("rust-fastrand" ,rust-fastrand-1))))
    (home-page "https://github.com/rodrimati1992/const_format_crates/")
    (synopsis "Implementation detail of the @code{const_format} crate")
    (description "Implementation detail of the @code{const_format} crate.")
    (license license:zlib)))


(define-public rust-sscanf-0.4.3
  (package
    (name "rust-sscanf-0.4.3")
    (version "0.4.3")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "sscanf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"1w6lfy9sr1fh1ar3k68wjyscc9kpdi4ngygwixf0613aafdh1lfb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("const-format" ,rust-const-format-0.2.31)
	("regex" ,rust-regex-1)
	("sscanf-macro" ,rust-sscanf-macro-0.4.3)
	("lazy-static" ,rust-lazy-static-1))))
    (synopsis "A Rust crate with a sscanf (inverse of format!()) Macro based on Regex")
    (description
     "A Rust crate with a sscanf (inverse of format!()) Macro based on Regex")
    (home-page "https://github.com/mich101mich/sscanf")
    (license license:isc)))

(define-public rust-sscanf-macro-0.4.3
  (package
    (name "rust-sscanf-macro")
    (version "0.4.3")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "sscanf_macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"0dqsrabv6zmphzm0ssrq3h07gq67ccrp7kvn4kdbqjsp19iy1z6g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("unicode-width" ,rust-unicode-width-0.1.12))))
    (synopsis "A Rust crate with a sscanf (inverse of format!()) Macro based on Regex")
    (description
     "A Rust crate with a sscanf (inverse of format!()) Macro based on Regex")
    (home-page "https://github.com/mich101mich/sscanf")
    (license license:isc)))

(define-public rust-colorsys-0.6.7
  (package
    (name "rust-colorsys")
    (version "0.6.7")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "colorsys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"1g8vwcv89n2dzi9bmbzqlj9cl9a89jz49668grbcncv4cjx1l9jl"))))
    (build-system cargo-build-system)
    (synopsis "A module for color conversion and mutation written in Rust.")
    (description
     "A module for color conversion and mutation written in Rust.")
    (home-page "https://github.com/emgyrz/colorsys.rs")
    (license license:isc)))

(define-public rust-estimated-read-time-1
  (package
    (name "rust-estimated-read-time")
    (version "1.0.0")
    (source (origin
	      (method url-fetch)
              (uri (crate-uri "estimated_read_time" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
		"1mz8pkgk9v0cfzfjw659zl997gilangb78ccds8gic8h2hsgv734"))))
    (build-system cargo-build-system)
    (synopsis "Calculates the time taken to read any text.")
    (description
     "Calculates the time taken to read any text.")
    (home-page "https://github.com/karthik512/estimated_read_time")
    (license license:isc)))

(define-public rust-dbus-codegen-0.12
  (package
    (name "rust-dbus-codegen")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dbus-codegen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04n8vpk9z0dc06cnh1i7707v7h5sg20c19vaa9ykrj0zv5signdw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://crates.io/crates/dbus-codegen")
    (synopsis "Application-level tracing for Rust")
    (description "")
    (license license:expat)))

(define-public rust-unicode-width-0.1.12
  (package
    (name "rust-unicode-width")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-width" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mk6mybsmi5py8hf8zy9vbgs4rw4gkdqdq3gzywd9kwf2prybxb8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://crates.io/crates/dbus-codegen")
    (synopsis "Application-level tracing for Rust")
    (description "")
    (license license:expat)))

(define-public rust-tracing-0.1.40
  (package
    (name "rust-tracing")
    (version "0.1.40")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vv48dac9zgj9650pg2b4d0j3w6f3x9gbggf43scq5hrlysklln3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f		    ; unresolved import `tracing_mock`
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-tracing-attributes" ,rust-tracing-attributes-0.1)
        ("rust-tracing-core" ,rust-tracing-core-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://tokio.rs")
    (synopsis "Application-level tracing for Rust")
    (description "@code{rust-tracing} is a framework for instrumenting Rust
programs to collect structured, event-based diagnostic information.")
    (license license:expat)))

(define-public rust-tracing-subscriber-0.3.18
  (package
    (name "rust-tracing-subscriber")
    (version "0.3.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-subscriber" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12vs1bwk4kig1l2qqjbbn2nm5amwiqmkcmnznylzmnfvjy6083xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f     ; use of undeclared crate or module `tracing_mock`
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-matchers" ,rust-matchers-0.1)
        ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.46)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sharded-slab" ,rust-sharded-slab-0.1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thread-local" ,rust-thread-local-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-core" ,rust-tracing-core-0.1)
        ("rust-tracing-log" ,rust-tracing-log-0.2)
        ("rust-tracing-serde" ,rust-tracing-serde-0.2)
        ("rust-valuable" ,rust-valuable-0.1)
        ("rust-valuable-serde" ,rust-valuable-serde-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-futures" ,rust-tracing-futures-0.2)
        ("rust-tracing-log" ,rust-tracing-log-0.2))))
    (home-page "https://tokio.rs")
    (synopsis "Implement and compose tracing subscribers")
    (description
     "This package provides utilities for implementing and composing tracing
subscribers.

Tracing is a framework for instrumenting Rust programs to collect scoped,
structured, and async-aware diagnostics.  The Subscriber trait represents the
functionality necessary to collect this trace data.  This crate contains tools
for composing subscribers out of smaller units of behaviour, and
batteries-included implementations of common subscriber functionality.

Tracing-subscriber is intended for use by both Subscriber authors and
application authors using tracing to instrument their applications.")
    (license license:expat)))

