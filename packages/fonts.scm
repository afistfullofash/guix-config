(define-module (packages fonts)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages node)

  #:export (fonts-nerd-fonts-dejavu))

(define-public fonts-nerd-fonts-dejavu
  (package
    (name "fonts-nerd-fonts-dejavu")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
             version
             "/DejaVuSansMono.zip"))
       (sha256
        (base32
         "1a7pz0g73jd4h7fbf2iafx8v28i4gj72ny3i0pm1h4rkv9dfn5s6"))))
    (build-system font-build-system)
    (synopsis "Nerd Fonts is a project that patches developer targeted fonts with a high number of glyphs (icons). Specifically to add a high number of extra glyphs from popular 'iconic fonts' such as Font Awesome, Devicons, Octicons, and others.")
    (description
     "Nerd Fonts is a project that patches developer targeted fonts with a high number of glyphs (icons). Specifically to add a high number of extra glyphs from popular 'iconic fonts' such as Font Awesome, Devicons, Octicons, and others.")
    (license license:bsd-2)
    (home-page "https://github.com/ryanoasis/nerd-fonts")))
