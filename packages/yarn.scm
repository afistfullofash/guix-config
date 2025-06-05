(define-module (packages yarn)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages node))

(define-public yarn
  (package
    (name "yarn")
    (version "1.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/yarnpkg/yarn/releases/download/v"
             version
             "/yarn-v"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wi47awrcy5kki9qmmia4h3rpsf5k7yvnpmwsbdlnb62krc4pk82"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out               (assoc-ref %outputs "out"))
                    (bin               (string-append out "/bin"))
                    (libexec-yarn      (string-append out "/libexec/yarn"))
                    (yarn-js           (string-append libexec-yarn "/bin/yarn.js")))
               (mkdir-p bin)
               (mkdir-p libexec-yarn)
               (copy-recursively "./" libexec-yarn)
               (symlink yarn-js (string-append bin "/yarn"))
               (symlink yarn-js (string-append bin "/yarnpkg"))))))))

    (inputs `(("node" ,node)))
    (synopsis "Fast, reliable, and secure dependency management")
    (description
     "Yarn is a fast, reliable, and secure dependency management.  Fast: Yarn
caches every package it has downloaded, so it never needs to download the
same package again.  It also does almost everything concurrently to maximize
resource utilization.  This means even faster installs.  Reliable: Using a
detailed but concise lockfile format and a deterministic algorithm for install
operations, Yarn is able to guarantee that any installation that works on one
system will work exactly the same on another system.
Secure: Yarn uses checksums to verify the integrity of every installed package
before its code is executed.")
    (license license:bsd-2)
    (home-page "https://yarnpkg.com")))

yarn
