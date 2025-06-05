(define-module (packages kubectl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:export (kubectl))

(define-public kubectl
  (package
   (name "kubectl")
   (version "1.33.1")
   (source (origin
            (method (url-fetch #:executable #t))
            (uri (string-append  "https://dl.k8s.io/release/v" version "/bin/linux/amd64/kubectl"))
            (sha256
             (base32
              "18q0l5s98s8zhq8zhw5n1194wzydq6h6a4kj5c8zsf374vrfkr2x"))))
   (build-system copy-build-system)
   (synopsis "Kubectl provides a command line tool for communicating with a Kubernetes cluster's control plane, using the Kubernetes API.")
   (description
    "Kubectl provides a command line tool for communicating with a Kubernetes cluster's control plane, using the Kubernetes API.")
   (home-page "https://kubernetes.io")
   (license license:isc)))
