(define-module (packages kubectl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy))

(define-public kubectl
  (package
   (name "kubectl")
   (version "1.33.1")
   (source (origin
            (method url-fetch/executable)
            (uri (string-append  "https://dl.k8s.io/release/v" version "/bin/linux/amd64/kubectl"))
            (sha256
             (base32
              "0rgjys6ac5yq2q9kmznav12pqyf9iswxczf8b9p7plzpf8fzsd5l"))))
   (build-system copy-build-system)
   (synopsis "Kubectl provides a command line tool for communicating with a Kubernetes cluster's control plane, using the Kubernetes API.")
   (description
    "Kubectl provides a command line tool for communicating with a Kubernetes cluster's control plane, using the Kubernetes API.")
   (home-page "https://kubernetes.io")
   (license license:isc)))
