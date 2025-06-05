(define-module (packages terraform)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)

  #:export (terraform))

(define-public terraform
  (package
   (name "terraform")
   (version "1.12.1")
   (source (origin
            (method url-fetch/zipbomb)
            (uri (string-append "https://releases.hashicorp.com/terraform/" version "/terraform_" version "_linux_amd64.zip"))
            (sha256
             (base32
              "0z7gjx1zwzqzjvrjwl9p8va1lv5nafx49v39cwd462k606l8pbyw"))))
   (build-system copy-build-system)
   (arguments
    '(#:install-plan '(("terraform" "bin/terraform"))))
   (synopsis "Terraform is an infrastructure as code tool that lets you build, change, and version infrastructure safely and efficiently. This includes low-level components like compute instances, storage, and networking; and high-level components like DNS entries and SaaS features.")
   (description
    "Terraform is an infrastructure as code tool that lets you build, change, and version infrastructure safely and efficiently. This includes low-level components like compute instances, storage, and networking; and high-level components like DNS entries and SaaS features.")
   (home-page "https://developer.hashicorp.com/terraform")
   (license license:isc)))
