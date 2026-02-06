(define-module (affa-guix-config systems nymph)
  #:use-module (affa-guix-config systems laptop)

  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)

  #:use-module (gnu services containers)
  
  #:use-module (guix packages)
  #:use-module (guix utils)

  
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (srfi srfi-1)

  #:export (nymph-operating-system))


(define nymph-operating-system
  (operating-system
    (inherit laptop-operating-system)
    (host-name "nymph")

    (swap-devices (list (swap-space
                         (target (uuid
                                  "846d349f-f175-4d7d-a9e7-e67611cf53eb")))))

    (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "c4b2bf2a-d877-46dd-8af0-9dc3bdf1e860"
                                  'btrfs))
                         (type "btrfs"))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "D42D-79C4"
                                       'fat32))
                         (type "vfat")) %base-file-systems))))
