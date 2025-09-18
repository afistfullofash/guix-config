(define-module (systems laptop)
  #:use-module (systems base)

  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  
  #:use-module (guix packages)
  #:use-module (guix utils)

  
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (srfi srfi-1)

  #:export (laptop-operating-system))


(define laptop-operating-system
  (operating-system
    (inherit base-system-operating-system)
    (host-name "siren")

    (swap-devices (list (swap-space
                        (target (uuid
                                 "23c8daff-2df4-4412-870a-3cf257d8f78d")))))
    
    (file-systems (cons* (file-system
                           (mount-point "/")
                           (device (uuid
                                    "5307cb84-d017-4990-a877-f491bb886955"
                                    'ext4))
                           (type "ext4"))
			 (file-system
                           (mount-point "/boot/efi")
                           (device (uuid "D160-D22E"
					 'fat32))
                           (type "vfat"))
			 %base-file-systems))))

laptop-operating-system
